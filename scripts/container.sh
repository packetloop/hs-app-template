#!/bin/bash

BUILD_NAME=$(basename $PWD)
BUILD_BINTRAY=arbornetworks-docker-docker.bintray.io
FORK_BUILD=true && [ "${CIRCLE_PROJECT_USERNAME:-}" = "packetloop" ] && [ "${CIRCLE_BRANCH:-}" = "master" ] && FORK_BUILD=false
BUILD_VERSION=$(cat *.cabal | grep -e "^version" | tr -s " " | cut -d' ' -f2)
BUILD_HASH=$(git rev-list --count HEAD) && ${FORK_BUILD} && BUILD_HASH="${BUILD_HASH}-$(git rev-parse --short HEAD)"
IMAGE_NAME=${BUILD_BINTRAY}/${BUILD_NAME}_${BUILD_VERSION}
BUILD_TAG=${IMAGE_NAME}:${BUILD_HASH}

_bintray_url="https://arbornetworks.bintray.com/maven-private/"
_bintray_username="$(grep '^user\>' "$HOME/.bintray/.credentials" | cut -d = -f 2 | xargs)"
_bintray_password="$(grep '^password\>' "$HOME/.bintray/.credentials" | cut -d = -f 2 | xargs)"
_bintray_credentials="${_bintray_username}:${_bintray_password}"

# check if login exists
cat "${HOME}/.docker/config.json" | grep "\"${BUILD_BINTRAY}\"" > /dev/null
if [ $? -ne 0 ]; then
  set +e
  docker login -e ${BINTRAY_EMAIL} -u ${BINTRAY_USER} -p ${BINTRAY_API_KEY} ${BUILD_BINTRAY}
  set -e
fi


case $1 in
  dev-build)
    set +e
    _image=fpco/stack-build:latest
    _img=$(docker images ${_image} | tail -n +2)

    if [ -z "${_img}" ]; then
      docker pull $_image
    fi
    stack --docker --docker-image=${_image} build
    docker build -t ${BUILD_TAG} .
    docker tag ${BUILD_TAG} ${IMAGE_NAME}:latest
    set -e
  ;;

  build)
    docker build -t ${BUILD_TAG} .
    docker tag ${BUILD_TAG} ${IMAGE_NAME}:latest
  ;;

  run)
    docker run --rm -v ${PWD}:/opt/src -it ${IMAGE_NAME}:latest
  ;;

  test)
    if [ -z ${CIRCLE_TEST_REPORTS+x} ]; then
      docker run --rm ${BUILD_TAG} "/test.sh";
    else
      docker run --rm -v ${CIRCLE_TEST_REPORTS}:/opt/test-results ${BUILD_TAG} "/test.sh";
    fi;
  ;;

  push)
    docker push ${BUILD_TAG}
    echo "Pushed: ${BUILD_TAG}"
  ;;
esac
