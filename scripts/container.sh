#!/bin/bash

BUILD_NAME=$(cat *.cabal | grep -e "^name" | tr -s " " | cut -d' ' -f2)
BUILD_BINTRAY=arbornetworks-docker-v2.bintray.io

BRANCH_NAME=${CIRCLE_BRANCH:-$(git rev-parse --abbrev-ref HEAD)}
BUILD_PROJECT_USER=${CIRCLE_PROJECT_USERNAME:-${USER}}
BUILD_USER=${CIRCLE_USERNAME:-${USER}}
BUILD_REPO_NAME=$(git remote | head | xargs git remote get-url)
BUILD_NUM=${CIRCLE_BUILD_NUM}
GIT_HASH=$(git rev-parse --short HEAD)
GIT_SHA1=${CIRCLE_SHA1:-$(git rev-parse HEAD)}
GIT_UNCLEAN=true && [ "$(git status -s)" = "" ] && GIT_UNCLEAN=false

FORK_BUILD=true && [ "${BUILD_PROJECT_USER:-}" = "packetloop" ] && [ "${BRANCH_NAME:-}" = "master" ] && FORK_BUILD=false
BUILD_VERSION=$(cat *.cabal | grep -e "^version" | tr -s " " | cut -d' ' -f2)
BUILD_HASH=$(git rev-list --count HEAD) && ${FORK_BUILD} && BUILD_HASH="${BUILD_HASH}-${GIT_HASH}"
IMAGE_NAME=${BUILD_BINTRAY}/${BUILD_NAME}_${BUILD_VERSION}
BUILD_TAG=${IMAGE_NAME}:${BUILD_HASH}

_bintray_url="https://arbornetworks.bintray.com/maven-private/"
_bintray_username="$(grep '^user\>' "$HOME/.bintray/.credentials" | cut -d = -f 2 | xargs)"
_bintray_password="$(grep '^password\>' "$HOME/.bintray/.credentials" | cut -d = -f 2 | xargs)"
_bintray_credentials="${_bintray_username}:${_bintray_password}"

# check if login exists
cat "${HOME}/.docker/config.json" | grep "\"${BUILD_BINTRAY}\"" > /dev/null
if [ $? -ne 0 ]; then
  set -e
  docker login -u ${BINTRAY_USER} -p ${BINTRAY_API_KEY} ${BUILD_BINTRAY}
  set +e
fi

case $1 in
  dev-build)
    set -e
    _image="fpco/stack-build:latest"
    docker run --rm -v $PWD:/bld -it $_image bash -c "cd /bld && ./scripts/build-librdkafka.sh"
    echo "Building the project"
    stack --docker \
          --docker-env="LD_LIBRARY_PATH=/usr/lib/librdkafka/" \
          --docker-mount="$PWD/.librdkafka/lib:/usr/lib/librdkafka" \
          --docker-mount="$PWD/.librdkafka/include/librdkafka:/usr/include/librdkafka" \
          --docker-image=$_image build \
          --extra-include-dirs "/usr/include/librdkafka" \
          --extra-lib-dirs "/usr/lib/librdkafka" \
          --extra-lib-dirs "$PWD/.librdkafka/lib" \


    echo "Building the container"
    docker build --tag ${BUILD_TAG} --tag ${IMAGE_NAME}:latest \
                 --label appName="${BUILD_NAME}" \
                 --label appVersion="${BUILD_VERSION}" \
                 --label buildUser="${BUILD_USER}" \
                 --label buildNum="${BUILD_NUM}" \
                 --label gitRepo="${BUILD_REPO_NAME}" \
                 --label gitSha1="${GIT_SHA1}" \
                 --label gitHash="${GIT_HASH}" \
                 --label gitBranch="${BRANCH_NAME}" \
                 --label buildUncommitted=$GIT_UNCLEAN \
                 .
    echo "Built: ${BUILD_TAG}"
    echo "Built: ${IMAGE_NAME}:latest"
    set +e
  ;;

  build)
    docker build --tag ${BUILD_TAG} --tag ${IMAGE_NAME}:latest \
                 --label appName="${BUILD_NAME}" \
                 --label appVersion="${BUILD_VERSION}" \
                 --label buildUser="${BUILD_USER}" \
                 --label buildNum="${BUILD_NUM}" \
                 --label gitRepo="${BUILD_REPO_NAME}" \
                 --label gitSha1="${GIT_SHA1}" \
                 --label gitHash="${GIT_HASH}" \
                 --label gitBranch="${BRANCH_NAME}" \
                 --label buildUncommitted=$GIT_UNCLEAN \
                 .
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