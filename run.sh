#!/bin/bash

set -e

export CLUB_NAME=${CLUB_NAME:-$USER}
export KAFKA_HOST=${KAFKA_HOST:-localhost}

exe=$(cat package.yaml | grep -A 1 executables: | tail -n 1 | cut -d : -f 1 | xargs)
echo "Running: $exe"

stack build
path=$(stack path --local-install-root)

${path}/bin/${exe} \
  --kafka-broker ${KAFKA_HOST}:9092 \
  --kafka-group-id ${CLUB_NAME}--hs-app-template-${USER} \
  --kafka-schema-registry http://${KAFKA_HOST}:8081 \
  --input-topic ${CLUB_NAME}--hs-app-template-input \
  --kafka-poll-timeout-ms 10000 \
  --kafka-debug-enable "broker,protocol" \
  --log-level LevelInfo
