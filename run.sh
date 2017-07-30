#!/bin/bash

exe=$(cat $(basename $PWD).cabal | grep executable | head -n 1 | cut -d' ' -f2)
echo "Running: $exe"

stack build
path=$(stack path --local-install-root)

${path}/bin/${exe} \
  --kafka-broker localhost:9092 \
  --kafka-group-id local-group \
  --kafka-schema-registry http://localhost:8081 \
  --commands-topic commands \
  --kafka-poll-timeout-ms 10000 \
  --kafka-debug-enable "broker,protocol" \
  --log-level LevelDebug
