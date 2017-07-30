# hs-app-template
Application template: Kafka, DataDog, AWS.

## Create new project

```
$ git clone git@github.com:packetloop/hs-app-template.git new_project
$ cd new_project
$ ./untemplate.sh
```

# kafka debug print
Now kafka debug print is configurable as below:
* Set `--log-level` to `LevelDebug`
* Set `--kafka-debug-enable` to comma separated module list, like "broker,protocol". Currently supported modules are "generic, broker, topic, metadata, queue, msg, protocol, cgrp, security, fetch, feature, all".
Like below:
```
${path}/bin/${exe} \
  --kafka-broker localhost:9092 \
  --kafka-group-id local-group \
  --kafka-schema-registry http://localhost:8081 \
  --kafka-poll-timeout 10000 \
  --kafka-debug-enable "broker,protocol" \
  --input-topic attacks-scored \
  --es-server http://localhost:9200 \
  --es-index-prefix attacks \
  --es-index-type attack \
  --batch-size 5 \
  --log-level LevelDebug
```