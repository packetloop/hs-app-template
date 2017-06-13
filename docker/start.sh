#!/bin/bash

echo "Starting service"

. set-environment

export GATEWAY_IP=$(ip route | grep default | cut -d ' ' -f 3)
export STATSD_HOST=${STATSD_HOST:-$GATEWAY_IP}

/usr/local/bin/set-environment

set -x
hs-app-template \
  ${AWS_REGION+                       --region                            "${AWS_REGION}"                       } \
  ${KAFKA_BROKER+                     --kafka-broker                      "${KAFKA_BROKER}"                     } \
  ${KAFKA_GROUP_ID+                   --kafka-group-id                    "${KAFKA_GROUP_ID}"                   } \
  ${KAFKA_SCHEMA_REGISTRY+            --kafka-schema-registry             "${KAFKA_SCHEMA_REGISTRY}"            } \
  ${KAFKA_POLL_TIMEOUT+               --kafka-poll-timeout                "${KAFKA_POLL_TIMEOUT}"               } \
  ${KAFKA_QUEUED_MAX_MESSAGES_KBYTES+ --kafka-queued-max-messages-kbytes  "${KAFKA_QUEUED_MAX_MESSAGES_KBYTES}" } \
  ${COMMANDS_TOPIC_IN+                --commands-topic                    "${COMMANDS_TOPIC_IN}"                } \
  ${STATSD_HOST+                      --statsd-host                       "${STATSD_HOST}"                      } \
  ${STATSD_SAMPLE_RATE+               --statsd-sample-rate                "${STATSD_SAMPLE_RATE}"               } \
  ${STATSD_TAGS+                      --statsd-tags                       "${STATSD_TAGS}"                      } \
  ${LOG_LEVEL+                        --log-level                         "${LOG_LEVEL}"                        }
