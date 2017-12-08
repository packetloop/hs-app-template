FROM ubuntu:16.04
LABEL maintainer="Tyler Durden <mayhem@arbor.net>"

ADD docker/set-environment.sh /usr/local/bin/set-environment

RUN apt-get update && \
    apt-get install -y jq libgmp10 libssl1.0.0 iproute netbase ca-certificates && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY .librdkafka/ /usr/

ENV LOG_LEVEL LevelInfo

ADD docker/start.sh /start.sh
ADD docker/test.sh /test.sh
ADD .stack-work/install/x86_64-linux*/*/*/bin/* /usr/local/bin/

# REQUIRE STATSD_HOST                         1.1.1.1
# REQUIRE STATSD_TAGS                         tags
# REQUIRE STATSD_SAMPLE_RATE                  0.1

# REQUIRE COMMANDS_TOPIC_IN                   string

# REQUIRE AWS_REGION                          string

# REQUIRE LOG_LEVEL                           LevelDebug

# REQUIRE KAFKA_POLL_TIMEOUT_MS               1000
# REQUIRE KAFKA_GROUP_ID                      group-id
# REQUIRE KAFKA_BROKER                        hostnames
# REQUIRE KAFKA_SCHEMA_REGISTRY               url

# OPTIONAL BATCH_SIZE                         600
# OPTIONAL KAFKA_QUEUED_MAX_MESSAGES_KBYTES   100000
# OPTIONAL KAFKA_CONSUMER_COMMIT_PERIOD_SEC   60

CMD ["/start.sh"]
