FROM fpco/ubuntu-with-libgmp:14.04
MAINTAINER Tyler Durden <mayhem@arbor.net>

ADD docker/set-environment.sh /usr/local/bin/set-environment

RUN apt-get install -y jq && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ADD rdkafka.tar.gz /usr/local/

ENV LOG_LEVEL INFO
ENV LD_LIBRARY_PATH /usr/local/lib

ADD docker/start.sh /start.sh
ADD docker/test.sh /test.sh
ADD .stack-work/install/x86_64-linux*/*/*/bin/* /usr/local/bin/

# REQUIRE STATSD_HOST                      1.1.1.1
# REQUIRE STATSD_TAGS                      tags
# REQUIRE STATSD_SAMPLE_RATE               0.1

# REQUIRE COMMANDS_TOPIC_IN                string

# REQUIRE AWS_REGION                       string

# REQUIRE KAFKA_POLL_TIMEOUT               1.0
# REQUIRE KAFKA_GROUP_ID                   group-id
# REQUIRE KAFKA_BOOTSTRAP                  hostnames
# REQUIRE KAFKA_SCHEMA_REGISTRY            url 


ENV LOG_LEVEL INFO

CMD ["/start.sh"]
