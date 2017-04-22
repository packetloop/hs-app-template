#!/bin/bash

PRJ=$PWD
SRC=`mktemp -d 2>/dev/null || mktemp -d -t 'src'`

cd ${SRC}
git clone --depth 1 https://github.com/edenhill/librdkafka librdkafka

cd librdkafka
./configure --prefix /tmp/rdkafka
cd src
make
make install
tar zcf $PRJ/rdkafka.tar.gz -C /tmp/rdkafka .
sudo tar zxf $PRJ/rdkafka.tar.gz -C /usr/local/