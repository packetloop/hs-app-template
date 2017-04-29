#!/bin/bash

PRJ=$PWD
SRC=`mktemp -d 2>/dev/null || mktemp -d -t 'src'`

cd ${SRC}
git clone --depth 1 https://github.com/edenhill/librdkafka librdkafka

cd librdkafka
./configure --prefix $PRJ/docker/rdkafka
cd src
make
make install
sudo cp -r $PRJ/docker/rdkafka/* /usr/local/