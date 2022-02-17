#!/bin/env bash

set -xe

HOST=${HOST:-0.0.0.0}
PORT=${PORT:-8080}
PROXY=${PROXY:-http://localhost:9000}

docker run -it --rm \
    -e HOST=$HOST \
    -e PORT=$PORT \
    -e PROXY=$PROXY \
    --network=host \
    -v $PWD:/app \
    --workdir /app \
    --entrypoint npm \
    node:17-alpine $@