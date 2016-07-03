#!/usr/bin/env bash

if [ $NODE_ENV = "production" ]; then
  npm install babel-cli babel-preset-es2015-node6 babel-preset-stage-2
  npm run build
fi
