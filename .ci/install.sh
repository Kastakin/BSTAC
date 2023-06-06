#!/bin/bash

OS=$1

if [[ $OS == "macos" ]]; then
    curl -L https://github.com/QB64Official/qb64/releases/download/v2.1/qb64_dev_2022-09-08-07-14-00_47f5044_osx.tar.gz -o qb64.tar.gz
else
    wget https://github.com/QB64Official/qb64/releases/download/v2.1/qb64_dev_2022-09-08-07-14-00_47f5044_lnx.tar.gz -O qb64.tar.gz
fi
tar -xf qb64.tar.gz
mv qb64_* qb64
cd qb64

if [[ $OS == "macos" ]]; then
    ./setup_osx.command
else
    ./setup_lnx.sh
fi
