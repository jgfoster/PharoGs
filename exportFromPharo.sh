#!/bin/sh

set -e -x
pwd

if [ ! -d pharo ]; then   # create Pharo directory
  echo "./pharo directory not found. Please clone from GitHub or create a symbolic link to a clone."
  exit 1
fi

if [ ! -e ./pharo/bootstrap-cache/PharoV60.sources ]; then
  cp ./pharo/bootstrap-cache/Pharo*.sources ./pharo/bootstrap-cache/PharoV60.sources
fi

rm -rf classes output PharoGs.tpz PharoGs.out PharoDebug.log
mkdir classes output

./pharo/bootstrap-downloads/vmtarget/pharo --headless \
  ./pharo/bootstrap-cache/Pharo*-metacello-*.image exportFromPharo.st
