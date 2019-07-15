#!/bin/sh
if [ ! -d pharo ]; then
  mkdir -p pharo
fi
if [ ! -d pharo/Pharo.app ]; then
  cp -r ~/Documents/Pharo/vms/80-x86/Pharo.app ./pharo
  cp ~/code/pharo/pharo-vm/PharoV60.sources ./pharo
fi
cp ~/code/pharo/bootstrap-cache/Pharo8.0-SNAPSHOT-metacello-32bit-*.changes ./pharo/Pharo8.0.changes
cp ~/code/pharo/bootstrap-cache/Pharo8.0-SNAPSHOT-metacello-32bit-*.image ./pharo/Pharo8.0.image
alias Pharo='./pharo/Pharo.app/Contents/MacOS/Pharo --headless ./pharo/Pharo8.0.image'

if [ ! -d classes ]; then
  mkdir -p classes
fi
if [ -f classes/Object.gs ]; then
  rm classes/*
fi
if [ -f output/Object.out ]; then
  rm output/*
fi
Pharo patches.st
Pharo globals.st
Pharo pools.st
Pharo classes.st
Pharo methods.st