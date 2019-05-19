#!/bin/sh
if [ ! -d classes ]; then
  mkdir -p classes;
fi
alias Pharo='~/Documents/Pharo/vms/70-x86/Pharo.app/Contents/MacOS/Pharo --headless ~/Documents/Pharo/images/Pharo7.0.3-minimal/Pharo7.0.3-minimal.image'
Pharo globals.st
Pharo pools.st
Pharo classes.st
Pharo methods.st
