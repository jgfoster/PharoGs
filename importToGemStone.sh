#!/bin/bash
if [ ! -d ./output ]; then
  mkdir ./output
fi
rm output/*
topaz -l -q << EOF
input importToGemStone.tpz
EOF
