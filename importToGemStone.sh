#!/bin/bash
if [ ! -d ./output ]; then
  mkdir ./output
fi
if [ -f output/Object.out ]; then
  rm output/*
fi
topaz -l -q << EOF
input importToGemStone.tpz
EOF
