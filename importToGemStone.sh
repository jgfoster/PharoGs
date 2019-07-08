#!/bin/bash
if ( ! -d ./output ); then
  mkdir ./output
fi
topaz -l -q << EOF
input importToGemStone.gs
EOF
