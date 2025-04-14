#!/bin/bash

for dir in $(find . -type d); do
  if [ -f "$dir/index.md" ]; then
    echo "处理目录: $dir"
    (cd "$dir" && lmd build) 
  fi
done
