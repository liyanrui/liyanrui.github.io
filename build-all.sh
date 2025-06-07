#!/bin/bash

for dir in $(find 2025 -type d); do
  if [ -f "$dir/index.md" ]; then
    echo "处理目录: $dir"
    (cd "$dir" && lmd build) 
  fi
done
