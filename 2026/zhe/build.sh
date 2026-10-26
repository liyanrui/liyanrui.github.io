#!/bin/bash
xi -c ~/.xi/mkd.conf -w -o index.md zhe.xi \
  && lmd build
