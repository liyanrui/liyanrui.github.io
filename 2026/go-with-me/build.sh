#!/bin/bash
awk -f escape.awk go-with-me.xi \
    | xi -c ~/.xi/mkd.conf -w -o index.md  && lmd build
