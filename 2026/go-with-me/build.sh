#!/bin/bash
sed 's/</\&lt;/g; s/>/\&gt;/g' go-with-me.xi \
    | xi -c ~/.xi/mkd.conf -w -o index.md
sed -i 's/_ESC_LT_/</g; s/_ESC_GT_/>/g' index.md
lmd build
