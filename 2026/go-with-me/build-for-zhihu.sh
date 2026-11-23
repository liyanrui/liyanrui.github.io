#!/bin/bash
awk -f escape.awk go-with-me.xi \
    | xi -c ~/.xi/mkd-for-zhihu.conf -w -o zhihu.md
sed -i 's/&lt;/</g; s/&gt;/>/g; s/&amp;/\&/g' zhihu.md
