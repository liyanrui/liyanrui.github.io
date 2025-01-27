#!/bin/bash
NAME=best-lmd
orez -w ${NAME}.orz -o t-${NAME}.orz
sed -i 's/ESC_LEFT_ANGLE */</g;
        s/ *ESC_RIGHT_ANGLE/>/g;
        s/ESC_AT */@/g' t-${NAME}.orz
orez-md t-${NAME}.orz > ${NAME}.md
rm t-${NAME}.orz
