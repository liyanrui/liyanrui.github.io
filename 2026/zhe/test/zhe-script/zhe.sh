#!/bin/bash
set -euo pipefail # 提升脚本健壮性
ZHE_SELF_PATH="$(cd "$(dirname "$0")" && pwd)"
cat "$ZHE_SELF_PATH"/zhdefine.m4 > zhe-meta.m4
cat $1 >> zhe-meta.m4
echo "divert(0)dnl" >> zhe-meta.m4
m4 zhe-meta.m4 > zhmdefs.out
gawk -f "$ZHE_SELF_PATH/split.awk" zhmdefs.out
gawk -v namesfile=zhm-names.txt -f "$ZHE_SELF_PATH"/zhe.awk "$2" > zhe-output.m4
m4 zhm-defs.m4 zhe-output.m4
rm -f zhe-meta.m4 zhmdefs.out zhm-names.txt zhm-defs.m4 zhe-output.m4