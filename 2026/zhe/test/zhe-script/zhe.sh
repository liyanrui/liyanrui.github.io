#!/bin/bash
set -euo pipefail
if [ $# -ne 2 ]; then
    echo "用法: $0 <宏定义.m4> <输入文本>" >&2
    exit 1
fi
for cmd in m4 awk; do
    command -v "$cmd" >/dev/null 2>&1 || { echo "缺少命令: $cmd" >&2; exit 1; }
done

ZHE_SELF_PATH="$(cd "$(dirname "$0")" && pwd)"
INPUT="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT
cat "$ZHE_SELF_PATH/zhdefine.m4" > "$TMPDIR/zhe-meta.m4"
cat "$1" >> "$TMPDIR/zhe-meta.m4"
echo "divert(0)dnl" >> "$TMPDIR/zhe-meta.m4"
m4 "$TMPDIR/zhe-meta.m4" > "$TMPDIR/zhmdefs.out"
(
    cd "$TMPDIR"
    awk -f "$ZHE_SELF_PATH/split.awk" zhmdefs.out
    awk -v names=zhm-names.txt -f "$ZHE_SELF_PATH/zhe.awk" "$INPUT" > zhe-output.m4
    m4 zhm-defs.m4 zhe-output.m4
)
