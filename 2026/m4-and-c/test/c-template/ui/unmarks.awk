#!/usr/bin/awk -f
# 整段收集，保留换行，支持跨行标记
{ buf = buf $0 }
END {
    out = ""
    s = buf
    while (match(s, /<[^<>]*>/)) {
        # 保留 < 之前的内容
        out = out substr(s, 1, RSTART - 1)
        # 只取尖括号内部，丢掉 < 和 >
        out = out substr(s, RSTART + 1, RLENGTH - 2)
        # 跳过整个 <...> 继续
        s = substr(s, RSTART + RLENGTH)
    }
    out = out s
    # 用 printf 避免 awk 额外补换行
    printf "%s", out
}
