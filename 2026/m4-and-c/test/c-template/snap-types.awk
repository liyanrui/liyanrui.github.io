#!/usr/bin/awk -f
{ buf = buf $0 "\n"}
END {
    n = 0
    s = buf
    while (match(s, /<[^<>]*>/)) {
        ty = substr(s, RSTART + 1, RLENGTH - 2)
        # 只去首尾空白，内部原样保留
        sub(/^[ \t\r\n]+/, "", ty)
        sub(/[ \t\r\n]+$/, "", ty)
        n++
        item[n] = "_def_type(`" ty "')indir(`_" ty "_type_')"
        s = substr(s, RSTART + RLENGTH)
    }
    SEP = "`_'"
    out = ""
    for (i = 1; i <= n; i++) {
        if (i > 1) out = out SEP
        out = out item[i]
    }
    printf "%s", out
}
