# zhe.awk
# 用法: gawk -v namesfile=zhm-names.txt -f zhe.awk input.txt > output.m4

BEGIN {
    if (namesfile == "") {
        print "用法: gawk -v namesfile=zhm-names.txt -f zhe.awk input.txt" > "/dev/stderr"
        exit 1
    }
    while ((getline name < namesfile) > 0) {
        gsub(/^[ \t]+|[ \t]+$/, "", name)
        if (name != "") defined[name] = 1
    }
    close(namesfile)
    depth = 0
    FULL = "\001"
    HALF = "\002"
}

function trim(s) {
    gsub(/^[ \t]+|[ \t]+$/, "", s)
    return s
}

# 扫描一行，跟踪 m4 引号深度；引号内的 ～/~ 换成占位符
function protect_quotes(line,    chars, k, c, out, d) {
    out = ""
    d = depth
    split(line, chars, "")
    for (k = 1; k <= length(chars); k++) {
        c = chars[k]
        if (c == "`") {
            d++
            out = out c
        } else if (c == "'") {
            if (d > 0) d--
            out = out c
        } else if (d > 0 && c == "～") {
            out = out FULL
        } else if (d > 0 && c == "~") {
            out = out HALF
        } else {
            out = out c
        }
    }
    depth = d
    return out
}

function restore(line) {
    gsub(FULL, "～", line)
    gsub(HALF, "~", line)
    return line
}

function process_seg(seg, tilde,    s, inner, parts, n, i, a, name, args) {
    s = trim(seg)
    if (s == "") return tilde tilde

    if (s ~ /^[（(].*[）)]$/) {
        inner = substr(s, 2, length(s) - 2)
        n = split(inner, parts, /[，,]/)
        name = trim(parts[1])
        if (!(name in defined)) return tilde seg tilde
        args = ""
        for (i = 2; i <= n; i++) {
            a = trim(parts[i])
            if (a != "") args = args ", " a
        }
        return "`'indir(`" name "'" args ")`'"
    }

    if (!(s in defined)) return tilde seg tilde
    return "`'indir(`" s "')`'"
}

{
    line = protect_quotes($0)
    out = ""
    while (match(line, /[～~][^～~]*[～~]/)) {
        pre  = substr(line, 1, RSTART - 1)
        seg  = substr(line, RSTART + 1, RLENGTH - 2)
        til  = substr(line, RSTART, 1)
        post = substr(line, RSTART + RLENGTH)
        out = out pre process_seg(seg, til)
        line = post
    }
    out = out line
    print restore(out)
}
