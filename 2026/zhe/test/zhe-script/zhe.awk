# 用法: gawk -v namesfile=zhm-names.txt -f zhe.awk input.txt > output.m4
BEGIN {
    if (namesfile == "") {
        print "用法: awk -v namesfile=zhm-names.txt -f zhe.awk input.txt" > "/dev/stderr"
        exit 1
    }
    while ((getline name < namesfile) > 0) {
        gsub(/^[ \t]+|[ \t]+$/, "", name)
        if (name != "") defined[name] = 1
    }
    close(namesfile)
    depth = 0
}
function trim(s) {
    gsub(/^[ \t]+|[ \t]+$/, "", s)
    return s
}
function contains_quote(s) {
    return index(s, "`") || index(s, "'")
}
# 按逗号分割参数，跳过括号内的逗号
function split_args(s, parts,    n, i, c, d, cur) {
    n = 0; d = 0; cur = ""
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "（" || c == "(") { d++; cur = cur c }
        else if (c == "）" || c == ")") { if (d > 0) d--; cur = cur c }
        else if ((c == "，" || c == ",") && d == 0) { parts[++n] = cur; cur = "" }
        else cur = cur c
    }
    parts[++n] = cur
    return n
}
function process_line(line,    out, k, c, n, seg, tilde, paren, nc) {
    out = ""
    n = length(line)
    k = 1
    while (k <= n) {
        c = substr(line, k, 1)
        if (c == "`") {
            depth++
            out = out c
            k++
        } else if (c == "'") {
            if (depth > 0) depth--
            out = out c
            k++
        } else if (depth == 0 && (c == "～" || c == "~")) {
            tilde = c
            k++
            seg = ""
            paren = 0
            while (k <= n) {
                nc = substr(line, k, 1)
                if (paren == 0 && (nc == "～" || nc == "~")) break
                if (nc == "（" || nc == "(") paren++
                else if (nc == "）" || nc == ")") { if (paren > 0) paren-- }
                seg = seg nc
                k++
            }
            if (k <= n) {
                out = out process_seg(seg, tilde)
                k++
            } else {
                out = out tilde seg
            }
        } else {
            out = out c
            k++
        }
    }
    return out
}
function process_seg(seg, tilde,    s, inner, parts, n, i, a, name, args) {
    s = trim(seg)
    if (s == "") return tilde tilde
    if (contains_quote(s)) return tilde seg tilde

    if (s ~ /^[（(].*[）)]$/) {
        inner = substr(s, 2, length(s) - 2)
        n = split_args(inner, parts)
        name = trim(parts[1])
        if (!(name in defined)) return tilde seg tilde
        args = ""
        for (i = 2; i <= n; i++) {
            a = process_line(trim(parts[i]))
            args = args ", `" a "'"
        }
        return "`'indir(`" name "'" args ")`'"
    }

    if (!(s in defined)) return tilde seg tilde
    return "`'indir(`" s "')`'"
}
{
    print process_line($0)
}