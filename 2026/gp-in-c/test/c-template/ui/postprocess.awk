# postprocess.awk
#
# 将占位符换回原字符。按字面匹配，不用正则。

BEGIN {
    OPEN_STR  = "\\001???001!!!"
    CLOSE_STR = "\\002???002!!!"
    OPEN_CHR  = "\\003???003!!!"
    CLOSE_CHR = "\\004???004!!!"
}

function replace_all(s, from, to,    out, p) {
    out = ""
    while ((p = index(s, from)) > 0) {
        out = out substr(s, 1, p - 1) to
        s = substr(s, p + length(from))
    }
    return out s
}

{
    line = $0
    line = replace_all(line, OPEN_STR,  "{")
    line = replace_all(line, CLOSE_STR, "}")
    line = replace_all(line, OPEN_CHR,  "'{'")
    line = replace_all(line, CLOSE_CHR, "'}'")
    print line
}
