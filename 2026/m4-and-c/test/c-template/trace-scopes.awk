# 用法：awk -f trace-scopes.awk foo.c
BEGIN {
    state = "normal"    # normal | string | char | line_comment | block_comment
}
{
    line = $0
    out  = ""
    n    = length(line)
    for (i = 1; i <= n; i++) {
        c  = substr(line, i, 1)
        c2 = substr(line, i + 1, 1)
        if (state == "normal") {
            if (c == "/" && c2 == "/") {
                out = out c c2; i++; state = "line_comment"
            } else if (c == "/" && c2 == "*") {
                out = out c c2; i++; state = "block_comment"
            } else if (c == "\"") {
                out = out c; state = "string"
            } else if (c == "'") {
                out = out c; state = "char"
            } else if (c == "{") {
                out = out "_begin_scope_"
            } else if (c == "}") {
                out = out "_end_scope_"
            } else {
                out = out c
            }
        } else if (state == "string" || state == "char") {
            out = out c
            if (c == "\\") {                 # 处理转义：\ 及紧随其后的字符原样输出
                i++; out = out substr(line, i, 1)
            } else if (state == "string" && c == "\"") {
                state = "normal"
            } else if (state == "char" && c == "'") {
                state = "normal"
            }
        } else if (state == "line_comment") {
            out = out c                     # 行注释整行原样输出
        } else if (state == "block_comment") {
            out = out c
            if (c == "*" && c2 == "/") {     # 块注释结束
                out = out c2; i++; state = "normal"
            }
        }
    }
    print out
    if (state == "line_comment") state = "normal"   # 行注释到行尾自动结束
}
