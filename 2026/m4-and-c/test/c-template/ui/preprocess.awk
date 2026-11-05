# preprocess.awk
#
# 用法：awk -f preprocess.awk foo.c
#
# 1. 删除 C 注释 // 和 /* */
# 2. 字符串 "..." 内的 { } 替换为占位符
# 3. 字符字面量 '{' '}' 整体替换为占位符
# 4. m4 引号 ` ' 按普通字符处理
BEGIN {
    in_block_comment = 0
    OPEN_STR  = "\\001???001!!!"
    CLOSE_STR = "\\002???002!!!"
    OPEN_CHR  = "\\003???003!!!"
    CLOSE_CHR = "\\004???004!!!"
}
{
    line = $0
    out = ""
    n = length(line)
    state = "normal"
    for (i = 1; i <= n; i++) {
        c  = substr(line, i, 1)
        c2 = substr(line, i + 1, 1)
        c3 = substr(line, i + 2, 1)
        if (state == "normal") {
            if (in_block_comment) {
                if (c == "*" && c2 == "/") { in_block_comment = 0; i++ }
            } else if (c == "/" && c2 == "/") {
                break
            } else if (c == "/" && c2 == "*") {
                in_block_comment = 1; i++
            } else if (c == "\"") {
                state = "string"; out = out c
            } else if (c == "'" && c2 == "{" && c3 == "'") {
                out = out OPEN_CHR; i += 2
            } else if (c == "'" && c2 == "}" && c3 == "'") {
                out = out CLOSE_CHR; i += 2
            } else {
                out = out c
            }
        } else if (state == "string") {
            if (c == "\\") {
                out = out c
                if (i < n) { out = out substr(line, i + 1, 1); i++ }
            } else if (c == "\"") {
                out = out c; state = "normal"
            } else if (c == "{") {
                out = out OPEN_STR
            } else if (c == "}") {
                out = out CLOSE_STR
            } else {
                out = out c
            }
        }
    }
    print out
}
