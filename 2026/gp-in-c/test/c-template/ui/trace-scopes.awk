# trace-scopes.awk
#
# 前提：输入已由 preprocess.awk 处理。
# 将真花括号替换为 _begin_scope_ / _end_scope_。
# 占位符不含花括号，不会被处理。

{
    line = $0
    n = length(line)
    out = ""
    for (i = 1; i <= n; i++) {
        c = substr(line, i, 1)
        if (c == "{") out = out "_begin_scope_"
        else if (c == "}") out = out "_end_scope_"
        else out = out c
    }
    print out
}
