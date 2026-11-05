# template-to-m4.awk
#
# 前提：输入已由 preprocess.awk 处理。
# 只识别 @template / @func_template，只数真花括号。
BEGIN {
    in_template = 0; t_name = ""
    in_func = 0; f_name = ""
    brace_depth = 0
    scan_pos = 0
}

function count_braces(text,    i, n, c, out) {
    n = length(text)
    out = ""
    scan_pos = 0
    for (i = 1; i <= n; i++) {
        c = substr(text, i, 1)
        if (c == "{") {
            brace_depth++
            out = out c
        } else if (c == "}") {
            brace_depth--
            if (brace_depth == 0) { scan_pos = i; break }
            out = out c
        } else {
            out = out c
        }
    }
    return out
}

!in_template && !in_func {
    line = $0
    tp = index(line, "@template(")
    fp = index(line, "@func_template(")

    if (fp > 0 && (tp == 0 || fp < tp)) {
        prefix = substr(line, 1, fp - 1)
        rest   = substr(line, fp)
        p1 = index(rest, "("); q1 = index(rest, ")")
        if (q1 == 0) { print line; next }
        name = substr(rest, p1 + 1, q1 - p1 - 1)
        if (name !~ /^[A-Za-z_][A-Za-z0-9_]*$/) { print line; next }
        after = substr(rest, q1 + 1)
        if (substr(after, 1, 1) != "(") { print line; next }
        depth = 0; args_end = 0
        for (i = 1; i <= length(after); i++) {
            c = substr(after, i, 1)
            if (c == "(") depth++
            else if (c == ")") { depth--; if (depth == 0) { args_end = i - 1; break } }
        }
        if (args_end == 0) { print line; next }
        args = substr(after, 2, args_end - 1)
        rest2 = substr(after, args_end + 2)
        sub(/^[ \t]+/, "", rest2)
        if (substr(rest2, 1, 1) != "{") { print line; next }
        tail = substr(rest2, 2); sub(/[ \t]+$/, "", tail)

        brace_depth = 1
        out = count_braces(tail)

        printf "divert(-1)\n"
        printf "%s", prefix
        printf "define(`%s_func', ``%s'_with($@)(%s) {", name, name, args
        printf "%s", out
        if (scan_pos > 0) {
            suffix = substr(tail, scan_pos + 1)
            printf "}\n"
            printf "dnl')\n"
            printf "define(`%s', ``$0'_with($@)')%s\n", name, suffix
            printf "divert(0)dnl\n"
        } else {
            printf "\n"
            f_name = name; in_func = 1
        }
        next
    }

    if (tp > 0) {
        prefix = substr(line, 1, tp - 1)
        rest   = substr(line, tp)
        p = index(rest, "("); q = index(rest, ")")
        if (q == 0) { print line; next }
        name = substr(rest, p + 1, q - p - 1)
        if (name !~ /^[A-Za-z_][A-Za-z0-9_]*$/) { print line; next }
        after = substr(rest, q + 1); sub(/^[ \t]+/, "", after)
        if (substr(after, 1, 1) != "{") { print line; next }
        tail = substr(after, 2); sub(/[ \t]+$/, "", tail)

        brace_depth = 1
        out = count_braces(tail)

        printf "divert(-1)\n"
        printf "%s", prefix
        printf "define(`%s_type', `typedef struct {", name
        printf "%s", out
        if (scan_pos > 0) {
            suffix = substr(tail, scan_pos + 1)
            printf "} `$0'_with($@)')\n"
            printf "define(`%s', `$0_`type'_with($@)')%s\n", name, suffix
            printf "divert(0)dnl\n"
        } else {
            printf "\n"
            t_name = name; in_template = 1
        }
        next
    }

    print line
    next
}

in_func {
    line = $0
    out = count_braces(line)
    if (scan_pos > 0) {
        suffix = substr(line, scan_pos + 1)
        printf "%s}\n", out
        printf "dnl')\n"
        printf "define(`%s', ``$0'_with($@)')%s\n", f_name, suffix
        printf "divert(0)dnl\n"
        in_func = 0; f_name = ""
    } else {
        print out
    }
    next
}

in_template {
    line = $0
    out = count_braces(line)
    if (scan_pos > 0) {
        suffix = substr(line, scan_pos + 1)
        printf "%s} `$0'_with($@)')\n", out
        printf "define(`%s', `$0_`type'_with($@)')%s\n", t_name, suffix
        printf "divert(0)dnl\n"
        in_template = 0; t_name = ""
    } else {
        print out
    }
    next
}

END {
    if (in_template) printf "ERROR: unterminated @template(%s)\n", t_name > "/dev/stderr"
    if (in_func)     printf "ERROR: unterminated @func_template(%s)\n", f_name > "/dev/stderr"
}
