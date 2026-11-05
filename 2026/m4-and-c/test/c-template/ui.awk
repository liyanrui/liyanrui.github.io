BEGIN {
    in_template = 0
    t_name = ""
    in_func = 0
    f_name = ""
}
# ---------- 正常状态 ----------
!in_template && !in_func {
    line = $0
    # --- @func_template ---
    fp = index(line, "@func_template(")
    if (fp > 0) {
        prefix = substr(line, 1, fp - 1)
        rest   = substr(line, fp)
        p1 = index(rest, "(")
        q1 = index(rest, ")")
        if (q1 == 0) { print line; next }
        name = substr(rest, p1 + 1, q1 - p1 - 1)
        if (name !~ /^[A-Za-z_][A-Za-z0-9_]*$/) { print line; next }
        after_name = substr(rest, q1 + 1)
        p2 = index(after_name, "(")
        if (p2 != 1) { print line; next }
        depth = 0
        args_end = 0
        for (i = 1; i <= length(after_name); i++) {
            c = substr(after_name, i, 1)
            if (c == "(") depth++
            else if (c == ")") {
                depth--
                if (depth == 0) { args_end = i - 1; break }
            }
        }
        if (args_end == 0) { print line; next }
        args = substr(after_name, 2, args_end - 2 + 1)
        rest2 = substr(after_name, args_end + 2)
        sub(/^[ \t]+/, "", rest2)
        if (substr(rest2, 1, 1) != "{") { print line; next }
        tail = substr(rest2, 2)
        sub(/[ \t]+$/, "", tail)
        printf "divert(-1)\n"
        printf "%s", prefix
        printf "define(`%s_type', ``%s'_with($@)(%s) {", name, name, args
        if (tail != "") printf "%s", tail
        printf "\n"
        f_name = name
        in_func = 1
        next
    }
    # --- @template ---
    tp = index(line, "@template(")
    if (tp > 0) {
        prefix = substr(line, 1, tp - 1)
        rest   = substr(line, tp)
        p = index(rest, "(")
        q = index(rest, ")")
        if (q > p) {
            name = substr(rest, p + 1, q - p - 1)
            after = substr(rest, q + 1)
            sub(/^[ \t]+/, "", after)
            if (substr(after, 1, 1) == "{" && name ~ /^[A-Za-z_][A-Za-z0-9_]*$/) {
                tail = substr(after, 2)
                sub(/[ \t]+$/, "", tail)
                printf "divert(-1)\n"
                printf "%s", prefix
                printf "define(`%s_type', `typedef struct {", name
                if (tail != "") printf "%s", tail
                printf "\n"
                t_name = name
                in_template = 1
                next
            }
        }
    }
    print line
    next
}
# ---------- @func_template 块体 ----------
in_func {
    line = $0
    stripped = line
    sub(/^[ \t]+/, "", stripped)
    if (substr(stripped, 1, 1) == "}") {
        pos = index(line, "}")
        brace_prefix = substr(line, 1, pos - 1)
        suffix = substr(line, pos + 1)
        printf "%s}\n", brace_prefix
        printf "dnl')%s\n", suffix
        printf "divert(0)dnl\n"
        in_func = 0
        f_name = ""
        next
    }
    printf "%s\n", line
}

# ---------- @template 块体 ----------
in_template {
    line = $0
    stripped = line
    sub(/^[ \t]+/, "", stripped)
    if (substr(stripped, 1, 1) == "}") {
        pos = index(line, "}")
        brace_prefix = substr(line, 1, pos - 1)
        suffix = substr(line, pos + 1)
        printf "%s} `$0'_with($@)')\n", brace_prefix
        printf "define(`%s', `$0_`type'_with($@)')%s\n", t_name, suffix
        printf "divert(0)dnl\n"
        in_template = 0
        t_name = ""
        next
    }
    printf "%s\n", line
}
END {
    if (in_template)
        printf "ERROR: unterminated @template(%s) block\n", t_name > "/dev/stderr"
    if (in_func)
        printf "ERROR: unterminated @func_template(%s) block\n", f_name > "/dev/stderr"
}
