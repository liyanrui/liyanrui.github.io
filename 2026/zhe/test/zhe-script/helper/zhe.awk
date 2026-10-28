BEGIN {
    while ((getline name < names) > 0) {
        # 去除首尾空白字符
        gsub(/^[ \t]+|[ \t]+$/, "", name)
        # 将宏名加入中文宏表
        # 需要查表时，用 name in zh_macros 表达式判断中文宏是否有定义
        if (name != "") zh_macros[name] = 1
    }
    # 逃逸深度，值为 0 表示非逃逸状态
    esc = 0
    # 宏调用语句嵌套深度，0 表示无嵌套
    depth = 0
}
function process_macros(a,
                        # 局部变量
                        b, n, i, x, c) {
    b = ""; n = length(a); i = 1
    while (i <= n) {
        x = substr(a, i, 1)
        if (x == "`") {
            esc++; b = b x; i++
        } else if (x == "'") {
            if (esc > 0) esc--
            b = b x; i++
        } else {
            if (esc > 0) {
                b = b x; i++
            } else {
                if (x == "~" || x == "～") {
                    c = x
                    if (i == n) break
                    i++; x = substr(a, i, 1); c = c x
                    if (x == "(" || x == "（") {
                        depth = 0
                        while (i < n) {
                            i++; x = substr(a, i, 1); c = c x
                            if (x == "~" || x == "～") {
                                if (i == n) break
                                x = substr(a, i + 1, 1)
                                # 试探之后的内容是否为嵌套的有参数宏
                                if (x == "(" || x == "（") {
                                    depth++
                                    c = c x
                                    i++
                                }
                                continue
                            }
                            if (x == ")" || x == "）") {
                                if (i == n) break
                                i++; x = substr(a, i, 1); c = c x
                                if (x == "~" || x == "～") {
                                    if (depth == 0) {
                                        c = expand_zh_macro(c)
                                        break
                                    }
                                    if (depth > 0) depth--
                                }
                            }
                        }
                    } else {
                        while (i < n) {
                            i++; x = substr(a, i, 1); c = c x
                            if (x == "~" || x == "～") {
                                c = expand_zh_macro(c)
                                break
                            }
                        }
                    }
                    b = b c
                    i++
                } else {
                    b = b x; i++
                }
            }
        }
    }
    return b
}
function expand_zh_macro(c,
                         # 局部变量
                         macro, m, n, i, j, x, parts, depth, args) {
    m = length(c)
    # 去掉宏调用语句两侧的 ~ 符号
    macro = substr(c, 2, m - 2); m -= 2
    if (macro) {
        x = substr(macro, 1, 1)
        if (x == "(" || x == "（") {
            # 去除两侧括号
            macro = substr(macro, 2, m - 2); m -= 2
            if (macro) {
                # 分割宏名与参数
                n = 1; depth = 0
                for (i = 1; i <= m; i++) {
                    x = substr(macro, i, 1)
                    if (x == "(" || x == "（") {
                        depth++
                        parts[n] = parts[n] x
                    } else if (x == ")" || x == "）") {
                        if (depth > 0) depth--
                        parts[n] = parts[n] x
                    } else if (x == "," || x == "，") {
                        if (depth == 0) n++
                        else parts[n] = parts[n] x
                    } else parts[n] = parts[n] x
                }
                # 去掉宏名与参数两侧的空白字符
                for (i = 2; i <= n; i++) {
                    gsub(/^[ \t]+|[ \t]+$/, "", parts[i])
                }
                # 展开有参数中文宏
                if (parts[1] in zh_macros) {
                    args = ""
                    for (i = 2; i <= n; i++) {
                        # 处理嵌套的中文宏调用语句
                        args = args ", `" process_macros(parts[i]) "'"
                    }
                    macro = "`'indir(`" parts[1] "'" args ")`'"
                } else macro = c
            }
        } else {
            gsub(/^[ \t]+|[ \t]+$/, "", macro)
            macro = "`'indir(`" macro "')`'"
        }
    }
    return macro
}
{ print process_macros($0) }