BEGIN {
    FS = ":"
    while (getline <"c-color.map" > 0) {
        gsub(/[ \t]+/, "", $1) # 去除特殊标记的前后空白字符
        gsub(/[ \t]+/, "", $2) # 去除颜色名的前后空白字符
        color[$1] = $2
    }
    FS = " "
    basic_types = "char|double|enum|float|int|long|short|signed|struct|union|unsigned|void|const"
    keywords = "static|typedef|sizeof|break|case|continue|default|do|else|for|goto|if|return|switch|while"
}
/\\starttyping/ { typing = 1; print; next}
typing && /\/\*/ {
    if (!/\*\//) in_comment = 1
    gsub(/\/\*.*/, "/BTEX\\color[" color["comment"] "]{&}/ETEX")
    $0 = gensub(/\\m{([^}]+)}/, "\\\\m{\\1}", "g") # 数学公式
    print; next
}
typing && in_comment {
    if (/\*\//) in_comment = 0
    gsub(/[^ \t].*/, "/BTEX\\color[" color["comment"] "]{&}/ETEX")
    $0 = gensub(/\\m{([^}]+)}/, "\\\\m{\\1}", "g") # 数学公式
    print; next
}
typing {
    # 渲染函数名
    $0 = gensub(/\\fn{([^}]+)}/, "/BTEX\\\\color[" color["\\fn"] "]{\\1}/ETEX", "g")
    # 渲染函数参数类型
    $0 = gensub(/\\t{([^}]+)}/, "/BTEX\\\\color[" color["\\t"] "]{\\1}/ETEX", "g")
    # 渲染函数参数
    $0 = gensub(/\\p{([^}]+)}/, "/BTEX\\\\color[" color["\\p"] "]{\\1}/ETEX", "g")
    # 渲染语句内嵌入的注释
    $0 = gensub(/\\c{([^}]+)}/, "/BTEX\\\\color[" color["\\c"] "]{/* \\1 */}/ETEX", "g")
    # 渲染字符串常量
    if (/"[^"]*"/) {
        # 处理反斜线
        gsub(/\\/, "\\backslash ")
        gsub(/"[^"]*"/, "/BTEX\\color[" color["string"] "]{&}/ETEX")
    }
    # 渲染基本类型
    gsub("\\<(" basic_types ")\\>", "/BTEX\\color[" color["basic_type"] "]{&}/ETEX")
    # 渲染关键词
    gsub("\\<(" keywords ")\\>", "/BTEX\\color[" color["keyword"] "]{&}/ETEX")
    print; next
}
/\\stoptyping/ { typing = 0; print; next}
{ print }
