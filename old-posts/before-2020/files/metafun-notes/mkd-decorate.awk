BEGIN {
    PROCINFO["sorted_in"] = "@ind_num_asc"
    def_pat = "\\\\cite\\[([^\\]]+)\\]{([^}]+)}"
    ref_pat = "\\\\cite\\[([^\\]]+)\\]"
    note_pat = "\\\\note{([^}]+)}"
    i = j = 1
}
{
    # 文献引用 -> [编号]
    while (t = def_or_ref(s)) {
        if (t == 1) {
            tags[i] = s[1]
            citations[i] = s[2]
            sub(def_pat, " **<sup>[" i++ "]</sup>** ", $0)
        } else {
            d = tag_already_existed(s[1], tags)
            if (d >= 0) sub(ref_pat, " **<sup>[" d "]</sup>** ", $0)
            else {
                print "Error: " NR ": " s[0] "不正确的引用！"
                exit
            }
        }
    }

    # 多个文献序号相邻时，去掉冗余的微型空格「U+200A」
    gsub(/  +/, " ", $0)

    # 注解 -> 上标
    current_note = j
    while (x = match($0, note_pat, s)) {
        notes[j] = s[1]
        sub(note_pat, " **<sup>注 " j++ "</sup>** ", $0)
    }

    # 输出处理后的段落及注解列表
    print $0
    if (j > current_note) {  # 段后增加注解
        print ""
        it = current_note
        while (it < j) {
            print "> **注 " it "**：" notes[it]
            it++
        }
    }
}
END {
    if (i > 1) {
        print "\n----\n"
        print "**引用的文献：**\n"
        for (it in citations) {
            print "[" it "]&nbsp;&nbsp;" citations[it]
        }
    }
}

########

# 0: 匹配失败 | 1: 新的引用 | 2: 使用已有的引用
function def_or_ref(s) {
    if (match($0, def_pat, s)) return 1
    else if (match($0, ref_pat, s)) return 2
    else return 0
}

function tag_already_existed(t, tags,    i) {
    for (i in tags) {
        if (t == tags[i]) return i
    }
    return -1
}
