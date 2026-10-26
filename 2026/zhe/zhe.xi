---
title: zhe 脚本
subtitle: 
date: 2026 年 10 月 26 日
...

# m4 语言

写文档或者写程序代码时，难免会有所修改。若内容中存在多处相同的修改，可利用所有文本编辑器都会提供的查找/替换功能批量完成修改，例如将所有的 `foo_1` 修改为 `foo-1`。

更好的文本编辑器会提供基于正则表达式的查找/替换功能，从而批量修改一些相似的内容，例如将 `foo_1`，`foo_2`……修改为 `foo-1`，`foo-2`……

若想基于待修改的相似内容中的部分文本作出不同的内容替换，例如我想将所有的 `foo`，按其在内容中的出现次序，依次替换为 `foo-1`，`foo-2`……这种任务对于多数文本编辑器而言通常是超纲的，需要编写脚本程序来解决，然而还有另一种可能更为节约的方案，用宏编程语言编写代码，完成文本替换。

GNU m4 是微小但完备的宏语言，倘若在其 `-1` 流中定义宏 `foo`，

@ foo.m4 # [m4]
divert(-1)dnl
define(`_N_', 0)
define(`foo', `define(`_N_', incr(_N_))`foo'-_N_')
divert(0)dnl
@

假设有一段话，

@ foo.txt # [text]
佛曰 foo，即非 foo，是名 foo。
@

m4 解释器读入 foo.m4 和 foo.txt 并处理它们，

```console
$ m4 foo.m4 foo.txt
```

相当于将 foo.m4 作用于 foo.txt，使得 foo.txt 中的 `foo` 皆以 foo.m4 中 `foo` 宏的定义展开，从而形成

```
佛曰 foo-1，即非 foo-2，是名 foo-3。
```

m4 宏可以有参数，这意味着 m4 语言能够完成更为复杂的文本替换任务。例如，定义宏 `start` 和 `stop`，

@ start-stop.m4 #
divert(-1)
define(`_start_', `pushdef(`block_name', $1)\`start'$1')
define(`_stop_', `\`stop'`'block_name`'popdef(`block_name')')
divert(0)dnl
@

若对

@ ctx-foo.tex # [tex]
\start(text)
... ... ...
\start(itemize)
\item ... ... ...
\item ... ... ...
\stop
\stop
@

执行

```
$ sed -E 's/\\(start|stop)\>/_\1_/g' ctx-foo.tex | m4 start-stop.m4 -
```

结果为

```TEX
\starttext
... ... ...
\startitemize
\item ... ... ...
\item ... ... ...
\stopitemize
\stoptext
```

上述命令之所以用 GNU sed 命令将 ctx-foo.tex 中的 `\start` 和 `\stop` 分别转换为 `_start_` 和 `_stop_`，是因为 m4 宏名不允许使用除下划线、字母和数字之外的符号。这也意味着，宏名无法使用汉字，亦即 m4 语言不支持定义中文宏。

# 中文 m4 宏

若说 m4 语言不支持中文宏，有些绝对了，它实际上间接支持非法宏名，例如

@ zh-macro-demo.m4 # [m4]
divert(-1)dnl
define(`正文开始', `\starttext')
define(`正文结束', `\stoptext')
divert(0)dnl
indir(`正文开始')
... ... ...
indir(`正文结束')
@

执行

```console
$ m4 zh-macro-demo.m4
```

得到

```
\starttext
... ... ...
\stoptext
```

只是在现实中，若使用 `indir` 调用中文宏，则宏的简约性将荡然无存。为了让中文宏能够像普通的 m4 宏那样调用，需要像上一节使用 sed 命令对含有宏调用的文本预处理为合法的 m4 宏名，只是 sed 难以胜任此事，我考虑使用 GNU awk 语言为 m4 文件编写一个预处理脚本，将中文宏调用语句转化为合法的 `indir` 形式，该脚本名为 zhe。

# m4 元文件

定义 `zhdefine` 宏，

@ zhdefine 宏 # [m4]
define(`zhdefine', `divert(0)@zh-macro: $1
divert(1)`define(`$1', `$2')'')dnl
@

`zhdefine` 专用于定义中文宏，宏名会发送到 0 号流作为输出，例如

@ zhdefine-test.m4 #
# zhdefine 宏 @
zhdefine(`正文开始', `\starttext')
zhdefine(`正文结束', `\stoptext')
@

我将像 zhdefine-test.m4 的 m4 文件称为 zhe 脚本的元文件，对其执行

```console
$ m4 zhdefine-test.m4
```

可得

```
@zh-macro: 正文开始
@zh-macro: 正文结束
define(`正文开始', \starttext)
define(`正文结束', \stoptext)
```

在 `zhdefine` 的定义中，将宏名写入 0 号流，将宏定义写入 1 号流。m4 解释器先将 1 号流的内容汇入至 0 号流，然后再输出 0 号流，故而上述示例所得结果中，先开始出现的是宏名，之后是宏的定义。

使用 awk 语言可将 zhdefine-test.m4 展开结果分割为两个文件，一个文件存储中文宏名，一个文件存储中文宏定义，

@ split.awk # [awk]
BEGIN { names = "zhm-names.txt"; defs = "zhm-defs.m4"; print "divert(-1)" > defs }
/^@zh-macro:/ {
    name = $0
    sub(/^@zh-macro:[[:space:]]*/, "", name)
    sub(/[[:space:]]+$/, "", name)
    if (name != "" && !(name in seen)) { seen[name] = 1; print name > names }
    next
}
{ print > defs }
END { print "divert(0)dnl" > defs }
@

假设，zhdefine-test.m4 展开结果为 zhdefine-test.out 文件，则

```console
$ awk -f split.awk zhdefine-test.out
```

可产生文件 zhm-names.txt 和 zhm-defs.m4，前者只含有中文宏名，后者含有中文宏的定义。

# 中文宏调用语法

m4 宏调用语法贴合英文，无参数宏是一个单词，有参数宏是一个单词后面紧跟带括号的参数表，在视觉上很容易分辨普通文字和宏语句，但是对于中文而言，没有单词，无论是无参数宏还是有参数宏，都不容易分辨出中文宏调用，必须为其构造新的语法。

中文宏，其调用语法是宏名前后出现全角或半角 `~` 符号，参数以全角或半角逗号间隔，例如

```m4
zhdefine(`中文宏 1', `无参数的中文宏')
zhdefine(`中文宏 2', `参数为「$1」和「$2」的中文宏')dnl
这是～中文宏 1～的调用
这是～（中文宏 2，啊，嗷）～的调用
```

应被展开为

```
这是无参数的中文宏调用
这是参数为「啊」和「嗷」的中文宏调用
```

需要编写一个 awk 脚本，扫描文本，将符合上述语法的中文宏调用语句转化为 `indir` 形式，例如

```m4
这是`'indir(`中文宏 1')`'的调用
这是`'indir(`中文宏 2', 啊, 嗷)`'的调用
```

若文本中需要对一些宏调用语句作逃逸处理，可沿用 m4 默认的引号 <code>`</code> 和 <code>'</code>。

# zhe 脚本

我将上述内容发给 DeepSeek，它很给力，一蹴而就，写出了中文宏调用语句的转换脚本，

@ zhe.awk # [awk]
# 用法: gawk -v namesfile=zhm-names.txt -f zhe.awk input.txt > output.m4
BEGIN {
    if (namesfile == "") {
        print "用法: gawk -v namesfile=zhm-names.txt -f zhe.awk input.txt" > "/dev/stderr"
        exit 1
    }
    while ((getline name < namesfile) > 0) {
        gsub(/^[ \t]+|[ \t]+$/, "", name)
        if (name != "") defined[name] = 1
    }
    close(namesfile)
    depth = 0
    FULL = "\001"
    HALF = "\002"
}
function trim(s) {
    gsub(/^[ \t]+|[ \t]+$/, "", s)
    return s
}
# 扫描一行，跟踪 m4 引号深度；引号内的 ～/~ 换成占位符
function protect_quotes(line,    chars, k, c, out, d) {
    out = ""
    d = depth
    split(line, chars, "")
    for (k = 1; k <= length(chars); k++) {
        c = chars[k]
        if (c == "`") {
            d++
            out = out c
        } else if (c == "'") {
            if (d > 0) d--
            out = out c
        } else if (d > 0 && c == "～") {
            out = out FULL
        } else if (d > 0 && c == "~") {
            out = out HALF
        } else {
            out = out c
        }
    }
    depth = d
    return out
}
function restore(line) {
    gsub(FULL, "～", line)
    gsub(HALF, "~", line)
    return line
}
function process_seg(seg, tilde,    s, inner, parts, n, i, a, name, args) {
    s = trim(seg)
    if (s == "") return tilde tilde

    if (s ~ /^[（(].*[）)]$/) {
        inner = substr(s, 2, length(s) - 2)
        n = split(inner, parts, /[，,]/)
        name = trim(parts[1])
        if (!(name in defined)) return tilde seg tilde
        args = ""
        for (i = 2; i <= n; i++) {
            a = trim(parts[i])
            if (a != "") args = args ", " a
        }
        return "`'indir(`" name "'" args ")`'"
    }

    if (!(s in defined)) return tilde seg tilde
    return "`'indir(`" s "')`'"
}
{
    line = protect_quotes($0)
    out = ""
    while (match(line, /[～~][^～~]*[～~]/)) {
        pre  = substr(line, 1, RSTART - 1)
        seg  = substr(line, RSTART + 1, RLENGTH - 2)
        til  = substr(line, RSTART, 1)
        post = substr(line, RSTART + RLENGTH)
        out = out pre process_seg(seg, til)
        line = post
    }
    out = out line
    print restore(out)
}
@


# 封装

将 split.awk 和 zhe.awk 脚本封装在 bash 脚本里，

@ zhe.sh # [bash]
#!/bin/bash
set -euo pipefail # 提升脚本健壮性
ZHE_SELF_PATH="$(cd "$(dirname "$0")" && pwd)"
cat "$ZHE_SELF_PATH"/zhdefine.m4 > zhe-meta.m4
cat $1 >> zhe-meta.m4
echo "divert(0)dnl" >> zhe-meta.m4
m4 zhe-meta.m4 > zhmdefs.out
gawk -f "$ZHE_SELF_PATH/split.awk" zhmdefs.out
gawk -v namesfile=zhm-names.txt -f "$ZHE_SELF_PATH"/zhe.awk "$2" > zhe-output.m4
m4 zhm-defs.m4 zhe-output.m4
rm -f zhe-meta.m4 zhmdefs.out zhm-names.txt zhm-defs.m4 zhe-output.m4
@