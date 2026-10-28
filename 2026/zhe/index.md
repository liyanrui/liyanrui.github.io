---
title: zhe 脚本
subtitle: 
date: 2026 年 10 月 26 日
...

本文档是文学程序 [lmd.xi](lmd.xi) 的文档编织结果，要看懂它，前提是需要对文学编程工具 xi 有所了解。以下文档

* [概览](https://zhuanlan.zhihu.com/p/2096395349250269589)
* [片段抽取（tangle）](https://zhuanlan.zhihu.com/p/2096539422770648315)
* [文档编织（weave）](https://zhuanlan.zhihu.com/p/2096703312229840628)
* [杂项](https://zhuanlan.zhihu.com/p/2096738038923773483)

是 xi 用户指南。若只是为了看懂本文档，并从中提取脚本代码用作试验，则上述文档只需阅读前两篇。

本文档实现的程序是一个中文宏展开器，它可将特定形式的中文文本转化为 m4 宏，由 m4 解释器展开。

# m4 语言

写文档或者写程序代码时，难免会有所修改。若内容中存在多处相同的修改，可利用所有文本编辑器都会提供的查找/替换功能批量完成修改，例如将所有的 `foo_1` 修改为 `foo-1`。

更好的文本编辑器会提供基于正则表达式的查找/替换功能，从而批量修改一些相似的内容，例如将 `foo_1`，`foo_2`……修改为 `foo-1`，`foo-2`……

若想基于待修改的相似内容中的部分文本作出不同的内容替换，例如我想将所有的 `foo`，按其在内容中的出现次序，依次替换为 `foo-1`，`foo-2`……这种任务对于多数文本编辑器而言通常是超纲的，需要编写脚本程序来解决，然而还有另一种可能更为节约的方案，用宏编程语言编写代码，完成文本替换。

GNU m4 是微小但完备的宏语言，倘若在其 `-1` 流中定义宏 `foo`，

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-1"> foo.m4 </span><span class="snippet-name-stop">#</span>
divert(-1)dnl
define(`_N_', 0)
define(`foo', `define(`_N_', incr(_N_))`foo'-_N_')
divert(0)dnl
</pre>

假设有一段话，

<pre class="text-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-2"> foo.txt </span><span class="snippet-name-stop">#</span>
佛曰 foo，即非 foo，是名 foo。
</pre>

m4 解释器读入 foo.m4 和 foo.txt 并处理它们，

```console
$ m4 foo.m4 foo.txt
```

相当于将 foo.m4 作用于 foo.txt，使得 foo.txt 中的 `foo` 皆以 foo.m4 中 `foo` 宏的定义展开，从而形成

```
佛曰 foo-1，即非 foo-2，是名 foo-3。
```

m4 宏可以有参数，这意味着 m4 语言能够完成更为复杂的文本替换任务。例如，定义宏 `_start_` 和 `_stop_`，

<pre class="${language}-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-3"> start-stop.m4 </span><span class="snippet-name-stop">#</span>
divert(-1)
define(`_start_', `pushdef(`block_name', $1)\`start'$1')
define(`_stop_', `\`stop'`'block_name`'popdef(`block_name')')
divert(0)dnl
</pre>

若对

<pre class="tex-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-4"> ctx-foo.tex </span><span class="snippet-name-stop">#</span>
\start(text)
... ... ...
\start(itemize)
\item ... ... ...
\item ... ... ...
\stop
\stop
</pre>

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

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-5"> zh-macro-demo.m4 </span><span class="snippet-name-stop">#</span>
divert(-1)dnl
define(`正文开始', `\starttext')
define(`正文结束', `\stoptext')
divert(0)dnl
indir(`正文开始')
... ... ...
indir(`正文结束')
</pre>

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

# 元信息

定义 `zhdefine` 宏，

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-6"> zhdefine.m4 </span><span class="snippet-name-stop">#</span>
define(`zhdefine', `divert(0)@zh-macro: $1
`'`'divert(1)`define(`$1', `$2')'')dnl
=><span class="caller"><a class="snippet-reference" href="#xi-7"> zhdefine-test.m4 </a></span>
</pre>

在 m4 宏定义中，若宏有多行定义，可以用引号形成的空文本完成缩进，从而让宏定义在视觉上好看一些。`zhdefine` 专用于定义中文宏，宏名会发送到 0 号流作为输出，例如

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-7"> zhdefine-test.m4 </span><span class="snippet-name-stop">#</span>
<span class="snippet-name-start">#</span><a class="snippet-reference" href="#xi-6"> zhdefine.m4 </a><span class="snippet-name-stop">@</span>
zhdefine(`正文开始', `\starttext')
zhdefine(`正文结束', `\stoptext')
</pre>

执行

```console
$ m4 zhdefine-test.m4
```

可得

```
zh-macro: 正文开始
zh-macro: 正文结束
define(`正文开始', `\starttext')
define(`正文结束', `\stoptext')
```

在 `zhdefine` 的定义中，将宏名写入 0 号流，将宏定义写入 1 号流。m4 解释器先将 1 号流的内容汇入至 0 号流，然后再输出 0 号流，故而上述示例所得结果中，先开始出现的是宏名，之后是宏的定义。

使用 awk 语言可将 zhdefine-test.m4 展开结果分割为两个文件，一个文件存储中文宏名，一个文件存储中文宏定义，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-8"> split.awk </span><span class="snippet-name-stop">#</span>
BEGIN {
    names = "zhm-names.txt"
    defs = "zhm-defs.m4"
    printf "" > names
    print "divert(-1)" > defs
}
/^@zh-macro:/ {
    name = $0
    sub(/^@zh-macro:[[:space:]]*/, "", name)
    sub(/[[:space:]]+$/, "", name)
    if (name != "" && !(name in seen)) {
        seen[name] = 1
        print name > names
    }
    next
}
{ print > defs }
END { print "divert(0)dnl" > defs }
</pre>

假设，将 zhdefine-test.m4 文件展开为 zhdefine-test.out 文件，然后执行 split.awk 脚本，

```console
$ m4 zhdefine-test.m4 > zhdefine-test.out 
$ awk -f split.awk zhdefine-test.out
```

可产生文件 zhm-names.txt 和 zhm-defs.m4，前者只含有中文宏名，后者含有中文宏的定义。像 zhm-names.txt 这样的文件，称为中文宏的元文件。

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

若文本中需要对一些宏调用语句作逃逸处理，可沿用 m4 默认的引号 <code>`</code> 和 <code>'</code>。

# zhe.awk 脚本

> 我将本文档上述内容发给 DeepSeek，它很给力，近乎一蹴而就，写出了中文宏调用语句的转换脚本，事后还能从工程视角对其修缮和增强。不过，诸如徒步 318 这类事情，路上搭车，终究是自欺欺人。有时候，我走路，是为了熟悉路如何走，而不是必须快速去往某个地方。

在 zhe.awk 脚本的准备阶段，需要读取中文宏的元文件，建立中文宏表，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-9"> zhe.awk </span><span class="snippet-name-stop">#</span>
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
</pre>

awk 语言默认以行为单元处理输入文件，zhe.awk 自然也是如此。假设一行文本 `a`，函数 `translate_macros` 将其转化为文本 `b`，使得 `a` 中所有中文宏调用语句在 `b` 中皆为 `indir` 调用形式。`process_macros` 大致结构为

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-10"> zhe.awk </span><span class="snippet-name-stop">#</span> +
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
                    <span class="snippet-name-start">#</span><a class="snippet-reference" href="#xi-11"> 进入中文宏调用语句处理状态 </a><span class="snippet-name-stop">@</span>
                } else {
                    b = b x; i++
                }
            }
        }
    }
    return b
}
</pre>

当 `a` 中当前字符 `x` 为全角或半角的 `~` 符号时，便意味着需要探索前方内容可能是一个有参数的中文宏调用，也可能是一个无参数的中文宏调用，也可能并非中文宏调用，这三种可能皆需要处理，这三种情况的处理结果皆存储于 `c` 中，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-11"> 进入中文宏调用语句处理状态 </span><span class="snippet-name-stop">#</span>
c = x
if (i == n) break
i++; x = substr(a, i, 1); c = c x
if (x == "(" || x == "（") {
    <span class="snippet-name-start">#</span><a class="snippet-reference" href="#xi-12"> 进入有参数中文宏调用语句处理状态 </a><span class="snippet-name-stop">@</span>
} else {
    <span class="snippet-name-start">#</span><a class="snippet-reference" href="#xi-13"> 进入无参数中文宏调用语句处理状态 </a><span class="snippet-name-stop">@</span>
}
b = b c
i++
=><span class="caller"><a class="snippet-reference" href="#xi-10"> zhe.awk </a></span>
</pre>

有参数宏调用语句由于存在嵌套的可能，亦即存在宏参数也是宏调用语句的情况，故而在探索 `x` 前方是够能构成中文宏调用语句的过程中，需要跟踪宏调用的嵌套深度，在遇到宏调用结束标记时，只有深度为 0 时，方能构成完整的宏调用语句，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-12"> 进入有参数中文宏调用语句处理状态 </span><span class="snippet-name-stop">#</span>
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
=><span class="caller"><a class="snippet-reference" href="#xi-11"> 进入中文宏调用语句处理状态 </a></span>
</pre>

无参数中文宏调用语句不存在宏嵌套调用情况，故而只需要遇到下一个全角或半角 `~` 符号，便可捕捉到一条宏调用语句，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-13"> 进入无参数中文宏调用语句处理状态 </span><span class="snippet-name-stop">#</span>
while (i < n) {
    i++; x = substr(a, i, 1); c = c x
    if (x == "~" || x == "～") {
        c = expand_zh_macro(c)
        break
    }
}
=><span class="caller"><a class="snippet-reference" href="#xi-11"> 进入中文宏调用语句处理状态 </a></span>
</pre>

无论所捕捉的是有参数的还是无参数的中文宏，都要发送给 `expand_zh_macro` 将其处理为 `indir` 形式，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-14"> zhe.awk </span><span class="snippet-name-stop">#</span> +
function expand_zh_macro(c,
                         # 局部变量
                         macro, m, n, i, j, x, parts, depth, args) {
    m = length(c)
    # 去掉宏调用语句两侧的 ~ 符号
    macro = substr(c, 2, m - 2); m -= 2
    if (macro) {
        x = substr(macro, 1, 1)
        if (x == "(" || x == "（") {
            <span class="snippet-name-start">#</span><a class="snippet-reference" href="#xi-15"> 展开有参数中文宏调用语句 </a><span class="snippet-name-stop">@</span>
        } else {
            <span class="snippet-name-start">#</span><a class="snippet-reference" href="#xi-16"> 展开无参数中文宏调用语句 </a><span class="snippet-name-stop">@</span>
        }
    }
    return macro
}
</pre>

有参数的中文宏调用语句，展开过程中需要分割宏名与参数，若参数中含有中文宏调用语句，则递归执行 `process_macros` 函数将其展开，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-15"> 展开有参数中文宏调用语句 </span><span class="snippet-name-stop">#</span>
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
=><span class="caller"><a class="snippet-reference" href="#xi-14"> zhe.awk </a></span>
</pre>

已经去除了两侧全角或半角 `~` 符号的无参数的中文宏调用语句，只需去除两侧空白字符，然后以 `indir` 调用即可，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-16"> 展开无参数中文宏调用语句 </span><span class="snippet-name-stop">#</span>
gsub(/^[ \t]+|[ \t]+$/, "", macro)
macro = "`'indir(`" macro "')`'"
=><span class="caller"><a class="snippet-reference" href="#xi-14"> zhe.awk </a></span>
</pre>

只需将 `process_macros` 函数作用于输入文本的每一行，便可完成中文宏调用语句的处理，

<pre class="awk-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-17"> zhe.awk </span><span class="snippet-name-stop">#</span> +
{ print process_macros($0) }
</pre>

# zhe.sh 脚本

zhe.sh 脚本为 zhe.awk 脚本提供了简单的封装，由 DeepSeek 写就，

<pre class="bash-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-18"> zhe.sh </span><span class="snippet-name-stop">#</span>
#!/bin/bash
set -euo pipefail
if [ $# -ne 2 ]; then
    echo "用法: $0 <宏定义.m4> <输入文本>" >&2
    exit 1
fi
for cmd in m4 awk; do
    command -v "$cmd" >/dev/null 2>&1 || { echo "缺少命令: $cmd" >&2; exit 1; }
done

ZHE_SELF_PATH="$(cd "$(dirname "$0")" && pwd)"
INPUT="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT
cat "$ZHE_SELF_PATH/zhdefine.m4" > "$TMPDIR/zhe-meta.m4"
cat "$1" >> "$TMPDIR/zhe-meta.m4"
echo "divert(0)dnl" >> "$TMPDIR/zhe-meta.m4"
m4 "$TMPDIR/zhe-meta.m4" > "$TMPDIR/zhmdefs.out"
(
    cd "$TMPDIR"
    awk -f "$ZHE_SELF_PATH/split.awk" zhmdefs.out
    awk -v names=zhm-names.txt -f "$ZHE_SELF_PATH/zhe.awk" "$INPUT" > zhe-output.m4
    m4 zhm-defs.m4 zhe-output.m4
)
</pre>

假设 split.awk、zhdefine.m4、zhe.awk 以及 zhe.sh 文件位于 `$HOME/opt/zhe` 目录，为 zhe.sh 设置可执行权限，

```console
$ chmod +x $HOME/opt/zhe/zhe.sh
```

假设中文宏定义文件 demo.m4 为

<pre class="${language}-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-19"> demo.m4 </span><span class="snippet-name-stop">#</span>
zhdefine(`中文宏 1', `无参数的中文宏')
zhdefine(`中文宏 2', `参数为「$1」和「$2」的中文宏')
</pre>

在文本文件 input.txt 调用了 demo.m4 中定义的中文宏，

<pre class="${language}-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-20"> input.txt </span><span class="snippet-name-stop">#</span>
这是～中文宏 1～的调用
这是～(中文宏 2，啊，嗷)～的调用
这是逃逸`～(中文宏 2，啊，嗷)～'的调用
</pre>

执行

```console
$ $HOME/opt/zhe/zhe.sh demo.m4 input.txt
```

结果为

```
这是无参数的中文宏的调用
这是参数为「啊」和「嗷」的中文宏的调用
这是逃逸～(中文宏 2，啊，嗷)～的调用
```

# 试验 1

验证 zhe.awk 能否处理有参数中文宏与无参数中文宏嵌套调用情况，将

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-21"> demo-1.m4 </span><span class="snippet-name-stop">#</span>
zhdefine(`开始', `pushdef(`block_name', $1)\`start'$1')
zhdefine(`结束', `\`stop'`'block_name`'popdef(`block_name')')
zhdefine(`正文', `text')
zhdefine(`列表', `itemize')
zhdefine(`项', `\item')
</pre>

作用于

<pre class="tex-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-22"> input-1.tex </span><span class="snippet-name-stop">#</span>
～（开始，～正文～）～
... ... ...
～（开始，～列表～）～
～项～ ... ... ...
～项～ ... ... ...
～结束～
～结束～
</pre>

结果为

```tex
\starttext
... ... ...
\startitemize
\item ... ... ...
\item ... ... ...
\stopitemize
\stoptext
```

# 试验 2

验证 zhe.awk 能否处理中文宏具有一般性的嵌套情况，即中文宏参数是中文宏调用。先定义三个中文宏，

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-23"> demo-2.m4 </span><span class="snippet-name-stop">#</span>
zhdefine(`内层', `【内层】')
zhdefine(`外层', `外层收到($1)')
zhdefine(`三层', `三层[$1|$2|$3]')
</pre>

将其作用于

<pre class="${language}-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-24"> input-2.txt </span><span class="snippet-name-stop">#</span>
A: ～内层～
B: ～(外层，～内层～)～
C: ～(外层，～(外层，～(外层，～内层～)～)～)～
D: ～(三层，a，，b)～
E: ～(外层，普通文本)～
F: ～(未定义，～内层～)～
</pre>

所得结果为

```
A: 【内层】
B: 外层收到(【内层】)
C: 外层收到(外层收到(外层收到(【内层】)))
D: 三层[a||b]
E: 外层收到(普通文本)
F: ～(未定义，～内层～)～
```
