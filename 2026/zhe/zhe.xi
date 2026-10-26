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
BEGIN { names = "zhm-names.txt"; defs = "zhm-def.m4" }
/^@zh-macro: / {
    if (!($2 in seen)) { seen[$2] = 1; print $2 > names }
    next
}
{ print > defs }
@

假设，zhdefine-test.m4 展开结果为 zhdefine-test.out 文件，则

```console
$ awk -f split.awk zhdefine-test.out
```

可产生文件 zhm-names.txt 和 zhm-defs.m4，前者只含有中文宏名，后者含有中文宏的定义。