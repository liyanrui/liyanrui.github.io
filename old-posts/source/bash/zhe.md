---
title: zhe：有时想写中文宏……
lang: zh-CN
date: 2023 年 05 月 19 日
abstract: 用于简化中文 m4 宏的展开。
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

zhe 是使用 bash 语言编写的脚本，可用于简化中文 m4 宏的展开。除 bash 和 m4 之外，执行 zhe 还需要 awk。

使用 zhe 脚本的前提是对 m4 语言有所了解。我曾经写过一份 GNU m4 学习笔记「让这世界再多一份 GNU m4 教程 [(1)](https://segmentfault.com/a/1190000004104696) [(2)](https://segmentfault.com/a/1190000004108113) [(3)](https://segmentfault.com/a/1190000004128102) [(4)](https://segmentfault.com/a/1190000004131031) [(5)](https://segmentfault.com/a/1190000004137562)」可供参考。

![zhe][01]


# 安装

假设在 bash 环境里，将 zhe 脚本安装在 $HOME/opt/zhe 目录，可参考以下步骤：

```bash
$ mkdir -p $HOME/opt
$ cd $HOME/opt
$ git clone https://github.com/liyanrui/zhe.git
$ echo 'export PATH=$HOME/opt/zhe:$PATH' >> $HOME/.bashrc
```

在任意目录下，执行以下命令可确认 zhe 脚本是否可用：

```bash
$ zhe -h
```

上述命令应当输出：

```
Usage: zhe [OPTION] FILE
Opitons:
  -i file	include file provides m4 macro definitions used in FILE.
  -o file	output result to file.
```

# 用法

假设文件 macros.m4 中定义了一些以汉字或标点符号命名的 m4 宏，例如

```m4
divert(-1)
define(`将进酒', `「君不见，黄河之水天上来……」')
define(`。', `.')
divert(0)dnl
```

假设文件 foo.txt 中使用了 macros.m4 中定义的宏，例如

```m4
@宏开
你听说过将进酒吗？我认为这是一首最优秀的唐诗。
```

使用以下命令可将 foo.txt 中位于 `@宏开` 之后的内容展开：

```bash
$ zhe -i macros.m4 foo.txt
你听说过「君不见，黄河之水天上来……」吗？我认为这是一首最优秀的唐诗.
```

上述示例表现了 `@宏开` 之后的语句中的 `将进酒` 和中文句号分别被展开为 `「君不见，黄河之水天上来……」` 和英文句号。

与 `@宏开` 相反，`@宏闭` 可令 zhe 忽略语句中的宏。例如

```m4
@宏闭
你听说过将进酒吗？我认为这是一首最优秀的唐诗。
```

zhe 命令对上述内容的处理结果为

```m4
你听说过将进酒吗？我认为这是一首最优秀的唐诗。
```

使用 `-o` 选项可将 zhe 的输出保存到指定的文件。例如

```bash
$ zhe -i macros.m4 -o foo-output.txt foo.txt
```

可将 zhe 的输出结果保存到与 foo.txt 位于同一目录下的文件 foo-output.txt 中。

# 宏的调用

zhe（基于 Awk 脚本）改变了 m4 宏的调用语法。对于无参数宏，如上一节示例里的两个宏，以普通文字的形式出现即可，亦可以中文括号（全角括号）囊括的形式出现，例如

```
你听说过（将进酒）吗？我认为这是一首最优秀的唐诗。
```

中文括号形式的宏调用主要是面向有参数的宏，调用语法为

```m4
（宏名，参数 1，参数 2，……）
```

注意，宏名以及参数之间的逗号，皆为中文逗号（全角逗号）。例如，在 macros.m4 文件中定义一个有参数的宏

```m4
define(`测试', `这是测试 $1。')
```

在 foo.txt 中可以像下面这样调用 `测试` 宏：

```
有参数的宏：（测试，2）
```

上述语句在 `@宏开` 模式下，可被 zhe 展开为

```
有参数的宏：这是测试 2。
```

# 缘由

m4 的宏调用语法是迎合英文习惯，英文单词之间是有空格作为间隔的，在视觉上容易分辨出普通文字和宏调用语句的区别。例如

```
when we say test(3), what are we talking about?
```

中文字符之间通常没有空格作为间隔，若采用 m4 宏的调用语法，固然可行，但是对于含有宏调用的语句，其可读性将大打折扣。例如，

```
当我们说测试（3）的时候，我们在说什么？
```

故而，zhe 修改了有参数的中文宏的调用方式（借鉴了 Lisp 函数调用），将宏名移到括号内，且为了避免频繁切换输入法以输入英文括号，逗号和引号，中文宏调用语句使用的标点符号皆为全角标点。

# 引号

m4 的引号可用于字符转义——将特殊字符转为普通字符。zhe 与之类似，只是使用中文单引号代替了 m4 的 \` 和 '。例如，以下语句

```m4
有参数的宏：‘（测试，2）’
```

其中宏调用 `（测试，2）` 会被引号转义为普通文字，zhe 不予展开，故而 zhe 对上述语句的处理结果为

```m4
有参数的宏：（测试，2）
```

# 示例

除了简单的文本替换之外，zhe 目前也能用于解决一些略微复杂的问题。该部分内容可能会持续更新。

## 计数器

文件 internal.m4：

```m4
define(`_N_', 0)
define(`计数器', `define(`_N_', incr(_N_))_N_')
```

文件 macros.m4：

```m4
divert(-1)
include(`internal.m4')
define(`测试', `这是测试 indir(`计数器')')
divert(0)dnl
@宏开
```

文件 foo.txt：

```m4
测试；（测试）；测试。
@宏闭
测试；（测试）；测试。
```

以下命令

```bash
$ zhe -i macros foo.txt
```

的输出为

```
这是测试 1；这是测试 2；这是测试 3。
测试；测试；测试。
```

## Lua 计算器

internal.m4：

```m4
define(`calc',
       `syscmd(`echo "io.write(string.format(\"%0.3f\", $1))" | lua')')
```

macros.m4：

```m4
divert(-1)
include(`internal.m4')
define(`计算', ` calc($1) ')
divert(0)dnl
@宏开
```

foo.txt：

```m4
当我们看到（计算，1/3）时，能意识到它是‘（计算，1/3）’的产物吗？
```

以下命令

```bash
$ zhe -i macros.m4 foo.txt
```

的输出为

```m4
当我们看到 0.333 时，能意识到它是（计算，1/3）的产物吗？
```

## 嵌套宏调用

zhe 支持嵌套形式的中文宏调用，例如 `（甲，‘乙’，（丙，‘丁’））`。有了嵌套宏调用，便可以做复杂一些的任务了。例如，汉化 ConTeXt。

macro.m4：

```m4
divert(-1)
define(`%中文',
`\usemodule[zhfonts][size=$2]
\setupzhfonts[serif][regular=$1]'
)
define(`%宋体', `simsun')
define(`%小四', `11pt')
define(`%文始', `\starttext')
define(`%文终', `\stoptext')
define(`%插图', `\placefigure[here][]{$1}{\externalfigure[$2][$3]}')
define(`*版宽', `\textwidth')
define(`%宽', `width=$1')
divert(0)dnl
@宏开
```

foo.txt：

```
（%中文，%宋体，%小四）
%文始
你好，\CONTEXT！
（%插图，标题，foo.pdf，（%宽，0.5*版宽））
%文终
```

以下命令

```bash
$ zhe -i macros.m4 foo.txt
```

的输出为

```TeX
\usemodule[zhfonts][size=11pt]
\setupzhfonts[serif][regular=simsun]

\starttext
你好，\CONTEXT！
\placefigure[here][]{标题}{\externalfigure[foo.pdf][width=0.5\textwidth]}
\stoptext
```

## 野蛮和优雅

见「[新蜗牛 · 附录](../metafun/appendix.html)」。

[01]: ../../figures/bash/zhe/01.png
