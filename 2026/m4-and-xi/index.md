---
title: 用 m4 武装 xi
subtitle: 
date: 2026 年 10 月 29 日
...

xi 是一个功能非常简单的文学编程工具，不过，它是命令行程序，支持 shell 标准输入，这意味着只需将其与其他命令组合，便可完成一些复杂的任务。

> xi 项目地址：<https://github.com/liyanrui/xi> 或 <https://gitee.com/garfileo/xi>

在代码片段的定义中，xi 不支持从文件导入内容，可有时会有这种需要，因为一些很长的代码直接嵌入在文学程序里，会导致编织出来的文档难以阅读。xi 可在 m4 的援助下解决该问题。

m4 是一种很古老的宏编程语言，其发明者是 C 语言之父 Dennis Ritchie 和《C 程序设计语言》的作者 Brian Kernighan。谈及宏编程，多数人大概会不屑一顾，甚至有些人为之反胃。不过，若从文本编辑的视角去看宏编程语言，其图像其实非常清晰，它像一个离线的文本编辑器，而我们平时所用的文本编辑器，可称为在线的文本编辑器。

> 关于 m4 语言，我写过几篇教程，见 <https://segmentfault.com/a/1190000004104696>

假设存在一个文学程序 foo.xi 中含有片段 foo，由文件 bar.c 的内容构成，

```
@ foo #
_include_(bar.c)
@
```

假设 bar.c 内容为

```
#include <stdio.h>
int main(void) {
        printf("hello world!\n");
        return 0;
}
```

xi 不具备从 bar.c 中导入内容的功能，但是若将 foo.xi 文件的内容视为待编辑的文本，将 m4 视为离线编辑器，则定义 m4 宏的过程相当于在定义一些文本编辑操作，只是这些操作需要放在 m4 文件中，例如 foo.m4,

```
divert(-1)
define(`_include_', `syscmd(`cat "$1"')dnl')
divert(0)dnl
```

> `syscmd` 是 GNU m4 处理器的内建宏，其他 m4 处理器是否支持，我并不清楚。

若 foo.xi 与 foo.m4 在同一目录下，只需执行

```
$ m4 foo.m4 foo.xi > foo-1.xi
```

便可完成 foo.xi 文件的编辑，bar.c 的内容会被导入 foo.xi，编辑结果保存在 foo-1.xi 文件里。foo 片段会被编辑为

```
@ foo #
#include <stdio.h>
int main(void) {
        printf("hello world!\n");
        return 0;
}
@
```

若使用 xi 提取 foo 片段，只需将上述命令的输出通过管道传于 xi 命令，

```
$ m4 foo.m4 foo.xi | xi -t -e "foo" -o foo.c
```

如果是用 xi 为文学程序 foo.xi 编织文档，若是编织为 markdown 或 html 文档，则 foo 片段中的内容可以构造为指向 bar.c 的连接，为此可以写出另一个 m4 文件 foo-weave.m4，

```
divert(-1)
define(`_include_', `<a href="$1">$1</a>')
divert(0)dnl
```

执行

```
$ m4 foo-weave.m4 foo.xi | xi -w -o foo.md
```

则 xi 输出的 foo.md 中的 foo 片段为

```
@ foo #
<a href="bar.c">bar.c</a>
@
```

这世上的事物，其好与坏，应是取决于打开它的方式。若能会一些 m4，不妨尝试用它去编辑一些文本，也许就能发现它的一些优美之处了。至少对于 xi 而言，此时的 m4 虽然满脸遍布着层层引号，但也是妙不可言。其实，也可以将 xi 视为一种离线的文本编辑工具。
