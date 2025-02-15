---
title: Awk：面向文本编程
subtitle:
abstract: 专业的事，交给专业的工具。
date: 02 月 15 日
...

# 前言

很多年后，我可能又一次不知道 Awk 的用法，就像此刻的你。

Awk 是小语言，能做很多小事。用 Awk 的人像农夫，平素话少，在一片土地上做着很多小事。文本，是 Awk 耕作的土地。人类不喜欢土地，故而不喜欢当农夫。人类也不喜欢文本，故而喜欢使用微软或金山的一系列办公软件和甲骨文公司的数据库，以取得与高楼大厦，宝马香车，西装革履，笙歌燕舞密切的联系，令人觉得先进，而在土地上耕作的生活，是落后的，徒劳的。

在现代化进程下，大多数时候，有一些小事，我们做不好，甚至不会做了，于是觉得这些都是小事，不会做又何妨？这是不扫一屋也能扫天下的时代，只是想时常吃到让人放心的萝卜青菜，粗茶淡饭，却愈发变成奢求了。

应该庆幸，土地还在，耕种土地的方法还在。只要愿意花点时间，学会如何做耕种方面的一些小事，身心便可得到有益的滋养。这就是在这次学习 Awk 语言的过程中，我颇为认真写下这份笔记的原因，并希望许多年后，我还知道 Awk 怎么用。

# Awk 教程

本文档只是 Awu 语言的学习笔记，并非面面俱到的教程。我曾经写过一篇文章，介绍了 Awk 语言的基本用法，详见「[Awk 小传](https://segmentfault.com/a/1190000016745490)」。

若需要更完整且更好的教程，请阅读 Awk 语言的三位作者所著的《The Awk programming language》。这是一本很薄的书，200 多页，第一版发布于 1988 年，第二版发布于 2023 年。这本书并非只是讲述如何使用 Awk 语言编写程序——这部分内容在全书只占不到 1/3，它更多地是基于 Awk 语言描述了数据库、虚拟机、编译器以及排序算法等计算机科学中的基本原理。在国内，不仅 Awk 语言长期被低估和冷落，这本书则更是被低估和冷落，出版社从未组织翻译。该书的第一版，近年有爱好者翻译并公开，详见「<https://github.com/wuzhouhui/awk>」。

GNU 所实现的 gawk，其文档「<https://www.gnu.org/software/gawk/manual/>」内容丰富，面面俱到，在涉及 Awk 语言细节时，可作为手册查阅。

# 选择 gawk

Awk 语言的解释器有多种实现，除 Awk 语言的作者实现的 awk 之外，还有 GNU 项目 gawk，运行速度很快的 mawk 以及面向嵌入式系统的 BusyBox 环境中的 awk 等。在众多 Linux 发行版中，gawk 最为常用，只有 Debian (版本 > 6.0) / Ubuntu (版本 > 12.04) 及其衍生版本的 Linux 系统默认使用 mawk。

若不清楚自己所用的 awk 是哪个实现，可执行以下命令

```console
$ awk --version
```

然后查看该命令的输出信息。

对于 Debian/Ubuntu 及其衍生版本的 Linux 系统，若确定 awk 并非 gawk，而是 mawk，将 gawk 设为默认 awk 最简单的方法是：

```console
$ sudo apt remove mawk
$ sudo apt install gawk
```

若希望保持多个不同的 awk 实现，可使用以下命令选择 gawk 个作为默认 awk：

```console
$ sudo update-alternatives --config awk
```

更推荐 gawk 作为默认 awk 的原因是，gawk 对 Awk 语言进行了扩展，使得 Awk 语言在处理文本时更为简便。本文档中出现的 Awk 程序皆面向 gawk，但是尽量保持与 mawk 的兼容并同时指出 gawk 对 Awk 语言的扩展之处。

# Hello world!

使用 Awk 语言编写的每个程序（脚本），都假设有一份要处理的文本，故而 Awk 程序通常用以下方式执行：

```console
$ awk -f 脚本 文本文件
```

实际上，每个 Awk 程序都可以组织成以下形式：

```awk
BEGIN {...}
/模式/ {动作}
END {...}
```

其中，`/模式/ {动作}` 部分用于处理文本，而 `BEGIN` 和 `END` 块的运行时机分别是处理文本之前和结束。

倘若只在 `BEGIN` 块中写一些代码，Awk 脚本便可无文本要处理的情况下得以运行，例如以下 Awk 脚本 hello.awk：

```awk
BEGIN {
    print "Hello world!"
}
```

执行 hello.awk 的命令是

```console
$ awk -f hello.awk
Hello world!
```

也可以将 Awk 程序写成 Shell 脚本的形式。例如，上述 Awk 语言的 Hello world 程序，可改写为 Bash 脚本 hello.sh：

```bash
#!/bin/bash
awk 'BEGIN {
    print "Hello world!"
}'
```

以下命令可为 hello.sh 添加可执行权限（让该脚本可以像程序一样运行的权限）并运行它：

```console
$ chmod +x hello.sh
$ ./hello.sh
Hello world!
```
