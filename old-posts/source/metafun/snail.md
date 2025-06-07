---
title: 蜗牛
homeback: ../../index.html
lang: zh-CN
date: 2023 年 04 月 28 日
abstract: 
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 下一篇：[框文](framed-text.html)

当我又一次颇为认真地学习 MetaPost 语言和 MetaFun 宏包时，我发现几年前的我好像很懂它们。那一年，我用 MetaFun 写了一个叫作「蜗牛」的模块 [snail.mp](https://github.com/liyanrui/snail) 用于绘制程序流程图，然而它的代码，我现在已经不甚明白了。现在我要为 snail.mp 补上一篇文档，并且希望该工作能够让我有可能学会 snail.mp 里没有的知识。

# 跬步

在 MetaPost 语言里，从一个点到另一个点，称为路径（path），例如

```MetaPost
path a;
a := (0, 0) -- (1cm, 0);
```

路径 `a` 从原点开始，向右或向东走 1 cm。用 `draw` 命令（可将其理解为 MetaPost 预定义的宏）可绘制路径，例如

```MetaPost
draw a;
```

若将上述代码嵌入至 ConTeXt 排版代码，例如

```TeX
% foo.tex
\startuseMPgraphic{foo}
path a;
a := (0, 0) -- (1cm, 0);
draw a;
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

使用 `context` 命令编译上述 ConTeXt 排版代码：

```bash
$ context foo.tex
```

便可将 MetaPost 图形显示于 ConTeXt 输出的 PDF 页面。结果如下图所示：

![向右或向东走 1 cm][01]

MetaFun 提供了两个更友好的宏，`drawpath` 和 `drawpoints`，前者用于画出路径，后者用于画出路径的节点，例如：

```MetaPost
path a;
a := (0, 0) -- (1cm, 0);
drawpath a; 
drawpoints a;
```

结果为

![向右或向东走 1 cm][02]

# 走四方

现在试着让蜗牛爬出一个正方形吧……

```MetaPost
path a;
a := (0, 0) -- (1cm, 0) -- (1cm, 1cm) -- (0, 1cm) -- (0, 0);
drawpath a;
drawpoints a;
```

![正方形][03]

上述图形，我画错了几次，因为需要将我习以为常的空间概念在脑海里转换为点的坐标。

MetaPost 提供了几个更直观的宏，便于走路，它们是 `left`，`right`，`up`，`down`，若用它们来走出一个正方形，需要定义一个变量，用于记录蜗牛在路径里的当前位置，该变量姑且叫作 `snail`：

```MetaPost
path a;
pair snail;
snail := (0, 0);
a := snail;
```

让 `snail` 向右走 1 cm 并将其新的位置添加至路径：

```MetaPost
snail := snail shifted (right * 1cm);
a := a -- snail;
```

可以继续让蜗牛一步一步走下去，直至它回到路径的起点：

```MetaPost
snail := snail shifted (up * 1cm);
a := a -- snail;
snail := snail shifted (left * 1cm);
a := a -- snail;
snail := snail shifted (down * 1cm);
a := a -- snail;
```

`left`，`right`，`up`，`down` 是 `(-1, 0)`，`(1, 0)`，`(0, 1)`，`(0, -1)` 的名字，使用它们的好处是，可以更为方便地让蜗牛在当前位置向四个方向的其中一个方向走一步。

如果按照地图方位，上北下南，左西右东，可从上帝视角去控制蜗牛的爬动方向。首选需要定义 4 个 MetaPost 宏：

```MetaPost
def 北 expr a = (up * (a)) enddef;
def 南 expr a = (down * (a)) enddef;
def 西 expr a = (left * (a)) enddef;
def 东 expr a = (right * (a)) enddef;
```

注意：

* MetaPost 允许以汉字作为宏名；
* 在宏定义中，倘若担心宏的参数在计算中陷入不可预料的境况，无需吝啬于使用括号。

用上述定义的东西南北宏重新表达蜗牛的正方形爬痕：

```MetaPost
snail := (0, 0);
a := snail;
snail := snail shifted (东 1cm);
a := a -- snail;
snail := snail shifted (北 1cm);
a := a -- snail;
snail := snail shifted (西 1cm);
a := a -- snail;
snail := snail shifted (南 1cm);
a := a -- snail;
```

上述代码与之前的硬性代码

```MetaPost
a := (0, 0) -- (1cm, 0) -- (1cm, 1cm) -- (0, 1cm) -- (0, 0);
```

相比，在规模上多出了数倍。下面考虑如何令其简洁。

# 路径的终点

上一节里用于记录蜗牛之前爬过的路径终点的 `snail` 变量并非必需，因为该任务可基于 MetaPost 宏 `point ... of ...` 及 `length` 实现。

假设一条路径

```MetaPost
path a;
a := (0, 0) -- (1cm, 0) -- (2cm ,3cm);
```

`point 0 of a` 便是 `a` 的起点 `(0, 0)`，而 `point 2 of a` 是 `a` 的终点 `(2cm, 3cm)`。

一条路径由多少个点构成，可通过 `length` 获得。例如上述路径 `a`，它由 `(length a) + 1` 个点构成。因此，借助 `length`，可以用更一般的方式获得路径的终点，例如

```MetaPost
point (length a) of a
```

基于上述知识，尝试消除变量 `snail`。首先，有

```MetaPost
path a;
a := (0, 0);
```

向东走一步：

```MetaPost
a := a -- (point (length a) of a) shifted (东 1cm);
```

再向北走一步：

```MetaPost
a := a -- (point (length a) of a) shifted (北 1cm);
```

类推下去，有

```MetaPost
a := a -- (point (length a) of a) shifted (西 1cm);
a := a -- (point (length a) of a) shifted (南 1cm);
```

路径 `a` 经过上述重构后，代码规模并未减少，但是我们完美地消除了一个变量。

# 向

若蜗牛爬行的路径可表达为

```MetaPost
path a;
a := (0, 0) 向 (东 1cm) 向 (北 1cm) 向 (西 1cm) 向 (南 1cm);
```

或

```MetaPost
path a;
a := (0, 0) 向 (东 1cm) 向 (北 1cm);  % 停了下来
a := a 向 (西 1cm) 向 (南 1cm);       % 继续走
```

我会有些担心，你会因此沉迷于 MetaPost 宏而不能自拔。

的确能够以 `向` 为名定义一个宏，只是在形式上与 `def` 定义的宏不同。`向` 的参数是在其两侧，这种形式的宏称为运算符。MetaPost 的运算符宏有三个级别，分别使用 `primarydef`，`secondarydef` 和 `tertiarydef` 进行定义。运算符宏的级别越高，在运算过程中的优先级越高。

下面是使用 `teriarydef` 定义运算符宏 `向`：

```MetaPost
tertiarydef a 向 b =
enddef;
```

现在 `向` 宏的定义是空的，下面开始为它逐步添加定义。

根据上述我想要的路径表达式，可以发现，`向` 的两个参数，有时是同一类型，有时是不同类型，但它们无非是 `pair` 或 `path`。因此，在 `向` 的定义里，首先需要区分参数类型，以便酌情处理。参数 `a` 可能是 `pair`，也可能是 `path`，而 `b` 的类型一直是 `pair`，因此可在 `向` 的定义中按以下方式判断参数类型：

```MetaPost
tertiarydef a 向 b =
  if pair a:
    ... ...
  elseif path a:
    ... ...
  else:
  fi
enddef;
```

`if pair a` 分支该如何写？由于 `a` 是一个点，`向` 宏展开的结果应当是：

```MetaPost
a -- (a shifted b)
```
至于 `elseif path a` 分支，由于 `a` 是一条路径，它要衔接的对象是一个点，该点是 `a` 的终经过 `shifted b` 变换的结果，于是可写出

```MetaPost
a -- (point (length a) of a) shifted b
```

至此可以得出 `向` 的完整定义，如下：

```MetaPost
tertiarydef a 向 b =
  if pair a:
    a -- (a shifted b)
  elseif path a:
    a -- (point (length a) of a) shifted b
  else:
  fi
enddef;
```

试试 `向` 能否工作：

```MetaPost
path a;
a := (0, 0) 向 (西 1cm) 向 (南 .5cm);
a := a 向 (东 2cm) 向 (北 1cm) 向 (西 2cm);
drawpath a;
drawpoints a;
```

结果为

![走向何方][04]

# 从

若定义一个无为的宏 `从`，会令蜗牛的爬行更为优雅：

```MetaPost
def 从 = enddef;

path a;
a := 从 (0, 0) 向 (西 1cm) 向 (南 .5cm);
a := 从 a 向 (东 2cm) 向 (北 1cm) 向 (西 2cm);
```

# 附录

完整的代码：

```MetaPost
\startuseMPgraphic{foo}
def 北 expr a = (up * (a)) enddef;
def 南 expr a = (down * (a)) enddef;
def 西 expr a = (left * (a)) enddef;
def 东 expr a = (right * (a)) enddef;

def 从 = enddef;

tertiarydef a 向 b =
  if pair a:
    a -- (a shifted b)
  elseif path a:
    a -- (point (length a) of a) shifted b
  else:
  fi
enddef;

path a;
a := 从 (0, 0) 向 (西 1cm) 向 (南 .5cm);
a := 从 a 向 (东 2cm) 向 (北 1cm) 向 (西 2cm);
drawpath a; 
drawpoints a;
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

[01]: ../../figures/metafun/snail/01.png
[02]: ../../figures/metafun/snail/02.png
[03]: ../../figures/metafun/snail/03.png
[04]: ../../figures/metafun/snail/04.png
