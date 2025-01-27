---
title: 配置文件与模块
lang: zh-CN
date: 2023 年 04 月 30 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[门](gate.html)

> 下一篇：[新蜗牛](new-snail.html)

蜗牛是简单的，只需要爬来爬去，偶尔经转，偶尔纬转。静物是复杂的，即使它一生只爬一次。

# Lua 表

不知不觉，我已经为频频用于表示静物的框文设立了一堆样式参数，它们以全局变量的形式出现在 MetaPost 代码里：

```MetaPost
numeric 框文配置.框.扩充, 框文配置.框.线粗, 框文配置.框.玩笑;
color 框文配置.文字.颜色, 框文配置.框.颜色, 框文配置.背景.颜色;
框文配置.框.扩充 := 4pt; 
框文配置.框.线粗 := 2pt; 
框文配置.框.玩笑 := 0;
框文配置.文字.颜色 := black;
框文配置.框.颜色 := darkgray;
框文配置.背景.颜色 := lightgray;
```

这些繁复的参数，若采用 Lua 的表结构表示，则非常优雅：

```Lua
框文配置 = {
    框 = {扩充 = "4pt", 线粗 = "2pt", 玩笑 = "0", 颜色 = "lightgray"},
    背景 = {颜色 = "lightgray"},
    文字 = {颜色 = "black"}
}
```

由于我是在 ConTeXt 中使用 MetaPost 作图，前者支持在排版代码中嵌入 Lua 程序，而且 MetaPost 本身现在亦能与 Lua 程序相互沟通，故而上述设想是可以实现的。

在 ConTeXt 排版代码中，可使用 Lua 的 `table.save` 函数，将 Lua 表保存到 ConTeXt 源文件所在目录，例如：

```TeX
\startluacode
local 框文配置 = {
    框 = {扩充 = "4pt", 线粗 = "2pt",
          玩笑 = "0", 透明度 = "0", 颜色 = "lightgray"},
    背景 = {颜色 = "lightgray"},
    文字 = {颜色 = "black"}
}
table.save("snail-conf.lua", 框文配置)
\stopluacode
```

在 MetaPost 绘图代码中，可使用 Lua 的 `table.load` 函数载入 snail-conf.lua 文件：

```MetaPost
lua("框文配置 = table.load('snail-conf.lua')");
```

上述代码里的 `lua` 是 MetaFun 宏，其参数是 MetaPost 字符串类型，值为 Lua 代码。`lua` 宏通过 Lua 解释器执行其参数传入的 Lua 代码。

现在，MetaPost 绘图代码里有了一个 Lua 表 `框文配置`，其中存储着框文的所有样式参数。不过，访问这些参数依然需要使用 `lua` 宏，例如

```MetaPost
lua("mp.print(框文配置.框.颜色)");
```

结果为颜色值 `lightgray`。

`mp.print` 是 MetaPost 定义的 Lua 函数（确切地说，是 LuaTeX 的 MPLib 库定义的函数），可将 Lua 数据转换为 MetaPost 语言支持的数据类型。

# 小例子

通过一个小例子，也许能够更好地理解上述的一切。假设有 ConTeXt 源文件 foo.tex，其内容如下：

```TeX
\startluacode
local 配置 = {
    形状 = "fullcircle",
    颜色 = "darkred"
}
table.save("snail-conf.lua", 配置)
\stopluacode

\startuseMPgraphic{foo}
lua("配置 = table.load('snail-conf.lua')");
pickup pencircle scaled 2pt;
draw lua("mp.print(配置.形状)") scaled 2cm withcolor lua("mp.print(配置.颜色)");
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

结果为

![圆][01]

需要注意的是，`lua` 是 MetaPost 宏（确切地说，是 LuaTeX 的 MPLib 库定义的宏），它的参数是字符串，只是该字符串的内容是 Lua 代码，倘若其中含有 Lua 字符串类型的数据，只能使用单引号或 `[[...]]` 的长字符串符号表达，从而避免与 MetaPost 的字符串的引号发生冲突。

# `获` 和 `设`

定义两个宏用于简化 `lua` 宏：

```MetaPost
def 获 expr a = lua("mp.print(" & a & ")") enddef;
def 设 expr a = lua(a) enddef;
```

例如，`获` 可将以下语句

```MetaPost
draw lua("mp.print(配置.形状)") scaled 2cm withcolor lua("mp.print(配置.颜色)");
```

简化为

```MetaPost
draw (获 "配置.形状") scaled 2cm withcolor (获 "配置.颜色");
```

# 模块

框文样式配置得到大幅简化后，便可考虑将一些自定义的宏归并至一份 ConTeXt 源文件，使之可重复使用。例如

```TeX
% snail.tex
\startluacode
local 配置 = {
    文字 = {颜色 = "black"},
    框形 = "fullsquare",
    框 = {余地 = "6pt", 线宽 = "2pt", 颜色 = "darkgray"},
    背景 = {颜色 = "lightgray"},
    路径 = {线宽 = "2pt", 颜色 = "darkgray"},
    玩笑 = "0"
}
table.save("snail-conf.lua", 配置)
\stopluacode

\startMPinclusions
lua("配置 = table.load('snail-conf.lua')");
def 获 expr a = lua("mp.print(" & a & ")") enddef;
def 设 expr a = lua(a) enddef;

def 框文 (suffix name) (expr a) =
  picture name;
  begingroup
    save 框, 文; path 框; picture 文;
    文 = textext(a);
    框 := fullsquare xyscaled (bbwidth 文, bbheight 文) enlarged ((获 "配置.框.余地") * (1, 1));
    框 := (获 "配置.框形") xyscaled (bbwidth 框, bbheight 框);
    if (获 "配置.玩笑") > 0: 框 := 框 randomized (获 "配置.玩笑"); fi;
    name := image (fill 框 withcolor (获 "配置.背景.颜色");
      draw 框 withpen pencircle scaled (获 "配置.框.线宽") withcolor (获 "配置.框.颜色");
      draw 文 withcolor (获 "配置.文字.颜色");
    );
  endgroup;
enddef;
\stopMPinclusions
```

在 ConTeXt 排版代码中，`MPinclusions` 环境包含的语句可被所有 MetaFun 环境共享。因此，上述 snail.tex 文件可作为模块在 ConTeXt 排版代码中使用，例如：

```TeX
% foo.tex
\usemodule[zhfonts]
\input snail % 或 \environment snail
\startuseMPgraphic{foo}
设 "配置.文字.颜色 = 'darkred'";
框文(天宫, "孙悟空闹过的天宫");
draw 天宫;
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

![天宫][02]

上述示例演示了 `设` 的用法，并略微修改了 `框文` 宏，以简化 `框文` 对象的定义。上例中的

```MetaPost
框文(天宫, "孙悟空闹过的天宫");
```

等价于

```MetaPost
picture 天宫;
天宫 := 框文("孙悟空闹过的天宫");
```

下一阶段的工作是逐步向 snail.tex 文件增加一些常用的宏的定义，很快便会有一个新的蜗牛模块可用。

# 非 ConTeXt

倘若并非在 ConTeXt 环境中使用 MetaPost 语言作图，上述定义模块的方式并不通用。不过，无论使用何种 TeX 宏包，只要 TeX 引擎是 LuaTeX（或 LuaMetaTeX，虽然不太可能），移植上述模块中的代码，在理论上是可行的。例如，可使用 `\directlua` 保存配置表，使用 MetaPost 的 `input` 宏代替 `MPinclusions` 环境。

[01]: ../../figures/metafun/config-and-module/01.png
[02]: ../../figures/metafun/config-and-module/02.png
