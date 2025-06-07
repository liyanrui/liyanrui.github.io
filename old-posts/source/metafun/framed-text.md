---
title: 框文
lang: zh-CN
date: 2023 年 04 月 29 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[蜗牛](snail.html)

> 下一篇：[山海经](shanhai-jing.html)

蜗牛生活的世界，有一些静物。

# 文字

最简单的静物是文字，确切地说，是 TeX 世界里的文字，可使用 `textext` 宏构建。例如

```MetaPost
vardef text (expr s) =
  if picture s:
    s
  else:
    fullsquare scaled 1cm withcolor red
  fi
enddef;

draw text(textext("\TEX"));
```

结果为

![TeX][01]


在 MetaPost 语言里，`vardef` 宏用于定义有返回值的宏。上述代码主要目的是为了展现，`textext` 构造的对象，其类型是 `picture`。

# 带框的文字

为文字加一个外框，只需先绘制一个外框，再绘制文字，例如：

```MetaPost
draw fullsquare xscaled 4.5cm yscaled 1cm 
     withpen pencircle scaled 4pt withcolor darkgray;
draw textext("来自 \TEX\ 世界的文字") withcolor darkred;
```

结果为

![带框的文字][02]

上面的结果仅仅是巧合—— `fullsquare` 构造的封闭路径和 `textext` 构造的 `picture` 对象，它们的中心在点默认情况下皆为原点，故而文字完美居于框中。貌合神离，不可接受。更好的解决方案是将文字和外框使用 MetaPost 的 `image` 宏组合为一个 `picture` 对象：

```MetaPost
picture framed_text;
framed_text := image(
  draw fullsquare xscaled 2cm yscaled 1cm 
       withpen pencircle scaled 4pt withcolor darkgray;
  draw textext("来自 \TEX\ 世界的文字") withcolor darkred;
);
draw framed_text;
```

将文字和外框组合为一个 `picture` 对象的好处是，可以对它们进行整体性的旋转和平移变换。例如

```MetaPost
draw framed_text shifted (-2cm, -1cm);
draw framed_text rotated 30;
draw framed_text rotated 75 shifted (2cm, 1cm);
```

![旋转/平移带框的文字][03]

# 包围盒

用上一节的方法，为不同的文字制作外框，需要人为设定外框的尺寸，使得外框能够包含文本。这种事情甚为无趣，有必要实现一个过程：根据文本宽度自动确定外框尺寸。

MetaFun 提供了两个宏，`bbwidth` 和 `bbheight`，可分别用于计算 `path` 或 `picture` 对象的包围盒宽度和高度，使用它们便可自动确定文字的外框尺寸，例如

```MetaPost
picture text;
text := textext("来自 \TEX\ 世界的文字");
path frame;
frame := fullsquare xscaled (bbwidth text) yscaled (bbheight text);

draw frame withpen pencircle scaled 4pt withcolor darkgray;
draw text withcolor darkred;
```

![文字包围盒][04]

若嫌文字的外框过于紧致，可使用 MetaFun 的 `enlarged` 宏令其有所扩大：

```MetaPost
path frame;
frame := fullsquare xscaled (bbwidth text) yscaled (bbheight text);
frame := frame enlarged (4pt, 4pt);
```

![文字外框留白][05]

# 框文

带框的文字，我原本是想表达为 framed text，但是忽然觉得，叫「框文」也不错：

```MetaPost
vardef 框文 expr a =
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (4pt, 4pt);
  image(draw 框 withpen pencircle scaled 4pt withcolor darkgray;
        draw 文 withcolor darkred;)
enddef;

draw 框文("来自 \TEX\ 世界的文字");
```

# 宏内局部变量

`框文` 宏，很前卫，但是它的定义里的变量，都是全局变量，即使它们在宏内。下面这个实验能够给出证明：

```MetaPost
draw 框文("来自 \TEX\ 世界的文字");
draw 文 rotated 30 shifted (1cm, 1cm);
```

结果为

![全局变量的副作用][06]

上文中宏内定义的所有变量皆为全局变量。过多使用全局变量，程序缺乏发展壮大的可能。MetaPost 支持局部变量，只是需要多写一些代码：

```MetaPost
vardef 框文 expr a =
  begingroup
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (4pt, 4pt);
  image(draw 框 withpen pencircle scaled 4pt withcolor darkgray;
        draw 文 withcolor darkred;)
  endgroup
enddef;
```

重新定义的 `框文`，其中的变量在外部便无法使用了。

`vardef` 定义的宏，它内含了 `begingroup` 和 `endgroup`，因此上述代码可写为

```MetaPost
vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (4pt, 4pt);
  image(draw 框 withpen pencircle scaled 4pt withcolor darkgray;
        draw 文 withcolor darkred;)
enddef;
```

# 背景色

现在，对 `框文` 再增加一个小功能，为框增加背景色：

```MetaPost
vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (4pt, 4pt);
  image(fill 框 withcolor lightgray;  % <= 新增代码
        draw 框 withpen pencircle scaled 4pt withcolor darkgray;
        draw 文 withcolor darkred;)
enddef;
```

![带背景色的框文][07]

# 外观

有什么理由决定，框文的框是暗灰色的，框的背景是浅灰色的，文字是暗红色的，以及有什么理由决定，框要向四周延伸 4pt 范围，框的边粗细为 2pt？

没有理由。在定义宏时，有两种办法对付这些不可确定的因素，一种是用宏的参数，一种是用全局变量。如果不确定的因素太多，宏就需要很多参数，其中有些参不确定因素在多数情况下也可以用默认的值。出于这一考虑，我觉得，用全局变量控制 `框文` 里的不确定因素，更为方便。

首先，尝试将 `enlarged` 的参数定义为全局变量：

```
numeric 扩充;
扩充 := 4pt;
```

然后在 `框文` 的定义里使用 `扩充`：

```MetaPost
vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (扩充, 扩充);
  image(fill 框 withcolor lightgray;
        draw 框 withpen pencircle scaled 4pt withcolor darkgray;
        draw 文 withcolor darkred;)
enddef;
```

然后在调用 `框文` 时，可以修改 `扩充` 的值，从而达到控制框文外观的目的，例如：

```MetaPost
扩充 := 1cm;
draw 框文("来自 \TEX\ 世界的文字");
```

结果完美：

![扩充][08]

既然如此，便可为 `框文` 定义所有的全局变量：

```MetaPost
numeric 扩充, 线粗, 玩笑;
扩充 := 4pt; 线粗 := 4pt; 玩笑 := 0;
color 文色, 框色, 背景;
文色 := black; 框色 := darkgray; 背景 := lightgray;
```

然后将宏 `框文` 的定义修改为：

```MetaPost
vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare xscaled (bbwidth 文) yscaled (bbheight 文) enlarged (扩充, 扩充);
  if 玩笑 > 0: 框 := 框 randomized 玩笑; fi;
  image(fill 框 withcolor 背景;
        draw 框 withpen pencircle scaled 线粗 withcolor 框色;
        draw 文 withcolor 文色;)
enddef;
```

那个叫玩笑的变量，轻易不要用，用了就不严肃了：

```MetaPost
文色 := darkred;
扩充 := 5mm;
玩笑 := 扩充;
draw 框文("来自 \TEX\ 世界的文字");
```

![玩笑][09]

# 姓甚名谁

利用 MetaPost 对变量名称的限定非常之少的特性，可以为全局变量建立命名空间，以防名字这种非常宝贵的资源被很快耗尽。

可将 `框文` 宏依赖的所有全局变量定义为

```MetaPost
numeric 框文.框.扩充, 框文.框.线粗, 框文.框.玩笑;
框文.框.扩充 := 4pt; 
框文.框.线粗 := 4pt; 
框文.框.玩笑 := 0;

color 框文.文字.颜色, 框文.框.颜色, 框文.框.背景;
框文.文字.颜色 := black; 
框文.框.颜色 := darkgray; 
框文.框.背景 := lightgray;
```

形如 `框文.框.扩充` 之类的变量名，不妨读作「框文的框的扩充」。虽然上述代码有些繁琐，但是安全。虽然安全，但是却不可行。因为全局变量名称里的 `框文` 与 `框文` 这个宏名存在冲突。我言不虚，名字的确是稀有资源。既然如此，就让全局变量的名字再冗长一些：

```MetaPost
numeric 框文配置.框.扩充, 框文配置.框.线粗, 框文配置.框.玩笑;
框文配置.框.扩充 := 4pt; 
框文配置.框.线粗 := 4pt; 
框文配置.框.玩笑 := 0;

color 框文配置.文字.颜色, 框文配置.框.颜色, 框文配置.框.背景;
框文配置.文字.颜色 := black; 
框文配置.框.颜色 := darkgray; 
框文配置.框.背景 := lightgray;
```

然后对 `框文` 宏定义适应性修改：

```MetaPost
vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare
        xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (框文配置.框.扩充 * (1, 1));
  if 框文配置.框.玩笑 > 0: 框 := 框 randomized 框文配置.框.玩笑; fi;
  image(fill 框 withcolor 框文配置.框.背景;
        draw 框 withpen pencircle scaled 框文配置.框.线粗 withcolor 框文配置.框.颜色;
        draw 文 withcolor 框文配置.文字.颜色;)
enddef;
```

上述关于框文的全局变量虽然驳杂繁复，现在不必为此焦虑，因为会有那么一天，它们将像下面这般简洁：

```Lua
框文配置 = {
    框 = {扩充 = "4pt", 线粗 = "2pt",
          玩笑 = "0", 透明度 = "0", 颜色 = "lightgray"},
    背景 = {颜色 = "lightgray", 透明度 = "0"},
    文字 = {颜色 = "black", 透明度 = "0"}
}
```

# 框形

有什么理由可以确定，框文的框一定是矩形或正方形呢？

没有。

所以，还应该再增加一个全局变量，

```MetaPost
path 框文配置.框形;
框文配置.框形 := fullsquare;
```

但是，现在到了重新理解 `enlarged` 宏的用法的时候了。`enlarged` 并非是对一个路径对象进行扩张或缩小，它左边的参数可以是任意路径，右边是扩张或缩小的程度，但是它的返回值是一个矩形。所以，

即使将框形设为圆：

```MetaPost
框文配置.框形 := fullcircle;
draw 框文("来自 \TEX\ 世界的文字");
```

得到的结果依然是矩形的框文。所以，需要继续修改 `框文` 的定义：

```MetaPost
vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare
        xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (框文配置.框.扩充 * (1, 1));
  框 := 框文配置.框形 xscaled (bbwidth 框) yscaled (bbheight 框);   % <= 关键之处
  if 框文配置.框.玩笑 > 0: 框 := 框 randomized 框文配置.框.玩笑; fi;
  image(fill 框 withcolor 框文配置.框.背景;
        draw 框 withpen pencircle scaled 框文配置.框.线粗 withcolor 框文配置.框.颜色;
        draw 文 withcolor 框文配置.文字.颜色;)
enddef;
```

下面构造一个椭圆形状的框文：

```MetaPost
框文配置.框形 := fullcircle;
框文配置.框.扩充 := 5mm;
框文配置.文字.颜色 := darkred;
draw 框文("来自 \TEX\ 世界的文字");
```

![椭圆形框文][10]

# 附录

椭圆形状的框文的全部代码（用了 zhfonts 模块排版汉字）：

```TEX
\usemodule[zhfonts]
\startuseMPgraphic{foo}
numeric 框文配置.框.扩充, 框文配置.框.线粗, 框文配置.框.玩笑;
框文配置.框.扩充 := 4pt; 
框文配置.框.线粗 := 4pt; 
框文配置.框.玩笑 := 0;

color 框文配置.文字.颜色, 框文配置.框.颜色, 框文配置.框.背景;
框文配置.文字.颜色 := black; 
框文配置.框.颜色 := darkgray; 
框文配置.框.背景 := lightgray;

path 框文配置.框形;
框文配置.框形 := fullsquare;

vardef 框文 expr a =
  save 框, 文;
  path 框; picture 文;
  文 = textext(a);
  框 := fullsquare
        xscaled (bbwidth 文) yscaled (bbheight 文)
        enlarged (框文配置.框.扩充 * (1, 1));
  框 := 框文配置.框形 xscaled (bbwidth 框) yscaled (bbheight 框);   % <= 关键之处
  if 框文配置.框.玩笑 > 0: 框 := 框 randomized 框文配置.框.玩笑; fi;
  image(fill 框 withcolor 框文配置.框.背景;
        draw 框 withpen pencircle scaled 框文配置.框.线粗 withcolor 框文配置.框.颜色;
        draw 文 withcolor 框文配置.文字.颜色;)
enddef;

框文配置.框形 := fullcircle;
框文配置.框.扩充 := 5mm;
框文配置.文字.颜色 := darkred;
draw 框文("来自 \TEX\ 世界的文字");
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

[01]: ../../figures/metafun/framed-text/01.png
[02]: ../../figures/metafun/framed-text/02.png
[03]: ../../figures/metafun/framed-text/03.png
[04]: ../../figures/metafun/framed-text/04.png
[05]: ../../figures/metafun/framed-text/05.png
[06]: ../../figures/metafun/framed-text/06.png
[07]: ../../figures/metafun/framed-text/07.png
[08]: ../../figures/metafun/framed-text/08.png
[09]: ../../figures/metafun/framed-text/09.png
[10]: ../../figures/metafun/framed-text/10.png
