---
title: 山海经
lang: zh-CN
date: 2023 年 04 月 29 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[框文](framed-text.html)

> 下一篇：[门](gate.html)

南山经之首曰鹊山。又东三百里，曰堂庭之山。又东三百八十里，曰猨翼之山……山海经的地理，真伪已难以考证，但是蜗牛世界里的静物觉得，这是一本为一些叫做山或海的静物而写的爬行指南。

# 出生地

鹊山，堂庭之山，猨翼之山，皆可由原点出生：

```MetaPost
def 山 = picture enddef;

山 鹊山, 堂庭之山, 猨翼之山;
鹊山 := 框文("鹊山");
堂庭之山 := 框文("堂庭之山");
猨翼之山 := 框文("猨翼之山");

for i = 鹊山, 堂庭之山, 猨翼之山:
  draw i;
endfor;
```

结果只能看到猨翼之山，其他两山皆在它背后：

![猨翼之山][01]

# 山的爬行

若用 MetaPost 捏造一个长度单位「里」，便可在方寸之间，让山的爬行与山海经的记录相符：

```MetaPost
numeric 里; 里 := .25;

draw 鹊山;
draw 堂庭之山 shifted (东 300里);
draw 猨翼之山 shifted (东 680里);
```

![山的爬行][02]

问题出现了！根据山海经的记载，猨翼之山在堂庭之山的东边，二者相距 380 里，但是在上述代码里，确定猨翼之山的基准却是鹊山。

山海经里的「又」字，用得极妙，它隐含了一个相对的起点，但是又不需要特意指出这个起点。使用 MetaPost 语言，能否也定义一个这样的「又」呢？

# 又

要定义又，需要一个全局变量，用它记录相对起点的绝对位置，这个变量的名字姑且叫作竖亥吧……

```MetaPost
pair 竖亥;
竖亥 := (0, 0);
```

然后便可定义 `又`：

```MetaPost
vardef 又 expr a =
  竖亥 := 竖亥 shifted a;
  竖亥
enddef;
```

使用 `又`：

```MetaPost
draw 鹊山;
draw 堂庭之山 shifted 又 (东 300里);
draw 猨翼之山 shifted 又 (东 380里);
```

# 曰

还可以定义一个 `曰`：

```MetaPost
tertiarydef a 曰 b =
  b shifted a
enddef;
```

于是，

```MetaPost
draw 鹊山;
draw (又 (东 300里)) 曰 堂庭之山;
draw (又 (东 380里)) 曰 猨翼之山;
```

# 首山

如果鹊山的位置并不在原点，那么竖亥的位置就需要重新定义了。竖亥的初始值应该是首山的位置，确切地说，是首山的中心点。

定义一个宏，叫 `首山`，用它帮助竖亥确定初始位置：

```MetaPost
vardef 首山 expr a =
  竖亥 := center a;
  a
enddef;
```

MetaPost 提供的 `center` 宏可以确定 `path` 或 `picture` 对象的中心点。

现在，可以给鹊山一个绝对的位置，

```MetaPost
鹊山 := 鹊山 shifted (-300里, -300里);
```

然后，将其作为首山，然后确定其他山：

```MetaPost
draw 框文("出生地");
draw 首山(鹊山);
draw (又 (东 300里)) 曰 堂庭之山;
draw (又 (东 380里)) 曰 猨翼之山;
```

![首山和其他山][03]

# 新山海经语

虽然可以用 MetaPost 复刻山海经语，用于摆放各种静物，但是不必如此崇古。

语言不过是一切游戏的出生地，而语言本身就是游戏。我可以发明一套新的山海经语言，它的样子大致如下

```MetaPost
令 鹊山 距 原点 有 (-300里, -300里);
令 堂庭之山 距 鹊山 有 (300里, 0);
令 猨翼之山 距 堂庭之山 有 (380里, 0);
```

`令`，`距`，`有` 皆为宏，其定义如下：

```MetaPost
def 令 suffix a =
  令之体(a)
enddef;
def 令之体 (suffix a) text b =
  a := a shifted (b)
enddef;

def 距 expr a =
  (center a)
enddef;

def 有 expr b =
  shifted b
enddef;
```

上述宏定义所用的所有技巧里，之前未用过的仅仅是 `suffix` 类型的参数，它的作用是，引用一个变量。在 MetaPost 里，只有 `suffix` 类型的参数，能够让变量真正的出现在宏的定义里，其他类型的参数仅能让变量的副本进入宏定义。

下面是基于新山海经语定位三山的示例：

```MetaPost
numeric 里; 里 := .25;
山 鹊山, 堂庭之山, 猨翼之山;

鹊山 := 框文("鹊山");
堂庭之山 := 框文("堂庭之山");
猨翼之山 := 框文("猨翼之山");

令 鹊山 距 (0, 0) 有 (-300里, -200里);
令 堂庭之山 距 鹊山 有 (300里, 0);
令 猨翼之山 距 堂庭之山 有 (380里, 0);

for i = 鹊山, 堂庭之山, 猨翼之山:
  draw i;
endfor;
```

结果是竖亥下岗了。

汉字，英文，笛卡尔坐标……混杂在一起，似乎不难看。

# 附录

新山海经语示例的完整代码：

```MetaPost
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
  image(draw 框 withpen pencircle scaled 框文配置.框.线粗 withcolor 框文配置.框.颜色;
        fill 框 withcolor 框文配置.框.背景;
        draw 文 withcolor 框文配置.文字.颜色;)
enddef;

def 山 = picture enddef;

def 令 suffix a =
  令之体(a)
enddef;
def 令之体 (suffix a) text b =
  a := a shifted (b)
enddef;

def 距 expr a =
  (center a)
enddef;

def 有 expr b =
  shifted b
enddef;

numeric 里; 里 := .25;
山 鹊山, 堂庭之山, 猨翼之山;

鹊山 := 框文("鹊山");
堂庭之山 := 框文("堂庭之山");
猨翼之山 := 框文("猨翼之山");

令 鹊山 距 (0, 0) 有 (-300里, -200里);
令 堂庭之山 距 鹊山 有 (300里, 0);
令 猨翼之山 距 堂庭之山 有 (380里, 0);

for i = 鹊山, 堂庭之山, 猨翼之山:
  draw i;
endfor;
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

[01]: ../../figures/metafun/shanhai-jing/01.png
[02]: ../../figures/metafun/shanhai-jing/02.png
[03]: ../../figures/metafun/shanhai-jing/03.png
