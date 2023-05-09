---
title: 新蜗牛 · 基本对象
lang: zh-CN
date: 2023 年 05 月 04 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[配置文件与模块](config-and-module.html)

> 下一篇：[新蜗牛 · 基本对象](road.html)

基于前面五篇文章的摸索，现在已基本具备了写一个新的 snail（蜗牛）模块的能力。与旧的 [snail](https://github.com/liyanrui/snail/tree/master/obsolete) 相比，新模块带有一些中文编程特征，可能会令人有所不适，但我觉得，除了我可能也没人会考虑使用它，故而请容许我牵黄擎苍，聊以自娱。

# 获取和安装

snail 适用于 ConTeXt LMTX 版本中的 MetaPost 环境，只需安装 [ConTeXt LMTX 独立包](https://wiki.contextgarden.net/Installation)便可拥有该环境。对于 TeX Live 用户，亦可尝试安装 ConTeXt LMTX 获得该环境，只是版本通常落后于 ConTeXt LMTX 包。 

获取 snail：

```bash
$ git clone https://github.com/liyanrui/snail.git
```

将所得 snail 目录移动至 ConTeXt 第三方模块目录（例如 $TEXROOT/texmf-local/tex/context/third 目录），然后执行

```bash
$ context --generate
```

亦可将 snail 目录内的 snail.tex 文件复制到你正在编写的 ConTeXt 源文件所在目录直接使用。

# 基本用法

倘若仅仅是体验 snail 模块，可使用以下 ConTeXt 源码框架：

```TEX
\usemodule[zhfonts] % 该模块提供中文排版支持
\input snail.tex    % 载入 snail.tex 文件
% 名字为 foo 的 MetaPost 作图环境
\startuseMPgraphic{foo}
% 此间放置 MetaPost 作图代码
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo} % 将 MetaPost 作图代码转化为插图
\stopTEXpage
```

如果是在正在编写的文档中使用 snail，ConTeXt 源码框架通常为

```TEX
\usemodule[zhfonts]
\input snail.tex
\startuseMPgraphic{foo}
% 此间放置 MetaPost 作图代码
\stopuseMPgraphic

\startext
\placefigure[here][引用标记]{插图标题}{\useMPgraphic{foo}}
\stoptext
```

# 宫，廷，景

带框的文字为「宫」，不带框的文字为「廷」，插图为「景」。

以下示例，构造了一个名为「甲」的宫，文字内容为「蜗居」：

```TEX
\usemodule[zhfonts]
\input snail.tex
\startuseMPgraphic{foo}
宫(甲, "蜗居");
呜呼 甲;
\stopuseMPgraphic

\startTEXpage[offset=4pt]
\useMPgraphic{foo}
\stopTEXpage
```

![宫][01]

上述代码中的「`呜呼 甲;`」可画出 `甲`，将 `呜呼` 理解为动词「画」即可，之所以不用 `画` 或 `画出`，是因为 `呜呼` 像是在念一句神秘的咒语。

`甲` 是 MetaPost 的一个变量，其类型为 `picture`。用 `甲` 是我个人偏好，你可以用任何其他名字，只要遵守 MetaPost 变量命名规则，例如

```MetaFont
宫(foo, "蜗居");
呜呼 foo;
```

廷的构造语句与宫相似，例如

```MetaFont
廷(甲, "蜗居");
呜呼 甲;
```

![廷][02]

构造景物，需要有外部插图文件。假设插图文件为 snail.png 且与使用它的 ConTeXt 源文件位于同一目录，则以下语句可构造景物：

```MetaFont
% 载入插图 snail.png，令其宽度为 4cm，高度由 snail 自动确定
景(甲, "snail.png", 4cm, "auto");
呜呼 甲;
```

![景][03]

# 粗略定位

定义宫、廷、景等对象时，其中心默认是坐标原点，可根据即有对象进行相对定位。例如

```MetaFont
宫(甲, "蜗居");
廷(乙, "蜗院") 位于 甲 偏 (东 3cm);
景(丙, "snail.png", 2cm, "auto") 位于 乙 偏 (东 3cm + 北 1.5cm);
呜呼 甲, 乙, 丙;
```

![定位][04]

宫、廷、景的构造语句后若存有类似 `位于 某对象 偏 (某方向 多少距离)` 的语句，可将其理解为所构造的对象，令其其中心与 `某对象` 重合，再沿某个方向偏移 `多少距离`。上述代码中的 `东 3cm + 北 1.5cm` 表示，向东偏移 3cm 之后，再向北偏移 1.5cm。此外，上述代码也展示了，`呜呼` 可用于绘制多个对象。

# 锚点

`宫`，`廷`，`景` 构造的对象，定位后，会自动带有 24 个锚点，使用 `关防要塞` 可显示所有锚点，例如

```MetaFont
廷(乙, "蜗院");
呜呼 乙;
关防要塞 乙;
```

![关防要塞][05]

每个锚点皆有唯一的中文名称，倘若对中国传统文化常识有所了解，记住它们并不难。

内层锚点，共计 16 个，首先按照现代地图方位，上北下南，左西右东，内层锚点中四个角点的名称分别为东北角，西北角，西南角，东南角，剩下的 12 个锚点按十二天干命名，分别为子门，丑门，寅门，卯门，辰门，巳门，午门，未门，申门，酉门，戌门，亥门，其中子门在正北，午门在正南，卯门在正东，酉门在正西，其他各门按序可知，不作赘述。

外层锚点共计 8 个，按八卦命名，对应方位为先天八卦方位，乾位正南，坤位正北，坎位正西，离位正东，艮位西北，兑位东南，震位东北，巽位西南。

基于锚点，可实现 `宫`，`廷`，`景` 所构造对象的精确定位。例如

```MetaFont
宫(甲, "蜗居");
廷(乙, "蜗院");
景(丙, "snail.png", 2cm, "auto");
定位(乙, 乙.坎位 位于 甲.离位);
定位(丙, 丙.酉门 位于 乙.兑位);

呜呼 甲, 乙, 丙;
关防要塞 甲, 乙, 丙;
```
![精确定位][06]

锚点在构造路径方面至关重要。例如构造一条由甲至乙再至丙的路径：

```MetaPost
宫(甲, "蜗居");
廷(乙, "蜗院"); 定位(乙, 乙.坎位 位于 甲.离位);
景(丙, "snail.png", 2cm, "auto"); 定位(丙, 丙.酉门 位于 乙.兑位);
呜呼 甲, 乙, 丙;
关防要塞 甲, 乙, 丙;

路(甲乙, 甲.酉门 -- 甲.坎位 -- 甲.艮位 -- 乙.坤位 -- 乙.子门);
路(乙丙, 乙.午门 -- 乙.乾位 -- 丙.坎位 -- 丙.巽位 -- 丙.兑位 -- 丙.离位 -- 丙.卯门);
```

![路][07]

# 蜗界参数

snail.tex 默认的蜗界参数存储于一个 Lua 表：

```Lua
local 蜗界 = {}
蜗界.字号 = "BodyFontSize" -- ConTeXt 正文字体所用字号
蜗界 = {
    字号 = 蜗界.字号,
    文字 = {颜色 = "black"},
    框形 = "fullsquare",
    框 = {余地 = 蜗界.字号, 
          线宽 = ".175" .. 蜗界.字号, 
          颜色 = "darkgray",
          各向同性 = "false",
          郊 = "1.5" .. 蜗界.字号},
    背景 = {颜色 = "lightgray"},
    路径 = {线宽 = ".2" .. 蜗界.字号,
            颜色 = "darkgray",
            有向 = "true",
            圆角 = ".25" .. 蜗界.字号},
    玩笑 = "0pt"
}
```

在使用 snail 模块时，该 Lua 表会自动被载入。通过 `获` 和 `设` 以及 `恢复默认蜗界` 等宏可以控制绘图样式。例如，可以更改宫的外框形状：

```MetaFont
设 "蜗界.框形 = 'fullcircle'";
设 "蜗界.框.各向同性 = 'true'";
宫(甲, "蜗居");
恢复默认蜗界;
宫(乙, "另一个蜗居") 位于 甲 偏 (东 4cm);
呜呼 甲, 乙;
关防要塞 甲, 乙;
```

需要注意的是，四角十二门的位置是随框形自适应变化的。

![圆形蜗居][11]

定义一个异形框体作为框形并不困难。例如

```MetaFont
numeric t; t := 0.38197;
path 五角星; 
五角星 := (0, 1) -- t * (cosd 54, sind 54) -- (cosd 18, sind 18) 
  -- t * (cosd 18, -sind 18) -- (cosd 54, -sind 54) -- t * (0, -1)
  -- (-cosd 54, -sind 54) -- t * (-cosd 18, -sind 18) -- (-cosd 18, sind 18)
  -- t * (-cosd 54, sind 54) -- cycle;
设 "蜗界.框形 = '五角星'";
设 "蜗界.框.余地 = '1cm'";
设 "蜗界.框.各向同性 = 'true'";
设 "蜗界.文字.颜色 = 'darkred'";
宫(甲, "\ss 蜗居");
呜呼 甲;
关防要塞 甲;
```

![五角星蜗居][12]

若异形框体需频繁使用，可为之定义专用的宫。例如，在 snail.tex 文件中添加以下定义

```MetaFont
def 五角星宫 (suffix name) (expr a) text 定位语句 =
  begingroup % 局部变量环境
  save t, 五角星; numeric t; path 五角星;
  t := 0.38197;
  五角星 := (0, 1) -- t * (cosd 54, sind 54) -- (cosd 18, sind 18) 
    -- t * (cosd 18, -sind 18) -- (cosd 54, -sind 54) -- t * (0, -1)
    -- (-cosd 54, -sind 54) -- t * (-cosd 18, -sind 18) -- (-cosd 18, sind 18)
    -- t * (-cosd 54, sind 54) -- cycle;
  设 "蜗界.框形 = '五角星'";
  设 "蜗界.框.余地 = '1cm'";
  设 "蜗界.框.各向同性 = 'true'";
  宫(name, a) 定位语句;
  恢复默认蜗界;
  endgroup;
enddef;
```

`五角星宫` 的用法与 `宫` 相同：

```MetaFont
五角星宫(甲, "蜗居") 位于 原点;
呜呼 甲;
关防要塞 甲;
```

对于蜗界而言，框体形状无穷无尽，但是取决于你的想象力以及在单位圆空间（圆心为原点，半径为 0.5）中构造图形的能力。

若不想一本正经，可通过设置 `蜗界.玩笑` 参数，随机轻微扰动宫和路的线条，以实现似手绘风格。例如

```MetaFont
设 "蜗界.玩笑 = '6pt'";
五角星宫(甲, "蜗居") 位于 原点;
呜呼 甲;
关防要塞 甲;
```

![玩笑][13]


[01]: ../../figures/metafun/new-snail/01.png
[02]: ../../figures/metafun/new-snail/02.png
[03]: ../../figures/metafun/new-snail/03.png
[04]: ../../figures/metafun/new-snail/04.png
[05]: ../../figures/metafun/new-snail/05.png
[06]: ../../figures/metafun/new-snail/06.png
[07]: ../../figures/metafun/new-snail/07.png
[08]: ../../figures/metafun/new-snail/08.png
[09]: ../../figures/metafun/new-snail/09.png
[10]: ../../figures/metafun/new-snail/10.png
[11]: ../../figures/metafun/new-snail/11.png
[12]: ../../figures/metafun/new-snail/12.png
[13]: ../../figures/metafun/new-snail/13.png
[a]: ../../figures/metafun/new-snail/a.png
