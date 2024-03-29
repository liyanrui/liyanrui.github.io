---
title: 新蜗牛 · 其他
lang: zh-CN
date: 2023 年 05 月 12 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[新蜗牛 · 指针](pointer.html)

> 下一篇：[新蜗牛 · 后记](postscript.html)

snail 模块的主要功能及其实现方法皆已有所介绍。本篇用于补充前文一些疏漏之处，且可能会保持更新，以记录 snail 模块的后续增加的一些功能。

# 调试

之前，显示某个对象的锚点，需要使用 `关防要塞` 宏。倘若希望显示所有对象的锚点，可以通过设置 `调试` 参数实现。例如

```MetaFont
设 "蜗界.框.线宽 = '8pt'; 蜗界.调试 = 'true'";
宫(甲, "甲");
宫(乙, "乙") 位于 甲 偏 (东 4cm);
呜呼 甲, 乙;
路(甲乙, 甲.寅门 纬转 .5[甲.离位, 乙.坎位] 经转 乙.申门);
```

在 `呜呼` 和构造路径的过程中，对象的锚点会被自动画出。

![调试][04]

# 量

以下代码

```MetaFont
量(线宽, 获 "蜗界.框.线宽");
```

等价于

```MetaFont
numeric 线宽; 
线宽 := 获 "蜗界.框.线宽";
```

# 令

宏 `令` 可用于对一个对象进行旋转或平移变换。例如

```MetaFont
path 甲 := fullsquare scaled 2cm;
令 甲 旋 30 偏 (东 3cm); 
```
上述代码等价于

```MetaFont
path 甲 := fullsquare scaled 2cm;
甲 := 甲 rotate 30 shifted (right * 3cm);
```

# 重修

有时，宫对象建好后，其尺寸可能需要作一些调整，宏 `重修` 可完成该任务。例如

```MetaFont
宫(甲, "甲"); 呜呼 甲;
重修(甲, 3cm, 2cm);
令 甲 偏 (东 4cm); 呜呼 甲;
```

![重修][02]

倘若希望调整一个宫对象的尺寸，令其与另一个宫对象的尺寸相同，此时可使用宏 `仿` 来实现。例如

```MetaFont
量(线宽, 获 "蜗界.框.线宽");
宫(甲, "今天");
宫(乙, "2023 年 5 月 12 日");
重修(甲, 仿 乙);
定位(乙, 乙.子门 位于 甲.午门 偏 (北 线宽));
呜呼 甲, 乙;
```

![仿][03]

上述代码中，需要注意的是，倘若希望两个对象紧密邻接——共享一条边界，需要进行线宽补偿。

# 城

由宏 `宫` 和 `廷` 构造的对象，可使用宏 `城` 组合为一个整体。城有锚点，有背景色，只是默认情况下无边框。

以下示例表现了 `城` 的用法，并以一个旋转变换验证城对象的整体性：

```MetaFont
设 "蜗界.调试 = 'true'";
表(a, "空","甲","乙","丙"); 
之乎者也 (a, a); 横陈(a, .5cm); 
城(b) 名(聚 a); 
令 b 旋 30 偏 (东 3cm); 
呜呼 空, b;
```

![城][01]


# 越过

有时难免会出现两条路径交叉的情况。宏 `越过` 可将其中一条路径在交叉处有所弯曲，表示该路径越过另一条路径而非相交。例如

```MetaFont
宫 (甲, "甲");
宫 (乙, "乙") 位于 甲 偏 (东 3cm);
呜呼 甲, 乙;

路 (甲乙, 甲.卯门 -- 乙.酉门);
路(乙甲, (乙.午门 经转 乙.巽位 经转 甲.坤位 -- 甲.子门) 越过 甲乙);
```

![越过][05]


[a]: ../../figures/metafun/others/a.png
[01]: ../../figures/metafun/others/01.png
[02]: ../../figures/metafun/others/02.pngt
[03]: ../../figures/metafun/others/03.png
[04]: ../../figures/metafun/others/04.png
[05]: ../../figures/metafun/others/05.png
