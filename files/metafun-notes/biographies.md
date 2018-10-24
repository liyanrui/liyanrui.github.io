MetaFun 是 ConTeXt 的一部分，是 ConTeXt 的排版功能与 MetaPost 相融合的结果。

ConTeXt 专事文字排版，功能匹于 LaTeX，但更易于使用，二者皆为 TeX 宏包，即二者皆基于 TeX 提供的宏编程功能，对 TeX 语言予以封装，建立更利于文字排版工作的高级语言。TeX 是一种计算机排版语言，供编排科技手稿以及著作出版印刷之用\cite[TeX 历史]{[序幕有些长](https://segmentfault.com/a/1190000003779240)}。MetaPost 是用于绘制矢量绘图的计算机语言。

目前最新的 ConTeXt 版本为 MkIV，安装 ConTeXt Standalone 可得\cite[ConTeXt Standalone]{[睦邻友好的 ConTeXt Standalone](https://segmentfault.com/a/1190000003786283)}。ConTeXt MkIV 的基本用法可参考之前我写的几篇文章\cite[My ConTeXt Tutorial 1]{[先写作，后排版](https://segmentfault.com/a/1190000003790820)}\cite[My ConTeXt Tutorial 2]{[ConTeXt MkIV 中文支持](https://segmentfault.com/a/1190000003795931)}\cite[My ConTeXt Tutorial 3]{[文稿的物理结构](http://segmentfault.com/a/1190000003797764)}\cite[My ConTeXt Tutorial 4]{[文稿的逻辑结构](http://segmentfault.com/a/1190000003798231)}\cite[My ConTeXt Tutorial 5]{[页面布局](http://segmentfault.com/a/1190000003803997)}，或阅读 ConTeXt 官方文档\cite[official-1]{[ConTeXt Mark IV an excursion](http://www.pragma-ade.nl/general/manuals/ma-cb-en.pdf)}\cite[official-2]{[ConTeXt Reference](http://pmrb.free.fr/contextref.pdf)}。

MetaFun 以 MetaPost 生成的矢量图形作为页面特定区域的背景，而后基于 ConTeXt 的排版功能在该背景上实现编排文字。

![以 MetaPost 生成的矢量图为背景的排版示例](https://upload-images.jianshu.io/upload_images/11203728-f85d33290aa38344.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

# 单页图

在排版空间中，可安置 MetaPost 图形之处大致有插图、单页图、页面元素背景以及页面背景等类别。若以先习得 MetaPost 的基本用法为目的，则单页图最为合用，并且生成的图形易于转化为位图以作他用。

所谓 MetaPost 单页图，本质上是 ConTeXt 输出的排版结果——PDF 文档，只是文档页面的大小恰好容得下图形。ConTeXt 为 MetaPost 单页面提供了 `MPpage` 环境：

```TeX
\startMPpage
MetaPost 绘图语句;
\stopMPpage
```

例如，假设存在 ConTeXt 文档 foo.tex，其内容为

```TeX
\startMPpage
path p;
u := 10cm; v := 3cm;
p := fullsquare xyscaled (u, v) randomized 0.07u;
drawpath p;
drawpoints p;
\stopMPpage
```

通过 context 命令便可基于 foo.tex 生成 foo.pdf，即

```console
$ context foo
```

结果得到的 foo.pdf 为单页文档，其页面只包含着一个边线被随机扰动的矩形：

![](https://upload-images.jianshu.io/upload_images/11203728-724311e004ab068f.png)


# MetaPost

MetaPost 是一种编程语言\note{确切地说，MetaPost 是一种宏编程语言。}，其编译器为 mpost。用该语言编写的程序，其输出结果为 PostScript 格式的矢量图形文件\note{PostScript 文件可转化为 PDF、SVG 等格式的矢量图形文件。}。`MPpage` 环境中的 MetaPost 语句即 MetaPost 程序。在使用 context 命令生成单页面图形文件的过程中，context 命令会调用 mpost，由后者处理 MetaPost 程序，生成 PostScript 图形文件。继而 context 命令调用 TeX 引擎\note{TeX 引擎即 TeX 文档的编译器。ConTeXt 文档本质上也是 TeX 文档，因此要通过 TeX 引擎对其其进行编译，输出排版结果。ConTeXt MkIV 的 TeX 引擎为 LuaTeX，其输出的排版结果为 PDF 格式文档。}会将 mpost 生成的图形文件嵌入至单页面文档中，并将图形的宽高作为页面宽高。

## 画笔

画笔即 MetaPost 的内置变量 `pen`。MetaPost 提供了两种画笔类型，`pencircle` 和 `pensquare`，前者为 MetaPost 默认，「笔尖」为圆形，后者「笔尖」为方形。MetaPost 允许用户自行定义画笔类型。

画笔主要用于控制所绘线条的粗细。线条默认的宽度为 PostScript 所规定的大点（Big Point）的直径尺寸，即 1 bp。MetaPost 将 1 bp 作为基准长度单位，其他单位皆为该单位的倍数：

```MetaPost
bp := 1
mm = 2.83464
cm = 28.34645
pc = 11.95517
cc = 12.79213
in := 72
pt = 0.99626
dd = 1.06601
```

`pickup` 宏可设定画笔，从而影响随后的绘图语句所绘制线条的粗细，这一影响直至 `pickup` 宏的调用。例如，

```MetaPost
pickup pencircle scaled 1mm;
一系列绘图语句;
pickup pencircle scaled 2mm;
一系列绘图语句;
```

定义了两个画笔，笔尖粗度分别为 1mm 和 2mm，分别会影响位于其后的绘图过程。`scaled` 宏用于数值大小的缩放变换；其他数值变换宏还有 `shifted`、`rotated` 以及 `slant`，分别用于平移、旋转以及错切等变换。在画笔的设定中，`scaled 1mm` 意味着将线条粗细程序由 MetaPost 默认的 1 bp 在水平和竖直方向上同等放大为 1 mm\note{在 MetaPost 程序中，数字与单位之间不能出现空格。事实上，在 MetaPost 中，诸如 `1mm`、`2cm` 此类的长度描述本质上是 `mm` 或 `cm` 等变量的倍数，即 `1 * mm`、`2 * cm`。}。可以使用 `xscaled` 或 `yscaled` 对画笔的水平或竖直方向的粗细进行调整，对于 `pencircle` 类型的画笔而言，此举意味着将笔尖由默认的圆形转化为椭圆，而对于 `pensquare`，则意味着将笔尖由正方形转化为矩形。

在 `pickup` 的影响范围内，绘图语句可以通过 `withpen` 宏局部调整线条的粗细，例如

```MetaPost
withpen pencircle scaled 1mm
```

# 颜色

MetaPost 以含有三个分量的向量表示颜色。向量的三个分量分别表示红色、绿色和蓝色，取值范围为 [0, 1]，例如 `(0.4, 0.5, 0.6)`。

可将颜色保存到 `color` 类型的变量中，以备绘图中重复使用。例如

```MetaPost
color darkred;
darkred := (0.625, 0, 0);
```

由于 MetaPost 内部已经定义了用于表示红色的变量 `red`，因此 `darkred` 变量的定义也可写为

```MetaPost
color darkred;
darkred := 0.625red;
```

在绘图语句中可以通过 `withcolor` 宏设定所绘线条或区域填充的颜色，例如

```MetaPost
withcolor 0.625red
```

若绘图语句未使用 `withcolor` 宏，则默认颜色为黑色。

# 线条

线条即画笔所走的路径。最简单的路径是点。MetaPost 用序对表示点，例如

```MetaPost
pair a;
a := (2cm, 3.5cm)
```

表示在直接坐标系中，横坐标 `x` 为 `2cm` 而纵坐标 `y` 为 `3.5cm` 之处有一个点 `a`。`draw` 宏用于路径的绘制，通过它可将点 `a` 绘制出来，即

```MetaPost
draw a;
```

从一个点到另一个点，可构成一条线段。例如

```MetaPost
pair a, b;
a := (2cm, 3.5cm); b := (5cm, 5cm);

path p; 
p := a -- b;
```

可构造从点 `a` 到 `b` 的线段 `a -- b`，并将其保存到路径变量 `p` 中。使用

```MetaPost
draw p withcolor 0.625green;
```

即可绘制这条线段。在该条语句中，线条颜色被设为暗蓝色 `0.625blue`。

由于 MetaPost 允许在 `draw` 宏语句中直接给出点的坐标的形式构造路径，因此上述 MetaPost 程序可缩减为一行语句：

```MetaPost
draw (2cm, 3.5cm) -- (5cm, 5cm) withcolor 0.625green;
```

但是，若要绘制复杂的图形，借助变量，会使得 MetaPost 程序更易于编写和理解。例如，

```MetaPost
pair a, b; path p;
a := (2cm, 3.5cm); b := (5cm, 5cm);
p := a -- b;

pickup pencircle scaled 2pt;
draw p withcolor 0.625green;

pickup pencircle scaled 4pt;
color darkgreen; darkred := 0.625red;
draw a withcolor darkred;
draw b withcolor darkred;
```

![](https://upload-images.jianshu.io/upload_images/11203728-bc40e50fadd67c52.png)

不仅绘制了线段，而且将线段的端点也绘制了出来。

利用线段可绘制任意的多边形。例如，绘制一个直角三角形，

```MetaPost
pair a, b, c; path p;
a := (0, 0); b := (4cm, 0); c := (4cm, 3cm);
p := a -- b -- c -- a;

% 注意：凡以百分号领起的文本为 MetaPost 代码注释。

pickup pencircle scaled 5; % 将画笔设为 5 bp
draw p withcolor 0.8white;

pickup pencircle scaled 4;
draw a; draw b; draw c;
```

![](https://upload-images.jianshu.io/upload_images/11203728-d84658abef4f281d.png)

为了便于图形的演示，MetaFun 提供了 `drawpath` 和 `drawpoints` 宏，前者用于绘制路径，后者用于绘制路径的节点。通过这两个宏，上例可简化为

```MetaPost
pair a, b, c; path p;
a := (0, 0); b := (4cm, 0); c := (4cm, 3cm);
p := a -- b -- c -- a;
drawpath p; drawpoints p;
```

显然，上述路径 `p` 是一条闭合路径，但 MetaPost 对此并不知情，需要通过 `cycle` 宏告诉它，即

```MetaPost
p := a -- b -- c -- cycle;
```

否则，虽然我们认为 `p` 是闭合路径，但 MetaPost 并不苟同，以致在使用 `fill` 宏对该路径包围的区域填充颜色时，会导致 MetaPost 报错并罢工。

`fill` 宏可闭合路径所包围的区域着色。例如

```MetaPost
pair a, b, c; path p;
a := (0, 0); b := (4cm, 0); c := (4cm, 3cm);
p := a -- b -- c -- cycle;
drawpath p; drawpoints p;
fill p withcolor 0.8blue;
```

![](https://upload-images.jianshu.io/upload_images/11203728-017c53d6e60f9228.png)

上例中的路径 `p` 皆为直线插值。MetaPost 支持以曲线插值的方式构造路径。倘若将直线插值符的 `--` 替换为曲线插值符 `..` 便可产生一条插值于点 `a`、`b` 和 `c` 的曲线路径，

```MetaPost
p := a .. b .. c .. cycle;
```

![](https://upload-images.jianshu.io/upload_images/11203728-63fc69d17d9fe55b.png)

直线插值符与曲线插值符可并用，例如

```MetaPost
p := a .. b .. c -- cycle;
```

![](https://upload-images.jianshu.io/upload_images/11203728-58cf0f7a68da82ba.png)

`controls` 宏可将路径中的某些结点转化为控制点，从而可构造 Bézier 曲线。例如


```MetaPost
p := a .. controls b ..c; draw p;
```


![](https://upload-images.jianshu.io/upload_images/11203728-a9be265660583441.png)

构造的是一条二次 Bézier 曲线路径，此时点 `b` 成为控制点，曲线只插值于点 `a` 和 `b`。MetaFun 提供了 `drawcontrollines` 以及 `drawcontrolpoints` 宏，分别用于绘制 Bézier 曲线的控制形及控制点，例如，

```MetaPost
p := a .. controls b ..c;
drawpath p; drawpoints p;
drawcontrollines p; drawcontrolpoints p;
```

![](https://upload-images.jianshu.io/upload_images/11203728-468fc7d14a59251c.png)

三次 Bézier 曲线需要在路径中设定 2 个控制点，例如

```MetaPost
pair a, b, c, d; path p;
a := (0, 0); b := (4cm, 0); c := (4cm, 3cm); d := (0, 3cm);

p := a .. controls b and c .. d;
drawpath p; drawpoints p;
drawcontrollines p; drawcontrolpoints p;
```

![](https://upload-images.jianshu.io/upload_images/11203728-64072d63c3e2849d.png)

无论是插值曲线还是 Bézier 曲线，MetaPost 最高支持三次曲线。不过，对于形状较为复杂的路径，MetaPost 支持以多段插值直线、曲线以及 Bézier 曲线拼接\note{对于一组曲线，MetaPost 会以切向连续并且近似曲率连续的方式予以光滑拼接。}的方式构造路径。

# 变换

为了便于对所绘图形作缩放、旋转、平移、错切以及随机扰动等处理，MetaPost 提供了一种数据类型——变换，即六元组

$$
T = (t_x, t_y, t_{xx}, t_{xy}, t_{yx}, t_{yy})
$$

对于任意一点　$p=(p_x, p_y)$，MetaPost 的 `transform` 宏可将 $T$ 作用于 $p$，即 `p transform T`，可将 $p$ 变换为

$$
q = (t_{xx}p_x + t_{xy}p_y + t_x, t_{yx}p_x + t_{yy}p_y + t_y)
$$

实质上，若以仿射坐标的形式看待 $p$，并采用列向量 $\left[\begin{matrix}p_x \\ p_y \\ 1\end{matrix}\right]$ 表示其坐标，则 $T$ 的 6 个分量可形成坐标变换矩阵

$$
M = \left[\begin{matrix}
t_{xx} & t_{xy} & t_x \\
t_{xx} & t_{xy} & t_x \\
0 & 0 & 1\end{matrix}\right]
$$

此时，`p transform T` 语句所描述的坐标变换，便可表示为 $q = Mp$。坐标变换矩阵 $M$ 所描述的是平移、旋转、缩放以及错切等变换的组合，亦即这些特定的变换皆为 $M$ 的特例。因此，在应用 `transform` 宏的时候，通常并不直接提供六元组形式的变换，而是以 `scaled`、`shifted` 以及 `rotated` 等宏的组合构造一个变换。

假设存在四个点，

```MetaPost
length := 5cm;

pair a, b, c, d;
a := length * (0.25, -0.75); 
b := length * (0.25, -0.25); 
c := length * (0.75, -0.25); 
d := length * (0.75, -0.75);
```

它们构成路径 `p`，

```MetaPost
path p; p := a -- b -- c -- d;
drawpath p; drawpoints p;
```



