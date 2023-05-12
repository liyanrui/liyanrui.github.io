---
title: 新蜗牛 · 秘密
lang: zh-CN
date: 2023 年 05 月 08 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[新蜗牛 · 路](road.html)

> 下一篇：[新蜗牛 · 指针](pointer.html)

MetaPost 的宏定义非常灵活，以致于有时会像脑筋急转弯那般容易让人翻车。在写 snail 模块时，为了追求绘图「语法」接近中文形式，我用了一些有些诡异的宏定义形式，简记于此，以作备忘。

# 无括号参数

有时，会希望能定义像

```MetaPost
test a b;
```

可以接受两个参数，且每个参数不需要外围括号夹持。MetaFun 手册 1.10 节提供了一个方案：

```MetaFont
def test expr a = dotest(a) enddef;
def dotest (expr a) text b = 
  ...
enddef;
test a ; test a b ;
```

我相信了这个方案，于是写出了以下宏定义：

```MetaFont
def 标注 expr a = 作标注(a) enddef;
def 作标注(expr a) text b =
  draw textext(b) shifted (point .5 alone a + (0, 6pt);
enddef;
```

使用 `标注`：

```MetaFont
path p; p := origin -- (3cm, 0);
draw p;
标注 p "foo";
```

结果为

![标注][01]

完美！

# 部分宏

再定义一个宏：

```MetaFont
def 路 (suffix name) text label =
  path name;
  name := origin -- (3cm, 0);
  draw name;
  label name;
enddef;
```

使用 `路` 可以定义一条路径，并为其添加标注。例如

```MetaPost
路(p) 标注 "foo";
```

结果与上一节的示例结果相同，因为上述代码等价于

```MetaFont
path p; p := origin -- (3cm, 0);
draw p;
标注 p "foo";
```

上述示例表达的是，可将一个宏语句的一部分作为 text 类型的参数传递给另一个宏。

# 可选参数

在使用 `路` 时，若只是构造路径，不需要为其增加标注，即

```MetaFont
路(p);
```

这时，MetaPost 编译器会报错：

```
error: Isolated expression
```

原因是，上述语句会令 `路` 的定义中的 `label` 参数为空，导致

```MetaFont
label name
```

变为

```MetaFont
name
```

`name` 的类型是 `path`。在 MetaPost 语言中，孤立的表达式（Isolated expression）是无意义的，故而报错。

MetaPost 语言未提供判断参数是否为空的原语或宏（或许有，只是我尚未发现）。从语义的角度看，空，本身也是一种参数，故而企图只通过修改 `路` 的定义避免上述错误，是不可能的（或许可能，只是我尚未发现）。

# 返回值限定

柳暗花明之处在于，MetaPost 并非完全不允许孤立的表达式存在，在 `vardef` 宏的最后一行可以放孤立的表达式，作为宏的「返回值」。例如

```MetaFont
vardef foo =
path p;
p := origin -- (4cm, 0);
p
enddef;

draw foo;
```

并不会导致 MetaPost 报错。

基于这一特性，只需将 `作标注` 修改为

```MetaFont
vardef 作标注(expr a) text b =
  draw textext(a) shifted (point .5 along b + (0, 6pt));
  origin -- origin % 用原点到原点的路径表示空路径
enddef;
```

再将 `路` 修改为

```MetaFont
def 路 (suffix name) text label =
  path name;
  name := origin -- (3cm, 0);
  draw name;
  path hack; hack := label name;
  if path hack: fi;
enddef;
```

亦即，当 `label` 为空时，`name` 会落入一个条件语句里，它便不再是孤立的表达式了。若 `label` 不为空，由于 `label name` 的结果是 `origin -- origin`，其类型为 `path`，故而条件语句的展开结果为空，对于 `路` 的定义并无妨碍。

现在，便可以像下面这样使用 `路`:

```MetaFont
路(p);
```

![无标注路][02]

不过，上述宏定义的副作用是，`标注` 不能单独使用了，例如

```MetaFont
路(p);
标注 "foo" p;
```

结果会出错，出错信息依然是 `error: Isolated expression`，原因是上述 `标注` 语句的展开结果为

```MetaFont
 origin -- origin
```

这是一个孤立的表达式，该问题无解。

# 以非指喻指之非指

对于上一节最后的问题，除非重新定义一个可以单独使用的宏，用于对路径进行标注。例如

```MetaFont
def 路标 text a =
  path hack; hack := 标注 a;
enddef;
```

`路标` 的用法与原来的 `标注` 相同：

```MetaFont
路(p);
路标 "foo" p;
```

# MetaFun 的办法

有一个事实是，字符串可作为独立表达式。例如

```MetaFont
path p; p := origin -- (4cm, 0);
"Hello"；
draw p;
```

其中字符串 `"Hello"` 尽管独占一行，但 MetaPost 不会报错。基于该事实，可基于 MetaFun 宏 `tostring` 实现更为健壮的 `标注`：

```MetaFont
def 标注 expr a = 作标注(a) enddef;

vardef 作标注(expr a) text b =
  draw textext(a) shifted (point .5 along b + (0, 6pt));
  ""
enddef;

def 路 (suffix name) text label =
  path name;
  name := origin -- (3cm, 0);
  draw name;
  tostring label name;
enddef;
```

`tostring` 可将其参数转化为字符串。当 `label` 为空时，`tostring` 将 `path` 类型的变量 `name` 转化为字符串。若 `label` 不为空，则 `label name` 的展开结果是空字串 `""`， `tostring` 将其转化为其本身。亦可将 `tostring` 换为 `quote`或 `quotation`。由于字符串指可作为孤立表达式，直接使用 `标注` 宏，也不会导致 MetaPost 编译器出错，故而无需再定义一个 `路标` 宏。

或许还有更好的解决方案，但愿在我遇到它时，我尚未将 MetaPost 忘光。

[01]: ../../figures/metafun/secret/01.png
[02]: ../../figures/metafun/secret/02.png
