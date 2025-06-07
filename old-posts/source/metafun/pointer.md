---
title: 新蜗牛 · 指针
lang: zh-CN
date: 2023 年 05 月 11 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

> 上一篇：[新蜗牛 · 秘密](secret.html)

> 下一篇：[新蜗牛 · 其他](others.html)

基于指针（或引用）可以间接访问某个变量。例如，在 C 语言中，

```C
int a = 3;
int *p = &a;
*p = 4;
printf("%d\n", a);  /* 结果为 4 */
```

MetaPost 这样的宏语言，该如何像 C 语言这样间接访问变量呢？

# scantokens

scantokens 是 MetaPost 原语，可将字符串转化为 Token（记号）。由于 MetaPost 的原语、宏以及变量等对象的名字皆为 Token，故而其字符串变量便可以作为指针使用。例如，以下代码

```MetaFont
numeric a; a := 3;
string p; p := "a";
scantokens(p) := 4;
draw textext(decimal a);
```

与上文的 C 代码等效。

# 字符串表

有了指针，便可以构造表。有了表，便可批量构造或访问一组变量。例如

```MetaPost
string a[];  a[0] := "空"; a[1] := "甲"; a[2] := "乙"; a[3] := "丙"; 
宫(scantokens(a[0]), a[0]);
for i = 1 upto 3: 
  宫(scantokens(a[i]), a[i]) 位于 scantokens(a[i - 1]) 偏 (东 1.5cm);
endfor
呜呼 甲, 乙, 丙;
```

![指针-1][01]

上述代码看上去有些繁琐。宏最擅长化解繁琐：

```MetaFont
def 表 (suffix a) (text b) =
  string a[];
  begingroup
    save i; numeric i; i := 0;
    for j = b:
      a[i] := j;
      i := i + 1;
    endfor
  endgroup
enddef;
```

基于 `表`，上述构造表的代码

```MetaFont
string a[];  a[0] := "空"; a[1] := "甲"; a[2] := "乙"; a[3] := "丙"; 
```

可简化为

```MetaFont
表(a, "空", "甲", "乙", "丙");
```

在遍历一个表时，为了避免使用幻数，需要定义一个宏，用于查询表中元素个数：

```MetaFont
vardef 表长 suffix a =
  save i; numeric i; i := 0;
  forever:
    exitif unknown a[i + 1];
    i := i + 1;
  endfor
  i
enddef;
```

基于 `表长`，对表 `a` 的遍历代码可写为

```MetaFont
for i = 1 upto 表长 a:
  ... ... ...
endfor
```

# 名

在代码中频繁使用 `scantokens`，也颇为繁琐，以 `名` 代之：

```MetaFont
def 名 expr a = scantokens(a) enddef;
```

于是

```MetaFont
宫(scantokens(a[i]), a[i]) 位于 scantokens(a[i - 1]) 偏 (东 1.5cm);
```

可简写为

```MetaFont
宫(名 a[i], a[i]) 位于 (名 a[i - 1]) 偏 (东 1.5cm);
```

# 合并

若为字符串表定义宏 `聚`，将表中除第一个字符串之外的其他字符串依序合并为一个字符串，例如将

```MetaFont
表(a, "空", "甲", "乙", "丙", "丁", "戊");
```

所构造的表 `a` 合并为 `"甲, 乙, 丙, 丁, 戊"`，则

```MetaFont
呜呼 甲, 乙, 丙, 丁, 戊;
```

便可简写为

```MetaFont
呜呼 名(聚 a);
```

宏 `聚` 可定义为

```MetaFont
vardef 聚 suffix a =
  save s, n; string s; numeric n;
  s := "";
  n := 表长 a;
  for i = 1 upto n:
    s := (tostring s) & a[i] & ",";
  endfor;
  s := s & a[n];
  s
enddef;
```

`&` 可将两个字符串合并为一个。

# 横陈纵列

基于上述宏，不难定义以下三个宏，用于构造一组对象并横向或纵向放置它们：

```MetaFont
def 之乎者也 suffix a =
  for i = 0 upto 表长 a: 宫(名 a[i], a[i]); endfor
enddef;

def 横陈 (suffix a) (expr 间距) =
  for i = 1 upto 表长 a:
    定位(名 a[i], 名(a[i]).酉门 位于 名(a[i - 1]).卯门 偏 (东 间距));
  endfor
enddef;

def 纵列 (suffix a) (expr 间距) =
  for i = 1 upto 表长 a:
    定位(名 a[i], 名(a[i]).子门 位于 名(a[i - 1]).午门 偏 (南 间距));
  endfor
enddef;
```

![指针-2][02]


[01]: ../../figures/metafun/pointer/01.png
[02]: ../../figures/metafun/pointer/02.png
