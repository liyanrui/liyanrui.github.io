---
title: 动态数组
abstract: 我有广厦千万间。
date: 2025 年 04 月 24 日
...

# 前言

SimStr 模块本质上是一个特定类型动态数组，即数组的容量会随着存储元素的增多或减少而扩大或缩小。难道你不想实现一个更为通用的动态数组类型 `SimAarray`，它的每个单元可存储任何类型的对象吗？我很想要这样的数组。

实际上，早已有很多人实现了这样的数组，例如 GLib 库里的 GArray 模块，但是他们的实现，没有使用过我的 SimErr2，所以我不得不再次重新发明一个轮子，其外围部分如下：

```c
/* sim-array.h */
#ifndef SIM_ARRAY_H
#define SIM_ARRAY_H
#include <sim-err2.h>

#endif
```

```c
/* sim-array.c */
#include "sim-array.h"
```

# 隐私

首先，让 `SimArray` 拥有隐私，即接口与实现分离原则：

```c
/* sim-array.h ++ */
typedef struct sim_array_priv SimArrayPriv;
typedef struct sim_array {
        size_t n; /* 数组长度复本 */
        SimArrayPriv *priv; /* 数组的隐私部分 */
} SimArray;
```

```c
/* sim-array.c ++ */
struct sim_array_priv {
        size_t n; /* 数组长度：即数组存储的单元个数 */
        size_t u; /* 单元字节数 */
        size_t m;
        void *data;
};
```

上述结构体定义中，`n` 为单元数量，亦即 `SimArray` 对象能存储多少条数据。`u` 为每个单元的字节数。`m` 为 `data` 成员占用的内存空间字节数。

需要注意的是，与 `SimStr` 对象不同，`SimArray` 对象并未将自身数据结构完全隐藏在 sim-array.c 中，而是将数组长度公开于 sim-arrah.h 中，目的是方便用户获取数组的长度和单元长度，不过它们只是 `priv->n` 的复本，即使用户有意或无疑修改了公开的 `n` 的值，也不会影响到 `priv->n`，且前者具备抗修改性，因为 `SimArray` 模块函数均会自动修正它。

# 构造与释放

`SIM_ARRAY` 函数用于构造 `SimArray` 对象，其参数为数据单元的字节数：

```c
/* sim-array.h ++ */
SimArray *SIM_ARRAY(size_t u);
```

```c
/* sim-array.c ++ */
SimArray *SIM_ARRAY(size_t u) {
        SimArray *array = malloc(sizeof(SimArray));
        if (!array) {
                SIM_ERR2("failed to allocate memory for object!");
                return NULL;
        }
        array->priv = malloc(sizeof(SimArrayPriv));
        if (!array->priv) {
                free(array);
                SIM_ERR2("failed to allocate memory for private part!");
                return NULL;
        }
        array->priv->n = 0;
        array->priv->u = u;
        array->priv->m = 0;
        array->priv->data = NULL;
        array->n = array->priv->n;
        return array;
}
```

`SIM_ARRAY` 的用法如下：

```c
/* 构造一个可存储 int 类型的值的动态数组 */
SimArray *x = SIM_ARRAY(sizeof(int));
```

`sim_array` 宏能略微简化 `SIM_ARRAY` 函数的用法，其定义为

```c
/* sim-array.h ++ */
#define sim_array(type) SIM_ARRAY(sizeof(type))
```

上述的 `SIM_ARRAY` 示例代码等效于以下代码：

```c
SimArray *x = sim_array(x, int);
```

**也许你会不解，在上述代码中为什么函数名用的都是大写字母，而宏用的是小写字母呢？这是我苦思多日想出的设计。不好用的函数，就用大写字母，让它更加不好用。将不好用的函数封装为宏，就用小写字母作为宏名，让它更加好用。**

`sim_array_free` 可释放 `SimArray` 对象，其声明与实现如下：

```c
/* sim-array.h ++ */
void sim_array_free(SimArray *array);
```

```c
/* sim-array.c ++ */
void sim_array_free(SimArray *array) {
        if (array) {
                if (array->priv) {
                        free(array->priv->data);
                        free(array->priv);
                } else SIM_ERR2("invalid private part!");
                free(array);
        } else {
                SIM_ERR2("invalid SimArray object!");
        }
}
```

# 插入

熟悉数组缺点的人应该很清楚，向数组插入数据，相当于现实生活中可耻的插队行为。这种行为不会影响插入位置之前的人，但是会影响到插入位置之后的所有人。如果你跑到队伍的头部插队，那对整个队伍里的人而言，都是一场灾难。既然如此可耻，我们放弃实现 `sim_array_insert` 函数如何？

# 排队

插队是可耻的，排队是道德高尚的。我们要实现 `SIM_ARRAY_ADD` 函数，它可以将一条数据复制到 `SimArray` 对象的尾部。

```c
/* sim-array.h ++ */
void SIM_ARRAY_ADD(SimArray *array, void *unit, size_t u);
```

```c
/* sim-array.c ++ */
void SIM_ARRAY_ADD(SimArray *array, void *unit, size_t u) {
        if (!array) {
                SIM_ERR2("invalid SimArray object!");
                return;
        }
        if (!unit) {
                SIM_ERR2("invalid unit!");
                return;
        }
        if (array->priv->data) {
                if (array->priv->u != u) {
                        SIM_ERR2("unit type not matched!");
                        return;
                }
                size_t s = array->priv->n * u;
                size_t t = s + u;
                if (t > array->priv->m) { /* 扩容 */
                        size_t m = 1.5 * t;
                        void *new_data = realloc(array->priv->data, m);
                        if (!new_data) {
                                m = t + u;
                                new_data = realloc(array->priv->data, m);
                                if (!new_data) {
                                        SIM_ERR2("failed to enlarge capacity!");
                                        return;
                                }
                        }
                        array->priv->data = new_data;
                        array->priv->m = m;
                }
                memcpy((char *)(array->priv->data) + s, unit, u);
                array->priv->n++;
                /* 主动修复用户层面的 n */
                array->n = array->priv->n;
        } else {
                array->priv->data = malloc(u);
                if (!array->priv->data) {
                        SIM_ERR2("failed to allocate memory for data member!");
                        return;
                }
                memcpy(array->priv->data, unit, u);
                array->priv->n = 1;
                array->priv->u = u;
                array->priv->m = u;
                /* 主动修复用户层面的 n */
                array->n = array->priv->n;
        }
}
```

`SIM_ARRAY_ADD` 看上去有些复杂，但是用起来……也颇为复杂，其用法如下：

```c
SimArray *x = sim_array(int);
int a = 6;
SIM_ARRAY_ADD(x, &a, sizeof(int));
```

用宏 `sim_array_add` 将 `SIM_ARRAY_ADD` 封装起来，可以简化其用法。

```c
/* sim-array.h */
#define sim_array_add(array, unit, type) do { \
        type tmp_unit = unit; \
        SIM_ARRAY_ADD(array, &tmp_unit, sizeof(type)); \
} while (0)
```

`sim_array_add` 的用法如下：

```c
SimArray *x = sim_array(int);
sim_array_add(x, 6, int);
```

`sim_array_add` 可以直接将字面量添加到 `SimArray` 对象。

# 有求必应

C 数组可以按下标（索引）取值，`SimArray` 对象也应当如此。`SIM_ARRAY_RAW` 可以取下标为 `i` 的数据，其声明如下：

```c
/* sim-array.h ++ */
const void *SIM_ARRAY_RAW(SimArray *array, size_t i, size_t u);
```

```c
/* sim-array.c ++ */
const void *SIM_ARRAY_RAW(SimArray *array, size_t i, size_t u) {
        if (!array) {
                SIM_ERR2("invalid SimArray object!"); SIM_ASK2;
                return NULL;
        }
        /* 主动修复用户层面的 n */
        array->n = array->priv->n;
        /* 类型匹配检测 */
        if (array->priv->u != u) {
                SIM_ERR2("unit type not mached!"); SIM_ASK2;
                return NULL;
        }
        if (i >= array->n) {
                SIM_ERR2("out-of-bounds access!"); SIM_ASK2;
                return NULL;
        }
        return (char *)(array->priv->data) + i * u;
}
```

注意上述代码中用 `char *` 转换了 `array->priv->data` 类型，原因是 C 标准不允许 `void *` 类型的指针参与运算。由于此处的 `array->priv->data` 参与的加法运算是以字节为单位的，故而可将其转换为 `char *`。

需要注意的是，对 `SIM_ARRAY_RAW` 太过于乐观的代码，可能会导致程序因段错误而崩溃退出。例如

```c
SimArray *x = sim_array(int);
sim_array_add(x, 6, int);
/* 越界访问 x */
int a = *(int *)SIM_ARRAY_RAW(x, x->n, sizeof(int));
```

上述代码中，**`SIM_ARRAY_RAW` 返回的 `NULL`，经过 `(int *)` 类型转换变成整型类型的指针，然后又遭遇解引用（对指针取值），导致程序必然会崩溃。不过，在 `SIM_ARRAY_RAW` 返回 `NULL` 之前，我已用 `SIM_ASK2`主动报错了，即使程序崩溃，也能看到有助于快速确定程序出错位置的错误信息。**

既然用大写字母定义了 `SIM_ARRAY_RAW` 函数，说明它是不方便使用的。宏 `sim_array_raw` 为其开启方便之门。

```c
#define sim_array_raw(x, i, type) \
        *(type *)SIM_ARRAY_RAW(x, i, sizeof(type))
```

上述 `SIM_ARRAY_RAW` 对内存越界访问的示例可简化为

```c
SimArray *x = sim_array(int);
sim_array_add(x, 6, int);
/* 越界访问 x */
int a = sim_array_raw(x, x->n, int);
```

# 删除

删除数组中的某个单元，其行为如同插队一样可耻，所以，我们可以不实现 `sim_array_del`。

# 用法

以下代码将 C 数组复制为 `SimArray` 对象，可作为 SimArray 模块的用法示例。

```c
/* foo.c */
#include <sim-array.h>
int main(void) {
        SimArray *x = sim_array(int);
        /* 将 a 中元素复制到 x */
        int a[] = {0, 1, 2, 3, 4};
        size_t n = sizeof(a) / sizeof(int);
        for (size_t i = 0; i < n; i++) {
                sim_array_add(x, a[i], int);
        }
        /* 添加字面量 */
        sim_array_add(x, 5, int);
        /* 遍历 x */
        for (size_t i = 0; i < x->n; i++) {
                int t = sim_array_raw(x, i, int);
                printf("%d\n", t);
        }
        /* 越界访问，程序在此崩溃！ */
        int foo = sim_array_raw(x, x->n, int);
        printf("foo = %d\n", foo);
        /* 释放               验错 */
        sim_array_free(x); SIM_ASK2;
        return 0;
}
```

编译 foo.c 并运行所得程序 foo：

```console
$ gcc -I. sim-err.c sim-str.c sim-err2.c sim-array.c foo.c -o foo
$ ./foo
```

输出结果应为

```
0
1
2
3
4
sim_array_get_unit error: out-of-bounds access!
Segmentation fault (core dumped)
```

# 总结

SimArray 模块实现了一个简单的动态数组，它比 C 的定长数组，仅仅是多了可动态追加新数据这一特性，除此之外，C 数组不支持的，它也不支持。不过，由于幕后有 SimErr2 的支持，SimArray 模块的安全性或可调试性得到了尽可能的保证。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
> * 动态数组：[sim-array.h](sim-array.h) 和 [sim-array.c](sim-array.c)
