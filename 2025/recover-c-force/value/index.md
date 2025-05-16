---
title: 无类型的值
abstract: 本是无一物。
date: 2025 年 04 月 28 日
...

# 前言

倘若放弃类型，只关心数据在内存中的位置及其长度，我们似乎拥有了某种被接触封印的原力。我们能已经利用了这种原力实现了可存储任意类型数据的「[动态数组](../array/index.html)」。现在，我们尝试继续使用这种原力构造类型 `SimVal`，其对象可存储任意类型的值。`SimVal` 等效于仅 1 个数据单元的 `SimArray`，但其结构比后者更为精简。

```c
/* sim-val.h */
#ifndef SIM_VAL_H
#define SIM_VAL_H
#include <sim-err3.h>

#endif
```

```c
#include "sim-val.h"
```

# 结构

`SimVal` 的声明与定义如下：

```c
/* sim-val.h ++ */
typedef struct sim_val SimVal;
```

```c
/* sim-val.c ++ */
struct sim_val {
        size_t u;
        char data[];
};
```

结构体 `sim_val` 使用了自 C99 标准便支持的柔性数组（亦称零长度数组）语法，用于表达结构体的长度并不确定。该语法的好处是，在堆空间中，只需一次内存分配便可构造结构体对象。若不使用柔性数组，而是使用指针，需要先为结构体对象分配内存，再为其指针成员分配内存。同理，在回收结构体对象所占内存时，用柔性数组只需一次内存释放，用指针则需要两次。

需要注意的是，若结构体的柔性数组成员需要放在结构体成员列表的最后，如此方能使得结构体的长度可自由扩展而不影响其他成员的内存布局。

# 构造与释放

`SIM_VAL` 函数用于构造 `SimVal` 对象，它可将 C 的任一可参与赋值运算的变量的值以复制的方式存入 `SimVal` 对象。

```c
/* sim-val.h ++ */
SimVal *SIM_VAL(void *data, size_t u);
```

```c
/* sim-val.c ++ */
SimVal *SIM_VAL(void *data, size_t u) {
        if (!data || u == 0) {
                SIM_ERR3("invalid data!");
                return NULL;
        }
        SimVal *v = malloc(sizeof(SimVal) + u);
        if (!v) {
                SIM_ERR3("failed to allocate memory for SimVal object!");
                return NULL;
        }
        memcpy(v->data, data, u);
        v->u = u;
        return v;
}
```

`SIM_VAL` 的用法如下：

```c
int a = 3;
SimVal *v = SIM_VAL(&a, sizeof(int));
```

为了简化 `SIM_VAL` 的用法，定义 `sim_val` 宏：

```c
/* sim-val.h ++ */
#define sim_val(anyone, type) SIM_VAL(&anyone, sizeof(type))
```

以下代码演示了 `sim_val` 宏的用法，它与上述 `SIM_VAL` 用法示例等效。

```c
int a = 3;
SimVal *v = sim_val(a, int);
```

`sim_val_free` 可释放 `SimVal` 对象占用的内存。

```c
/* sim-val.h ++ */
void sim_val_free(SimVal *val);
```

```c
/* sim-val.c ++ */
void sim_val_free(SimVal *val) {
        if (val) free(val);
        else SIM_ERR3("invalid SimVal object!");
}
```

# 借出

`SimVal` 对象数据获取函数为 `SIM_VAL_RAW`，其声明与实现如下：

```c
/* sim-val.h ++ */
const void *SIM_VAL_RAW(SimVal *val, size_t u);
```

```c
/* sim-val.c ++ */
const void *SIM_VAL_RAW(SimVal *val, size_t u) {
        if (!val) {
                SIM_ERR3("invalid SimVal object!"); SIM_ASK3;
                return NULL;
        }
        if (val->u != u) {
                SIM_ERR3("type not mached!"); SIM_ASK3;
                return NULL;
        }
        return val->data;
}
```

将 `SIM_VAL_RAW` 封装为更好用的 `sim_val_raw` 宏：

```c
/* sim-val.h ++ */
#define sim_val_raw(val, type) \
                *(type *)SIM_VAL_RAW(val, sizeof(type))
```

`sim_val_raw` 的用法如下：

```c
int tmp = 3;
SimVal *v = sim_val(tmp, int);
int a = sim_val_raw(v, int);
/* 应输出 a = 3 */
printf("a = %d\n", a);
```

上述代码等效于以下代码

```c
int tmp = 3;
SimValue *v = SIM_VAL(&tmp, sizeof(int));
int a = *(int *)SIM_VAL_RAW(v, sizeof(int));
/* 应输出 a = 3 */
printf("a = %d\n", a);
```

# 示例

foo.c 演示了 SimVal 模块的基本用法：

```c
/* foo.c */
#include <sim-val.h>
int main(void) {
        /* 常规测试 */ {
                double pi = 3.1415926;
                SimVal *x = sim_val(pi, double);
                double a = sim_val_raw(x, double);
                printf("a = %lf\n", a);
                sim_val_free(x);
        }
        /* 边界测试 */ {
                SimVal *x = SIM_VAL(NULL, 8);
                /* 报错，并引发程序崩溃 */
                double a = sim_val_raw(x, double);
                printf("a = %lf\n", a);
                sim_val_free(x);                
        }
        return 0;
}
```

请在 Bash 环境里编译 foo.c 并运行所得程序 foo：

```console
$ gcc -I. sim-{err,str,err2,array,err3,val}.c foo.c -o foo
$ ./foo
```

foo 应当输出：

```
a = 3.141593
SIM_VAL error: invalid data!
SIM_VAL_RAW error: invalid SimVal object!
Segmentation fault (core dumped)

```

# 总结

SimVal 模块贯彻了 SimArray 的两条设计哲学：

* 将不好用的函数封装为宏，使之好用，但前者的名字用大写字母，后者的名字用小写字母。
* 若预判函数的返回结果会引发程序崩溃，应提前主动报错，以免程序崩溃，失去验看错误的机会。

关于上述第 2 条中的预判，可从以 `SIM_VAL` 和 `SIM_VAL_RAW` 函数的设计获得理解。`SIM_VAL` 函数在返回 `NULL` 时，仅记录错误，并未报错，原因是我们无法预判调用者会对该函数返回 `NULL` 的情况作何处理。`SIM_VAL_RAW` 在返回 `NULL` 前主动报错，原因是我们能够确定 `SIM_VAL_RAW` 返回 `NULL`，必定造成其调用者 `sim_val_raw` 宏因对 `NULL` 指针解引用而导致程序崩溃。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
> * 动态数组：[sim-array.h](sim-array.h) 和 [sim-array.c](sim-array.c)
> * 错误机制 3：[sim-err3.h](sim-err3.h) 和 [sim-err3.c](sim-err3.c)
> * 无类型的值：[sim-val.h](sim-val.h) 和 [sim-val.c](sim-val.c)
