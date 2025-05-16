---
title: 错误 2
abstract: 更好的错误。
date: 2025 年 04 月 23 日
...

基于 SimStr 模块，可实现 SimErr 的第一次升级，从而得到一个新的错误处理模块，我们将其命名为 SimErr2。

在 SimErr2 中，用于记录错误的全局变量是 `SimStr` 类型的对象。

```c
/* sim-err2.h */
#ifndef SIM_ERR2_H
#define SIM_ERR2_H
#include <sim-str.h>

typedef SimStr * SimErr2;
extern SimErr2 sim_err2;

#endif
```

C 语言标准提供了标识符 `__func__`，它表示当前的函数名。基于该标识符，可以让 `SIM_ERR2` 宏自动在错误信息里存储函数名，无需用户像使用 `SIM_ERR` 时手动输入，亦即我们要将

```c
SIM_ERR("sim_str_insert error: overrange!");
```

升级为

```c
SIM_ERR2("overrange!");
```

但后者记录的错误信息，在输出时，依然是

```
sim_str_insert error: overrange!
```

`SIM_ERR2` 的实现如下：

```c
/* sim-err2.h ++ */
#define SIM_ERR2(v) do { \
        if (sim_err2) { /* 清空 sim_err2 */ \
                sim_str_del(sim_err2, 0, sim_str_size(sim_err2)); \
        } else {  /* 初始化 sim_err2 */ \
                sim_err2 = sim_str(NULL); \
        } \
        sim_str_suffix(sim_err2, __func__); \
        sim_str_suffix(sim_err2, " error: "); \
        sim_str_suffix(sim_err2, (v)); \
} while (0)
```

`SIM_MER`，`SIM_ASK` 以及 `SIM_PUN` 相应的新版本如下：

```c
/* sim-err2.h ++ */
#define SIM_MER2 if (sim_str_size(sim_err2) > 0) { \
        if (sim_err2) { \
                fprintf(stderr, "%s\n", sim_str_raw(sim_err2)); \
                sim_str_del(sim_err2, 0, sim_str_size(sim_err2)); \
        } else { /* 初始化 sim_err2 */ \
                sim_err2 = sim_str(NULL); \
        } \
} else
#define SIM_ASK2 SIM_MER2{}

#define SIM_PUN2 if (sim_str_size(sim_err2) > 0) { \
        if (sim_err2) { \
                fprintf(stderr, "%s\n", sim_str_raw(sim_err2)); \
                sim_str_free(sim_err2); \
        } \
        exit(-1); \
}
```

在 sim-err2.c 中实现 `sim_err2` 的初始化：

```c
/* sim-err2.c */
#include "sim-err2.h"
SimErr2 sim_err2 = NULL;
```

以下是 SimErr2 的用法示例：

```c
/* foo.c */
#include <sim-err2.h>

float foo(float a) {
        if (a == 0) {
                SIM_ERR2("division by zero!");
                return 0;
        } else return 1 / a;
}

int main(void) {
        float a = foo(3); SIM_ASK2;
        printf("a = %f\n", a);
        float b = foo(0); SIM_ASK2;
        printf("b = %f\n", b);
        return 0;
}
```

编译 foo.c 并运行所得程序 foo:

```console
$ gcc -I. sim-err.c sim-str.c sim-err2.c foo.c -o foo
$ ./foo
```

结果应当是

```
a = 0.333333
foo error: division by zero!
b = 0.000000
```

于是，我们就有了 SimErr2。SimErr 完成了它的历史使命，而我们却不能废弃它，因为今后我们经常要使用的 SimStr 无法基于 SimErr2 实现。

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
