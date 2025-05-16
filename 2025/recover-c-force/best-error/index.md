---
title: 错误 3
abstract: 也许是我能创造的最好的错误。
date: 2025 年 04 月 25 日
...

# 前言

发明 [SimStr](../str/index.html)，是为了创造 [SimErr2](../better-error/index.html)。发明 [SimArray](../array/index.html) 自然是为了创造 SimErr3。三生万物，应该不会有四了。SimErr3 是一个日志系统形式的错误机制，它会努力记下程序运行过程中的每一处错误，甚至不惜让你没有内存可用。

# sim_err3

`sim_err3` 依然是全局变量，只是它的类型是 `SimArray *`。

```c
/* sim-err3.h */
#ifndef SIM_ERR3_H
#define SIM_ERR3_H
#include <sim-array.h>

typedef SimArray * SimErr3;
extern SimErr3 sim_err3;

#endif
```

```c
/* sim-err3.c */
#include "sim-err3.h"
SimErr3 sim_err3 = NULL;
```

# SIM_ERR3

`SIM_ERR3` 需要将记录错误信息的 `SimStr` 对象存入 `sim_err3`：

```c
/* sim-err3.h ++ */
#define SIM_ERR3(v) do { \
        if (!sim_err3) sim_err3 = sim_array(SimStr *); \
        SimStr *e = sim_str(__func__); \
        sim_str_suffix(e, " error: "); \
        sim_str_suffix(e, (v)); \
        sim_array_add(sim_err3, e, SimStr *); \
} while (0)
```

# 消解宏

先定义消解 `sim_err3` 的宏，即输出 `sim_err3` 中所有的错误信息，然后释放 `sim_err3`：

```c
/* sim-err3.h ++ */
#define SIM_PRINT_ERR_AND_RESET do { \
        for (size_t i = 0; i < sim_err3->n; i++) { \
                SimStr *e = sim_array_raw(sim_err3, i, SimStr *); \
                printf("%s\n", sim_str_raw(e)); \
                sim_str_free(e); \
        } \
        sim_array_free(sim_err3); \
        sim_err3 = NULL; \
} while (0)
```

基于上述宏，定义 `SIM_MER3`，`SIM_ASK3` 和 `SIM_PUN3`：

```
/* sim-err3.h ++ */
#define SIM_MER3 if (sim_err3) { \
                SIM_PRINT_ERR_AND_RESET; \
        } else

#define SIM_ASK3 SIM_MER3{}

#define SIM_PUN3 if (sim_err3) { \
                SIM_PRINT_ERR_AND_RESET; \
        } \
        exit(-1);
#endif
```

# 用法

以下示例，尽可能的制造多条错误，以此验证 SimErr3 能否记录全部错误。

```c
/* foo.c */
#include <sim-err3.h>
void foo_1(void) {
        SIM_ERR3("狼来了！");
}
void foo_2(void) {
        SIM_ERR3("狼来了！");
}
void foo_3(void) {
        SIM_ERR3("狼来了！");
}
int main(void) {
        foo_1(); foo_2(); foo_3(); SIM_ASK3;
        return 0;
}
```

编译 foo.c 并运行所得程序 foo：

```console
$ gcc -I. sim-err.c sim-str.c sim-err2.c sim-array.c sim-err3.c foo.c -o foo
$ ./foo
```

结果应该输出

```
foo_1 error: 狼来了！
foo_2 error: 狼来了！
foo_3 error: 狼来了！
```

# 总结

从 SimErr 到 SimErr2 再到 SimErr3，并非量变式的升级，而是质变的跃迁，像单细胞生物进化为多细胞生物的过程。具有动态特性的字符串和数组，像是它们进化过程中产生的组织。它们需要基于这些组织完成进化，同时这些组织也需要它们的参与方能稳定。由此，可以作出一个假设，每当处于低级层次的有机生物体与外在的某些无机结构发生了融合，便为高级层次的生物体的诞生创造好了基础。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
> * 动态数组：[sim-array.h](sim-array.h) 和 [sim-array.c](sim-array.c)
> * 错误机制 3：[sim-err3.h](sim-err3.h) 和 [sim-err3.c](sim-err3.c)
