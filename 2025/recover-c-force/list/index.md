---
title: 双向链表
abstract: 进退自如。
date: 2025 年 04 月 25 日
...

# 前言

数组，是一种害怕有人插队的队伍。这世上，有不害怕有人插队的队伍吗？当然是有的，例如向敌人的方向前行的队伍，链表大概就是这样的队伍吧。

为了弥补 SimArray 模块不支持数据插入和删除的的缺陷，并验证 SimErr3 的实用性，有必要实现一个双向链表模块，即 SimList。以下代码为该模块构造了必要的外围环境：

```c
/* sim-list.h */
#ifndef SIM_LIST_H
#define SIM_LIST_H
#include <stdbool.h>
#include <sim-err3.h>

#endif
```

```c
/* sim-list.c */
#include "sim-list.h"
```

# 三层结构

以 `SimVal` 对象表示链表节点中存储的数据，将链表节点类型定义为：

```c
/* sim-list.h ++ */
#include <sim-val.h>

typedef struct sim_link {
        struct sim_link *prev;
        struct sim_link *next;
        SimVal *value;
} SimLink;
```

我并没有依照传统，用 `ListNode` 这样的名字表达链表节点，因为我不想打么多字，况且 `Link` 足以传神表意。

链表类型由链表首端和尾端节点以及链表节点个数构成：

```c
/* sim-list.h ++ */
typedef struct sim_list {
        SimLink *head;
        SimLink *tail;
        size_t n;
} SimList;
```

# 构造与释放

`sim_list` 函数可构造空的链表：

```c
/* sim-list.h ++ */
SimList *sim_list(void);
```

```c
/* sim-list.c ++ */
SimList *sim_list(void) {
        SimList *list = malloc(sizeof(SimList));
        if (list) {
                list->head = NULL;
                list->tail = NULL;
                list->n = 0;
                return list;
        } else {
                SIM_ERR3("failed to allocate memory for SimList object!");
                return NULL;
        }
}
```

`sim_list_free` 函数用于释放链表对象：

```c
/* sim-list.h ++ */
void sim_list_free(SimList *list);
```

```c
/* sim-list.h ++ */
void sim_list_free(SimList *list) {
        if (!list) {
                SIM_ERR3("invalid list!");
                return;
        }
        SimLink *it = list->head;
        while (it) {
                SimLink *next = it->next;
                sim_val_free(it->value);
                free(it);
                it = next;
        }
        free(list);
}
```

# 边界检测

对链表对象进行任何修改前，均需要通过模块的私有函数 `sim_list_boundry_check` 函数检查链表的边界是否足够安全，但是只检测其边界。每个数据单元或节点的正确性，可在构造或释放时检测。

```c
/* sim-list.c ++ */
static bool sim_list_boundry_check(SimList *list) {
        if (!list) {
                SIM_ERR3("invalid list!");
                return false;
        }
        if (list->n == 0) {
                if (list->head != NULL || list->tail != NULL) {
                        SIM_ERR3("invalid empty list!");
                        return false;
                }
                return true;
        }
        if (list->n > 0) {
                if (list->head == NULL || list->tail == NULL) {
                        SIM_ERR3("head and tail should not be NULL!");
                        return false;
                }
        }
        return true;
}
```

# 数据增删

若已知链表对象 `list` 中的某个节点，即持有指向它的指针，以该节点为基准，可在其前后插入数据。前和后，怎么区分呢？朝着 `list->head` 的方向是前，朝着 `list->tail` 的方向是后。我们用 forward 和 backward 分别表示向前和向后，二者可分别简写为 f 和 b。

模块私有函数 `sim_link` 用于构造链表节点对象，它可将插入链表的数据复制到节点对象的 `value` 成员。

```
/* sim-list.c ++ */
static SimLink *sim_link(void *data, size_t u) {
        SimLink *new_link = malloc(sizeof(SimLink));
        if (!new_link) {
                SIM_ERR3("failed to allocate memory for SimLink object!");
                return NULL;
        }
        new_link->prev = NULL;
        new_link->next = NULL;
        new_link->value = SIM_VAL(data, u);
        return new_link;
}
```


`SIM_LIST_INSF` 向指定节点之前插入数据（insert-forward），并返回包含该数据的新节点：

```c
/* sim-list.h ++ */
SimLink *SIM_LIST_INSF(SimList *list, SimLink *here, void *data, size_t u);
```

```c
/* sim-list.c ++ */
SimLink *SIM_LIST_INSF(SimList *list, SimLink *here, void *data, size_t u) {
        if (!sim_list_boundry_check(list)) return NULL;
        SimLink *new_link = sim_link(data, u);
        if (here) {
                new_link->prev = here->prev;
                new_link->next = here;
                here->prev = new_link;
        } else {
                if (list->n == 0) { /* 仅当 list 为空表时允许 here 为空 */
                        list->head = new_link;
                        list->tail = new_link;
                } else {
                        SIM_ERR3("invalid link!");
                        return NULL;
                }
        }
        if (!new_link->prev) list->head = new_link;
        list->n++;
        return new_link;
}
```

`SIM_LIST_INSF` 的用法如下：

```c
/* 构造链表 3 <=> 2 <=> 1 */
SimList *x = sim_list();
int a = 1, b = 2, c = 3;
SimLink *la = SIM_LIST_INSF(x, x->head, &a, sizeof(int));
SimLink *lb = SIM_LIST_INSF(x, la, &b, sizeof(int));
SimLink *lc = SIM_LIST_INSF(x, lb, &c, sizeof(int));
```

`SIM_LIST_INSB`可在指定节点之后插入数据（insert-backward）：

```c
/* sim-list.h ++ */
SimLink *SIM_LIST_INSB(SimList *list, SimLink *here, void *data, size_t u);
```

```c
SimLink *SIM_LIST_INSB(SimList *list, SimLink *here, void *data, size_t u) {
        if (!sim_list_boundry_check(list)) return NULL;
        SimLink *new_link = sim_link(data, u);
        if (here) {
                new_link->prev = here;
                new_link->next = here->next;
                here->next = new_link;
        } else {
                if (list->n == 0) {
                        list->head = new_link;
                        list->tail = new_link;
                } else {
                        SIM_ERR3("invalid link!");
                        return NULL;
                }
        }
        if (!new_link->next) list->tail = new_link;
        list->n++;
        return new_link;
}
```

`SIM_LIST_INSB` 的用法如下：

```c
/* 构造链表 1 <=> 2 <=> 3 */
SimList *x = sim_list();
int a = 1, b = 2, c = 3;
SimLink *la = SIM_LIST_INSB(x, x->tail, &a, sizeof(int));
SimLink *lb = SIM_LIST_INSB(x, la, &b, sizeof(int));
SimLink *lc = SIM_LIST_INSB(x, lb, &c, sizeof(int));
```

为了简化链表构造过程，定义两个宏 `sim_list_prefix` 和 `sim_list_suffix`，用于向链表首部之前或尾部之后插入数据。

```c
/* sim-list.h ++ */
#define sim_list_prefix(list, v, type) \
        SIM_LIST_INSF(list, list->head, &(v), sizeof(type))
#define sim_list_suffix(list, v, type) \
        SIM_LIST_INSB(list, list->tail, &(v), sizeof(type))
```

上述 `SIM_LIST_INSF` 和 `SIM_LIST_INSB` 示例代码，基于  `sim_list_prefix` 和 `sim_list_suffix` 可分别改写为：

```c
/* sim_list_prefix 的用法 */
/* 构造链表 3 <=> 2 <=> 1 */
SimList *x = sim_list();
sim_list_prefix(x, 1, int);
sim_list_prefix(x, 2, int);
sim_list_prefix(x, 3, int);
```

```c
/* sim_list_suffix 的用法 */
/* 构造链表 1 <=> 2 <=> 3 */
SimList *x = sim_list();
sim_list_suffix(x, 1, int);
sim_list_suffix(x, 2, int);
sim_list_suffix(x, 3, int);
```

`sim_list_del` 可删除一个指定的节点：

```c
/* sim-list.h ++ */
void sim_list_del(SimList *list, SimLink *target);
```

```c
/* sim-list.c ++ */
void sim_list_del(SimList *list, SimLink *target) {
        if (!sim_list_boundry_check(list)) return;
        if (target) {
                SimLink *prev = target->prev;
                SimLink *next = target->next;
                if (prev) prev->next = next;
                else list->head = next;
                if (next) next->prev = prev;
                else list->tail = prev;
                /* 释放 target */
                sim_val_free(target->value);
                free(target);
                list->n--;
        } else SIM_ERR3("target is NULL!");
}
```

`sim_list_del` 的用法如下：

```c
/* 构造链表 */
SimList *x = sim_list();
sim_list_suffix(x, 1, int);
sim_list_suffix(x, 2, int);
sim_list_suffix(x, 3, int);
/* 删除第 2 个节点 */
SimLink *it = x->head;
for (int i = 0; i < x->n; i++) {
        if (i == 1) {
                sim_list_del(x, it);
                break;
        }
        it = it->next;
}
```

# 借出

链表节点中存储的值，仅有其地址和长度，而没有其类型，从中取出数据，需要通过类型转换赋予它类型，但这一过程难免会遇到对空指针解引用而导致程序崩溃的问题。

`SIM_LINK_RAW` 函数可返回指定节点中存储的数据，在此过程中会检验类型长度是否匹配。

```c
/* sim-list.h ++ */
const void *SIM_LINK_RAW(SimLink *link, size_t u);
```

```c
/* sim-list.c ++ */
const void *SIM_LINK_RAW(SimLink *link, size_t u) {
        if (link) {
                return SIM_VAL_RAW(link->value, u);
        } else {
                SIM_ERR3("invalid link!"); SIM_ASK3;
                return NULL;
        }
}
```

为了便于使用，将 `SIM_LINK_RAW` 函数封装为宏 `sim_link_raw`：

```c
#define sim_link_raw(link, type) \
        *(type *)SIM_LINK_RAW(link, sizeof(type))
```

`sim_link_raw` 的用法如下：

```c
/* 构造链表 1 <=> 2 <=> 3 */
SimList *x = sim_list();
sim_list_suffix(x, 1, int);
sim_list_suffix(x, 2, int);
sim_list_suffix(x, 3, int);
/* 从链表中借用第 2 个节点的值 */
SimLink *it = x->head;
for (int i = 0; i < x->n; i++) {
        if (i == 1) {
                sim_link_raw(it, int, second);
                printf("the second value is %d.\n", second);
                break;
        }
        it = it->next;
}
```

# 用法

foo.c 演示了 SimList 模块的基本用法。

```c
/* foo.c */
#include <sim-list.h>
int main(void) {
        /* 构造链表 x */
        SimList *x = sim_list();
        int a[] = {1, 2, 3, 4, 5};
        int n = sizeof(a) / sizeof(int); /* 数组长度 */
        for (size_t i = 0; i < n; i++) {
                sim_list_suffix(x, a[i], int);
        }
        /* 遍历 x */
        printf("NULL <=> ");
        for (SimLink *it = x->head; it; it = it->next) {
                sim_link_raw(it, int, t);
                printf("%d <=> ", t);
        }
        printf(" NULL\n");
        /* 释放 x，检错 */
        sim_list_free(x); SIM_ASK3;
        return 0;
}
```

在 Bash 环境里编译 foo.c：

```c
$ gcc -I. sim-{err,str,err2,array,err3,val,list}.c foo.c -o foo
```

之所以强调在 Bash 环境里编译，是因为其他 Shell 环境可能不支持这种「花括号展开」的写法。

运行 foo，它应该输出以下内容：

```
NULL <=> 1 <=> 2 <=> 3 <=> 4 <=> 5 <=>  NULL
```

# 总结

如果 SimArray 支持数据插入，也许我就没有动力实现 SimList 了。当自身条件不足以支撑某种算法时，切勿自作聪明，发明一些奇技淫巧，从而导致失去进化的可能。现在已经有很多人认为，链表完全没必要存在了，有动态数组便已足够，这意味着，他们所写的程序也许已经失去进化的可能了。这也是 SimStr、SimArray 和 SimList 只提供了基本的功能的主要原因。每当我需要它们拥有新的功能时，我首先考虑的应该是，我是否需要让它们进化，而不是在原有的形式里一味地量变。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
> * 动态数组：[sim-array.h](sim-array.h) 和 [sim-array.c](sim-array.c)
> * 错误机制 3：[sim-err3.h](sim-err3.h) 和 [sim-err3.c](sim-err3.c)
> * 无类型的值：[sim-val.h](sim-val.h) 和 [sim-val.c](sim-val.c)
> * 双向链表：[sim-list.h](sim-list.h) 和 [sim-list.c](sim-list.c)
