---
title: 信号
abstract: 你等，也可以不等。信号就在那里，不动不静。
date: 2025 年 05 月 04 日
...

# 前言

基于「[连接](../connection/index.md)」，可有效解除两个函数的耦合。对于任何程序而言，解除其内部存在的耦合，通常可降低其设计、实现以及维护方面的难度。我们可否用类似的方式，实现模块之间的通信呢？显然，解除模块之间的耦合，获得的收益应当远大于解除函数之间的耦合。

与函数相比，模块更像是生命体，它可以拥有数据以及操作数据的函数。若每个模块能拥有一组信号，其中每个信号与模块里的某个函数对应，那么不同模块之间便可基于信号实现协作。如此，便可避免因模块的耦合而带来的诸多麻烦。这是一个非常伟大的想法，可惜它不是我想出来的。在上古时代，Unix 系统里的进程之间早已可以基于信号实现通信了。一些 GUI 库也实现了类似的机制，例如 Qt 和 GTK 皆实现了信号机制。

我原本的计划是，仅在「[连接](../connection/index.md)」中阐明其基本原理，但终归无法抗拒自己想亲手一试或重新发明轮子的念头，故有模块 SimSig。

# 信号

用 C 字符串类型表示信号：

```c
/* sim-sig.h ++ */
typedef const char * SimSig;
```

函数 `sim_sig_is` 用于判断两个信号是否相同。

```c
/* sim-sig.h ++ */
bool sim_sig_is(SimSig a, SimSig b);
```

```c
/* sim-sig.c ++ */
bool sim_sig_is(SimSig a, SimSig b) {
        return (strcmp(a, b) == 0) ? true : false;
}
```


# 路由函数

每个模块都必须实现一个函数，我们将其称为路由函数，它可将模块外部发来的信号映射到对应的函数上。路由函数的形式必须是

```c
/* sim-sig.h ++ */
typedef SimVal *(*SimRouter)(SimSig sig, SimVal *v);
```

# 总线

解除任意两个事物的耦合，最有效的办法是，创造一个新的事物，去包含前者，并将前者的耦合化为自身的结构。对于模块之间的耦合，我们要创造的新事物是总线。实际上我们早已创造了它，即双向链表。总线用于连接各个设备。每个设备由激活状态、可接收的信号、路由函数以及输入/输出构成。

```c
/* sim-sig.c ++ */
typedef struct {
        bool active;
        SimSig sig;
        SimRouter router;
        SimVal *input;
        SimVal *output;
} SimDevice;
```

`SimDevice` 是 SimSig 模块的内部类型，外部不可见。外部只能看到 `sim_bus_connect` 函数制造的假象：信号与路由函数的连接。

```c
/* sim-sig.h ++ */
void sim_bus_connect(SimList *bus, SimSig sig, SimRouter router);
```

```c
/* sim-sig.c ++ */
void sim_bus_connect(SimList *bus, SimSig sig, SimRouter router) {
        if (!bus) {
                SIM_ERR3("invalid bus!");
                return;
        }
        if (!router) {
                SIM_ERR3("invalid router!");
                return;
        }
        SimDevice *dev = malloc(sizeof(SimDevice));
        if (!dev) {
                SIM_ERR3("failed to allocate memory for device!");
                return;
        }
        *dev = (SimDevice){false, sig, router, NULL, NULL};
        sim_list_suffix(bus, dev, SimDevice *);
}
```

`sim_bus_del` 函数可从总线上根据信号删除一个设备。

```c
/* sim-sig.h ++ */
void sim_bus_del(SimList *bus, SimSig sig);
```

```c
/* sim-sig.c ++ */
void sim_bus_del(SimList *bus, SimSig sig) {
        if (!bus) {
                SIM_ERR3("invalid bus!");
                return;
        }
        SimLink *it = bus->head;
        while (it) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (sim_sig_is(dev->sig, sig)) {
                        free(dev);
                        SimLink *next = it->next;
                        sim_list_del(bus, it);
                        it = next;
                        continue;
                }
                it = it->next;
        }
}
```

`sim_bus_free` 用于释放总线。

```c
/* sim-sig.h ++ */
void sim_bus_free(SimList *bus);
```

```c
/* sim-sig.c ++ */
void sim_bus_free(SimList *bus) {
        if (!bus) return;
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                free(dev);
        }
        sim_list_free(bus);
}
```

# 发射信号

`sim_bus_emit` 函数可向总线发出信号和数据。

```c
/* sim-sig.h ++ */
void sim_bus_emit(SimList *bus, SimSig sig, SimVal *value);
```

```c
/* sim-sig.c ++ */
void sim_bus_emit(SimList *bus, SimSig sig, SimVal *value) {
        if (!bus) {
                SIM_ERR3("invalid bus!");
                return;
        }
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (sim_sig_is(dev->sig, sig)) {
                        if (dev->active || dev->input) {
                                SIM_ERR3("device busy!");
                                continue;
                        }
                        dev->input = value;
                        dev->active = true; /* 激活设备 */
                }
        }
}
```

# 总线调度

`sim_bus_schedule` 函数可让总线上所有激活的设备进入工作状态。

```c
/* sim-sig.h ++ */
void sim_bus_schedule(SimList *bus);
```

```c
/* sim-sig.c ++ */
void sim_bus_schedule(SimList *bus) {
        if (!bus) {
                SIM_ERR3("invalide bus!");
                return;
        }
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (dev->active) {
                        dev->output = dev->router(dev->sig, dev->input);
                }
        }
}
```

# 数据收集

`sim_bus_wait` 可从总线收集某个信号的响应者的反馈。

```c
/* sim-sig.h ++ */
SimArray *sim_bus_wait(SimList *bus, SimSig sig);
```

```c
/* sim-sig.c ++ */
SimArray *sim_bus_wait(SimList *bus, SimSig sig) {
        if (!bus) {
                SIM_ERR3("invalide bus!");
                return NULL;
        }
        SimArray *values = sim_array(SimVal *);
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (!dev->active) continue;
                if (sim_sig_is(dev->sig, sig)) {
                        if (dev->output) {
                                sim_array_add(values, dev->output, SimVal *);
                                dev->output = NULL;
                        }
                        /* 设备重置，等待下一次被激活 */
                        dev->input = NULL;
                        dev->active = false;
                }
        }
        return values;
}
```

# 示例

下面是一个简单的 Foo 模块，可以响应 `"hi"` 信号。

```c
/* foo.h */
#include <sim-sig.h>

void foo_hi(void);
SimVal *foo_router(SimSig sig, SimVal *v);
```

```c
/* foo.c */
#include "foo.h"
void foo_hi(void) {
        printf("hi, i am foo!\n");
}
SimVal *foo_router(SimSig sig, SimVal *v) {
        if (sim_sig_is(sig, "hi")) {
                foo_hi();
                return NULL;
        }
        return NULL;
}
```

在程序的主模块，向总线发射 `"hi"` 信号，实现对 `foo_hi` 的调用。

```c
/* test.c */
#include "foo.h"
int main(void) {
        SimList *bus = sim_list();
        sim_bus_connect(bus, "hi", foo_router);
        sim_bus_emit(bus, "hi", NULL);
        sim_bus_schedule(bus);
        sim_bus_free(bus);
        return 0;
}
```

编译 test.c，运行所得程序：

```c
$ gcc -I. sim-{err,str,err2,array,err3,val,list,sig}.c foo.c test.c -o test
$ ./test
hi, i am foo!
```

# 总结

在更高的层次上消除低层事物的耦合。相濡以沫，不若相忘于江湖。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
> * 动态数组：[sim-array.h](sim-array.h) 和 [sim-array.c](sim-array.c)
> * 错误机制 3：[sim-err3.h](sim-err3.h) 和 [sim-err3.c](sim-err3.c)
> * 无类型的值：[sim-val.h](sim-val.h) 和 [sim-val.c](sim-val.c)
> * 双向链表：[sim-list.h](sim-list.h) 和 [sim-list.c](sim-list.c)
> * 信号、路由和总线：[sim-sig.h](sim-sig.h) 和 [sim-sig.c](sim-sig.c)
