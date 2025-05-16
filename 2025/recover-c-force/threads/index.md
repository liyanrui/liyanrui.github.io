---
title: 并发
abstract: 终有一天，程序的命运，不再由我们掌控。
date: 2025 年 05 月 16 日
...

# 前言

Sim 项目已经构造了 4 个全局变量。在错误机制的进化中，出现了 3 个，分别是 `sim_err`，`sim_err2` 和 `sim_err3`。在 SimMod 模块中，模块表 `sim_module_table` 也是全局变量。

在古代，程序里出现全局变量，便意味着程序在多线程环境里缺乏安全性，于是太需要线程安全的人便将全局变量视为洪水猛兽。这种观念延续至今，甚至导致某些现代编程语言故意为全局变量的使用增加难度，逼迫开发者放弃使用全局变量的想法。我始终觉得，全局变量是无罪的，罪责应该在多线程机制不能自然兼容全局变量的存在。

直到有一天，多线程机制开始支持线程局部存储，实现了对全局变量的兼容。又直到有一天，C11 标准开始支持多线程编程，在语言层面统一了多线程编程范式。现在，我们可以允许全局变量一笑倾城，也可以用 C 语言编写不受操作系统平台兼容性限制的多线程程序了。

C11 标准问世已十余载，GCC 编译器也已将其设为默认模式，Sim 项目应当对得起它对多线程编程的支持。现在，开始学习 C11 的多线程编程，尝试拥有它。

# 线程函数

每个程序都拥有一个主线程，通常我们无需关心它，因为它就是 `main` 函数。当程序运行时，操作系统会自动执行 `main` 函数。

除了 `main` 函数外，任何一个函数若想作为线程运行，其形式必须像以下示例：

```c
int foo(void *arg) {
        printf("i am thread function!\n");
        return 0;
}
```

即接受 1 个无类型指针且返回整型值的函数。

# 线程创建与回收

`thrd_create` 函数可以创建线程，其声明如下：

```c
#include <threads.h>
int thrd_create(thrd_t *thr, thrd_start_t func, void *arg);
```

`thrd_create` 接受 3 个参数，第一个是线程标识，第二个是线程函数，第三个是传递给线程函数的参数。若 `thrd_create` 成功创建线程，会返回 `thrd_success`，否则在内存不足以创建线程的情况下会返回 `thrd_nomem`，至于其他错误则返回 `thrd_error`。

`thrd_create` 的用法如下：

```c
thrd_t thread;
if (thrd_create(&thread, foo, NULL) != thrd_success) {
        printf("failed to create thread!");
}
```

其中 `foo` 是上一节所述的线程函数。

线程创建后，操作系统会令其与主线程并发运行。在主线程中，需对 `thrd_create` 创建的线程资源予以回收，否则程序会存在线程资源泄漏的隐患。`thrd_join` 函数可以回收线程资源，其声明如下：

```c
#include <threads.h>
int thrd_join(thrd_t thr, int *res);
```

`thrd_join` 的第一个参数是线程表示，第二个参数记录了线程函数的返回值。若 `thrd_join` 运行成功，会返回 `thrd_sccess`，否则返回 `thrd_error`。

以下代码演示了 `thrd_join` 的用法，回收上述示例创建的线程：

```c
int res;
if (thrd_join(thread, &res) == thrd_success) {
        printf("thread returns %d.\n", res);
}
```

以下代码是在主线程中创建和回收线程的完整程序 foo.c：

```c
/* foo.c */
#include <stdio.h>
#include <threads.h>

int foo(void *arg) {
        int *x = arg;
        printf("thread parameter is %d.\n", *x);
        return 0;
}

int main(void) {
        /* 创建线程 */
        thrd_t thread;
        int x = 3;
        if (thrd_create(&thread, foo, &x) != thrd_success) {
                fprintf(stderr, "failed to create thread!\n");
                return -1;
        }
        /* 回收线程 */
        int res;
        if (thrd_join(thread, &res) == thrd_error) {
                fprintf(stderr, "failed to join thread!\n");
                return -1;
        }
        printf("thread returns %d.\n", res);
        return 0;
}
```

使用以下命令编译 foo.c：

```console
$ gcc -std=c11 foo.c -o foo
```

运行 foo，其输出应为

```
thread parameter is 3.
thread returns 0.
```

需要永远记住的是，不同的线程是并发运行的，通常你可以将这一点理解为，不同的线程是同时运行的，不分先后。只有记住这一点，你才会对多线程编程有所敬畏。多线程编程范式，若用得好，程序的性能可以倍增，若用得不好，程序的行为难以预测，且程序性能甚至不及传统的单线程范式。

# 加锁

一个程序，若存在多个函数共享某个变量的情况，这些函数通常难以作为线程并发运行。若强行使然，必须在以牺牲一定程度的并发性能为代价。使用互斥锁，将程序中访问共享变量的代码区域保护起来，便是强行使然的办法。

`mtx_init` 函数可创建互斥锁，其声明如下：

```c
include <threads.h>
int mtx_init(mtx_t *mutex, int type);
```

`mtx_init` 的第一个参数是互斥锁标识，第二个参数是互斥锁的类型，最为简单且常用的类型是 `mtx_plain`。若 `mtx_init` 运行成功，会返回 `thrd_success`，否则返回 `thrd_error`。

`mtx_init` 的用法如下：

```c
mtx_t mutex;
if (mtx_init(&mutex, mtx_plain) == thrd_error) {
        fprintf(stderr, "failed to init mutex!\n");
}
```

成功创建互斥锁后，便可用它锁定一段代码。互斥锁的锁定和解锁函数是

```c
include <threads.h>
int mtx_lock(mtx_t *mutex);
int mtx_unlock(mtx_t *mutex);
```

这两个函数若成功运行，皆返回 `thrd_success`，否则返回 `thrd_error`。下面是在一个线程函数里，对一段访问全局变量的代码进行锁定：

```c
int shared_counter = 0;

int foo(void* arg) {
        /* 加锁 */
        if (mtx_lock(&mutex) == thrd_error) {
                fprintf(stderr, "failed to lock mutex!\n");
                return -1;
        }
        shared_counter++;
        printf("Counter: %d\n", shared_counter);
        /* 解锁 */
        if (mtx_unlock(&mutex) == thrd_error) {
                fprintf(stderr, "failed to unlock mutex!\n");
                return -1;
        }
        return 0;
}
```

当 `foo` 函数在多个线程中运行时，原本并发运行的线程，会在 `foo` 函数的加锁区域前排队，逐一运行加锁区域里的代码。如同人群可以并发涌入车站，但是必须在售票口前排队买票。

加锁可以解决多线程访问共享变量的问题，但存在着很大的妥协性，导致多线程因此失去并发能力。我的建议是，若某个函数，原本是线程不安全的，用加锁的办法可将其变成线程安全的，但是又可以确定即便如此，程序的运行效率不会因此而得到明显改善，便意味着应该尽量避免让这个函数运行于多线程环境。

# 条件变量

除了并发能力，多线程机制也能构造可以等待时机的线程，即一个线程运行后，可进入休眠或挂起状态，等待另一个线程的唤醒。函数 `cnd_wait` 可让线程休眠，而 `cnd_signal` 可唤醒休眠的线程。这两个函数的声明如下：

```c
#include <threads.h>
int cnd_wait(cnd_t *cond, mtx_t *mutex);
int cnd_signal(cnd_t *cond);
```

`cnd_wait` 和 `cnd_signal` 的第一个参数皆为条件变量标识。`cnd_wait` 的第二个参数是互斥锁，这意味着线程的休眠和唤醒需要互斥锁。这两个函数若成功运行，皆会返回 `thrd_success`，否则返回 `thrd_error`。

假设已存在条件变量 `cond` 和互斥锁 `mutex`，`cnd_wait` 的用法如下：

```c
mtx_lock(&mutex); /* 加锁 */
cnd_wait(&cond, &mutex); /* 休眠 */
mtx_unlock(&mutex); /* 解锁 */
```

`cnd_wait` 会对互斥锁进行解锁，并将线程就此休眠，其后的代码则处于待运行状态。当某个线程，使用 `cnd_signal` 通过同一条件变量唤醒休眠的线程时，`cnd_wait` 之后的代码便可得以运行。针对上述示例代码，`cnd_signal` 的用法如下：

```c
mtx_lock(&mutex); /* 加锁 */
cnd_signal(&cond); /* 唤醒休眠线程 */
mtx_unlock(&mutex); /* 解锁 */
```

通过上述示例代码，应当能领悟到为何 `cnd_wait` 和 `cnd_signal` 为何需要互斥锁了，因为条件变量是两个线程的共享变量。

上述 `cnd_wait` 的用法示例并不稳定，因为休眠线程可能会被虚假唤醒。有些操作系统可能为了减少线程切换的延迟，允许在某些情况下（如锁竞争激烈时）无视条件变量的存在而提前唤醒休眠线程。为了避免线程被虚假唤醒，需要再引入一个共享变量，以它为条件，让线程在被虚假唤醒时可再度休眠。例如，假设存在全局变量 `ready`：

```c
static int ready = 0;
```

使用 `ready` 防止休眠线程被虚假唤醒的方法如下：

```c
mtx_lock(&mutex); /* 加锁 */
while (ready == 0) {
        cnd_wait(&cond, &mutex); /* 休眠 */
}
mtx_unlock(&mutex); /* 解锁 */
```

唤醒线程的方式如下：

```c
mtx_lock(&mutex); /* 加锁 */
ready = 1;
cnd_signal(&cond); /* 唤醒休眠线程 */
mtx_unlock(&mutex); /* 解锁 */
```

以下代码是线程休眠和唤醒的完整示例。为了简明，该示例码调用的所有线程函数均未作安全防御处理。

```c
/* foobar.c */
#include <stdio.h>
#include <threads.h>

mtx_t mutex;
cnd_t cond;
int ready = 0;

int foo(void *arg) {
        mtx_lock(&mutex);
        printf("foo: sleeping.\n");
        while (ready == 0) {
                cnd_wait(&cond, &mutex);
        }
        printf("foo: awakened!\n");
        mtx_unlock(&mutex);
        return 0;
}

int bar(void *arg) {
        mtx_lock(&mutex);
        ready = 1;
        cnd_signal(&cond);
        printf("bar: i woke up foo!\n");
        mtx_unlock(&mutex);
}

int main(void) {
        /* 创建线程 */
        thrd_t foo_thrd, bar_thrd;
        thrd_create(&foo_thrd, foo, NULL);
        thrd_create(&bar_thrd, bar, NULL);
        /* 回收线程 */
        thrd_join(foo_thrd, NULL);
        thrd_join(bar_thrd, NULL);
        return 0;
}
```

上述程序应当输出以下内容：

```
bar: i woke up foo!
foo: sleeping.
foo: awakened!
```

也可能是

```
foo: sleeping.
bar: i woke up foo!
foo: awakened!
```

# 线程局部存储

线程局部存储（Thread-Local Storage, TLS）允许每个线程拥有一个全局变量的独立副本，每个线程对该变量的读写操作仅影响自身的副本，不与其他线程共享。

线程局部存储技术的原理是，类似于给足球场上的球员每人发了个足球，让他们不再为争抢 1 个球而头破血流了。此事听起来固然可笑，但的确能解决 Sim 项目的两个全局变量而导致的线程不安全问题，而且解决的方式非常简单。例如，对于 `sim_err3`，只需将 sim-err3.c 中的 `sim_err3` 的定义修改为

```c
/* sim-err3.h [改] */
extern _Thread_local SimErr3 sim_err3;
```

```c
/* sim-err3.c [改] */
_Thread_local SimErr3 sim_err3 = NULL;
```

亦即在 `sim_err3` 原来的定义前面增加了 `_Thread_local` 关键字。该关键字是 C11 标准引入的，用于将全局变量变为每个线程的独立副本。

以下示例，展示了 `_Thread_local` 修饰的全局变量在两个线程中的表现。

```c
/* foo.c */
#include <stdio.h>
#include <threads.h>

_Thread_local static int a = 0;

int foo(void *arg) {
        printf("foo thread: a = %d\n", a);
        return 0;
}

int main(void) {
        /* 在主线程中修改 a 的值 */
        a = 3;
        printf("main thread: %d\n", a);
        sleep(a); /* 让程序阻塞 3 秒 */
        /* 创建 foo 线程 */
        thrd_t foo_t;
        thrd_create(&foo_t, foo, NULL);
        thrd_join(foo_t, NULL);
        return 0;
}
```

上述程序输出结果类似于以下结果：

```c
main thread: 0x756e2a2ac73c = 3
foo thread: 0x756e29fff6bc = 0
```

在主线程中对全局变量 `a` 的值进行了修改，但是这种修改不会影响 `foo` 线程，原因是二者所拥有的全局变量 `a` 并非同一个变量，通过上述的输出结果可以看到，它们的地址不同。

# 信号并发处理

在 Sim 项目中，有一处代码，若将其修改为多线程形式，是展示多线程编程范式优势的绝佳机会。在「[信号](../signal/index.html)」中，用于调度总线上所有设备的 `sim_bus_schedule` 函数，其定义如下：

```c
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

倘若将每个设备的路由函数 `router` 置于一个新的线程，则 `sim_bus_schedule` 便可近乎同时驱动总线上的全部设备。改造方法很简单，首先定义一个线程函数：

```c
static int router_run(void *data) {
        SimDevice *dev = data;
        dev->output = dev->router(dev->sig, dev->input);
        return 0;
}
```

然后在 `sim_bus_schedule` 函数中创建线程：

```c
void sim_bus_schedule(SimList *bus) {
        if (!bus) {
                SIM_ERR3("invalide bus!");
                return;
        }
        /* 创建线程 */
        SimArray *threads = sim_array(thrd_t);
        for (SimLink *it = bus->head; it; it = it->next) {
                SimDevice *dev = sim_link_raw(it, SimDevice *);
                if (dev->active) {
                        thrd_t t;
                        thrd_create(&t, router_run, dev);
                        sim_array_add(threads, t, thrd_t);
                }
        }
        /* 回收线程 */
        for (size_t i = 0; i < threads->n; i++) {
                thrd_t t = sim_array_raw(threads, i, thrd_t);
                thrd_join(t, NULL);
        }
        sim_array_free(threads);
}
```

由于运行 `router_run` 函数的所有线程，面对的设备是不同的，故而不会出现数据读写冲突问题，自然也无需加锁。

# 总结

不必强行使用多线程，否则代码里就到处都是锁了。

努力消除程序中低层次的耦合——共享变量和函数调用关系，让真正需要使用多线程的场景清晰地暴露出来。唯有如此，方能真正发挥多线程的力量。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
> * 错误机制 2：[sim-err2.h](sim-err2.h) 和 [sim-err2.c](sim-err2.c)
> * 动态数组：[sim-array.h](sim-array.h) 和 [sim-array.c](sim-array.c)
> * 错误机制 3（线程安全版本）：[sim-err3.h](sim-err3.h) 和 [sim-err3.c](sim-err3.c)
> * 无类型的值：[sim-val.h](sim-val.h) 和 [sim-val.c](sim-val.c)
> * 双向链表：[sim-list.h](sim-list.h) 和 [sim-list.c](sim-list.c)
> * 信号、路由和总线（多线程版本）：[sim-sig.h](sim-sig.h) 和 [sim-sig.c](sim-sig.c)
> * 模块类型（线程安全版本）：[sim-mod.h](sim-mod.h) 和 [sim-mod.c](sim-mod.c)
