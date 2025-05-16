---
title: 连接
abstract: 相濡以沫，不若相忘于江湖。
date: 2025 年 04 月 26 日
...

# 前言

错误机制已成，程序有了史官。

动态数组也进化成了双向链表，程序自此可以自由伸展。

现在我们思考一个新的主题。一个函数 `foo` 调用了另一个函数 `bar`，若后者也想调用前者，该如何实现呢？

# 声明

在源码文件 foobar.c 里，定义函数 `foo` 和 `bar`，在 `main` 函数里调用 `foo`：

```c
/* foobar.c */
#include <stdio.h>
#include <string.h>
void bar(const char *caller) {
        printf("bar: hi, %s!\n", caller);
        foo("bar");
}
void foo(const char *caller) {
        printf("foo: hi, %s\n", caller);
        if (strcmp(caller, "bar") == 0) return;
        bar("foo");
}
int main(void) {
        foo("main");
        return 0;
}
```

上述的 foobar.c 是无法通过编译的，原因是，在 C 语言里，若一个函数调用另一个函数，后者的定义必须先于前者出现。若 `bar` 能调用 `foo`，则 `foo` 必须在 `bar` 之前定义，但是上述代码为了让 `foo` 能调用 `bar`，`bar` 的定义已经先于 `foo` 出现了。

C 语言的设计者用了声明的语法克服了上述问题，即只要提前声明函数，便可无需考虑函数定义的先后。例如，在上述代码片段添加以下内容，便可实现 `bar` 反向调用 `foo` 了。

```c
/* foo.c ++ */
/* [上文]: #include <stdio.h> */
void foo(const char *caller);
void bar(const char *caller);
/* [下文]: void bar(const char *... */
```

现在，便可顺利编译 foobar.c，若运行所得程序，可得到以下输出：

```
foo: hi, main!
bar: hi, foo!
foo: hi, bar!
```

也许你觉得上述的 foobar.c 程序平平无奇，不过是 C 程序的常规形式，然而越是寻常事物，越可能蕴含着巨大的惊奇。你想藏一件珍奇之物，最好的办法是让所有人都不觉得它珍奇，而非藏舟于壑，藏山于泽，谓之固也。

若将 C 编译器视为通信业务运营商，我们向其注册了两个电话号码：

```c
void foo(const char *caller);  /* 主叫号码 */
void bar(const char *caller);  /* 被叫号码 */
```

上述代码片段便可拥有一个新的理解角度，即 `main` 呼叫了 `foo`。`foo` 呼叫了 `bar`。`bar` 向 `foo` 问好。`foo` 回应了 `bar`。

不过，作为通信运营商的 C 编译器只能在函数之间建立专线通信，这种方式非常浪费线路。试想，若 `foo` 函数呼叫成百上千的不同的函数，则必须为它建立同样数量的线路，亦即在 `foo` 函数的定义里，要写出这些函数的调用语句，想必谁也不愿意做这种体力活。

# 回调函数

在 C 语言中，可以通过指针，将一个函数作为另一个函数的参数，后者可通过该指针调用前者，通常称前者为回调（Callback）函数。这个术语很容易让初学者跌入它编织的概念陷阱里难以逃逸。倘若你一时不知何谓「回调」，不妨先将其放下，待时机成熟，你自会理解它，这就是回调。callback 的原意是稍候调用。例如，英文里可以说， I'll call you back。

基于回调函数，`foo` 函数只需一条线路便可呼叫任何与它连接的函数，亦即任何一个可被 `foo` 呼叫的函数，变成了 `foo`的的参数。

```c
void foo(const char *caller, void (*callback)(const char *)) {
        printf("foo: hi, %s\n", caller);
        if (strcmp(caller, "bar") == 0) return;
        callback("foo");
}
```

上述代码中的 `callback` 便是函数指针，莫要认为它是返回 `void *` 类型的函数，而应将其理解为指向某个函数的指针，该函数的参数类型为 `const char *` 且无返回值。上一节定义的函数 `bar` 便符合 `callback` 所指类型。以下代码里， `main` 函数呼叫了 `foo` 并嘱托它呼叫 `bar`。

```c
int main(void) {
        foo("main", bar);
        return 0;
}
```

程序的输出结果与上一节相同。如此，定义 `foo` 函数时无需再为它铺设成千上万条线路，不过问题仍在，只是转移到了 `main` 函数里了，亦即 `main` 函数里需要成千上万条 `foo` 函数调用语句，例如

```c
int main(void) {
        foo("main", bar_1);
        foo("main", bar_2);
        foo("main", bar_3);
        ... ... ... ... ...
        return 0;
}
```

这是没有办法的事，总要有人做这件事。做这件事的 `main` 函数便成了通信运营商了。倘若将这件事交给 `foo` 来做，无异于 `foo` 呼叫其他函数，自己运营了一个通信公司。

# 对称性

`foo` 函数有什么特殊的吗？为何必须是它主动呼叫其他函数，实际上它不也是被 `main` 函数呼叫的吗？除了作为通信运营商的 `main` 函数，其他任何一个函数都不具备特殊性，它们应当既可作为主叫方，亦可作为被叫方。

为了消除 `foo` 函数的特殊性，将 `foo` 和 `bar` 都变成简单的被叫函数，即二者并不存在调用关系。

```c
void foo(const char *caller) {
        printf("foo: hi, %s\n", caller);
}
void bar(const char *caller) {
        printf("bar: hi, %s\n", caller);
}
```

然后定义函数 `connect`，它不呼叫其他函数，也不被除 `main` 函数之外的其他函数呼叫，它的任务就是接线，为通信双方铺设线路。

```c
/* 电话机类型 */
typedef void (*Phone)(const char *caller);

/* 通话双方形成的连接 */
typedef struct connection {
         /* 通话方 x */
         Phone x; const char *x_name;
         /* 通话方 y */
         Phone y; const char *y_name;
} Connection;

/* 构造连接表 */
void connect(SimList *connections, 
             Phone x, const char *x_name, 
             Phone y, const char *y_name) {
        Connection one = {.x = x, .x_name = x_name, 
                          .y = y, .y_name = y_name};
        sim_list_suffix(connections, one, Connection);
}
```

`Phone` 是函数指针类型，可以指向上述重新定义的 `foo` 和 `bar` 的函数。`connect` 函数很简单，就是将两个回调函数集中到一个 `Connection` 对象里，再将该对象存入链表 `connections`。

现在，我们在 `main` 函数中为 `foo` 和 `bar` 接线，并让二者通话：

```c
int main(void) {
        SimList *x = sim_list();
        /* 连接 foo 和 bar */
        connect(x, foo, "foo", bar, "bar");
        /* 通话 */
        for (SimLink *it = x->head; it; it = it->next) {
                Connection c = sim_link_raw(it, Connection);
                c.x(c.y_name); /* x 呼叫 y */
                c.y(c.x_name); /* y 回应 x */
        }
        sim_list_free(x);
        return 0;
}
```

`main` 函数的输出应当如下：

```
foo: hi, bar!
bar: hi, foo!
```

上述代码，我们用一个生成连接的函数 `connect`，将两个看似无关的函数 `foo` 和 `bar` 绑定为一个连接，然后将该连接添加到一个列表中。之后，遍历列表，执行被绑定的函数。如此，原本存在相互调用关系的 `foo` 和 `bar`，此时则相当独立，甚至不知在某个时刻，它们彼此形成了调用关系，只是这个调用关系是在 `main` 函数中构建的。正所谓，相濡以沫，不若相忘于江湖。

# foobar 程序

完整的 foobar.c 如下：

```c
/* foobar.c */
#include <sim-list.h>
/* 电话机类型 */
typedef void (*Phone)(const char *caller);
/* 通话双方形成的连接 */
typedef struct connection {
         /* 通话方 x */
         Phone x; const char *x_name;
         /* 通话方 y */
         Phone y; const char *y_name;
} Connection;

/* 构造连接表 */
void connect(SimList *connections, 
             Phone x, const char *x_name, 
             Phone y, const char *y_name) {
        Connection one = {.x = x, .x_name = x_name, 
                          .y = y, .y_name = y_name};
        sim_list_suffix(connections, one, Connection);
}
void foo(const char *caller) {
        printf("foo: hi, %s\n", caller);
}
void bar(const char *caller) {
        printf("bar: hi, %s\n", caller);
}

int main(void) {
        SimList *x = sim_list();
        /* 连接 foo 和 bar */
        connect(x, foo, "foo", bar, "bar");
        /* 通话 */
        for (SimLink *it = x->head; it; it = it->next) {
                Connection c = sim_link_raw(it, Connection);
                c.x(c.y_name); /* x 呼叫 y */
                c.y(c.x_name); /* y 回应 x */
        }
        sim_list_free(x);
        return 0;
}
```

在 Bash 环境中编译 foobar.c 并运行所得程序：

```console
$ gcc -I. sim-{err,str,err2,array,err3,val,list}.c foobar.c -o foobar
$ ./foobar
foo: hi, bar!
bar: hi, foo!
```

# 总结

为什么要颇费心力将函数直接的调用关系转化为通信方式呢？回答这个问题，最好的方式是反问，为什么人类要发明写信、电报以及电话等通信方式呢？函数不是人类，不妨将其视为人类的行为，而函数依赖的数据则可视为人类的躯体。每个人专心于自己的工作和自己的感受，彼此之间以各种通信方式沟通，与耳提面命相比，通信效率固然略低，但是群体协作性却得以飞升，以胜任各种复杂的任务。不过，意义固然重大，但本文仅是 SimList 模块用法示例，且幸好 C 语言有函数指针类型。
