---
title: 子类型多态
abstract: 一切有为法，如梦幻泡影。
date: 2025 年 05 月 14 日
...

# 前言

面向对象编程中，有一个很重要的概念，即多态。在非面向对象编程中也有相应的概念，例如泛型或接口。这些概念，在编程语言层面上的各自的实现固然不同，但殊途同归，它们解决的问题本质上是相似的，即以同一逻辑处理不同的数据类型。有些专家将多态和泛型皆视为多态，前者称为子类型多态，后者称为参数化多态。

C 语言不是面向对象编程语言，亦非泛型编程语言，不过在「[模块类型](../module/index.html)」的基础上，只需略作工作，便可在 C 程序中模拟子类型多态。至于参数化多态，实际上早已在 [SimArray](../array/index.html)、[SimVal](../value/index.html) 以及 [SimList](./list/index.html) 等模块中模拟过了。

# 多态模块

定义 Foo 模块，其类型为 `FooMod`，继承 `SimMod`。

```c
/* foo.h */
#ifndef FOO_H
#define FOO_H
#include <sim-mod.h>
/* FooMod 的特性表 */
typedef struct foo_mod_traits FooModTraits;
/* 模块结构体 */
typedef struct foo_mod {
        SimMod *parent;
        const char *name;
        FooModTraits *traits; /* 特性表 */
} FooMod;

/* FooMod 的核心类型 */
typedef struct foo {
        FooMod *mod;
} Foo;

SIM_MOD_DECLARE(FooMod, foo_mod);
#define FOO_MOD(mod) ((FooMod *)(mod))
#define foo_mod_is(mod) FooMod_is(FOO_MOD(mod))
#endif
```

```c
/* foo.c */
#include "foo.h"
SIM_MOD_DEFINE(FooMod, foo_mod, SimMod, sim_mod);
```

需要注意的是，`Foo` 对象没有构造函数，这意味着 `Foo` 对象只有意义，没有存在。此外，上述代码与「[模块类型](../module/index.html)」中的示例相比，在 `FooMod` 的定义中多了一个所谓的特性表。

# 特性表

特性，实质上是函数指针，它可指向 `FooMod` 对象的某个子对象中的函数。特性表，是一组特性构成的结构体。

实现子类型多态，关键在于构造特性表。在面向对象编程语言里，特性表对应的事物通常称为虚函数表。我不喜欢虚函数这个术语，它的英文单词太长了。特性的英文单词 trait，更为简洁。

Foo 模块的特性表如下：

```c
struct foo_mod_traits {
        void (*zero)(void);
        void (*add)(void);
};
```

`zero` 可以指向一个用于构造零值的函数。`add` 可以指向一个求和的函数。不过，这些解释毫无意义，原因是这两个函数指针所指函数的类型即无参数也无返回值，与构造零值和求和或数据毫无关系。请相信你的判断和认识，的确如你所见，但是还需要耐心地向下看。

为了让意图更清晰，我在 sim-mod.h 里增加一个类型：

```c
/* sim-mod.h ++ */
typedef void(*SimTrait)(void);
```

以后，我用 `SimTrait` 可用于表示特性。

上述的 Foo 模块的特性表可修改为

```c
/* foo.h ++ */
struct foo_mod_traits {
        SimTrait zero;
        SimTrait add;
};
```

现在，`zero` 和 `add` 在语义上清晰了，它们是 Foo 模块的特性。

每个特性表需要对应一个协议（protocol）。例如，Foo 模块的特性表应当对应

```c
/* foo.h ++ */
typedef struct foo_mod_proto {
        Foo *(*zero)(void);
        void (*add)(Foo *, Foo *);
} FooModProto;
```

显然，`FooModProto` 也是一个函数指针表。为什么要将函数指针表分为特性表与协议呢？特性表是给 Foo 模块的继承者使用的，后者可将具体函数的地址写入特性表。

特性表与协议，它们包含的函数指针，名字和位置必须严格对等。建议在模块的头文件里，将模块的特性表与协议的定义相邻，以便在修改特性表时能记得对协议也作对应修改。

# 求和函数

在 foo.c 中定义一个多态的求和函数 `foo_sum`，它可以对一个数组里的所有对象求和，这些对象隶属的模块必须继承 Foo 模块。

```c
/* foo.h ++ */
Foo *foo_sum(SimArray *objects);
```

```c
/* foo.c ++ */
Foo *foo_sum(SimArray *objects) {
        /* 从 objects 任取一个对象，获取特性表并按协议自动转换 */
        Foo *sample = sim_array_raw(objects, 0, Foo *);
        FooModProto *proto = (FooModProto *)sample->mod->traits;
        /* 构造零值 */
        Foo *res = proto->zero();
        /* 求和 */
        for (size_t i = 0; i < objects->n; i++) {
                Foo *a = sim_array_raw(objects, i, Foo *);
                proto->add(res, a);
        }
        return res;
}
```

为了代码的清晰，在 `foo_sum` 的定义里未作任何安全处理。若追求安全，需对 `res` 以及 `objects` 中每个成员予以检测，如下：

```c
/* foo.c [改] */
Foo *foo_sum(SimArray *objects) {
        SIM_ASSERT_RET(object, NULL);
        /* 从 objects 任取一个对象，获取特性表，并按协议转换 */
        Foo *sample = sim_array_raw(objects, 0, Foo *);
        SIM_ASSERT_RET(sample, NULL);
        SIM_ASSERT_RET(foo_mod_is(sample->mod), NULL);
        FooModProto *proto = (FooModProto *)sample->mod->traits;
        /* 构造零值 */
        Foo *res = proto->zero();
        SIM_ASSERT_RET(foo_mod_is(res->mod), NULL);
        /* 求和 */
        for (size_t i = 0; i < objects->n; i++) {
                Foo *a = sim_array_raw(objects, i, Foo *);
                SIM_ASSERT_RET(foo_mod_is(a->mod), NULL);
                proto->add(res, a);
        }
        return res;
}
```

现在，`foo_sum` 尚无法使用，因为我们尚未定义继承 `FooMod` 且支持 `zero` 和 `add` 特性的模块类型。

# Foo 模块

Foo 模块的完整内容如下：

```c
/* foo.h */
#ifndef FOO_H
#define FOO_H
#include <sim-mod.h>
/* FooMod 的特性表 */
typedef struct foo_mod_traits FooModTraits;
/* 模块结构体 */
typedef struct foo_mod {
        SimMod *parent;
        const char *name;
        FooModTraits *traits; /* 特性表 */
} FooMod;

/* FooMod 的核心类型 */
typedef struct foo {
        FooMod *mod;
} Foo;

struct foo_mod_traits {
        SimTrait zero;
        SimTrait add;
};

typedef struct foo_mod_proto {
        Foo *(*zero)(void);
        void (*add)(Foo *, Foo *);
} FooModProto;

SIM_MOD_DECLARE(FooMod, foo_mod);
#define FOO_MOD(mod) ((FooMod *)(mod))
#define foo_mod_is(mod) FooMod_is(FOO_MOD(mod))

Foo *foo_sum(SimArray *objects);
#endif
```

```c
/* foo.c */
#include "foo.h"
SIM_MOD_DEFINE(FooMod, foo_mod, SimMod, sim_mod);

Foo *foo_sum(SimArray *objects) {
        SIM_ASSERT_RET(object, NULL);
        /* 从 objects 任取一个对象，获取特性表，并按协议转换 */
        Foo *sample = sim_array_raw(objects, 0, Foo *);
        SIM_ASSERT_RET(sample, NULL);
        SIM_ASSERT_RET(foo_mod_is(sample->mod), NULL);
        FooModProto *proto = (FooModProto *)sample->mod->traits;
        /* 构造零值 */
        Foo *res = proto->zero();
        SIM_ASSERT_RET(foo_mod_is(res->mod), NULL);
        /* 求和 */
        for (size_t i = 0; i < objects->n; i++) {
                Foo *a = sim_array_raw(objects, i, Foo *);
                SIM_ASSERT_RET(foo_mod_is(a->mod), NULL);
                proto->add(res, a);
        }
        return res;
}
```

# Bar 模块

定义 Bar 模块，使之继承 Foo 模块，并实现 Foo 模块的特性 `zero` 和 `add`。

```c
/* bar.h */
#ifndef BAR_H
#define BAR_H
#include <foo.h>
/* Bar 模块类型 */
typedef struct bar_mod {
        FooMod *parent;
        const char *name;
        FooModTraits *traits;
} BarMod;

/* Bar 模块的核心类型 */
typedef struct bar {
        BarMod *mod;
        double data;
} Bar;

SIM_MOD_DECLARE(BarMod, bar_mod);
#define BAR_MOD(mod) ((BarMod *)(mod))
#define bar_mod_is(mod) BarMod_is(BAR_MOD(mod))

/* Bar 模块初始化函数 */
BarMod *bar_mod_init(void);
/* Bar 对象构造函数和释放宏 */
Bar *bar(double data);
#define bar_free(obj) free(obj)

/* zero 和 add 特性的实现 */
Bar *bar_zero(void);
void bar_add(Bar *a, Bar *b);
#endif
```

```c
/* bar.c */
#include "bar.h"
SIM_MOD_DEFINE(BarMod, bar_mod, FooMod, foo_mod);
FooModTraits bar_traits = {
        (SimTrait)bar_zero,
        (SimTrait)bar_add
};

BarMod *bar_mod_init(void) {
        BarMod *mod = bar_mod();
        mod->traits = &bar_traits;
        return mod;
}

Bar *bar(double data) {
        Bar *a = malloc(sizeof(Bar));
        SIM_ASSERT_RET(a, NULL);
        a->mod = bar_mod_init();
        a->data = data;
        return a;
}

Bar *bar_zero(void) {
        Bar *a = bar(0);
        return a;
}

void bar_add(Bar *a, Bar *b) {
        SIM_ASSERT_RET(a && b);
        a->data += b->data;
}
```

# 求和示例


foobar.c 是多态求和函数 `foo_sum` 的用法示例。

```c
/* foobar.c */
#include <bar.h>
int main(void) {
        /* 构造 Bar 对象数组 */
        SimArray *bar_objects = sim_array(Bar *);
        sim_array_add(bar_objects, bar(1.0), Bar *);
        sim_array_add(bar_objects, bar(2.7), Bar *);
        sim_array_add(bar_objects, bar(3.14), Bar *);
        
        /* 求和：结果的类型需要转换为 Bar 对象 */
        Bar *sum = (Bar *)foo_sum(bar_objects);
        printf("result = %lf\n", sum->data);
        
        /* 资源释放 */
        bar_free(sum);
        for (size_t i = 0; i < bar_objects->n; i++) {
                bar_free(sim_array_raw(bar_objects, i, Bar *));
        }
        sim_array_free(bar_objects);
        return 0;
}
```

编译 foobar.c：

```console
$ gcc -I. sim-{err,str,err2,array,err3,mod}.c foo.c bar.c foobar.c -o foobar
```

运行 foobar，应当输出

```
result = 6.840000
```

# 总结

这一切……都是什么呢？

Foo 模块本质上是空的，它只拥有模块类型 `FooMod` 和数据类型 `Foo`，后者并无实体。在 C++ 之类的语言里，这种形式的事物称为虚基类。

Bar 模块继承自 Foo 模块，即 `BarMod` 的内存布局与 `FooMod` 兼容，亦即后者是前者的子集，亦即 `BarMod` 的前 `sizeof(FooMod)` 字节的布局必须与 `FooMod` 相同。

Foo 模块定义了多态函数 `foo_sum`，若调用该函数，必须实现 Foo 模块的两个特性 `zero` 和 `add`。Bar 模块以 `bar_zero` 和 `bar_add` 实现了它们，并将该情况记录于 `BarMod` 对象的特性表。每个 `Bar` 对象都含有 `BarMod` 对象，故而可通过 `Bar` 对象获取该特性表。

在 `foo_sum` 函数里，获取数组中存储的对象的特性表，按 `FooModProto` 协议将特性转化为可用的函数指针，从而完成数组中对象的求和运算，并将结果按 `Foo` 对象的形式返回。

`foo_sum` 返回的 `Foo` 对象指针实际上并无意义——毕竟 `Foo` 对象连构造函数都没有——，只是用于记录求和结果的地址，而 `foo_sum` 函数的调用者知道该地址意味着什么，通过类型转换便可取得最终的求和结果。

这一切不过是基于结构体内存布局以及指针类型的强行转换而精心设计的层层幻象。只是这些幻象并未脱离现实，它们是现实的抽象。如《周易·系辞传上》所言，在天成象，在地成形，变化见矣。抽象的意义并非仅仅是为了逻辑复用，更重要的是，在更大的尺度上呈现变化。

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
> * 模块类型：[sim-mod.h](sim-mod.h) 和 [sim-mod.c](sim-mod.c)
