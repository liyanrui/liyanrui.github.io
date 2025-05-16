---
title: 模块类型
abstract: 沿着掌纹，烙着宿命，今宵酒醒无梦。
date: 2025 年 05 月 07 日
...

# 前言

Sim 项目已有多个模块了。每个模块都是以某种数据类型为核心，设计出一些功能上彼此正交的函数……像傅里叶变换。

模块本身是否可以成为类型呢，就像面向对象编程中的类？SimMod 模块是将模块类型化的一个尝试，模拟面向对象编程语言中的类和继承。

# 模块类型

用以下结构体表达模块：

```c
/* sim-mod.h ++ */
typedef struct sim_mod SimMod;
struct sim_mod {
        SimMod *parent;
        const char *name;
};
```

上述定义表明，允许一个模块拥有一个父模块，亦即模块之间可存在继承关系。根模块的 `parent` 为 `NULL`。

务必记住，每个模块，只能具化为单个对象，该对象存储于全局数组 `sim_module_table`。

```c
/* sim-mod.h ++ */
extern SimArray *sim_module_table;
```

```c
/* sim-mode.c ++ */
SimArray *sim_module_table = NULL;
```

# 注册

`sim_mod` 函数是 `SimMod` 对象的构造函数，该函数将创建唯一的 `SimMod` 对象，将其存入 `sim_module_table`，并将其保存于私有的全局变量 `this_mod`，该过程称为模块注册。

```c
/* sim-mod.h ++ */
SimMod *sim_mod(void);
```

```c
/* sim-mod.c ++ */
static SimMod *this_mod = NULL;
SimMod *sim_mod(void) {
        SIM_MOD_REG(sim_module_table, this_mod, SimMod);
        return this_mod;
}
```

上述代码中所用的 `SIM_MOD_REG`，其定义如下：

```c
/* sim-mod.h ++ */
#define SIM_MOD_REG(modules, mod, type) do { \
        static size_t id = 0; \
        if (id == 0) { \
                if (!modules) { \
                        modules = sim_array(SimMod *); \
                } \
                mod = malloc(sizeof(type)); \
                if (!mod) { \
                        SIM_ERR3("failed to allocate memory for Module object!"); \
                        return NULL; \
                } \
                mod->parent = NULL; \
                mod->name = #type; \
                sim_array_add(modules, mod, type *); \
                id = modules->n; \
        } \
        mod = sim_array_raw(modules, id - 1, type *); \
} while (0)
```

上述代码通过静态变量 `id`，让 `sim_module_table` 的构建以及模块注册过程只运行一次。第 2 次执行 `sim_mod` 函数，它会直接返回 `sim_module_table` 中索引为 `id - 1` 的模块。在面向对象编程领域中，像 `sim_mod` 这样的过程，有一个专业的称谓，单例模式，即通过 `sim_mod` 只能为 `SimMod` 构造唯一的对象。

需要注意的是，`SimMod` 类型唯一的对象，其 `parent` 为 `NULL`，意味着它没有父模块对象可继承，但是它可被未来的新模块继承。

由于模块类型只拥有唯一的对象，很多时候为了便于叙述，不对模块类型、模块类型的对象以及模块对象这些术语加以区分，而是将其统称为模块。

# 模块自省

`SimMod_is` 可判断模块是否为 `SimMod`。

```c
/* sim-mod.h ++ */
bool SimMod_is(SimMod *mod);
```

```c
/* sim-mod.c ++ */
bool SimMod_is(SimMod *mod) {
        SIM_MOD_IS_COMMON(mod, this_mod);
}
```

`SIM_MOD_IS_COMMON` 的定义如下：

```c
/* sim-mod.h ++ */
#define SIM_MOD_IS_COMMON(mod_a, mod_b) do { \
        SimMod *t = (SimMod *)(mod_a); \
        while (1) { \
                if (!t) { \
                        SIM_ERR3("invalid object!"); \
                        return false; \
                } \
                if ((void *)t == (void *)(mod_b)) return true; \
                t = t->parent; \
        } \
        return false; \
} while (0)
```

# 定义新模块

为了便于定义新模块，提供了 `SIM_MOD_DECLARE` 和 `SIM_MOD_DEFINE` 宏，前者在模块的头文件（.h 文件）中自动生成三个函数的声明，后者在模块源文件（.c 文件）中定义这三个函数。

```c
/* sim-mod.h ++ */
#define SIM_MOD_DECLARE(ModType, mod_type) \
        ModType *mod_type(void); \
        bool ModType##_is(ModType *mod); \
        ModType *mod_type##_this(ModType *mod)
```

```c
/* sim-mod.h ++ */
#define SIM_MOD_DEFINE(ModType, mod_type, ParentModType, parent_mod_type) \
        static ModType *this_mod = NULL; \
        ModType *mod_type(void) { \
                ParentModType *parent = parent_mod_type(); \
                SIM_MOD_REG(sim_module_table, this_mod, ModType); \
                this_mod->parent = parent; \
                this_mod->name = #ModType; \
                return this_mod; \
        } \
        bool ModType##_is(ModType *mod) { SIM_MOD_IS_COMMON(mod, this_mod); } \
        ModType *mod_type##_this(ModType *mod) { \
                return mod_type##_is(mod) ? this_mod : NULL; \
        } \
        /* 下面这行代码可让宏调用语句后面允许出现分号而不引发编译器警告 */ \
        void foo_this_func_not_defined_forever(void)
```

`SIM_MOD_DECLARE` 的用法如下：

```c
SIM_MOD_DECLARE(FooMod, foo_mod);
```

上述代码可展开为

```c
SimMod *foo_mod(void);
bool FooMod_is(SimMod *mod);
FooMod *foo_mod_this(SimMod *mod);
```

`SIM_MOD_DEFINE` 用法如下：

```c
SIM_MOD_DEFINE(FooMod, foo_mod, SimMod, sim_mod);
```

上述代码的展开结果为

```c
static FooMod *this_mod = NULL;
FooMod *foo_mod(void) {
        SimMod *parent = parent_mod();
        SIM_MOD_REG(sim_module_table, this_mod, FooMod);
        this_mod->parent = parent;
        this_mod->name = "FooMod";
        return (SimMod *)this_mod;
}
bool FooMod_is(SimMod *mod) { 
        SIM_MOD_IS(mod, this_mod);
}
FooMod *foo_mod_this(SimMod *mod) {
        return foo_mod_is(mod) ? this_mod : NULL;
}
/* 一个不会被任何人使用的私有全局变量，为了迁就编译器，
  令其不对 SIM_MOD_DEFINE 调用语句后面的分号给出警告。
  因为严格的 C 标准不允许函数定义的末尾出现分号。 */
static const char *this_mod_name = "FooMod";
```

# 类型转换

凡是继承 SimMod 的模块，皆可通过 `SIM_MOD` 宏，将模块类型降级为 `SimMod`。`SIM_MOD` 宏的定义如下：

```c
/* sim-mod.h ++ */
#define SIM_MOD(mod) ((SimMod *)(mod))
```

继承 SimMod 的模块，也建议定义类似的宏。假设 FooMod 继承 SimMod，应当为前者定义以下形式的类型转换宏：

```c
#define FOO_MOD(mod) ((FooMod *)(mod))
```

若判断某个模块 `x` 的祖上是否为 SimMod，上文定义的 `SimMod_is` 函数可配合 `SIM_MOD` 使用，例如

```c
if (SimMod_is(SIM_MOD(x))) {
        printf("module x is derivered from SimMod.\n");
}
```

为了不显式进行类型转换，需定义 `sim_mod_is` 宏：

```c
/* sim-mod.h ++ */
#define sim_mod_is(mod) SimMod_is(SIM_MOD(mod))
```

# 示例

以下代码定义了 Foo 模块，它继承 SimMod 模块。

```c
/* foo.h */
#ifndef FOO_H
#define FOO_H
#include <sim-mod.h>
/* 模块结构体 */
typedef struct foo_mod {
        SimMod *parent;
        const char *name;
} FooMod;

/* 隶属于 Foo 模块的数据类型 */
typedef struct foo {
        FooMod *mod;
        const char *anything;
} Foo;

SIM_MOD_DECLARE(FooMod, foo_mod);
#define FOO_MOD(mod) ((FooMod *)(mod)) /* 类型转换 */
#define foo_mod_is(mod) FooMod_is(FOO_MOD(mod)) /* 自省 */

Foo *foo(const char *anything); /* Foo 对象的构造函数 */
#define foo_free(a) free(a)  /* Foo 对象释放宏 */
#endif
```

```c
/* foo.c */
#include "foo.h"
/* FooMod 继承 SimMod */
SIM_MOD_DEFINE(FooMod, foo_mod, SimMod, sim_mod);

Foo *foo(const char *anything) {
        Foo *a = malloc(sizeof(Foo));
        SIM_ASSERT_RET(a, NULL);
        a->mod = foo_mod();
        a->anything = anything;
        return a;
}
```

需要记住的是，任何一个模块类型，其结构体的内存布局，在开始处必须与父模块类型相同，且这部分共同的内存布局对应的结构体成员的名字亦需相同。若不理解这一点，请认真审视 `FooMod` 结构体与 `SimMod` 结构体，寻找二者的共性。

上述代码中的 `SIM_ASSERT_RET` 宏是 SimErr3 模块新添加的断言宏，其定义如下：

```c
/* sim-err3.h ++ */
#define SIM_ASSERT_RET(bool_exp, ...) do { \
        if (!(bool_exp)) { \
                SIM_ERR3("invalid assertion!"); \
                return __VA_ARGS__; \
        } \
} while (0)
```

下面是 Foo 模块的应用示例：

```c
/* test.c */
#include <foo.h>
int main(void) {
        Foo *a = foo("i am foo!");
        if (foo_mod_is(a->mod)) {
                printf("%s\n", a->anything);
                printf("i belong to FooMod.\n");
        }
        if (sim_mod_is(a->mod)) {
                printf("i also belong to SimMod.\n");
        }
        foo_free(a);
        return 0;
}
```

以下命令可编译 test.c：

```console
$ gcc -I. sim-{err,str,err2,array,err3,mod}.c foo.c test.c -o test
```

运行 test 程序，其输出应当为

```
i am foo!
i belong to FooMod.
i also belong to SimMod.
```

# 总结

在实现 SimMod 模块之前，所谓的模块是由某种数据类型及其相关函数构成的编译单元。现在，若需要将一个新的模块整体作为一种类型，只需让该模块对应的类型继承 `SimMod` 类型或其任一子类型。除此之外，模块作为编译单元的其他一切存在皆如常实现即可。

SimMod 实现的模块的继承，与面向对象编程中类的继承，有显著区别，前者是穷继承，子模块只继承父模块的类型和名字，后者是富继承，子类继承的是父类的属性（数据）和行为（函数）。

SimMod 模块及其子模块，只用于表达程序运行时产生的某些类型。基于这些类型的作用可以消除静态类型的差异，从而在一定程度上实现让函数具备多态性，即便于定义一些不受静态类型限制的更为通用的函数。

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
