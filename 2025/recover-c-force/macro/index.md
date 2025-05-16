---
title: 宏
abstract: 他们说，宏是丑陋的，可怕的……
date: 2025 年 05 月 15 日
...

# 前言

当我感觉有必要介绍 C 语言宏的一些经验时，我才发现 Sim 项目已经存在了许多宏。很多人在谈及 C 语言的宏时，会皱眉头，说宏是丑陋的，可怕的……上一次他们皱眉头的时候，大概是谈及 C 语言的指针。

至目前为止，Sim 项目里的所有宏，我保证，都是自然出现的，而非我刻意设计。使用 C 语言编写程序，需要程序员像工匠，将不同形状的木头或石头，用斧头修整成建筑整体所需要的局部形状。宏就像斧头。

在更为现代的编程语言里，斧头或锤子升级为机床之类的重型装备了。这种升级是面向工业的，适应社会化生产的需求，但不能据此否定斧头或锤子继续存在的必要，毕竟你的家里，很有可能没有机床，却有斧头。一切工具，实际上是平等的，不存在尊卑贵贱的分别，只存在应用场景的分别。

编程，可以是现代工程化的人类群体活动，崇尚安全和效率，但它依然可以作为一门个人的技艺，用于表达一个人对这个世界的认识和探险。C 语言尊重前者，它已经实现过甚至依然继续在实现着前者，但它更推崇后者。对个人技艺的崇尚，并不意味着否定工程化的群体活动，甚至能让这种群体活动更为活跃。编程之道，终在工程化的集体无意识与个体技艺的清醒梦之间找到平衡。

# 条件编译

对于 C 编译器而言，每份源文件（.c 文件），是一个编译单元，通常是唯一的，它不会被其他文件包含。在源文件里，若使用 `#include` 包含头文件（.h 文件）时，编译器会将头文件的内容复制到源文件里，使之参与编译。

若一份头文件，被多份源文件包含，编译器会重复复制该头文件的内容以使编译单元达到完整。若这种被多份源文件包含的头文件里只存放函数或全局变量的声明而非定义，编译器多次复制它们，只是会降低编译效率，不会影响编译结果。但是，若头文件里存放了函数或全局变量的定义，便会导致编译器因函数或变量重复定义而报错终止。

使用条件编译指令，并辅以简单的宏定义，便可使得一份头文件成为单例，亦即无论被多少份源文件包含，在编译器的角度，该头文件仅被某个源文件包含了一次。这种方法便是 Sim 项目从一开始便使用了，例如 sim-err.h 中含有以下代码

```c
#ifndef SIM_ERR_H
#define SIM_ERR_H

#endif
```

`#ifndef` 和 `#endif` 便是条件编译指令。上述代码的含义是，若 SIM_ERR_H 未定义，则以下区域直至 #endif 语句之前的代码，都是有效的，否则是无效的，予以忽略。于是，上述代码在第一次被编译时，宏 `SIM_ERR_H` 得以定义。当编译器第二次编译这段代码时，因 `SIM_ERR_H` 已被定义，这段代码便会被忽略了。

`#ifndef` 中的 `n` 表示 `not`。`#ifdef` 表达的逻辑与 `#ifndef` 相反，它表达的是，若定义了某个宏，则会如何。在 C 语言里，条件编译主要用于控制编译器的行为，让编译器有选择的编译代码片段。

# 续行符

请记住，宏的定义是单行代码。得益于 C 语言不依赖缩进于换行，亦即无论多么复杂的 C 程序，其源代码可以写成一行。之所以对 C 代码分行，是为了便于人类阅读。

在宏定义里，为了使得代码便于理解，也需要换行，但是又要满足宏定义的语法规则，此时可用续行符 `\` 化解这一矛盾。例如

```c
#define SIM_MOD_DECLARE(ModType, mod_type) \
        bool ModType##_is(ModType *mod); \
        ModType *mod_type##_this(ModType *mod)
```

以下宏调用

```c
SIM_MOD_DECLARE(FooMod, foo_mod);
```

会被展开为

```c
bool FooMod_is(FooMod *mod); FooMod *foo_mod_this(FooMod *mod);
```

而不是展开为

```c
bool FooMod_is(FooMod *mod);
FooMod *foo_mod_this(FooMod *mod);
```

# 连接符

在「[模块类型](../module/index.html)」中，多次使用了 `##` 符号。例如

```c
#define SIM_MOD_DECLARE(ModType, mod_type) \
        bool ModType##_is(ModType *mod); \
        ModType *mod_type##_this(ModType *mod)
```

`##` 可以将宏参数与标识符或其他宏参数连接起来，构成新的标识符。所谓标识符，即变量名与函数名等，亦即由非数字开头由大写字母、小写字母、数字或下划线构成的文本。例如，若宏参数 `ModType` 的值为 `FooMod`，`mod_type` 的值为 `foo_mod`，则 `ModType##_is` 会被展开为 `FooMod_is`，而 `mod_type##this` 会被展开为 `foo_mod_this`。

# 字符串化

在「[模块类型](../module/index.html)」中，使用了单个 `#` 符号。例如

```c
this_mod->name = #ModType;
```

`#` 可为其后的标识符增加引号。`ModType` 是宏参数，若它的值为 `FooMod`，则上述代码会被展开为

```c
this_mod->name = "FooMod";
```

在 C 语言的宏语法里，`#` 是字符串化操作符，其主要用途是，将宏参数转化为字符串字面量。

# 可变参数

`SIM_ASSERT_RET` 是 Sim 项目的错误机制里定义的断言宏。它能够断言一个布尔表达式的结果为真，否则会返回指定的值。例如

```c
int *foo(void) {
        int *a = malloc(sizeof(int));
        SIM_ASSERT_RET(a, NULL);
        return a;
}
```

上述代码断言内存分配结果是有效的，即 `a` 的值并非 `NULL`。倘若内存分配失败，则函数 `foo` 在 `SIM_ASSERT_RET` 处便会以返回 `NULL` 而结束。

可以将 `SIM_ASSERT_RET` 定义为

```c
#define SIM_ASSERT_RET(bool_exp, result) \
        if (!(bool_exp)) { \
                SIM_ERR3("invalid assertion!"); \
                return result; \
        }
```

对于大多数情况， `SIM_ASSERT_RET` 宏不会出问题，直到它遇到没有返回值的函数。例如

```c
void foo(int *a) {
        SIM_ASSERT_RET(a, ...); /* SIM_ASSERT_RET 无法使用 */
        printf("a = %d\n", *a);
}
```

此时，无法调用 `SIM_ASSERT_RET` 宏，因为无法向它提供一个不应该存在的参数，而它又必须有这个参数。C 语言自 C99 标准开始支持可变参数宏，可以化解这一窘境。需要将 `SIM_ASSERT_RET` 宏定义修改为

```c
#define SIM_ASSERT_RET(bool_exp, ...) \
        if (!(bool_exp)) { \
                SIM_ERR3("invalid assertion!"); \
                return __VA_ARGS__; \
        }
```

现在，若在上述无返回值的函数中调用 `SIM_ASSERT_RET`，只需

```c
void foo(int *a) {
        SIM_ASSERT_RET(a);
        printf("a = %d\n", *a);
}
```

此时，`SIM_ASSERT_RET` 定义中的 `__VA_ARGS__` 的值为空，于是

```c
return __VA_ARGS__;
```

的展开结果是

```c
return ;
```

# 隔离层

在 C 语言宏的应用中，第一个使用 `do { ... } while (0)` 表达式的人，无疑是个天才。这个表达式能够有效阻止宏的展开结果与外围环境产生错误的结合。

例如，以下代码定义了宏 `FOO`。

```c
#define FOO(x) \
        x = 1; \
        printf("%d\n", x)
```

然后在某个条件表达式中调用 `FOO`：

```c
int a = 0;
if (a == 0) FOO(a);
```

则上述代码的展开结果为

```c
int a = 0;
if (a == 0) x = 1; printf("%d\n", x)
```

由于 C 语言的语法规定，条件语句的作用域仅限于其后的第一条语句，故而上述代码中的 `printf` 语句，无论条件是否成立，它都会被执行。

对于上述问题，若不修改宏调用语句，只修改 `FOO` 的定义，用 `do { ... } while (0)` 表达式便可让程序逻辑符合预期。例如

```c
#define FOO(x) do { \
        x = 1; \
        printf("%d\n", x) \
} while (0)
```

在宏定义中使用 `do { ... } while (0)` 表达式也能为一些局部变量构造可靠的作用域。例如

```c
#define sim_array_add(array, unit, type) do { \
        type tmp_unit = unit; \
        SIM_ARRAY_ADD(array, &tmp_unit, sizeof(type)); \
} while (0)
```

# 保护层

在微观层面，有时需要使用括号作为宏参数的保护层。一个非常经典的例子是平方宏的定义：

```c
#define SQUARE(x) x * x
```

若是像下面这样调用 `SQUARE`，

```c
SQUARE(a + b);
```

展开结果为

```c
a + b * a + b
```

而不是预期的

```c
(a + b) * (a + b)
```

若想得到预期结果，需要将 `SQUARE` 定义为

```c
#define SQUARE(x) (x) * (x)
```

# 宏调用语句

C 程序的语句，总是以分号结束。很多时候，宏调用若为独立的语句，我们自然希望它也能以分号结束。通常情况下，在宏定义末尾本该放置分号的情况下，可以选择不放置分号，而是在宏调用语句后面添加分号。例如上一节最后定义的 `FOO` 宏，`while (0)` 后面本可以放置分号的，在调用 `FOO` 宏时，便可用分号补全。

由于 C 语言标准规定，一个分号可以构成一条空语句，故而在 `FOO` 的定义中，即使以分号作为结束，即

```c
#define FOO(x) do { \
        x = 1; \
        printf("%d\n", x) \
} while (0);
```

若 `FOO` 的调用语句也以分号结束，例如

```c
FOO(a);
```

则上述语句会被展开为

```c
do {
        a = 1;
        printf("%d\n", a);
} while (0);;
```

虽然 `while (0)` 后面出现了两个分号，但依然是符合 C 语法的，第 2 个分号表示空语句，副作用是让 C 编译器轻微受阻。

真正需要认真对待的是，宏的展开结果是函数定义的情况。例如

```c
#define FOO(x) \
        void foo(int x) { \
                printf(#x "= %d\n", x); \
        }
```

`FOO` 的调用语句

```c
FOO(a);
```

与上述宏调用语句展开结果等效的代码如下：

```c
void foo(int a) { 
        printf("a = %d\n", a); 
};
```

函数定义之后出现分号，是不符合 C 语法的。为了解决这类问题，在「[模块类型](../module/index.html)」中发明了一种额外声明函数的方法。基于该方法，可将上述 `FOO` 的定义修改为

```c
#define FOO(x) \
        void foo(int x) { \
                printf(#x "= %d\n", x); \
        } \
        void this_func_not_defined_forever(void)
```

如此便可支持 `FOO` 的调用语句以分号结束。

# 总结

C 语言的宏，实际上与指针差不多简单。我不妨自大一些，本文至此，我已将它的 90% 的用法都讲完了。剩下的 10%，我还没有遇到必须使用它们的机会，亦即现在我也不会。

在实际应用中，宏通常用于简化代码，但是也很容易出现错误。虽有万般法度，但实际上最简单的道理是，只要足够熟练，出错的概率就会足够小。C 编译器通常会提供 `-E` 选项，使用该选项，你可以查看宏调用语句的展开结果。只要见识过的宏的展开结果足够多，容易出错之处便愈发清楚。例如，foo.c 文件里调用了一些宏，使用以下命令便可得到所有宏的展开结果。

```console
$ gcc -E foo.c -o foo.i
```
