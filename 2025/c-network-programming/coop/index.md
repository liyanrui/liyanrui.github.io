---
title: C 面向对象编程
abstract: 像上帝创造生物……
date: 2025 年 04 月 04 日
...

# 前言

现在，你已经实现了一个简单的 C/S 架构的程序，这个程序分为两个部分，一个是 ywj，它是客户端（Client），另一个是 threebody，它是服务端（Server）。这两个部分通过套接字合为一体。不妨再勇敢一些，因特网（Internet）也不过是使用了不计其数的套接字将各个部分连接起来的一个庞大的慢吞吞的程序罢了，就像很多小块相互咬合形成的巨大的拼图。

到目前为止，一台计算机中任何一个进程所能想象的最大空间，便是因特网，它是进程的宇宙。这个宇宙并没有任何所谓的真空，其中的任何一台计算机，不是忙于计算，便是忙于收发数据，即使它们有时处于空闲状态，但这种状态也颇像量子力学所认为的，真空非空，充满了量子涨落。

不过，还是先不要沉湎于这些无尽的遐想，我们需要认真看一看 ywj.c 和 threebody.c 中的 `main` 函数，它们的代码已失去了简明性，像是此刻你桌面上摆放的那些用于连接计算机的电源线以及乱七八糟的信号线和数据线。我们需要尝试用面向对象的办法收纳这些代码。

C 语言在语法上不能像 C++、C#、Java 等语言那样优雅地支持面向对象编程，但是若将面向对象编程视为一种编程范式，C 在一定程度上可以实现其基本思想。我们可以用结构体模拟类，用普通函数模拟对象的方法。

# sim 项目

从此刻开始，我们不仅仅是学习一些套接字 API 函数的用法，而是围绕它们，学习如何使用 C 语言模拟面向对象编程的基本理念，开发一个容易被他人使用的库。这是一个很小的项目。项目的名称或代号是 simple，质朴，简单。

由于 C 语言没有命名空间，程序中定义的所有类型和函数的名字，都在一个共同的空间里，很容易易出现重复，导致程序无法通过编译，或通过编译后，运行时出现异常。一般情况下，在一个项目里定义的类型和函数，通常以项目名作为前缀，可避免多数的命名重复情况。例如以 simple 作为类型和函数名的前缀：

```c
typedef struct simple_client SimpleClient;

SimpleClient *simple_client_new(const char *host, const char *port);
```

simple 作为前缀，有些长了。为了让代码不至于太长，我努力将其缩写为 sim，于是上述代码可改写为

```c
typedef struct sim_client SimClient;

SimClient *sim_client_new(const char *host, const char *port);
```

# 示意标记

下文为了叙事方便，使用了一些示意型的标记，它们以 C 语言注释文本的形式，用于表明向 C 源码文件增加和修改代码片段。在此，我以 foo.c 为例，解释这些标记的含义。

首先，是初始化标记。比如新建一份 C 源码文件 foo.c，并写入一些初始内容，或 foo.c 已存在，但对其完全重写，这类情况对应以下标记：

```c
/* foo.c */
```

上述标记之后的内容便为 foo.c 的初始内容或重写的内容。

若在 foo.c 中增加一些代码，则用 C 语言的自增运算符 `++` 表示：

```c
/* foo.c ++ */
```

若对 foo.c 中的一些内容进行修改，则用以下标记

```c
/* foo.c [改] */
```

至于改动了何处，可根据变动后的内容中的不变之处自行确定。既然是改，定有不变之处。


# 字符串类的声明

C 语言标准库对字符串的支持颇为孱弱，以致许多用于处理字符串的函数在设计上似乎以吓退 C 语言初学者为荣，实则不得已而为之。想必有许多 C 语言初学者被 `strlen` 伤害过，其中应该也有很多人被伤害过，却不自知。不过，C 字符串之所以如此，是为了程序性能。倘若我们不在意损失程序些许性能，实现一个更好用一些的 C 字符串模块，不仅便于 sim 项目的开发，也是学习用 C 编写面向对象程序的良好起点。

首先，我们创建 sim-string.h 文件，它包含了一些它需要的头文件：

```c
/* sim-str.h */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
```

我们定义一个类型 `SimString` 表示字符串类，它实际上是一个结构体类型的别名：

```c
/* sim-str.h ++ */
typedef struct sim_str SimStr;
```

至于结构体 `sim_str` 的定义，不会在 sim-str.h 中给出，这意味着，作为字符串类的用户，你不必知情。

在面向对象编程中，类是一种数据类型，它只是比 `int`，`char *` 这些普通类型复杂一些，确切地说，类在绝大多数情况下是由一些基本类型组合而成。在编程中，我们需要为类型构造实例，如同用基本类型声明和定义变量，只不过我们通常将类的实例称为「类的对象」。构造对象，往往需要以函数的方式，这类函数便称为对象的构造函数。我们声明 `SimStr` 对象的构造函数为

```c
/* sim-str.h ++ */
SimStr *sim_str(const char *raw);
```

`sim_str` 函数与结构体 `sim_str` 同名，不违背 C 标准，因为结构体有单独的命名空间，即 `struct`。这个函数的命名，可能会让很多人内心隐隐不安，他们更习惯 `sim_str_new` 这样的命名，而我觉得实在没必要多写 4 个字符。`sim_str` 基于 C 的普通字符串构造 `SimStr` 对象。

对象的构造函数未必只有一种。`SimStr` 对象也可以由下面这个函数构造：

```c
/* sim-str.h ++ */
/* 将 m 个字节且前 n 个字节存有数据的缓冲区转化为字符串对象 */
SimStr *sim_str_absorb(void *buffer, size_t m, size_t n); 
```

与构造函数相反的是析构函数，它用于释放对象，实际上这个函数应该叫解构。倘若你知道何为解构主义，想必会认同我的这个观点。不过，既然业界已约定俗成，我们也用析构函数这个术语。我们的析构函数，从现在开始，约定为构造函数名加上后缀 `_free`。`SimStr` 对象的析构函数为

```c
void sim_str_free(SimStr *str);
```

如果你对 C 语言内存模型中的堆与栈的概念有基本的认识，即使你尚未看到我如何定义 `SimStr` 对象的构造与析构函数，应当也能猜得出，`SimStr` 是堆空间里的对象，事实正是如此。

`SimStr` 对象还有一些方法，它们实际上也是函数，只是若将 `SimStr` 对象视为一个生物体，这些函数是该生物体的一些行为。我们目前所需要的 `SimStr` 对象的方法如下：

```c
/* sim-str.h ++ */
/* 获取字符串长度 */
size_t sim_str_size(SimStr *self);

/* 允许外界访问 data 成员 */
const char *sim_str_share(SimStr *self);

/* 将字符串对象转化为普通的 C 字符串 */
const char *sim_str_raw(SimStr *self);
```

根据代码中的注释，你大概清楚上述方法的用途。但是在读这些函数时，以 `sim_str_raw` 为例，我希望你的读法是这样的，「我（`self`），作为 `SimStr` 对象（结构体 `sim_str` 的实例）的原始形式（`raw`）」。对象的方法里的第一个参数，便是对象本身。那些在语法层面支持面向对象的语言，上述代码中的 `self` 往往是不可见的。

# 字符串类的实现

在 sim-str.c 中实现 sim-str.h 所声明的一切。首先，载入 sim-str.c 所需的头文件：

```c
/* sim-str.c */
#include "sim-str.h"
```

然后定义名义上是 `SimStr` 的结构体 `str_str`：

```c
/* sim-str.c ++ */
struct sim_str {
        char *data; /* 支持以 '\0' 为字符串结束标志，亦可不支持 */
        size_t n;   /* data 中存储的字符串的长度（字节数） */
        size_t m;   /* data 占用内存空间的字节数，m >= n */
        const char *error;
};
```

结构体 `sim_str` 的前三个成员，我给出的注释已足够清楚了，若不能完全理解，在后续定义构造函数时，必见分晓。

也许你早已猜出我会如何定义这个结构体，但是你应该想不到，我会为 `SimSir` 对象配备一个字符串，用于记录 `SimStr` 对象可能的出错信息。此举虽然多耗费了一个指针的空间，但它很有用处。例如，在 `SimSir` 对象的构造函数中便可用到：

```c
/* sim-str.c ++ */
SimStr *sim_str(const char *raw) {
        SimStr *str = malloc(sizeof(SimStr));
        if (!str) {
                fprintf(stderr, "sim_str: malloc error!\n");
                return NULL;
        }
        str->n = 0;
        str->m = 0;
        if (raw) {
                size_t n = strlen(raw);
                str->data = malloc(n * sizeof(char));
                if (!str->data) {
                        str->error = "sim_str: failed to malloc 'data' member!";
                } else {
                        str->n = n;
                        str->m = n;
                        str->error = NULL;
                        memcpy(str->data, raw, n);
                }
        } else {
                str->data = NULL;
                str->error = NULL;
        }
        return str;
}
```

`sim_str` 函数考虑了构造 `SimStr` 对象过程中所有可能的情况。若你觉得我过于自信，请不吝摧毁。尤其是在为 `str->data` 分配内存时，若分配失败，依然可以返回一个 `SimStr` 对象，只不过它是带着错误信息的空的字符串对象。

`SimStr` 的错误信息成员，是上一节我在类的声明时并未想到的，现在需要 `SimStr` 对象具备一个能够判断自身是否安全的方法。首先，在 sim-str.h 中的头文件区包含 stdbool.h：

```c
/* sim-str.h ++ */
#include <stdbool.h>
```

用于检测 `SimStr` 对象安全性的方法的声明与定义如下：

```c
/* sim-str.h ++ */
/* self 安全，返回 true，否则返回 false */
bool sim_str_safe(SimStr *self);
```

```c
bool sim_str_safe(SimStr *self) {
        if (self) {
                if (self->error) {
                        fprintf(stderr, "%s\n", self->error);
                        return false;
                }
        } else {
                fprintf(stderr, "sim_str_safe error: NULL pointer!\n");
                return false;
        }
        return true;
}
```

现在，为 `SimStr` 对象定义另一个构造函数：

```c
/* sim-str.c ++ */
/* buffer 为 m 字节，前 n 个字节存有数据 */
SimStr *sim_str_absorb(void *buffer, size_t m, size_t n) {
        if (!buffer || n == 0) return NULL;
        SimStr *str = malloc(sizeof(SimStr));
        if (!str) {
                fprintf(stderr, "sim_str: malloc error!\n");
                return NULL;
        }
        str->n = n;
        str->m = m;
        str->data = buffer;
        str->error = NULL;
        return str;
}
```

`buffer` 被 `sim_str_absorb` 吸入后，创建的 `SimStr` 对象便得到了 `buffer` 所指空间的所有权。此时，倘若 `buffer` 还被其他对象或变量拥有，若程序通过它们释放了 `buffer` 指向的空间，对这个 `SimStr` 对象而言，极为致命。所以，用这个构造函数时，需要保证 `SimStr` 对象能够唯一拥有 `buffer` 所指空间的所有权。

`SimStr` 对象的析构函数定义如下：

```c
/* sim-str.c ++ */
void sim_str_free(SimStr *str) {
        if (str) {
                if (str->data) free(str->data);
                free(str);
        } else {
                fprintf(stderr, "sim_str_free error: NULL pointer!");
        }
}
```

`SimStr` 对象的三个方法，实现起来颇为简单，只要你敢于放弃安全性：

```c
/* sim-str.c ++ */
size_t sim_str_size(SimStr *self) {
        return self->n;
}
```

```c
/* sim-str.c ++ */
const char *sim_str_share(SimStr *self) {
        return self->data;
}
```

```c
/* sim-str.c ++ */
const char *sim_str_raw(SimStr *self) {
        if (self->n == self->m) {
                /* 对 self->data 扩容 1 个字节 */
                /* 用于存储 C 字符串结束符 */
                void *p = realloc(self->data, self->m + 1);
                if (!p) return NULL;
                else {
                        self->m += 1;
                        self->data = p;
                }
        }
        /* 添加字符串结束符 */
        *(self->data + self->n) = '\0';
        return self->data;
}
```

我觉得安全性不应该完全由开发基础库的人负责。类似于菜刀的安全性若完全由开发基础库的人负责，那它可能什么东西都切不了，最钝的菜刀是最安全的。若需要担保上述两个方法的安全性，可在使用它们之前用 `sim_str_safe` 检测 `SimStr` 对象的安全性。

# 防碰头设计

复杂程序，往往由许多模块构成，其中可能会出现一些模块依赖其他模块的情况，会导致某个模块的 .h 文件可能会被其他模块多次 `#include`。倘若 .h 文件里只有类型和函数的声明，它被多次 `#include`，只是增加了 C 编译器的工作量，但是若 .h 含有变量或函数的定义，这种情况下会因变量或函数重复定义而导致编译出错。

使用条件宏，可避免 .h 文件被多次重复载入。例如，在 sim-str.h 开始处，添加

```c
#ifndef SIM_STR_H
#define SIM_STR_H
```

然后在 sim-str.h 的结尾处，添加

```c

#endif
```

这三个宏组合起来，表达的含义是，当头文件 sim-str.h 第一次被 `#include` 时，宏 `SIM_STR_H` 尚未定义，于是 sim-str.h 的内容便被正常载入。当 sim-str.h 再次被 `#include`，此时编译器可确定宏 `SIM_STR_H` 已定义，便拒绝再次载入 sim-str.h 的内容。

# 测试

下面的程序 sim-str-test.c 可用于验证 `SimStr` 类是否可用，但它更是在向你演示这个类的用法。

```c
/* sim-str-test.c */
#include <sim-str.h>

int main(void) {
        const char *raw = "Hello world!";
        SimStr *foo = sim_str(raw);
        if (sim_str_safe(foo)) {
                printf("foo length: %lu\n", sim_str_size(foo));
                printf("foo content: %s\n", sim_str_raw(foo));
        }
        sim_str_free(foo);
        
        size_t n = strlen(raw);
        size_t m = 2 * n;
        void *buffer = malloc(m * sizeof(char));
        memcpy(buffer, raw, n);
        SimStr *bar = sim_str_absorb(buffer, m, n);
        if (sim_str_safe(bar)) {
                fwrite(sim_str_share(bar), 
                       sizeof(char),
                       sim_str_size(bar),
                       stdout);
                printf("\n");
                printf("%s\n", sim_str_raw(bar));
        }
        
        sim_str_free(bar);
        return 0;
}
```

用 gcc 编译 sim-str-test.c，并使用 `-g` 选项让编译所得程序 test 具有调试信息：

```console
$ gcc -I. sim-str.c sim-str-test.c -g -o test
```

注意，在 sim-str-test.c 中，我们已经是像使用库的头文件那样使用 sim-str.h 了，足以彰显 sim 是一个库的志向。只是此刻 sim 还不是库，sim-str.h、sim-str.c 以及 sim-str-test.c 都在同一目录下。

通常，用尖括号包含的头文件，C 编译器会首先在系统的头文件目录（例如 /usr/include）中搜索头文件，不会在其他目录包括当前目录中搜索，而引号包含的头文件，像 gcc 这样的 C 编译器只会在当前目录中搜索，不会考虑其他路径。故而，在上述 gcc 命令中，我使用了 `-I.` 选项（其中的 `.` 表示当前目录）让 gcc 在当前目录中搜索  sim-str.h。

运行 test：

```console
$ ./test
foo length: 12
foo content: Hello world!
Hello world!
```

在 Linux 里，可使用 valgrind 工具检测 test 程序的内存安全性：

```console
$ valgrind --leak-check=yes ./test
==35099== Memcheck, a memory error detector
==35099== Copyright (C) 2002-2024, and GNU GPL'd, by Julian Seward et al.
==35099== Using Valgrind-3.23.0 and LibVEX; rerun with -h for copyright info
==35099== Command: ./test
==35099== 
foo length: 12
foo content: Hello world!
Hello world!
==35099== 
==35099== HEAP SUMMARY:
==35099==     in use at exit: 0 bytes in 0 blocks
==35099==   total heap usage: 7 allocs, 7 frees, 1,138 bytes allocated
==35099== 
==35099== All heap blocks were freed -- no leaks are possible
==35099== 
==35099== For lists of detected and suppressed errors, rerun with: -s
==35099== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```

# 总结

也许你并不想逐一复制本文的所有代码片段，拼凑成完整的 sim-str.h 和 sim-str.c 文件，我允许你通过下面的连接不劳而获：

* [sim-str.h](sim-str.h)
* [sim-str.c](sim-str.c)
* [sim-str-test.c](sim-str-test.c)

虽然我不喜欢 C++，Java 以及 C# 之类的面向对象编程语言，但我不得不承认，面向对象编程范式，着实是了不起的发明。现在，应该考虑如何用这种方式封装那些用起来颇为繁琐的套接字函数，以及如何在网络编程中使用本文实现的字符串类。
