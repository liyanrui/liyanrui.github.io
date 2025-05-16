---
title: 字符串
abstract: 计算机给人类的第一缕温情。
date: 2025 年 04 月 21 日
...

# 前言

数字都是冷冰冰的，无论它在现实中，还是在计算机里。计算机的冰冷，一直持续到字符串的出现。人类第一本 C 语言教材的第一个程序，就是在计算机屏幕上输出「Hello world!」。然而，很多年以后，新生代程序员开始讨伐 C 语言字符串的简陋了，如同他们讨伐 C 标准库所用的错误码。

C 语言的字符串，是以 `\0` 为结尾的字节数组，在使用 `strlen` 算其长度时，不计 `\0` 所用的那个字节。的确简陋，但是 C 给了我们最大的自由，以致每个人都可以实现符合自己所需的字符串类型。自由，已是这世间最大的恩赏了。C 之后的很多语言，可以赏赐你更好用的字符串，而你总得付出一些什么。自由又富足的生活，谁都想拥有，然而真正美好的生活，似乎永远都需要我们亲手去创造。

先以创造 sim-str.h 和 sim-str.c 的开头作为开始吧！

```c
/* sim-str.h */
#ifndef SIM_STR_H
#define SIM_STR_H
#include <string.h>
#include <sim-err.h>

/* 待定区域 */

#endif
```

```c
/* sim-str.c */
#include <string.h>
#include <stdint.h>
#include "sim-str.h"
```

我要创造的这个字符串类型，使用 SimErr 模块提供的错误机制，详见「[错误](../error/inde.html)」。下文会逐步向 sim-str.h 和 sim-str.c 增加新代码。不过，需要注意，对于 sim-str.h，新增的代码肯定皆在注释示意为「待定区域」内，不然 sim-str.h 精心设计的防止头文件被多次载入的预处理机制就失效了。

# 隐私

我是认为，每个人都应该有一些秘密，而且这些秘密的意义，并非不可示人，而是它们若不是秘密，可能会造成我们自己以及亲朋好友，就很难有多么美好的生活了。若你觉得一个人非常美好，你应该想到，也许他/她守着很多的秘密。字符串也有隐私，即它占用的内存，通常大于它实际的长度。像 C 字符串在毫无隐私的情况下，都多占了 1 个字节的内存用于存储 `\0`。

在 sim-str.h 文件里，定义一个类型 `SimStr`：

```c
/* sim-str.h ++ */
typedef struct sim_str SimStr;
```

`SimStr` 实际上是 `struct sim_str` 这个结构体类型的别名，而这个结构体类型的定义，不会在 sim-str.h 中出现，而是在 sim-str.c 中出现，原因是，它有隐私。注意，上述代码中的注释是有独特的示意性，它表示我们在 sim-str.h 文件中增加一些代码，`++` 表示增加。

`struct sim_str` 的隐私是，长度为 `n` 的字符串，占用了 `m` 个字节：

```c
/* sim-str.c ++ */
struct sim_str {
        size_t n;
        size_t m;
        char *data;
};
```

如此便为字符串构造了隐私。sim-str.h 是可以对外界公开的，表明我定义了一个字符串类型 `SimStr`，至于它的数据结构，是藏在 sim-str.c 中的。sim-str.c 是可以让外界无法看到，因为它的内容可以编译为库文件。库文件是二进制文件，很难从中确知 `struct sim_str` 的数据结构。不过，sim 项目应该是一个自由软件项目，它不会禁止任何人阅读 sim-str.c，甚至可对其进行修改。

在自由软件项目中，头文件不暴露某种类型的数据结构，用意是，若你只是该项目成果的用户，你尽可不必关心这种类型的数据结构。你的时间和精力，应当用在那些更值得学习的知识上，而不在这些工具的种种细节，除非你想参与这个项目的开发工作。

# 构造与释放

为 `SimStr` 类型构造对象的函数是 `sim_str`，它与 `SimStr` 的本体 `struct sim_str` 同名，不违背 C 语言标准，只是你可能应该觉得这个函数的名字应该是 `sim_str_new` 或 `sim_str_create`。我只是不想多打几个字符，而且我觉得，函数名必须有动词，太过于教条。对于 `sim_str`，你的大脑应该构思下面的对话：

```
计算机：阁下想要点什么？
你想到了 struct sim_str，便脱口而出：sim_str！
```

我是因为想到了这样的场景，决定用 `sim_str` 更贴近生活。

`sim_str` 函数可将 C 字符串常量复制为 `SimStr` 对象，其声明如下：

```c
/* sim-str.h ++ */
SimStr *sim_str(const char *raw);
```

`sim_str` 的实现如下：

```c
/* sim-str.c ++ */
SimStr *sim_str(const char *raw) {
        SimStr *str = malloc(sizeof(SimStr));
        if (!str) goto ERROR;
        if (raw) {
                size_t n = strlen(raw);
                size_t m = n + 1;
                str->data = malloc(m * sizeof(char));
                if (str->data) {
                        str->n = n; str->m = m;
                        memcpy(str->data, raw, n);
                        /* 为 str->data 增加 C 字符串结束符 */
                        str->data[n] = '\0';
                        return str;
                } else {
                        free(str);
                        goto ERROR;
                }
        } else {
                str->data = malloc(sizeof(char));
                if (str->data) {
                        str->n = 0;
                        str->m = 1;
                        str->data[0] = '\0';
                        return str;
                } else {
                        free(str);
                        goto ERROR;
                }
        }
ERROR: 
        SIM_ERR("sim_str error!"); 
        return NULL;
}
```

从 `sim_str` 的实现可以看出，`SimStr` 完全兼容 C 字符串，亦即 `SimStr` 对象的 `data` 是 C 字符串。

`SimStr` 的对象的释放函数如下：

```c
/* sim-str.h ++ */
void sim_str_free(SimStr *str);
```

```c
/* sim-str.c ++ */
void sim_str_free(SimStr *str) {
        if (str) {
                if (str->data) free(str->data);
                free(str);
        } else {
                SIM_ERR("sim_str_free error: NULL pointer!");
        }
}
```

以下代码演示了 `sim_str` 和 `sim_str_free` 的用法：

```c
SimStr *a = sim_str("hi!"); SIM_MER {
        /* 对字符串进行一些操作 */
        ... ... ... ... ...
}
sim_str_free(a); SIM_MER { a = NULL; }
```

# 字符串长度

用于获取字符串长度的函数是 `sim_str_size`，这个名字也许让你又觉得不适，你习惯的是 `sim_str_get_size` 这样的名字，但是 C 标准库提供的 `strlen` 和我的命名风格颇为一致，请扔掉那些学究气，`new`，`create`，`get`，`set`……这些，在编程中，实际上往往是废话，至少在 C 编程中，完全可以不需要它们。

```c
/* sim-str.h ++ */
size_t sim_str_size(SimStr *str);
```

```c
* sim-str.c ++ */
size_t sim_str_size(SimStr *str) {
        if (str) {
                return str->n;
        } else {
                SIM_ERR("sim_str_size error: invalid string!");
                return 0;
        }
}
```

`sim_str_size` 的用法如下：

```c
SimStr *a = sim_str("hi!");
printf("the length of a is %lu bytes.\n", sim_str_size(str, &e));
SIM_ASK;
```

亦即，实际上不必每次调用敬重错误的函数，随后都去检查它是否出错。如同你走在路上，也不是一步一回头，看看身后有没有危险的情况，通常只需在转弯或者穿过马路时，需要环顾四周，确定自己的安全。对于错误，可以用奥卡姆剃刀原则，如无必要，无须言错。SimErr 模块提供的错误机制，只用于记录错误，不会干涉程序的运行，故而当错误第一次出现时，只要你的程序里认真考虑了各种可能出错的情况，错误会自然传播，环环相扣。当你决意查看程序是否出错，此时必定能看到最后一个错误出现的位置。

`SIM_ASK` 宏是我临时想出来的，故而在 SimErr 模块中未有定义，现在可以有了。

```c
/* sim-err.h ++ */
/* [上文] #define SIM_MER ... */
#define SIM_ASK SIM_MER {}
```

请熟悉我新发明的代码示意标记，即使用 `[上文]` 和 `[下文]` 表示新增代码的插入位置。

若只是想在某个时机，查看程序是否出错，就用 `SIM_ASK` 宏。若发现程序某处输出了错误信息，设法找到它的位置，然后溯源纠错。`SIM_ASK` 有些像中医的脉诊。

# 插入

`sim_str_insert` 可向 `SimStr` 对象指定位置插入 C 字符串，其声明和实现如下：

```c
/* sim-str.h ++ */
void sim_str_insert(SimStr *str, size_t index, const char *raw);
```

```c
/* sim-str.c ++ */
void sim_str_insert(SimStr *str, size_t index, const char *raw) {
        if (!str || !raw) {
                SIM_ERR("sim_str_insert error: invalid string!");
                return;
        }
        /* 判断 index 是否在 [0, str->n] 之内或者为无穷大 */
        if (index > str->n || index == SIZE_MAX) {
                SIM_ERR("sim_str_insert error: overrange!");
                return;
        }
        /* 考虑是否对 str 扩容 */
        size_t n = strlen(raw);
        size_t m = str->n + 2 * n; /* 扩容时，不妨多扩一些 */
        if (str->m < m) {
                char *new_data = realloc(str->data, m);
                if (!new_data) {
                        SIM_ERR("sim_str_insert error: failed to realloc!");
                        return;
                }
                str->data = new_data;
                str->m = m;
        }
        /* 将 str->data 的 [index, str->n) 部分移到尾部 */
        size_t new_n = str->n + n;
        size_t d = str->n - index;
        if (d > 0) {
                memcpy(str->data + new_n - d, str->data + index, d);
        }
        /* 将 raw 插入 str->data */
        memcpy(str->data + index, raw, n);
        str->n = new_n;
        str->data[str->n] = '\0';
}
```

有了 `sim_str_insert` 函数，实现向 `SimStr` 对象的首部和尾部追加 C 字符串，便不费力气了。

```c
/* sim-str.h ++ */
void sim_str_prefix(SimStr *str, const char *raw);
void sim_str_suffix(SimStr *str, const char *raw);
```

```c
/* sim-str.c ++ */
void sim_str_prefix(SimStr *str, const char *raw) {
        sim_str_insert(str, 0, raw);
}
void sim_str_suffix(SimStr *str, const char *raw) {
        sim_str_insert(str, str->n, raw);
}
```

下面代码以 `sim_str_prefix` 为例，演示这几个插入函数的用法：

```c
SimStr *a = sim_str("error!");
sim_str_prefix(a, "foo ");
SIM_ASK(e);
```

# 删除

有插入，就该有删除。`sim_str_del` 可从 `SimStr` 对象中删除一段字符串，它需要给定删除区域的起始位置和长度。

```c
/* sim-str.h ++ */
void sim_str_del(SimStr *str, size_t begin, size_t n);
```

```c
/* sim-str.c ++ */
void sim_str_del(SimStr *str, size_t begin, size_t n) {
        if (!str) {
                SIM_ERR("sim_str_insert error: invalid string!");
                return;
        }
        if (!str->data) {
                SIM_ERR("sim_str_insert error: invalid string!");
                return;
        }
        /* 检查 [begin, begin + n] 是否超范围，或者 begin 为无穷大 */
        size_t j = begin + n;
        if (begin >= str->n || j > str->n || begin == SIZE_MAX) {
                SIM_ERR("sim_str_del error: overrange!");
                return;
        }
        if (n == 0) return;
        /* 用 begin + n 之后的内容补位 */
        memcpy(str->data + begin, str->data + j, str->n - j);
        str->n -= n;
        str->data[str->n] = '\0';
        /* 缩容 */
        size_t new_m = str->n + 2 * n;
        if (new_m < str->m) {
                char *new_data = realloc(str->data, new_m);
                if (new_data) {
                        str->data = new_data;
                        str->m = new_m;
                } else {
                        SIM_ERR("sim_str_del error: failed to realloc!");
                        return;
                }
        }
}
```

`sim_str_del` 通常要配合 `sim_str_search` 使用，后者可在 `SimStr` 对象中找出与作为目标的 C 字符串初次匹配的位置。由于 `SimStr` 对象的 `data` 成员是 C 字符串，故而 `sim_str_search` 可基于 C 标准库的 `strstr` 函数实现：

```c
/* sim-str.h ++ */
size_t sim_str_find(SimStr *str, const char *target);
```

```c
/* sim-str.c ++ */
size_t sim_str_find(SimStr *str, const char *target) {
        if (!str || !target) {
                SIM_ERR("sim_str_find error: invalid string!");
                return SIZE_MAX;
        }
        if (!str->data) {
                SIM_ERR("sim_str_find error: invalid string!");
                return SIZE_MAX;
        }
        char *where = strstr(str->data, target);
        if (where) {
                return where - str->data;
        } else {
                SIM_ERR("sim_str_find error: no such target!");
                return SIZE_MAX;
        }
}
```

注意，上述 `sim_str_find` 的实现，若未能从 `SimStr` 对象中查找到目标字符串的存在，则意味着出错，返回的位置是超出 `SimStr` 对象的 `data` 成员有效范围的值，即大于 `str->n` 的值，我用的是 `size_t` 类型所能支持的最大值 `SIZE_MAX`，它表示 `size_t` 类型的无穷大。

`sim_str_del` 和 `sim_str_find` 的用法如下：

```c
SimStr *a = sim_str("hello world!");
const char *t = " world!";
sim_str_del(a, sim_str_find(a, t), strlen(t));
SIM_ASK(e);
```

# 出借

我无意于为 `SimStr` 对象实现任何输出函数，若需要将其内容输出，可使用 C 标准库的有关函数，我只需要将 `SimStr` 对象的 `data` 成员借出：

```c
/* sim-str.h ++ */
const char *sim_str_raw(SimStr *str);
```

```c
/* sim-str.c ++ */
const char *sim_str_raw(SimStr *str) {
        if (!str) {
                SIM_ERR("sim_str_raw error: invalid string!");
                SIM_ASK;
                return NULL;
        } else {
                if (str->data) return str->data;
                else {
                        SIM_ERR("sim_str_raw error: emppty string!");
                        SIM_ASK;
                        return NULL;
                }
        }
}
```

注意，若直接使用 `sim_str_raw` 返回值，例如

```c
printf("%s\n", sim_str_raw(a));
```

上述代码，若 `sim_str_raw` 返回 `NULL`，则程序会以段错误的形式崩溃而终止。不过，在 `sim_str_raw` 返回 `NULL` 之前，我已用 `SIM_ASK`主动报错，即使程序崩溃，也能看到有助于快速确定程序出错位置的错误信息。

`sim_str_raw` 的用法如下：

```c
SimStr *a = sim_str("hello world!");
const char *t = " world!";
sim_str_del(a, sim_str_find(a, t, strlen(t));
printf("%s\n", sim_str_raw(a));
SIM_ASK;
```

# 示例

str-test.c 是 `SimStr` 模块的测试程序，不过也可作为该模块的用法示例。

```c
/* str-test.c */
#include <sim-str.h>
void str_print(SimStr *a) {
        printf("%s: %lu bytes.\n",
               sim_str_raw(a), sim_str_size(a));
}

int main(void) {
        SimStr *a = sim_str(NULL); str_print(a);
        char *w = "world";
        sim_str_suffix(a, w); str_print(a); SIM_ASK;
        
        char *h = "hello";
        sim_str_prefix(a, h); str_print(a);
        sim_str_insert(a, 5, " "); str_print(a);
        sim_str_suffix(a, "!"); str_print(a); SIM_ASK;
        
        sim_str_del(a, sim_str_find(a, w), strlen(w));
        str_print(a);
        sim_str_del(a, strlen(h), 1); str_print(a);
        sim_str_del(a, 0, sim_str_size(a)); str_print(a);
        SIM_ASK;
        
        sim_str_free(a); SIM_ASK;
        return 0;
}
```

编译 str-test.c 的命令为：

```console
$ gcc -I. sim-err.c sim-str.c str-test.c -o str-test
```

运行 str-test：

```console
$ ./str-test
```

结果应该是

```
: 0 bytes.
world: 5 bytes.
helloworld: 10 bytes.
hello world: 11 bytes.
hello world!: 12 bytes.
hello !: 7 bytes.
hello!: 6 bytes.
: 0 bytes.
```

若程序中的 `SIM_ASK` 未输出任何错误信息，意味着这个程序很有可能是正确的。在 Linux 里，我们可以使用像 valgrind 工具检验程序是否存在内存错误：

```console
$ valgrind --leak-check=yes ./str-test
==23781== Memcheck, a memory error detector
... ... 程序输出结果，略 ... ...
==23781== HEAP SUMMARY:
==23781==     in use at exit: 0 bytes in 0 blocks
==23781==   total heap usage: 5 allocs, 5 frees, 1,081 bytes allocated
==23781== 
==23781== All heap blocks were freed -- no leaks are possible
==23781== 
==23781== For lists of detected and suppressed errors, rerun with: -s
==23781== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```

# 总结

很抱歉，我发明的这个轮子可能跑得并不比别人的更快。我之所以要发明它，一个原因是，想试试我的错误处理机制是否可用。再者，这个轮子可以帮我改进 SimErr 模块。其三，我向将其作为范本，讲述一个 C 程序模块的结构和实现过程。文中以注释形式的一些修改示意，诸如

```c
/* sim-str.c ++ */
/* [上文] ... */
新增的代码；
/* [下文] ... */
```

在之后的章节里将会频繁使用，而且以后可能会根据需要，再发明一些新的示意标记。

# 附录

Sim 项目进化到本文所描述的时代，所有的源码可通过以下链接获取。

> * 错误机制：[sim-err.h](sim-err.h) 和 [sim-err.c](sim-err.c)
> * 动态字符串：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
