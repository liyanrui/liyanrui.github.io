---
title: 错误
abstract: 是一种艺术。
date: 2025 年 04 月 20 日
...

# 前言

无论是开始做一件事情，或者是为计算机编写一个程序，追求的应该都是正确的结果。我们害怕错误，为了求得正确，发明了各种工程方法学，与错误对抗。多数人倾向于认为，只要消灭了所有错误，便可得到正确，却从未意识到，有生于无。当你决定追求正确的那个瞬间，其实已经意味着整个过程里必然存在着甚至充满着错误。你认为正确是必然的，那么错误也必定是必然的。若有一天，心静下来，重新认识错误的意义，不再畏惧或厌恶它而与之抗衡，也许会发现，你正以艺术的速度获得正确，而非工程的速度。

# 错误码

C 语言对程序的错误处理是朴素的。后来许多新的语言认为，朴素只是原始的代名词，于是有一些发明了异常机制，有一些发明了错误传播机制，其爱好者大多以此宣告 C 语言在错误处理方面的落后。在我看来，他们像是手持数码单反相机到处寻找绝美风光的人们，在嘲笑着画家的纸笔。

Linux 的系统调用以及 C 标准库的许多函数，它们的返回值是 `int` 类型，以非 0 表示函数运行错误，反之运行正确；还有一些函数，返回值是指针类型，以 `NULL` 表示函数运行错误，反之运行正确。同时，会通过 errno.h 文件中定义的全局变量 `errno` 保存错误码。

下面以打开一个不存在的文件为例，证实并展示 C 标准库错误处理机制的存在：

```c
/* foo.c */
#include <stdio.h>
#include <string.h>
#include <errno.h>
int main(void) {
        FILE *file = fopen("nonexistent.txt", "r");
        if (file == NULL) {
                printf("Error: %d\n", errno);
                exit(-1);
        }
        fclose(file);
        return 0;
}
```

编译上述程序 foo.c，并运行所得程序 foo：

```console
$ gcc foo.c -o foo
$ ./foo
Error: 2
```

上述试验表明，`fopen` 函数运行错误，返回 `NULL`，同时错误码即 `errno` 的值为 2。倘若你想知道错误码 2 表征何种错误，可以用 `strerror` 获得它对应的错误信息：

```c
printf("Error: %s\n", strerror(errno));
```

对于上例，输出应该为

```
Error: No such file or directory
```

错误码 2，在 Linux 系统的 C 标准库里，将其定义为宏：

```c
#define	ENOENT 2
```

Linux 系统调用以及 C 标准库中，所有的错误码都有类似于上述的宏定义，且首字母为 `E`。错误码为 2 对应的宏 `ENOENT` 的意思是，Error：No Entry。

倘若熟悉错误码对应的宏名，或者手头有一份记录这些宏名及其含义的表格，我们要在编写应用程序时，可以直接使用这套错误处理机制。`errno` 是全局变量，在多线程环境里原本是不安全的。例如，同时有多个线程为其赋值，便会导致 `strerror` 输出的错误信息可能是错的。不过，现代的 Linux 甚至 Windows 已基于线程本地存储（TLS）技术保证了 `errno` 在多线程环境中的安全性。

我并不觉得错误码机制是落后的。若认为简单明确是落后的，想必繁冗隐晦是先进的……错误码机制只是设计时颇费心机，需要站在上帝视角，掌控全部错误类型，为其编码。作为凡人，不能说上帝落后。C++、Java 以及 C# 语言发明的异常机制，还有 Rust 语言发明的错误传播机制，也许能将你从错误管理得到拯救，然而你可能需要以一次又一次牺牲深刻认识错误的哲学意义为代价，即本文前言中所述的意义。

也许你觉得软件里的错误，不过是错误，就像人的毛病，能有什么意义呢？我建议你用一下反证法，确定错误的意义的存在。创造 Unix/Linux、C 编译器以及 C 标准库的人，是一群绝顶聪明的人。我想，你应该无力反对这一点，那么倘若错误没有意义，这些绝顶聪明的人为何不厌其烦，为管理错误而用心设计错误码机制，是因他们不够聪明，设计不出异常或错误传播机制吗？

他们比常人更敬重软件里的一切可能的错误，每一个错误码都意味着他们对错误一份敬意。也许是他们的这种态度成就了他们的聪明，也许是他们的聪明让他们持有这样的态度。他们甚至并不强求你对错误作出响应。上述示例，你完全可以写成

```c
#include <stdio.h>
int main(void) {
        FILE *file = fopen("nonexistent.txt", "r");
        fclose(file);
        return 0;
}
```

只要你能接受这个程序的运行结果是

```
Segmentation fault (core dumped)
```

是谓，它错由它错，人间本蹉跎。

# 我的敬意

使用 C 语言已有二十年，我对错误一向缺乏敬意，现在我觉得有必要为其存在而致敬。我的敬意是，以后凡是我写的函数，只要足够重要，其最后一个参数必定是 `Err` 类型，一个指向字符串常量的指针。

```c
typedef const char * Err;
```

下面这种错误参数的用法示例：

```c
float foo(float a, Err *e) {
        if (a == 0) {
                *e = "foo error!";
                return 0;
        }
        return 1 / a;
}
```

注意，若 `e` 指向一处不确定的空间或者它是空指针，则 `foo` 的第一步，给 `e` 赋值的语句，便是不确定行为，这足以惩罚任何一个对错误缺乏敬意的人。

可能你还是不习惯给指针赋值，经常忘记用 `*` 取出指针保存的地址（所谓的解引用），然后向该地址赋值（写入数据），我们可以用对错误的更多一些敬意，为这个过程定义一个宏：

```c
#define ERR(e, v) do { *(e) = (v); } while(0)
```

`ERR` 宏的用法如下：

```c
ERR(e, "hello error!");
```

上述 `ERR` 宏的调用语句会被 C 的预处理器展开为

```c
do {
        *(e) = (v);
} while (0);
```

在 `do {...} while (0)` 和括号的层层保护下，`ERR` 是足够稳健，其调用语句不会与周边代码产生非确定的纠缠。

现在，我们在 `main` 函数里调用 `foo`：

```c
int main(void) {
        Err e = NULL;
        float a = foo(3, &e);
        if (e) {
                fprintf(stderr, "%s\n", e);
        } else {
                printf("a = %f\n", a);
        }
        return 0;
}
```

若再定义一个宏，

```c
#define MER(e) if (e) { printf(stderr, "%s\n", e); e = NULL;} else
```

则 `main` 函数可简化为

```c
int main(void) {
        Err e;
        float a = foo(3, &e);
        MER(e) {
                printf("a = %f\n", a);
        }
        return 0;
}
```

每次调用 `MER`，在输出 `e` 记录的错误信息后，便将其重置为 `NULL`，相当于消解了一次 `e`，以便在下一次用它记录错误时无需对其初始化。

尝试让 `foo` 出错：

```c
float b = foo(0, &e);
MER(e) {
        printf("b = %f\n", a);
}
```

这段代码里的 `printf` 没机会运行，而且它会输出一条错误信息：

```
foo error!
```

这就是我创造的错误机制。它是否适合你，我不确定，不过，你也可以创造你的。实际上，它是否适合我，我也不确定，因为我还没有开始在实际的项目里使用它。

# 惩戒

`MER` 宏是仁慈（mercy）的，它只是指出你的程序出现了错误。这类错误对程序本体并非致命，只是会导致程序出现非预期行为。有些错误对程序的本体是致命的，例如内存分配失败，或者访问了空指针。我对这些致命的错误的敬意可表现为

```c
#define PUN(e) if (e) { fprintf(stderr, "%s\n", e); exit(-1); }
```

亦即当错误成立，程序便打印一条错误信息，然后终止运行，并将 -1 作为程序的返回值。

`PUN` 的用法如下：

```c
Err e;
float a = foo(0, &e); PUN(e);
```

或

```c
void *p = malloc(1024 * sizeof(char));
if (!p) { Err e = "malloc error!"; PUN(e);}
```

`PUN` 是惩戒（punish）。是慈悲为怀，还是视错如死，取决于你的意愿。通常情况下，若你写的代码给他人用，你对错误不妨宽容，而若你用别人的代码，对错误不妨严厉，此谓严以律己，宽以待人。

# 命名空间

若在代码里用了他人写的库函数，`#include` 的这个库的头文件里也定义了 `ERR`、`MER` 和 `PUN` 宏，与上文定义的宏同名，该如何解决呢？

传统的方案是，以简写的项目名作为我们定义的宏的前缀。例如，我的项目名为 simple，可将其简写为 sim，于是上文定义的类型和宏，需修改为

```c
/* sim-macros.h */
typedef const char * SimErr;
#define SIM_ERR(e, v) do { *(e) = (v); } while(0)
#define SIM_MER(e) if (e) { fprintf(stderr, "%s\n", e); e = NULL; } else
#define SIM_PUN(e) if (e) { fprintf(stderr, "%s\n", e); exit(-1); }
```

同理，我们项目里的一些要供他人使用的函数，通常也需要加上 sim 前缀。这种方案，是我们非常熟悉的，即阁下姓甚名谁？

# 全局变量

若我定义的每个函数都像上文的 `foo` 函数，必须带有一个用于记录错误信息的参数，我对此并不反对，但是若他人调用这些函数，他们也许会觉得，如此严格，像一场灾难。程序中的错误处理机制，最终目的不过是为了方便程序的调试，而不是作为裹脚布，让人步履蹒跚。

为了减轻函数调用时的负担，我们必须使用全局变量记录错误。在 sim 项目中，用于记录错误信息的全局变量为

```c
SimErr sim_err;
```

围绕 `sim_err`，上文定义的宏需要修改为

```c
#define SIM_ERR(v) do { sim_err = (v); } while(0)
#define SIM_MER if (sim_err) { fprintf(stderr, "%s\n", sim_err); sim_err = NULL; } else
#define SIM_PUN if (sim_err) { fprintf(stderr, "%s\n", sim_err); exit(-1); }
```

# SimErr 模块

现在，我可以为 sim 项目写出它独有的错误机制，一切代码皆在 sim-err.h 和 sim-err.c 文件里，前者全部内容如下：

```c
/* sim-err.h */
#ifndef SIM_ERR_H
#define SIM_ERR_H
#include <stdio.h>
#include <stdlib.h>

typedef const char * SimErr;
extern SimErr sim_err;

#define SIM_ERR(v) do { sim_err = (v); } while(0)
#define SIM_MER if (sim_err) { \
        fprintf(stderr, "%s\n", sim_err); \
        sim_err = NULL; } else
#define SIM_PUN if (sim_err) { \
        fprintf(stderr, "%s\n", sim_err); \
        exit(-1); }
#endif
```

上述代码使用了预处理指令中的条件指令，即 `#ifndef` 与 `#endif`，以实现 sim-err.h 即使被多次 `#include` 而实际上只被载入 1 次。这是 C 库头文件的常规写法。若你对此不熟悉，可从此刻理解它的作用。

全局变量 `sim_err` 在 sim-err.c 中定义。sim-err.c 全部内容如下：

```c
/* sim-err.c */
#include "sim-err.h"
SimErr sim_err = NULL;
```

以下代码演示了错误机制的用法：

```c
/* foo.c */
#include <sim-err.h>
float foo(float a) {
        if (a == 0) {
                SIM_ERR("foo error!");
                return 0;
        }
        return 1 / a;
}
int main(void) {
        float a = foo(0);
        SIM_MER {
                printf("a = %f\n", a);
        }
        float b = foo(3); SIM_PUN;
        printf("b = %f\n", b);
        return 0;
}
```

在 Linux 环境里，若 sim-err.h 在系统默认的头文件目录内，亦即 C 编译器会自动搜索的头文件目录，可使用以下命令编译 foo.c：

```console
$ gcc sim-err.c foo.c -o foo
```

若 sim-err.h 不在系统头文件目录，假设它与 foo.c 在同一目录，需要使用 `-I.` 选项，告诉 gcc 在当前目录（即 `.`）搜索所需头文件：

```console
$ gcc -I. sim-err.c foo.c -o foo
```

要清楚以下两种包含头文件方式的区别，即

```c
#include <sim-err.h>
```

和 

```c
#include "sim-str.h"
```

尖括号形式，gcc 会自动从系统的头文件目录搜索所需的头文件，但不会搜索当前目录。引号形式，gcc 自动从当前目录搜索头文件，而不会搜索系统的头文件目录。若项目想成为库，则用前者，否则用后者。

sim-err.h 和 sim-err.c 可统称为 SimErr 模块。在 C 程序编程中，需要习惯接口与实现的分离。sim-err.h 便是 SimErr 的接口，它像一份协议，告知 SimErr 模块的用户该如何使用该模块的功能。sim-err.c 负责实现 SimErr 模块的功能。

# 总结

SimErr 实现了一个非常简单的错误处理机制。我认为这个模块足以胜任小型 C 项目中的错误处理。sim 项目便是一个小型的 C 项目，是我企图用于找回已尘封多年的 C 语言力量的项目。不过，随着 sim 项目的展开，SimErr 模块的功能也会日益增强，我对程序错误处理的理念或哲学认识应当也会更为深入。
