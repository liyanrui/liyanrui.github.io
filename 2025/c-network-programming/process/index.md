---
title: 世界是你的，也是我的
abstract: 我们都有美好的未来！
date: 03 月 17 日
...

# 前言

进程，也许是绝大多数人既熟悉又陌生的事物。

若一个人很自信的说，万物皆备于我……他便是一个进程无疑了。在操作系统中运行的任何一个进程，它都会认为，整个系统资源，它可以随意使用，例如，内存可以想要多少，就能拥有多少……完全可以说，孟老夫子在 2000 多年前已经提前拥有共产主义意识了。

矛盾之处在于，任何一个程序，它的设计者都不会让它浪费资源，而程序在操作系统中运行起来，亦即变成进程时，它却觉得自己拥有整个了世界。如果大多数人都没有这样的觉悟，我对人类能否实现共产主义表示悲观。

# 电子穿过双缝

经常翻阅量子力学科普书的人应该都知道，量子力学最有名的实验，是电子双缝实验。一组电子仿佛同时穿过两个缝隙，在缝隙后面的屏幕上形成干涉图像，一旦观测它们，便会导致它们立刻选择某条路径，于是干涉图像消失。

Unix 或 Linux 操作系统也为进程提供了颇为相像的机制，即 `fork` 函数：

```c
#include <unistd.h>

pid_t fork(void);
```

`fork` 能够为进程制造一个类似双缝装置的程序逻辑，因为它不像普通的 C 函数那样，每次调用，只能返回一次，`fork` 可以同时返回两次。然后，进程会像电子穿过双缝那样穿过这两次返回所对应的程序逻辑。例如

```c
/* foo.c */
#include <unistd.h>
#include <stdio.h>

int main(void) {
        pid_t a = fork();
        if (a == 0) {
                printf("I am %d.\n", getpid());
        } else {
                printf("I am %d.\n", getpid());
        }
        return 0;
}
```

`getpid` 函数用于获取当前进程的 ID，它与 `fork` 函数皆为系统调用，且二者的返回值类型皆为 `pid_t`，这是整型类型的别名，专用于表达进程 ID，当然也有 0 和 `-1` 这种含的特殊意义的值。

编译上述的 foo.c 并执行所得程序：

```console
$ gcc foo.c -o foo
$ ./foo
I am 33989.
I am 33990.
```

亦即上述代码中的 `if ... else ...` 的两个条件分支都被执行了。

倘若你的思维之前一直停留在经典模式，但愿此刻你不会开始怀疑这个世界是否真实了。费曼说，没人能理解量子力学。我们不妨同意这个观点，不过，我们应该能理解同时通过 `fork`「双缝」的进程，这个机制毕竟是上个世纪下半叶的人类创造的。

现在，将上述 foo.c 略微修改一处，只需关注 `main` 函数：

```c
int main(void) {
        pid_t a = fork();
        if (a == 0) {
                printf("I am %d.\n", getpid());
        } else {
                printf("I am %d.\n", a);
        }
        return 0;
}
```

`else` 分支里打印的不在是 `getpid()` 的返回值，而是 `fork` 的两次返回的值之一，因为这两次返回是同时的，因此我无法使用「第一次返回的值或第二次返回的值」之类的说法。

重新编译 foo.c 并运行所得程序，则程序里的两次 `printf` 打印的内容是相同的，例如

```
I am 41590.
I am 41590.
```

这说明，对于 `else` 分支里的那个进程而言，它持有 `if` 分支里的进程的 ID。

对 foo.c 再略作修改：

```c
int main(void) {
        printf("I am %d.\n", getpid());
        pid_t a = fork();
        if (a == 0) {
                printf("(%d, %d)\n", a, getpid());
        } else {
                printf("(%d, %d)\n", a, getpid());
        }
        return 0;
}
```

再度编译 foo.c，然后运行所得程序，结果如下：

```
I am 41851.
(41852, 41851)
(0, 41852)
```

现在能看出一些端倪了么？`main` 函数所在的进程，ID 为 41851，在调用 `fork` 之后，在 `else` 分支里的进程恰好是 `main` 函数所在的进程，而 `if` 分支里的进程是一个新的进程。

现在，可以下一个结论：在 `fork` 的返回值为 0 的情况里，会出现一个新的进程，而原有的进程——`main` 函数所在的进程继续在 `fork` 返回值为非 0 的情况中存在，并且它能够持有那个新进程的 ID。若不理解这个结论，需要你再认真回顾一下上述对 foo.c 的两次修改及程序运行结果。原有的进程，习惯上称为父进程，而新的进程称为子进程。

实际上，有的时候，`fork` 会返回 -1，表示它执行时出错。这种情况，可能性虽然很小，但理论上是存在的，因此健壮的多进程并发程序，需严谨对待。例如

```c
int a = fork();
if (a == 0) { /* 子进程 */
        ... ... ...
} else { /* 父进程 */
        if (a == -1) {
                fprintf(stderr, "fork error!\n");
                exit(-1);
        } else {
                ... ... ...
        }
}
```

考考你，你知道下面这段代码（假如 `fork` 的执行都是成功的）的结果是什么吗？

```c
int main(void) {
        fork();
        fork();
        printf("I am %d.\n", getpid());
        return 0;
}
```

建议动手试验，若结果不符合你的答案，你需要再认真理解关于 `fork` 同时返回两次这一事实。

# 麦田守望者

物理学家可能至今依然不明白一个电子如何同时穿过双缝，而我们却已经很清楚一个进程如何穿过 `fork` 返回的两个条件分支，父进程将「自身」复制为一个新的子进程，二者同时穿过各自的条件分支，并相互干涉，只是这种干涉，进程毫无所知，它们只是以为自己在独立地运行着。若不理解操作系统的进程调度原理，人类也以为通过 `fork` 产生的条件分支的两个进程不会干涉。事实上，并发的进程可能存在干涉，甚至能创造出薛定谔的猫。

在操作系统看来，父进程及其 `fork` 出来的子进程，有很多人会按照一些过时的教科书所说的，操作系统会将父进程的资源完整复制一份交给子进程。倘若如此，那父进程和子进程在资源方面是完全独立的，也就不发生任何干涉，但实际上现代操作系统采用的是 Copy-on-Write 机制。COW 机制就像两个人同时阅读同一本书，其中有一人想在某页上做笔记，操作系统会为他单独复制这一页供他使用。

父进程和子进程的各自独立运行实际上往往也是假象，因为操作系统在调度它们的时候，可能会让它们运行在单个 CPU 核心上。在这种情况下，操作系统会运行一会父进程，然后将其挂起，再运行一会子进程，然后再将子进程挂起，恢复父进程的运行，如此循环交替，直至它们终止。

还有一个令人伤脑筋的事实是，若子进程结束，父进程尚在运行，则对于操作系统而言，子进程会处于即死又生的状态，所以你可以联想到薛定谔的猫了。这样的子进程，叫僵尸进程。名字有些吓人，但实际上它已经死了，只是还占用着资源，即尸位素餐。制作僵尸进程很容易，例如

```c
#include <unistd.h>

int main(void) {
    if (fork() == 0) {  /* 创建子进程 */
        return 0;       /* 子进程立即退出，成为僵尸 */
    }
    while (1) {         /* 父进程仍在一直运行 */
            sleep(1);
    }
    return 0;
}
```

假设上述源码编译成的程序为 foo，运行 foo 之后，可通过以下命令查看是 foo 的子进程是否为僵尸进程：

```console
$ ps aux | grep foo
```

在我的机器上，上述命令给出的结果为

```
...    9232  ...   S+   ... ./foo
...    9233  ...   Z+   ... [foo] <defunct>
```

其中含有 `Z+` 和 `defunct` 的进程，便是僵尸进程。

好的父亲，他无需去干涉孩子的行为，他只需要守望。

> 一大群小孩儿在一大块麦田里玩一种游戏，有几千个，旁边没人——我是说没有岁数大一点的——我是说只有我。我会站在一道破悬崖边上。我要做的，就是抓住每个跑向悬崖的孩子——我是说要是他们跑起来不看方向，我就得从哪儿过来抓住他们。我整天就干那种事，当个麦田里的守望者。
<p style="text-align: right;">——《麦田守望者》</p>

守望是很简单的，只需要使用 `wait` 函数，它也是个系统调用，作用是让父进程先不要做别的事情，静静等待子进程的结束。

`wait` 的用法并不难，倘若你不想追究细节，只需在父进程中添加

```c
wait(NULL);
```

例如

```c
#include <unistd.h>
#include <sys/wait.h>

int main(void) {
    if (fork() == 0) {
        return 0;
    }
    wait(NULL); /* 守望 */
    while (1) {
            sleep(1);
    }
    return 0;
}
```

将上述源码再次编译为程序 foo 并运行，再使用 `ps | grep foo` 命令查看进程状态，便看不到僵尸态的子进程了，因为它被父进程回收了。

不妨将 `fork` 简单理解为 `malloc`，将 `wait` 简单理解为 `free`。构建新的进程，像是构建一块有生命的内存空间。大多数情况下，父进程不 `wait` 子进程也无妨，除非它是网络服务端之类的程序，需要保持长期运行。对于网络服务端而言，若子进程开启过多且不回收，系统资源可能很快会被消耗殆尽，从而导致父进程崩溃。

父进程等着子进程先结束，这件事会让人觉得不太舒适，试问世上可有白发人想送黑发人么？其实，可以浪漫一些。我的父亲虽然不在了，这事的确发生了，可他依然在守望着我。一代又一代，构成绵延不绝的守望链，直至家族绝后。

有的时候，父进程先于子进程结束，原因也是它没有守望子进程。例如

```c
#include <unistd.h>
#include <sys/wait.h>

int main(void) {
    if (fork() == 0) {
        sleep(3); /* 将子进程阻塞 3 秒 */
        return 0; /* 子进程退出 */
    }
    return 0; /* 父进程退出 */
}
```

这更符合现实的情况，即黑发人送白发人，只是黑发人会变成孤儿，以天为父，以地为母。失去父进程的子进程会被操作系统中的 `init` 进程领养。`init` 进程是操作系统启动后运行的第 1 个进程，亦称 1 号进程，如天如地。

应该没人想当孤儿。父母们，责任在肩，还是努力多给子女一些守望吧。

# 守望的艺术

`wait` 的声明如下：

```c
#include <sys/wait.h>

pid_t wait(int *status);
```

`wait` 执行成功，它会返回结束的子进程 ID，并将子进程的退出状态保存在 `status` 指向的对象，否则返回 -1。有一组宏，可以破译各种可能的子进程退出状态。

* 若子进程正常退出，`WIFEXITED(status)` 的结果为非 0；
* `WEXITSTATUS(status)` 返回子进程的退出状态——子进程返回的值；
* 若子进程因其他进程发送给它的信号而引发终止，`WIFSIGNALED(status)` 返回真值；
* `WTERMSIG(status)` 返回引起子进程退出的信号。

现在还不需要了解操作系统为进程提供的信号处理机制，故而上述的 4 个宏，我们暂且只需要关心前两个即可，以下示例程序演示了它们的基本用法：

```c
/* foo.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(void) {
        int a = fork();
        if (a == 0) {  /* 创建子进程 */
                printf("子进程：我是 %d！\n", getpid());
                sleep(7);
                return 0;
        } else {
                if (a == -1) {
                        fprintf(stderr, "fork error!\n");
                        exit(-1);
                }
                int status;
                pid_t child_id = wait(&status);
                if (child_id == -1) {
                        fprintf(stderr, "wait error!\n");
                        exit(-1);
                }
                if (WIFEXITED(status)) {
                        printf("父进程：%d 正常退出了，"
                               "其退出状态为 %d\n",
                               child_id, WEXITSTATUS(status));
                } else {
                        printf("父进程： %d 的退出存在异常！\n", child_id);
                }
        }
        return 0;
}
```

上述代码的运行结果如下：

```console
子进程：我是 17119，我退出了！
父进程：17119 正常退出了，其退出状态为 0。
```

需要注意，上述代码中的父进程要等待子进程大概 7 秒，方能输出内容，而子进程的输出则是在程序甫运行时便出现了，它给出了子进程的 ID。若在父进程等待期间，快速在另一个终端用 `kill` 命令杀死子进程，例如

```console
$ kill -9 17119
```

则父进程会立刻输出：

```console
父进程： 17175 的退出存在异常！
```

`kill -9` 命令是 `kill` 进程通过 ID 向对应的进程发送 `SIGKILL` 信号，然后导致操作系统会强行杀死收到该信号的进程。有些信号，进程可以捕捉并作出响应，但它们对 `SIGKILL` 却无能为力，这个信号是进程无法违抗的死亡通知。

# 非阻塞等待

`wait` 是阻塞式的，它会让父进程挂起，无法执行后续指令，直到子进程退出。若只有父进程只有一个子进程，为了回收该子进程，父进程阻塞等待，这种做法实际上是没有太大意义的，因为在实际效用上，它与单进程的程序并无区别。

使用比 `wait` 更为基础的 `waitpid` 函数，能够实现父进程非阻塞等待子进程退出。`waitpid` 的声明如下：

```c
#include <sys/wait.h>

pid_t waitpid(pid_t pid, int *status, int options);
```

该函数比 `wait` 多出两个参数，`pid` 和 `options`，前者用于指定等待某个子进程，若为 -1，表示等待所有子进程，后者用于设定等待方式，若为 `WNOHANG`，表示非阻塞等待。

父进程用 `waitpid` 回收指定的子进程，若子进程并未退出，则 `waitpid` 会返回 0，而不像 `wait` 那样将父进程挂起。若 `waitpid` 返回 -1，表示执行失败。

`waitpid` 的非阻塞特性，意味着我们需要在一个循环中不断使用 `waitpid` 尝试回收子进程，直至 `waitpid` 返回子进程的 ID，而不是 0 或 -1。例如

```c
/* foo.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(void) {
        int a = fork();
        if (a == 0) {  /* 创建子进程 */
                printf("子进程：我是 %d！\n", getpid());
                sleep(5);
                return 0;
        } else {
                if (a == -1) {
                        fprintf(stderr, "fork error!\n");
                        exit(-1);
                }
                int once = 1;
                while (1) {
                        int status;
                        pid_t child_id = waitpid(-1, &status, WNOHANG);
                        if (child_id > 0) {
                                printf("父进程：%d 退出了！\n", child_id);
                                break;
                        } else if (child_id == 0) {
                                if (once) {
                                        printf("父进程：等待子进程退出。\n");
                                }
                        } else {
                                fprintf(stderr, "waitpid error!\n");
                                exit(-1);
                        }
                        once = 0;
                        sleep(1); /* 降低 cpu 占用 */
                }
        }
        return 0;
}
```

上述代码，我的试验结果如下：

```console
父进程：等待子进程退出。
子进程：我是 8191！
父进程：8191 退出了！
```

# 信号处理

以轮询的方式回收已结束的子进程，虽然能让父进程非阻塞运行，但是在非阻塞等待期间，父进程要执行的操作只能在轮循过程中精心部署，例如上一节示例程序中，为了避免父进程重复说「等待子进程退出」，我使用了变量 `once` 控制父进程在非阻塞等待期的行为不会被轮询过程重复，虽然有效，但使得程序逻辑更为复杂。实际上，子进程在退出时，会向父进程发送一个信号 `SIGCHLD`。若父进程主动忽略该信号，或对该信号作出一些响应，则退出的子进程不会变成僵尸进程。

让父进程忽略子进程的退出信号，非常简单，只需在 `main` 函数的开始，或者所有的调用 `fork` 函数的语句之前插入以下语句：

```c
signal(SIGCHLD, SIG_IGN)
```

`SIG_IGN` 是 `SIGNAL IGNORE` 的缩写。很多操作系统级别的信号，皆可凭此法忽略。不过，多数时候，我们需要回收子进程占用的一些资源，需要主动对子进程的退出信号作出响应。

最简单的响应机制是将一个用于处理信号的函数作为参数传递给 `signal`。例如

```c
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

void handler(int signal) {
        int pid = wait(NULL);
        printf("父进程：%d 退出了！\n", pid);
}

int main(void) {
        signal(SIGCHLD, handler);
        int a = fork();
        if (a == 0) {  /* 创建子进程 */
                printf("子进程：我是 %d！\n", getpid());
                sleep(5);
                return 0;
        }
        /* 让父进程阻塞，模拟长时间运行 */
        while (1) sleep(1);
        return 0;
}
```

上面示例，在 `fork` 出子进程后，父进程进入主动阻塞模式，相当于它在执行很多其他任务，同时等待子进程退出信号。我的试验结果如下：

```
子进程：我是 19221！
父进程：19221 退出了！
```

试验结果出现后，可使用 Ctrl + C，即 Ctrl 和 C 的组合键关闭父进程。这个组合键可驱动操作系统向终端前台正在运行的进程发送 `SIGINT` 信号，若进程未对该信号作出响应，操作系统会终止它。

# 更安全的信号处理

历史原因，即使符合 POSIX 标准的操作系统所实现的 `signal` 也不尽相同，此外，当父进程存在较多子进程时，`signal` 可能会存在丢失信号的情况，即当它正在响应某个信号的同时，又有不断有新的信号发送过来，而 `signal` 不能对信号进行排队，故而新的信号可能会丢失。在生产环境中，更推荐使用 `sigaction` 函数，当然它的用法比 `signal` 略复杂。

使用 `sigaction` 函数，首先需要准备一个结构体，例如

```c
struct sigaction sa;
sa.sa_handler = handler;
sigemptyset(&sa.sa_mask);
sa.sa_flags = SA_RESTART|SA_NOCLDSTOP;
```

先不必害怕，以上代码还是较为简单的，`struct sigaction` 结构体实际上有 5 个成员变量，上面只设定了其中 3 个。`sa.handler` 的值是上一节定义的信号处理函数。`sigemptyset(&sa.sa_mask)` 可将信号掩码清空，表示在信号处理期间不阻塞任何信号。倘若需要在信号处理期间阻塞某些信号，可以通过 `sigaddset` 函数增加信号掩码，不过，目前我们对 Unix 或 Linux 系统的信号机制知之甚少，这些高级知识暂且不必理会。`sa.sa_flags` 是一组标志，用于精确控制 `sigaction` 的行为，其中 `SA_RESTART` 的作用是自动重启被信号中断的系统调用，`SA_NOCLDSTOP` 的作用是忽略子进程暂停或恢复时产生的 `SIGCHLD` 信号。

对于处理 `SIGCHLD` 信号而言，上述对 `sa` 的设定已经足够，若不甚理解，完全可不求甚解，暂且照抄，如同学习模拟电路的路数，贵在实践，而非理论。下面只需简单调用 `sigaction` 函数，即可完成对 `SIGCHLD` 的安全处理：

```c
int a = sigaction(SIGCHLD, &sa, NULL);
if (a == -1) {
        fprintf(stderr, "sigaction error!\n");
        exit(-1);
}
```

`sigaction` 第三个参数，也是一个结构体，类型与 `sa` 相同，用于返回进程对 `SIGCHLD` 信号的原有配置，若不关心，设为 `NULL` 即可。若 `sigaction` 执行成功，返回 0，否则返回 -1。

基于上述知识，对上一节的示例进行修改：

```c
#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

void handler(int signal) {
        int pid = wait(NULL);
        printf("父进程：%d 退出了！\n", pid);
}

int main(void) {
        /* SIGCHLD 信号处理 */
        struct sigaction sa;
        sa.sa_handler = handler;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART|SA_NOCLDSTOP;
        int a = sigaction(SIGCHLD, &sa, NULL);
        if (a == -1) {
                fprintf(stderr, "sigaction error!\n");
                exit(-1);
        }
        /* 开启子进程 */
        int a = fork();
        if (a == 0) {  /* 创建子进程 */
                printf("子进程：我是 %d！\n", getpid());
                sleep(5);
                return 0;
        }
        /* 让父进程阻塞，模拟长时间运行 */
        while (1) sleep(1);
        return 0;
}
```

# 坍缩


现在，请将历史回退到「[封装](../wrapper/index.html)」，那是我们曾经的田园时代！基于那段历史中的字符串类和网络类，再结合多进程机制，在套接字函数皆为阻塞的情景中为 threebody 赋予并发能力。

```c
/* threebody.c */
#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#include "sim-network.h"

void handler(int signal) {
        wait(NULL);
}

int main(void) {
        /* SIGCHLD 信号处理 */
        struct sigaction sa;
        sa.sa_handler = handler;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART|SA_NOCLDSTOP;
        int a = sigaction(SIGCHLD, &sa, NULL);
        if (a == -1) {
                fprintf(stderr, "sigaction error!\n");
                exit(-1);
        }
        /* 服务端 */
        SimServer *threebody = sim_server("localhost", "8080");
        while (1) {
                sim_server_run(threebody);
                if (fork() == 0) {
                         /* 子进程不需要监听套接字 */
                        sim_server_close_listener(threebody);
                        /* 从客户端接收数据 */
                        SimStr *msg_from = sim_server_receive(threebody);
                        if (sim_str_safe(msg_from)) {
                                printf("%s\n", sim_str_raw(msg_from));
                        }
                        sim_str_free(msg_from);
                        /* 向客户端发送信息 */
                        SimStr *msg_to = sim_str("threebody: Hi!");
                        sim_server_send(threebody, msg_to);
                        sim_str_free(msg_to);
                        sim_server_close_client(threebody);
                        exit(0);
                }
                /* 父进程不需要客户端套接字 */
                sim_server_close_client(threebody);
        }
        sim_server_free(threebody);
        return 0;
}
```

上述所有的代码，都应该很熟悉的，因为我已经用尽了 200% 的力气在前面的章节中进行了铺垫。不过，「[封装](../wrapper/index.html)」所实现的网络类尚未提供 `close_listener` 和 `close_client` 方法，下面分别予以实现：

```c
/* sim-network.h ++ */
void sim_server_close_listener(SimServer *self);
```

```c
/* sim-network.c ++ */
void sim_server_close_listener(SimServer *self) {
        if(self) {
                close(self->listener);
                self->listener = -1;
        } else {
                fprintf(stderr, "sim_server_close error: NULL pointer!\n");
        }
}
```

置于 `close_client` 方法，实际上已经实现了，即 `close` 方法。现在我们需要对该方法进行更名，将 `sim_server_close` 更名为 `sim_server_close_client`：

```c
/* sim-network.h [改] */
void sim_server_close_close(SimServer *self);
```

```c
/* sim-network.c [改] */
void sim_server_close_client(SimServer *self) {
        close(self->client);
}
```

基于修改后的网络类，编译上述 threebody.c 并运行所得 threebody：

```console
$ gcc -I. sim-str.c sim-network.c threebody.c -o threebody
$ ./threebody
```

然后打开两个终端，分别用于运行「[两朵乌云](../blocking/index.html)」中的 other-ywj 和 ywj。先在一个终端里运行 other-ywj，它与 threebody 建立连接后，会主动延迟 15 秒，然后才开始与 threebody 通信。随即在另一个终端迅速运行 ywj。

上述试验所得结果与「[select 不负重望](../socket-and-select/index.html)」相似，ywj 无需等到 other-ywj 得到 threebody 的回复后放能与 threebody 通信。基于多进程机制为服务端实现的并发，在拥现代普遍拥有多颗 CPU 核心的计算机上，性能远胜基于 I/O 多路复用的并发。即使计算机只有单颗 CPU 核心，多进程实现的并发，在代码的简洁性上也远胜 I/O 多路复用实现的并发。

在处理与「同时」有关的问题上，量子力学胜过广义相对论，早已屡经实验验证过的。

# 总结

现在先休息一下，喝杯春茶，欣赏一下窗外的世界，万物竞发，这就是我们的现实，时不我待。

进程以复制自身的方式同时穿过 `fork` 构造的双缝，演化出父进程和子进程的叠加态。

在父进程中，我们可以使用 `wait` 或 `waitpid` 终结这个分身术，迫使这个系统坍缩，从而塑造了子进程的历史。若父进程能够响应 `SIGCHLD` 信号，则系统坍缩的方式几乎天衣无缝，将系统的不确定性保留到了最后一刻，具有电子双缝延迟选择实验一样的魅力，即电子同时通过双缝之后再去观测它，依然会导致它发生坍缩。

父进程始终是那个父进程，它不断开启子进程，又不断回收子进程，最终它拥有了许多子进程的历史。也许我们看不到的那一个又一个电子，它们可能承载着自宇宙大爆炸以来它们经过的全部历史。

人类以为是他们的观测导致了电子叠加态的坍缩，他们可能过于多情了。观测自己的分身，创造一段历史，也许原本便是电子与生俱来的能力。之所以电子双缝会发生干涉效应，仅仅是因为电子觉得它还没必要让自己的分身变成历史。

