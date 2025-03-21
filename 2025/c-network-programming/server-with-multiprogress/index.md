---
title: 坍缩
abstract: 也许是电子与生俱来的能力。
date: 03 月 21 日
...

进程以复制自身的方式同时穿过 `fork` 构造的双缝，演化出父进程和子进程的叠加态。

在父进程中，我们可以使用 `wait` 或 `waitpid` 终结这个分身术，迫使这个系统坍缩，从而塑造了子进程的历史。若父进程能够响应 `SIGCHLD` 信号，则系统坍缩的方式几乎天衣无缝，将系统的不确定性保留到了最后一刻，具有电子双缝延迟选择实验一样的魅力，即电子同时通过双缝之后再去观测它，依然会导致它发生坍缩。

父进程始终是那个父进程，它不断开启子进程，又不断回收子进程，最终它拥有了许多子进程的历史。也许我们看不到的那一个又一个电子，它们可能承载着自宇宙大爆炸以来它们经过的全部历史。

人类以为是他们的观测导致了电子叠加态的坍缩，他们可能过于多情了。观测自己的分身，创造一段历史，也许原本便是电子与生俱来的能力。之所以电子双缝会发生干涉效应，仅仅是因为电子觉得它还没必要让自己的分身变成历史。

量子系统的这种特性，有什么用处呢？我不清楚，这种特性它是否拥有，我也无法确定。我只知道，多进程模型能够使得网络服务端真正支持客户端的并发访问，而且无需借助较为复杂的同步 I/O 多路复用机制。

现在，请将历史回退到「[两朵乌云](blocking/index.html)」，基于其中的 [network.h](blocking/network.h) 和 [network.c](blocking/network.c)，重写 threebody 程序：

```c
/* threebody.c */
#include <signal.h>
#include <sys/wait.h>
#include "network.h"

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
        /* 服务端通信过程 */
        Socket *x = server_socket("localhost", "8080");
        /* 接受无数个连接 */
        while (1) {
                server_socket_accept(x);
                if (fork() == 0) {
                        close(x->listen); /* 子进程不需要该 socket */
                        /* 从 x 读取信息 */
                        char *msg = socket_receive(x);
                        printf("%s:%s said: %s\n", x->host, x->port, msg);
                        free(msg);
                        /* 从 x 发送信息 */
                        socket_send(x, "Hi, I received your message!");
                        sleep(10); /* 主动阻塞，模拟通信时间很长 */
                        exit(0);
                }
                close(x->connection); /* 父进程不需要该 socket */
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
```

上述所有的代码，都应该很熟悉的，因为我已经用尽了 200% 的力气在前面的章节中进行了铺垫。

再进行一些试验之前，先对新的 threebody.c 中的部分代码予以封装。首先，在 network.h 中包含以下头文件：

```c
#include <signal.h>
#include <sys/wait.h>
```

然后在 network.h 中声明用于处理 `SIGCHLD` 的函数：

```c
void observer_init(void);
```

在 network.c 中定义 `observer_init` 函数：

```c
static void handler(int signal) {
        wait(NULL);
}

void observer_init(void) {
        struct sigaction sa;
        sa.sa_handler = handler;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART|SA_NOCLDSTOP;
        int a = sigaction(SIGCHLD, &sa, NULL);
        if (a == -1) {
                fprintf(stderr, "sigaction error!\n");
                exit(-1);
        }
}
```

然后再度重写 threebody.c：

```c
/* threebody.c */
#include "network.h"

int main(void) {
        Socket *x = server_socket("localhost", "8080");
        observer_init();
        while (1) {
                server_socket_accept(x);
                if (fork() == 0) {
                        close(x->listen); /* 子进程不需要该 socket */
                        /* 从 x 读取信息 */
                        char *msg = socket_receive(x);
                        printf("%s:%s said: %s\n", x->host, x->port, msg);
                        free(msg);
                        /* 从 x 发送信息 */
                        socket_send(x, "Hi, I received your message!");
                        sleep(10); /* 主动阻塞，模拟通信时间很长 */
                        exit(0);
                }
                close(x->connection); /* 父进程不需要该 socket */
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
```

编译上述 threebody.c，然后运行所得 threebody 程序：

```console
$ gcc network.c threebody.c -o threebody
$ ./threebody
```

然后并行运行两次 ywj：

```console
$ parallel ::: ./ywj ./ywj
```

结果表明，两个 ywj 与 ywj 同时运行，虽然它们连接的服务器器端每个子进程中都有模拟通信时间很长的约 10 秒的阻塞，但是它们都能立刻同时得到服务端的消息，丝毫不受该阻塞的影响。这意味着什么呢？笼罩在我们头顶的第 2 朵乌云已悄然散去。
