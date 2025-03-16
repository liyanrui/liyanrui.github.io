---
title: 两朵乌云
abstract: 你能同时接听两个电话吗？
date: 2025 年 03 月 09 日
...

# 前言

这份不守江湖规矩的网络编程指南，实际上已经几乎完结了。一台计算机上作为服务端的进程 A，能够与另一台计算机上作为客户端的进程 B 通信，这几乎就是网络编程的全部内容了。不过，我们的头上飘荡着一朵乌云。假设还有一台计算机上的进程 C，令它也作为客户端，与进程 A 通信，此时进程 A 该如何设计呢？

# 接受两个连接

[network.h](../wrapper/network.h) 和 [network.c](../wrapper/network.c) 让我们编写实验性的网络代码更为容易，只需将 threebody 程序写为以下形式，便可实现与两个客户端通信：

```c
/* threebody.c */
#include "network.h"

int main(void) {
        Socket *x = server_socket("localhost", "8080");
        /* 接受 1 个连接 */
        server_socket_accept(x);
        { /* 从 x 读取信息 */
                char *msg = socket_receive(x);
                printf("%s:%s said: %s\n", x->host, x->port, msg);
                free(msg);
        }
        socket_send(x, "Hi, I received your message!");
        close(x->connection);
        /* 再接受 1 个连接 */
        server_socket_accept(x);
        { /* 从 x 读取信息 */
                char *msg = socket_receive(x);
                printf("%s:%s said: %s\n", x->host, x->port, msg);
                free(msg);
        }
        socket_send(x, "Hi, I received your message!");
        close(x->connection);
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
```

编译 threebody.c，运行所得程序 threebody：

```console
$ gcc network.c threebody.c -o threebody
$ ./threebody
```

然后同时运行两次 ywj 程序：

```console
$ parallel ::: ./ywj ./ywj
```

parallel 是可以并行执行命令的 perl 脚本程序，我曾为它的一些基本用法写过一篇笔记，详见「[GNU parallel](https://segmentfault.com/a/1190000007687963)」。上述命令是这篇笔记未曾提及的简单用法，它同时运行了两个 ywj 进程。若你用的 Linux 是 Ubuntu 或其衍生版本，且无 parallel 程序，可使用以下命令安装：

```console
$ sudo apt install parallel
```

threebody 和并行运行的两个 ywj 的运行结果如下图所示，threebody 进程接受到了两个 ywj 进程发来的信息，每个 ywj 进程也得到了 threebody 进程发来的信息。

![threebody 接受两次 ywj 的连接](figures/01.png)

若需要让 threebody 接受无数个客户端的连接，只需将 threebody.c 修改为

```c
/* threebody.c */
#include "network.h"

int main(void) {
        Socket *x = server_socket("localhost", "8080");
        /* 接受无数个连接 */
        while (1) {
                server_socket_accept(x);
                { /* 从 x 读取信息 */
                        char *msg = socket_receive(x);
                        printf("%s:%s said: %s\n", x->host, x->port, msg);
                        free(msg);
                }
                socket_send(x, "Hi, I received your message!");
                close(x->connection);
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
```

此时，threebody 进程便是一个永不休息的服务端了。

# 阻塞

现在，对「[封装](../wrapper/index.html)」中重写的 ywj 程序再略作修改，结果为 new-ywj.c：

```c
/* new-ywj.c */
#include "network.h"

int main(void) {
        Socket *x = client_socket("www.threebody.com", "8080");
        socket_send(x, "I am here!");
        { /* 从 x 读取信息 */
                char *msg = socket_receive(x);
                printf("%s:%s said: %s\n", x->host, x->port, msg);
                free(msg);
        }
        sleep(3);
        close(x->connection);
        socket_free(x);
        return 0;
}
```

与 ywj.c 相比，new-ywj.c 的改写之处仅仅是在关闭 socket 之前增加了 `sleep(3)`，将该程序与 threebody 的通信的时间延长了约 3 秒。

编译 new-ywj.c，得到 new-ywj 程序：

```c
$ gcc network.c new-ywj.c -o new-ywj
```

然后，并行运行 new-ywj 和 ywj：

```console
$ parallel ::: ./new-ywj ./ywj
```

可以发现，虽然 ywj 与 new-ywj 是同时运行的，但 ywj 可能要晚收到 threebody 给它发送的信息约 3 秒。这意味着什么呢？

意味着，threebody 是逐一与客户端通信的，即可能会与 new-ywj 进程通信完毕后，再与 ywj 进程通信。这种形式的通信，称为阻塞式通信。该通信形式，在 threebody 看来，是非常自然的——有很多人给我发信息，我自然是要逐一进行处理的，但在 ywj 看来，非常不合理——我是与其他进程同时发起了通信，为什么本该很慢的它们却很快得到了回复，而我需要等待很久？

在服务端造成通信阻塞的 socket API 函数是封装在 `server_socket_accept` 中的 `accept`，该函数在默认情况下，会逐一接受客户端的连接，从而构造与客户端通信的 socket。实际上，用于接受信息的 `recv` 也是阻塞的，因为该函数要等待位于 socket 另一端的进程发送信息。

一些非 socket API 函数也会造成进程阻塞，例如 `sleep` 的函数以及等待用户输入信息的 `scanf` 函数。进程被阻塞时，操作系统会将其由「运行」状态切换为「等待」状态，此时进程不会占用 CPU，直到特定事件发生（例如有客户端发起网络连接），使其能够继续运行。

实际上，并非是某些函数导致进程阻塞，而是文件的状态设置成阻塞模式时，在一些特殊情况下，会导致读写文件的进程进入等待状态，只是当时恰好该进程的某个函数正在读写文件，从而形成了是这些函数阻塞了进程的错觉——可能只是我有这种错觉。文件阻塞模式导致的进程阻塞，是被动的。由 `sleep` 函数以及其他一些时间延迟函数导致的进程阻塞，是主动的。

# 不阻塞会如何？

在大致理解进程的阻塞机制之后，我们需要考虑的问题是，在网络通信中，若将服务端的一些 socket（它们也是文件啊）设置称非阻塞状态，会发生什么？

在 Linux 系统（或其他遵守 POSIX 协议的操作系统），使用 `fcntl` 函数可修改文件状态。我不打算认真介绍这个函数的用法，原因是，很多应用软件层面的开发者（我也是其中一员）往往对操作系统层面的文件概念并不熟悉，引入过多细节，可能会破坏学习信心。对于网络编程而言，若将一个 socket 设为非阻塞状态，只需调用下面这个封装好的函数：

```c
/* 参数 x 是一个 socket */
int socket_nonblock(int x) {
    return fcntl(x, F_SETFL, fcntl(x, F_GETFL) | O_NONBLOCK);
}
```

其中，`fcntl(x, F_GETFL) | O_NONBLOCK` 用于获取 `x` 的当前状态标志并将其与 `O_NONBLOCK` 合并，这样便拥有了非阻塞状态标志，并且包含 `x` 原有的状态标志，然后通过 `fcntl(x, F_SETFL, 新标志)` 将新标志赋予 `x`。

真正令初学者沮丧的是这段代码中的缩写。首先，`fcntl`，是 `file control` 的意思，即控制文件状态。`F_GETFL` 和 `F_SETFL` 都是操作指令，分别是 `FILE GET FLAGS` 与 `FILE SET FLAGS` 的缩写，可驱使 `fcntl` 获取或设定文件状态标志。文件状态标志是一组简单的二进制位，它们可以通过位运算符 `|` 进行组合，从而为文件设定多种状态。

`fcntl` 的返回值依赖于所用的操作指令。`F_GETFL` 会让 `fcntl` 返回文件的当前状态标志，而 `F_SETFL` 会让它返回 0。还有一些其他的情况，现在用不到。如果 `fcntl` 返回 -1，表示它运行失败了。

像 `fcntl` 这样的函数，它的参数以及所用的位运算，都散发着古奥的气息。原因是这类函数在早期的 Unix 系统诞生时就存在了。函数名与操作指令之所以简写，犹如在纸张尚未发明的年代，古人在竹简上写着简洁但难懂的语句。理解它们，然后用更为现代的名字封装它们吧。

将上述 `socket_nonblock` 函数的定义添加到 [network.c](../wrapper/network.c)，然后在 [network.h](../wrapper/network.h) 增加 `fcntl` 所需的头文件包含语句和 `socket_nonblock` 的声明：

```c
#include <fcntl.h>

int socket_nonblock(int x);
```

现在重写 threebody.c，将监听 socket 和通信 socket 皆增加 `O_NONBLOCK` 标志：

```c
/* threebody.c */
#include "network.h"

int main(void) {
        Socket *x = server_socket("localhost", "8080");
        socket_nonblock(x->listen); /* 为监听 socket 设定非阻塞标志 */
        /* 接受无数个连接 */
        while (1) {
                server_socket_accept(x);
                socket_nonblock(x->connection); /* 为通信 socket 设定非阻塞标志 */
                { /* 从 x 读取信息 */
                        char *msg = socket_receive(x);
                        printf("%s:%s said: %s\n", x->host, x->port, msg);
                        free(msg);
                }
                socket_send(x, "Hi, I received your message!");
                close(x->connection);
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
```

重新编译 threebody.c，运行所得程序 threebody：

```console
$ gcc network.c threebody.c -o threebody
$ ./threebody
```

然后并行运行两个 ywj 程序：

```console
$ parallel ::: ./ywj ./ywj
```

会发生什么奇迹……或灾难呢？结果是，我还没来得及运行 ywj 程序，threebody 进程便失败退出了，给出的错误信息是：

```plain
failed to accept!
```

原因是，`server_socket_accept` 中的 `accept` 函数基于非阻塞状态的监听 socket 以及客户端的连接构造通信 socket 时，因为该过程是非阻塞的，但是却没有客户端连接过来，故而 `accept` 只能返回 `-1`，即与客户端的连接失败。对此，老子微笑着说，孩子们，看到了吧，这就是欲速则不达！

# 主动阻塞

操作系统将文件的状态默认设定为阻塞，是为了保证访问文件的进程不会出错。若我们决定将文件状态设定为非阻塞，便需要在进程某处主动设置一个阻塞，以保证程序不会因失去阻塞而无法遏制地迅速消亡。

前文说过，`sleep` 之类的函数能实现进程的主动阻塞。倘若在上一节的 threebody.c 的 while 循环的开始增加 `sleep` 主动阻塞：

```c
while (1) {
        server_socket_accept(x);
        socket_nonblock(x->connection); /* 为通信 socket 设定非阻塞标志 */
        ... ... ...
        sleep(10); /* 主动阻塞进程 10 秒 */
}
```

那么在改写的 threebody 程序运行后的 10 秒内，并行运行 new-ywj 和 ywj，它们皆能与 threebody 正常通信，但是在 10 秒之后 threebody 依然会因没有新的连接而出错退出。不过，若 new-ywj 与 threebody 先建立了连接，则 ywj 依然需要等 new-ywj 约 3 秒，方能与 threebody 通信，亦即无论服务端是被动阻塞还是主动阻塞，一个客户端与服务端较长时间的通信倒置其他客户端不得不等待，该问题依然无法解决。

不过，主动阻塞依然有一个好处。若理解这一点，请观察

```c
while (1) {
        ... ... ...
}
```

和

```c
while (1) {
        ... ... ...
        sleep(10);
}
```

虽然都是无休止的循环，但前者会一直占用 CPU，而后者是每隔 10 秒占用片刻的 CPU。我们需要设法让主动阻塞变得更灵活一些，即不像 `sleep` 这样机械，便可以实现一个低功耗的服务器，它可以处理无数个客户端的连接。

# 总结

我们的头上依然还飘荡着那朵乌云，可是现在似乎又多了一朵，不过我已隐约看到可以将其驱散的希望了。请怀念这一刻吧，这大概是我们最后的田园时光了。作为服务端的进程，很快要进化得令我们这些旧时代的人觉得它面目全非，难以理解，难以调试。
