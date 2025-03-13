---
title: I/O 多路复用
abstract: 拒绝内耗，见机行事。
...

# 前言

网络编程的那朵乌云，驱散之道不在网络层面，而在网络中的每台计算机的操作系统层面。操作系统提供了一些监视文件内容是否发生变化的功能。

假设存在两个文件 a.txt 和 b.txt，基于操作系统的 `select` 机制能够编写一个可以监视 a.txt 和 b.txt 的内容是否发生变化的程序。这种程序所实现的功能即 I（输入）/O（输出）多路复用，即操作系统提供的 `select` 函数，能够监视多个 I/O 设备。a.txt 和 b.txt 与 I/O 设备有什么关系呢？在 Unix 或 Linux 系统中，I/O 设备皆表示为文件，我只是这两个文件表示 I/O 设备。

# 打开文件

熟悉 C 语言的人，肯定知道，打开一份文件，需要使用 C 标准库提供的 `fopen` 函数，但是现在我们需要掌握更底层的 `open`，它也能打开文件，而且它是 POSIX 系统（Unix 或 Linux）暴露给用户的接口。像 `open` 这样的函数亦称系统调用，而 `fopen` 则称为库函数，后者可基于前者实现，且功能更为丰富。

以下代码可打开 a.txt 文件：

```c
int fd_a = open("a.txt", O_RDONLY | O_NONBLOCK);
if (fd_a == -1) {
        fprintf(stderr, "failed to open a.txt");
        exit(-1);
}
```

上述代码以只读（`O_RDONLY`）和非阻塞（`O_NONBLOCK`）模式打开 a.txt 文件。若打开文件失败，`open` 返回 `-1`，否则返回所打开的文件的描述符。

我们是用 a.txt 指代一个文件，而对于操作系统而言，它会用 `fd_a` 这样的文件描述符指代一个文件。还记得「[网络地址](../getaddrinfo/index.html)」吗？人类总是习惯用文字指代某物，而操作系统则习惯使用数字指代某物。两者同出，异名同谓。玄之又玄，众妙之门。

`open` 与 `fopen` 的行为有一些相似性。例如以下代码使用 `fopen` 打开文件：

```c
FILE *fp_a = fopen("a.txt", "r");
if (!fp_a) {
        fprintf(stderr, "failed to open a.txt");
        exit(-1);
}
```

`open` 返回的是文件描述符，而 `fopen` 返回的是文件指针。顺便多说一点，若使用 `fopen` 打开文件，但是有时又需要文件描述符，可使用 `fileno` 获取，例如：

```c
int fd_a = fileno(fp_a);
```

# 文件描述符集合

为了监视打开的多个文件，亦即多路 I/O，需要为 `select` 函数（它也是系统调用）准备一个集合，用于存储文件描述符。

POSIX 系统调用层面为文件描述符集合定义了数据结构 `fd_set`，并围绕该结构实现了一组集合n预算宏，诸如 `FD_ZERO`、`FD_SET`、`FD_CLR` 以及 `FD_ISSET`。使用这些宏操作文件描述符集合，可以让我们无需关心 `fd_set` 的定义。可能没有什么事情比无需关心底层如何实现更令人觉得快乐了。

以下代码，将打开的 a.txt 和 b.txt 文件对应的描述符 `fd_a` 和 `fd_b` 添加到文件描述符集合：

```c
fd_set fds;
FD_ZERO(&fds);
FD_SET(fd_a, &fds);
FD_SET(fd_b, &fds);
```

`FD_ZERO` 将文件描述符集合清空，亦即将其所占空间的数据皆初始化为 0。`FD_SET` 将文件描述符加入集合。

`FD_CLR` 可从文件描述符集合中移除给定的文件描述符，例如：

```c
FC_CLR(fd_b, &fds);
```

`FD_ISSET` 用于判断给定的文件描述符是否在集合中，例如

```c
if (FD_ISSET(fd_a, &fds)) {
        printf("fd_a is in fds.\n");
} else {
        printf("fd_a is not in fds.\n");
}
```

# 阻塞式轮询

`select` 的作用很简单，它能够轮循一组文件——以文件描述符集合表示，从中获得符合需求的子集。不过，理解 `select` 函数的作用，犹如从牛顿力学跃迁到广义相对论，因为它能够让你看到，cpu 的速度是不变的，但时间却可以膨胀或收缩，同时空间在收缩或膨胀，这就是同步 I/O 多路复用的魅力所在。

`select` 的声明为

```c
#include <sys/select.h>

int select(int nfds, fd_set *_Nullable restrict readfds,
           fd_set *_Nullable restrict writefds,
           fd_set *_Nullable restrict exceptfds,
           struct timeval *_Nullable restrict timeout);
```

现在我们已经看惯了来自 man 的函数声明，就像之前的 socket API 函数那样，先主动略过 `_Nullable` 和 `restrict` 这样的定语，这样就可以清晰看到，`select` 接收 5 个参数，第一个参数表示文件描述符的最大值，第二、三、四个参数，都是文件描述符集合，第五个参数表示超时时间值。对于本文要解决的问题，我们只需关心前两个参数，后三个参数皆设为 `NULL` 即可，而且函数声明里的 `_Nullable` 也明确标定了这些参数是可以为 `NULL` 的。

`select` 的第一个参数也许是最不好懂的，它的作用是让 `select` 有机会终止轮询文件描述符集合。以下构造文件描述符集合的过程顺便记录了集合中的文件描述符的最大值：

```c
fd_set fds;
int fd_max = (fd_a > fd_b) ? fd_a : fd_b;
FD_ZERO(&fds);
FD_SET(fd_a, &fds);
FD_SET(fd_b, &fds);
```

我们可以写出以下代码，遍历 `fds`：

```c
for (int i = 0; i < fd_max + 1; i++) {
        if (FD_ISSET(i, &fds)) {
                /* i 是文件描述符，可基于它读写文件  */
        }
}
```

`select` 函数也像上述循环语句这般遍历文件描述符集合，故而它的第一个参数是集合中文件描述符最大值加上 1。倘若我们只让 `select` 轮循文件是否可读的文件描述符集合，在上述构建文件描述符集合 `fds` 之后，可像下面这样调用它：

```c
int ret = select(fd_max + 1, &fds, NULL, NULL, NULL);
```

若 `select` 成功，返回值是文件描述符集合中有多少个文件描述符指代的文件是可读的（属于第 2 个参数），可写的（属于第 3 个参数），出现异常的（属于第 4 个参数），上例只关心可读的。若 `select` 返回 -1，表示轮询文件过程出错。
