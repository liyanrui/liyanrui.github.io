---
title: 我在这里！
abstract: 毁灭地球的计划以失败而告终。
date: 03 月 01 日
...

# 前言

叶文洁将地球在宇宙中的坐标暴露给三体人时，她并不知道这个信息要多久能抵达，而且她对三体人几乎一无所知，她只是知道那个遥远的星系有着一个比地球更强大的文明。

我们试着模仿叶文洁，写一个向外太空暴露地球坐标的网络程序。等到有一天我们对人类彻底失去希望的时候，就运行这个程序。

假设网络上有一台计算机，它的域名是 `www.threebody.com`。这台计算机中运行的某个网络服务端所用的端口是 `8080`，于是该服务端的套接字地址便是 `www.threebody.com:8080`。由于我们已经学会了 `getaddrinfo` 函数，因此很容易将这个文字化套接字地址转化为数字化的套接字地址：

```c
struct addrinfo hints, *res;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;
int a = getaddrinfo("www.threebody.com", "8080", &hints, &res);
if (a != 0) {
        fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
        exit(-1);
}
```

对于叶文洁而言，她所用的向服务端发送消息的进程即客户端。客户端并不需要知道域名为 `www.threebody.com` 的计算机在网络世界中位于何处，它只需向地址为 `www.threebody.com:8080` 的套接字发送信息即可。

再次强调，在网络编程中，服务端和客户端，都是进程。进程，就是计算机中正在运行的程序。

# 套接字

在 Unix 或 Linux 系统或者任一合乎 POSIX 标准的操作系统中，套接字实际上是文件描述符，指代专用于网络通信的文件。为了行文简便，我们只需认为套接字是一种特殊的文件。以后我们会经常说，从套接字读取数据，向套接字写入数据。你只需将这些描述理解为，从套接字指代的文件读取数据，向套接字指代的文件写入数据。

创建套接字的函数，是 `socket`，其声明如下：

```c
#include <sys/socket.h>

int socket(int domain, int type, int protocol);
```

在 `getaddrinfo` 生成的结构体 `addrinfo` 列表中，每一个结点都含有 `socket` 函数所需要的参数值。以 `addrinfo` 列表首结点为例，`socket` 函数用法如下：

```c
int server = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
```

若 `socket` 函数运行成功，则返回文件描述符，否则返回 `-1`。不过，`socket` 函数生成的套接字，尚无地址，不能用于通信。

# 连接

`connect` 函数可将服务端的套接字地址赋予套接字，其形式如下：

```c
#include <sys/socket.h>

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

毫无意外，`connect` 所需要的参数值，我们也已经有了：

```c
int a = connect(server, res->ai_addr, res->ai_addrlen);
```

`connect` 运行成功，会返回 0，否则会返回 `-1`。`connect` 函数第一个参数是用于表示服务端的套接字，后两个参数分别是套接字地址和地址长度（字节数），亦即该函数可将服务端套接字地址赋予一个尚无地址的套接字。同时，`connect` 会悄悄地构造一个对于我们而言不可见的套接字，将本机的 IP 地址和一个随机的端口号作为套接字地址赋予它。因此，`connect` 创建的实际上是一对套接字，对于上例而言，这对套接字可用文字形式的地址表示为：

```c
(www.threebody.com:8080, localhost:随机端口)
```

前者为服务端套接字地址，后者则是客户端套接字地址——对于本文示例而言，即叶文洁用于向三体人发送信息的进程的套接字地址。这对套接字构成了一个网络连接。基于该连接，客户端便可向服务端发送和接收数据。

# 发送数据

`send` 函数可发送数据，其形式如下：

```c
#include <sys/socket.h>

ssize_t send(int sockfd, const void *buf, size_t len, int flags);
```

`send` 函数可将长度为 `len` 的缓冲区 `buf` 中的数据按照 `flags` 规定的方式写入套接字 `sockfd`。大多数时候只需要将 `flags` 设为 `0`，表示采用普通的数据写入方式。若 `send` 函数运行成功，返回值是已发送数据的字节数，否则为 `-1` 表示数据写入失败。

`send` 函数的用法如下：

```c
char *earth_coordinate = "I am here!";
size_t n = strlen(earth_coordinate);
ssize_t b = send(server, earth_coordinate, n, 0);
if (b < 0) {
        fprintf(stderr, "send error!\n");
        exit(-1);
}
```

# 多个套接字地址？

在此需要再次强调，`getaddrinfo` 函数构造的套接字地址可能是多个，若已经忘记了，需要再度回顾网络编程的核心技术「[套接字地址](../getaddrinfo/index.html)」。我们需要从这些地址中选出一个可用的。

何谓「可用的套接字地址」？答案是，能够让 `connect` 函数成功运行的地址。以下代码便是可用地址的一个简单的选择过程：

```c
int server = -1;
for (struct addrinfo *it = res; it; it = it->ai_next) {
        server = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        if (server == -1) continue;
        int a = connect(server, it->ai_addr, it->ai_addrlen);
        if (a == -1) {
                close(server); /* 关闭套接字 */
                continue;
        }
        break;
}
if (server == -1) {
        fprintf(stderr, "failed to connect!\n");
        exit(-1);
}
```

注意，在上述代码的 `for` 循环中，每一次循环，都会构造套接字 `server`，若 `connect` 失败，便会 `close(server)`。之前从未提及 `close` 函数，是因为它并非套接字 API 中的函数，而是 C 标准库中通过文件描述符关闭文件的函数。之所以要关闭文件，是因为 `socket` 函数中已经打开了套接字，而套接字指代文件。

若上述代码所得的 `server` 的值并非 `-1`，则意味着有了可用的服务端套接字以及客户端套接字，这对套接字构成了一个可用的通信连接，亦即客户端套接字与服务端套接字处于连通状态，然后便可向服务端套接字写入数据：

```c
char *earth_coordinate = "I am here!";
size_t n = strlen(earth_coordinate);
ssize_t b = send(server, earth_coordinate, n, 0);
if (b < 0) {
        fprintf(stderr, "send error!\n");
        exit(-1);
}
```

# 完整的程序

```c
/* ywj.c */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>  /* close 函数需要它 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main(void) {
        /* 服务端套接字列表 */
        struct addrinfo hints, *addr_list;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo("www.threebody.com", "8080", &hints, &addr_list);
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        /* 选择可用的连接 */
        int server = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                server = socket(addr_list->ai_family,
                                 addr_list->ai_socktype,
                                 addr_list->ai_protocol);
                if (server == -1) continue;
                int a = connect(server, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(server); /* 关闭 socket 函数打开的文件 */
                        continue;
                }
                break;
        }
        freeaddrinfo(addr_list);
        if (server == -1) {
                fprintf(stderr, "failed to connect!\n");
                exit(-1);
        }
        /* 向服务端发送数据 */
        char *earth_coordinate = "I am here!";
        size_t n = strlen(earth_coordinate);
        ssize_t b = send(server, earth_coordinate, n, 0);
        if (b == -1) {
                fprintf(stderr, "send error!\n");
                exit(-1);
        }
        close(server);
        return 0;
}
```

编译上述源码：

```console
$ gcc ywj.c -o ywj
```

现在，我们对这个世界假装着失望了，运行 ywj 程序：

```console
$ ./ywj
```

可惜的是，我们企图毁灭地球的信息无法发送出去，大概几分钟后，ywj 程序以

```plain
send error!
```

而告终。

原因是，www.threebody.com 对应的计算机并不存在。

幸好它不存在。

# 总结

务必清楚，作为客户端，要向服务端发送数据，需要在明面上创建表达服务端的套接字，同时也需要暗自创建表达客户端自身的套接字，如此方能形成可用的通信连接。这个过程完整的套接字 API 管线是：

```c
getaddrinfo -> socket -> connect -> send
```

其中，`socket` 和 `connect` 所需要的参数，`getaddrinfo` 皆已备好。现在，你应该大概明白，为何在「[套接字地址](../getaddrinfo/index.html)」中说，网络编程真正重要的问题，从来都不是两台计算机上的进程如何传送信息，而是套接字地址如何表示。
