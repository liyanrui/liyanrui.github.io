---
title: 有一封信，无处可寄
abstract: 一个毁灭地球的计划以失败而告终。
date: 03 月 01 日
...

# 前言

叶文洁将地球在宇宙中的坐标暴露给三体人时，她并不知道这个信息要多久能抵达，而且她对三体人几乎一无所知，她只是知道那个遥远的星系有着一个比地球更强大的文明。我们试着模仿叶文洁，写一个向外太空暴露地球坐标的网络程序。等到有一天我们对人类彻底失去希望的时候，就运行这个程序。

# 收信人地址

假设网络上有一台计算机，它的域名是 `www.threebody.com`，提供网络服务（某个进程）所用的端口是 `8080`，我们需要向这个网络地址发送信息。由于我们已经学会了 `getaddrinfo` 函数，因此很容易将这个域名和端口转化为数字化的网络地址：

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

# 空信封

基于数字化的网络地址可在计算机中创建文件描述符，指代用于网络通信的文件。创建这种文件描述符的函数，是 `socket`，其声明如下：

```c
#include <sys/socket.h>

int socket(int domain, int type, int protocol);
```

还记得在「[Socket 是什么](what-is-a-socket/index.html)」一文是如何理解 socket 的吗？当时是这样说的：

> 网络编程中的 socket，形式上是网络上任何一台计算机的 IP 地址加上某个端口的编号，例如 192.168.0.31:8000，其中冒号之前的部分是 IP 地址，8000 是端口号。socket 的用途是，作为网络中一台计算机上的某个进程用于网络通信的端口地址，且该地址具有唯一性。

网络编程中的 socket，它实际上是文件描述符（整型数），所指代的文件需要含有通信双方的网络地址信息。可将这种文件类比为现实中的信封。`socket` 函数生成的文件描述符，此刻指代的文件还是一个空文件，可将其类比为空信封，不仅没有装入信件，甚至上面连双方的通信地址都没有。

在 `getaddrinfo` 生成的结构体 `addrinfo` 列表中，每一个结点都含有 `socket` 函数所需要的参数值。下面以首结点为例，演示 `socket` 函数的用法：

```c
int envelop = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
```

如果 `socket` 函数运行成功，会返回文件描述符，否则返回 `-1`。

# 信封上的地址

`connect` 函数可以向空信封写上通信双方的地址，其声明如下：

```c
#include <sys/socket.h>

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

毫无意外，`connect` 所需要的参数值，我们也已经都有了：

```c
int a = connect(envelop, res->ai_addr, res->ai_addrlen);
```

`connect` 运行成功，会返回 0，否则会返回 `-1`。

`connect` 函数第一个参数是 `socket`创建的文件描述符——空信封，后两个参数分别是信息接收者的网络地址以及给地址的长度（字节数），亦即该函数相当于在空信封上帮你写上收信人的地址。不过，`connect` 也会会悄悄地将你的地址也写在信封上，因为操作系统知道你的 IP 地址，它会自动给你分配一个随机的端口，从而组成一个网络地址。

# 寄信

`send` 函数可发送信息，其发送信息的过程可类比为，将书信塞进已经带有地址的信封，邮寄出去。`send` 函数的形式如下：

```c
#include <sys/socket.h>

ssize_t send(int sockfd, const void buf[.len], size_t len, int flags);
```

`send` 函数的第 1 个参数是文件描述符，所指向的文件已被 `connect` 函数处理过了，可将其类比为带有通信双方地址的空信封，`send` 函数将长度（字节数）为 `len` 的 `buf` 中的信息塞入这个空信封，然后寄走。`flags` 函数用于更精细的控制这个寄信过程的运作，例如平信、挂号信以及特快专递，它们的运作方式是有区别的，但是大多数时候我们只需要将 `flags` 设为 `0` 即可，类比为寄出的是平信。如果 `send` 函数运行成功，它的返回值是它发送信息的字节数，否则返回 `-1`。

以下代码是 `send` 函数的调用示例：

```c
char *earth_coordinate = "I am here!";
size_t n = strlen(earth_coordinate);
ssize_t b = send(envelop, earth_coordinate, n, 0);
if (b < 0) {
        fprintf(stderr, "send error!\n");
        exit(-1);
}
```

# 多个收信地址？

在此需要再次强调，`getaddrinfo` 函数为信息的接受者构造的收信地址可能是多个，若已经忘记了，需要再度回顾网络编程的核心技术「IP 地址和端口的数字化](getaddrinfo/index.html)」。我们需要从这些地址中选出一个可用的。何谓「可用的地址」？答案是，能够让 `connect` 函数成功运行的地址。

以下代码便是可用地址的一个简单的选择过程：

```c
int envelop = -1;
for (struct addrinfo *it = res; it; it = it->ai_next) {
        envelop = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        if (envelop == -1) continue;
        int a = connect(envelop, it->ai_addr, it->ai_addrlen);
        if (a == -1) {
                close(envelop); /* 关闭 socket 函数打开的文件 */
                continue;
        }
        break;
}
if (envelop == -1) {
        fprintf(stderr, "failed to connect!\n");
        exit(-1);
}
```

注意，在上述代码的 `for` 循环中，每一次循环，都会构造文件描述符 `envelop`，若 `connect` 失败，便会 `close(envelop)`。之前从未提及 `close` 函数，是因为它并非 socket API 中的函数，而是 C 标准库中通过文件描述符关闭文件的函数。之所以要关闭文件，是因为 `socket` 函数中已经打开了这份文件。

若上述代码所得的 `envelop` 的值并非 `-1`，则意味着有了一个可用的信封——它记录了通信双方网络地址，便可使用它，发送信息：

```c
char *earth_coordinate = "I am here!";
size_t n = strlen(earth_coordinate);
ssize_t b = send(envelop, earth_coordinate, n, 0);
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
        /* 构造用于接收信息的网络地址列表 */
        struct addrinfo hints, *addr_list;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo("www.threebody.com", "80", &hints, &addr_list);
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        /* 选择可用的信封 */
        int envelop = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                envelop = socket(addr_list->ai_family,
                                 addr_list->ai_socktype,
                                 addr_list->ai_protocol);
                if (envelop == -1) continue;
                int a = connect(envelop, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(envelop); /* 关闭 socket 函数打开的文件 */
                        continue;
                }
                break;
        }
        freeaddrinfo(addr_list);
        if (envelop == -1) {
                fprintf(stderr, "failed to connect!\n");
                exit(-1);
        }
        /* 寄信 */
        char *earth_coordinate = "I am here!";
        size_t n = strlen(earth_coordinate);
        ssize_t b = send(envelop, earth_coordinate, n, 0);
        if (b < 0) {
                fprintf(stderr, "send error!\n");
                exit(-1);
        }
        close(envelop);
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

原因是，www.threebody.com 现在不存在了。

幸好它不存在。

# 总结

socket 是一个记录这通信双方地址的信封。


