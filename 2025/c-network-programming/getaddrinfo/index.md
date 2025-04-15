---
title: 套接字地址
abstract: 两者同出，异名同谓。玄之又玄，众妙之门。
date: 02 月 27 日
...

# 前言

可能没有任何一本网络编程方面的书籍会告诉你，网络编程真正重要的问题，从来都不是两台计算机上的进程如何传送信息，而是套接字地址如何表示。

倘若你对上述说法觉得奇怪甚至荒谬，那么请思考这样一个问题，一个普通的计算机用户，他要上网，在十多年前，他可能还需要知道 IP 地址、网关、DNS 等概念以及如何为操作系统设置它们，而在现在，他只需知道可用的 WIFI 的名字。

我们既然要编写网络程序，是以网络体系已经存在且能正常运行为前提的，故而无需关心其原理和内幕，亦即你无需知道 OSI 模型是什么，Internet 四层架构是什么，架构的每一层有哪些协议，在计算机操作系统中如何实现这些协议，以及 TCP 协议需要几次握手和几次挥手……你只需要知道什么是 IP 地址，什么是域名，便已足够。不过，要阅读本文及其后续一系列文章，你需要熟悉 C 语言，无需精通。

如果你在找与网络通信相关的工作，面试官可能会考问上述这些我觉得不重要的东西。这种情况下，你的确需要了解它们。不过，当你学会网络编程之后再去学习网络原理，也许更知其然。

# 存在先于本质

你刚开始学习 C 语言，有人告诉你需要先了解计算机组成原理，汇编语言编程，操作系统原理，编译原理，然后你才能写一个 Hello world 程序：

```c
/* hello-world.c */
int main(void)
{
        printf("Hello world!\n");
        return 0;
}
```

然后用 gcc 编译它：

```console
$ gcc hello-world.c -o hello-world
```

这个人也许是善意的，真心想给你有用的建议，可是他很容易让你根本没有机会写出上述的 Hello world 程序。

我看过的网络编程书籍，好像每本书的作者都是这个人。

# 是否有些恐慌？

现今的操作系统，无论是 Linux 还是任何一种还活着的 Unix，以及我很少使用的 Windows，它们都以套接字为核心提供了一组 C 函数，通常将其统称为套接字 API（Application Program Interface，应用程序接口）。基于套接字 API，我们能能够自己的程序从操作系统中获得与其他进程通信的功能，亦即这些功能不必我们亲自实现，只需我们学会使用。

学习基于套接字 API 的网络编程，第一个需要学会的函数是 `getaddrinfo`，它可以将文字形式套接字地址转化为一个数字，我可以不厌其烦地称其为数字形式的套接字地址。

`getaddrinfo` 的形式如下：

```c
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int getaddrinfo(const char *node,
                const char *service,
                const struct addrinfo *hints,
                struct addrinfo **res);
```

现在，你可能已经开始有些恐慌，如同此刻的我。请自信一些，上述代码给出了 `getaddrinfo` 的声明，以及使用它时所需的头文件。

`getaddrinfo` 接受 4 个参数，若它运行成功，返回 0，否则返回一个非 0 的错误码。`node` 参数表示 IP 地址，`service` 表示端口，`hints` 可以让我们对 `getaddrinfo` 的运行施加一些干扰，`res` 存储着 `getaddrinfo` 对文字形式的套接字地址的转化结果。

何谓 IP 地址？每一台连接在网络上的计算机都会有的一个地址，形式上通常是以英文句号隔开的 4 个数字，且每个数字的最大值是 255。例如 `192.168.1.10` 是我的计算机的 IP 地址。这种形式的地址，称为 IP v4 地址，即第 4 代 IP 地址。如果你不知道自己的 IP 地址，可以询问你的网络管理员，或者你的朋友，该如何使用像 `ifconfig` 这样的命令查出你计算机上的网卡目前正在使用的 IP 地址：

```console
$ ifconfig
... ... ...
wlp3s0: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 192.168.1.10  netmask 255.255.255.0  broadcast 192.168.1.255
        inet6 2409:8a3c:1502:4bc0:b06d:459:cd45:f30f  prefixlen 64  scopeid 0x0<global>
        ... ... ...
```

现代还有一种地址，叫 IP v6，即第 6 代 IP 地址，例如上述我的 `ifconfig` 命令输出的 `inet6` 后面的一串数字：

```plain
2409:8a3c:1502:4bc0:b06d:459:cd45:f30f
```

便是 IP v6 地址。现在使用 IP v6 地址的网络不太多，至于它的未来如何，对于学习网络编程而言，暂且无需关心。

何谓端口？一个普通的数字，通常可以随意指定，但尽量不要小于 1024，因为低于 1024 的端口有一些是被操作系统或其他明星级的网络服务占用的。

现在，我们已经可以很轻松地向 `getaddrinfo` 提供文字形式的套接字地址了。例如

```c
int a = getaddrinfo("192.168.1.10", "8080", 第 3 个参数, 第 4 个参数);
```

接下来看 `getaddrinfo` 的第 3 个参数 `hints`，它是一个指针，指向类型为结构体 `addrinfo` 的对象。结构体 `addrinfo` 的定义如下：

```c
struct addrinfo {
    int              ai_flags;
    int              ai_family;
    int              ai_socktype;
    int              ai_protocol;
    socklen_t        ai_addrlen;
    struct sockaddr *ai_addr;
    char            *ai_canonname;
    struct addrinfo *ai_next;
};
```

不要急于恐慌，通常情况下，我们真正需要关心的只有 `ai_family` 和 `ai_socktype` 这两个成员（或字段），其他成员皆可清零，例如

```c
struct addrinfo hints;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_INET;
hints.ai_socktype = SOCK_STREAM;
```

`memset` 可将指定的内存空间清零，即以 0 值填充该空间。`ai_family` 用于设定 IP 地址是第 4 代（AF_INET）还是第 6 代（AF_INET6）。至于 `ai_socktype`，暂且理解为用于设定在网络通信时数据传输过程的可靠程度，`SOCK_STREAM` 表示相当可靠。还有一种不太可靠的设定，即 `SOCK_DATAGRAM`。这两种可靠程度分别对应着两种信息传输协议，`SOCK_STREAM` 对应 TCP 协议 `IPPROTO_TCP`，而 `SOCK_DATAGRAM` 对应 UDP 协议 `IPPROTO_UDP`，如同我们曾经用邮局寄信，前者相当于挂号信形式，后者相当于平信形式，平信较挂号信，容易出现信件丢失的情况。信息传输协议是选择 TCP 还是 UPD，可以通过 `ai_protocol` 设定，但是 `ai_socktype` 已经决定了所用的协议，故而无需设定。

`hints` 参数用于通告 `getaddrinfo`，`192.168.1.10` 是 IP v4 地址，数据传输是要用要用靠谱的 `SOCK_STREAM` 对应的传输协议，即 TCP，而不是不靠谱的传输协议 UDP。不过，UDP 也并非无用，其优势在于传输速度更快，适用于传输一些不太重要的数据，例如网络上聊天方面的信息。不过在学习网络编程的过程中，可以先忽略 UDP，我们不会遇到必须使用它的情况。待日后用到，现学即可，它比 TCP 容易多了。

现在，我们对 `getaddrinfo` 已经熟悉到了以下程度：

```c
struct addrinfo hints;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_INET;
hints.ai_socktype = SOCK_STREAM;
int a = getaddrinfo("192.168.1.10", "8080", &hints, 第 4 个参数);
```

第 4 个参数 `res` 是一个双重指针：

```c
struct addrinfo **restrict res
```

`res` 指向的对象是一个指针，后者指向与上述 `hints` 同类型的结构体对象。之所以是双重指针，是因为 `getaddrinfo` 要将 IP 地址和端口转换的结果存于一个链表，需要用一个指针记录该链表的首结点位置（内存地址），`res` 则指向这个指针。

# 插曲：双重指针

可能你对 C 语言的熟悉程度不足以理解上述关于双指针的讨论，不必为此不安，毕竟并没有任何人要求必须对 C 语言了如指掌方可学习网络编程。

假设在一个函数内部动态分配了一块内存，用于存储某个对象，该如何将该对象作为函数的返回值呢？例如，

```c
int *foo(void) {
        int *a = malloc(sizeof(int));
        *a = 3;
        return a;
}
```

为了让 `foo` 函数更安全一些，要求它的返回值只能是正确或错误码，像 `getaddrinfo` 函数那样，返回 0 表示成功，返回非 0，表示失败，此时 `foo` 该如何将指针 `a` 的值返回呢？答案是让 `foo` 参数是一个双重指针：

```c
int foo(int **p) {
        int *a = malloc(sizeof(int));
        if (a) {
                *a = 3;
                *p = a;
                return 0;
        } else return -1;
}
```

请记住这个原则：一个函数内，若需要通过参数将内部的某个变量的值传给函数外部，则参数的类型必须是该变量类型的指针。在上述的 `foo` 函数中，内部的变量 `a`，若将其值通过参数 `p` 传给函数外部，由于 `a` 的类型是 `int *`，那么 `p` 的类型便是 `a` 类型的指针，亦即 `int **`。

希望上述简单的 C 代码示例能够让你明白为什么 `getaddrinfo` 的第 4 个参数是双重指针，并且你也学会了如何在函数的参数中通过灵活地使用指针取出函数内部的数据。

# 迷雾重重

现在我们已经能够让 `getaddrinfo` 运行起来了，即

```c
struct addrinfo hints, *res;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_INET;
hints.ai_socktype = SOCK_STREAM;
int a = getaddrinfo("192.168.1.10", "8080", &hints, &res);
```

可是 `getaddrinfo` 将文字形式的 `192.168.1.10:8080` 转化的数字藏于 `res` 所指向的结构体中的何处呢？答案是，在结构体 `addrinfo` 的 `ai_addr` 成员所指向的结构体对象之中。`ai_addr` 的类型是 `struct sockaddr *`，而结构体 `sockaddr` 的定义是

```c
struct sockaddr {
    sa_family_t     sa_family;      /* Address family */
    char            sa_data[];      /* Socket address */
};
```

即使不知道 `sa_family_t` 是无符号的整型类型，应该也能看出，`sockaddr` 并无套接字地址信息，原因是 `sockaddr` 只是一个通用的类型，它几乎无意义，但其指针形式能指向真正有用的结构体 `sockaddr_in`：

```c
struct sockaddr_in {
    sa_family_t     sin_family;     /* AF_INET */
    in_port_t       sin_port;       /* Port number */
    struct in_addr  sin_addr;       /* IP address */
};
```

`sockaddr_in` 描述的是文字形式的套接字地址转化为数字的结果，`sin_port` 是无符号的 16 位整型类型，表示端口，IP 地址在 `sin_addr` 中。`sin` 不是正弦的意思，其中的 `s` 表示 socket，`in` 表示 Internet……真是鬼画符一样的缩写。`sin_addr` 类型又是一个结构体，但这个结构体跟没有差不多：

```c
struct in_addr {
    in_addr_t s_addr;
};
```

`s_addr` 即文本形式的 IP 地址转换而成的数字，一个 32 位的无符号整型数。

`sockaddr_in` 存储的套接字地址中的 IP 地址是 IP v4。套接字 API 为 IP v6 准备的套接字地址数据结构是 `sockaddr_in6` 结构体类型。

`struct sockaddr *` 类型的指针可以安全地指向任何一个 `struct sockaddr_in` 类型的对象（IP v4 地址），也可以指向任何一个 `struct sockaddr_in6` 类型的对象（IP v6 地址）。之所以需要这么多结构体，只有一个原因，套接字 API 为了兼容 IP v4 和 IP v6 地址在设计上尽了自己最大的努力。

也许你早已看出，实际上套接字 API 无需如此努力，使用 `void *` 类型的指针便可指向任何类型的对象。你的看法是正确的，问题在于套接字 API 诞生时，C 语言还没有 `void *` 指针。后来，为了与历史兼容，它只能保持旧时方式。

现在我们基本上已经知道如何查看 `getaddrinfo` 生成的数字形式的 IP 地址和端口了，即

```c
struct sockaddr_in *my_addr = (struct sockaddr_in *)(res->ai_addr);
printf("%u:%hu\n", my_addr->sin_addr.s_addr, my_addr->sin_port);
```

试着将程序写完整：

```c
/* 源文件 foo.c */
#include <stdio.h>        /* printf 需要 */
#include <string.h>       /* memset 需要 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main(void) {
        struct addrinfo hints, *res;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo("192.168.1.10", "8080", &hints, &res);
        
        struct sockaddr_in *my_addr = (void *)res->ai_addr;
        printf("%u:%hu\n", my_addr->sin_addr.s_addr, my_addr->sin_port);
        
        return 0;
}
```

使用 gcc 编译 foo.c，并运行所得程序：

```console
$ gcc foo.c -o foo
$ ./foo
167880896:36895
```

IP 地址 `192.168.1.10` 被转换为 `167880896`，端口 `8080` 被转换为 `36895`。或许你会觉得，一路辗转，所得不过如此，前途依然渺茫。且莫着急，毕竟我们已经得到了类似邮政编码的东西了，距离将信件发送出去，已近在咫尺。

# 更安全，更稳健

关于 `getaddrinfo`，还有一些事情需要探索，例如直到现在，我们似乎未曾真正关心过它的返回值，亦即上述代码中的 `int a`。前面说过，`a` 的值若为 0，则表示 `getaddrinfo` 运行正确，否则运行失败。如何处理失败的情况呢？基本的做法是，打印错误信息，让程序退出：

```c
if (a != 0) {
        fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
        exit(-1); /* 让程序以非 0 值退出 */
}
```

套接字 API 有 `gai_strerror` 函数，它可将 `getaddrinfo` 返回的错误码「翻译」为人类可理解的文本信息。

还有一个问题，`getaddrinfo` 用于保存数字化的套接字地址信息，即上述代码 `res` 所指向的结构体对象，其类型为 `struct addrinfo`，它是在 `getaddrinfo` 内部创建的，亦即它占用了一段动态分配的内存空间，在不再使用它时，需要进行释放，以防程序出现内存泄漏。本文的开始伴随 `getaddrinfo` 的声明一同出现的另一个函数 `freeaddrinfo`，便是用于释放动态分配的类型为 `struct addrinfo` 的对象。

更为安全的 foo.c 应该像下面这样写：

```c
/* 源文件 foo.c */
#include <stdio.h>        /* printf 需要它 */
#include <string.h>       /* memset 需要它 */
#include <stdlib.h>       /* exit 需要它 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main(void) {
        struct addrinfo hints, *res;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo("192.168.1.10", "8080", &hints, &res);
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1); /* 让程序以非 0 值退出 */
        }
        
        struct sockaddr_in *my_addr = (void *)res->ai_addr;
        printf("%u:%hu\n", my_addr->sin_addr.s_addr, my_addr->sin_port);
        freeaddrinfo(res);
        
        return 0;
}
```

还有一个问题，前文提到过，`getaddrinfo` 将得到的数字化的 IP 地址和端口存于一个链表中，上述代码中的 `res` 指向的是该链表的首结点。也许对此你会觉得奇怪，一个文字形式的套接字地址，怎么可能会被转换成多个数字化的套接字地址呢？答案是，`getaddrinfo` 所做的工作远比我们想象得更多，它不仅可将文字形式的套接字地址转换为数字形式，也可以将域名或主机名与端口号构成的地址转化为数字形式的套接字地址。

域名或主机名，可以指代一台拥有多个 IP 地址的计算机。这并不奇怪，毕竟一台计算机上可以装配多张网卡，而每张网卡都可以配置一个 IP 地址。为稳妥起见，需要在遍历 `getaddrinfo` 所生成的地址列表，以获得所有可能的数字化的套接字地址：

```c
for (struct addrinfo *it = res; it; it = it->ai_next) {
        struct sockaddr_in *my_addr = (void *)it->ai_addr;
        printf("%u:%hu\n", my_addr->sin_addr.s_addr, my_addr->sin_port);
}
```

现在，对上述的 foo.c 略作修改，除了增加套接字地址列表的遍历访问过程，将 `getaddrinfo` 所需的文字形式的 IP 地址和端口由硬性设定改为命令行参数传入：

```c
/* 源文件 foo.c */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main(int argc, char **argv) {
        struct addrinfo hints, *res;
        if (argc != 3) {
                fprintf(stderr, "usage: %s <host> <port>\n", argv[0]);
                exit(-1);
        }
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo(argv[1], argv[2], &hints, &res);
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1); /* 让程序以非 0 值退出 */
        }
        for (struct addrinfo *it = res; it; it = it->ai_next) {
                struct sockaddr_in *my_addr = (void *)it->ai_addr;
                printf("%u:%hu\n", my_addr->sin_addr.s_addr, my_addr->sin_port);
        }
        freeaddrinfo(res);
        return 0;
}
```

重新编译 foo.c，然后按以下方式执行 foo 程序：

```console
$ ./foo 192.168.1.10 8080
167880896:36895
$ ./foo localhost 8080
16777343:36895
$ ./foo 127.0.0.1 8080
16777343:36895
$ ./foo 0.0.0.0 8080
0:36895
```

`localhost` 和 `127.0.0.1` 的数字转换结果相同，都是 `16777343`。`localhost` 表示本地计算机，即运行 foo 程序的这台计算机，而 `127.0.0.1` 表示网络回环地址，任何一台计算机，即使它没有网卡，也拥有这个地址。可将 `localhost` 和 `127.0.0.1` 理解成「自我」。一个人，可以不与整个社会发生任何关系，但是只要他还活着，必定会有「自我」。

# 有些计算机，不单纯

试着用 foo 程序访问域名 www.baidu.com 的 IP 地址：

```console
$ ./foo www.baidu.com 80
776379431:20480
4014382119:20480
```

可以发现，`www.baidu.com` 指向的计算机，它拥有两个 IP 地址。事实上，许多知名的网页服务器（端口默认为 80），都有多个 IP。这便是 `getaddrinfo` 生成的是一个地址列表，而不是单个地址的原因。也许你会觉得有些不可思议，`getaddrinfo` 是如何获得域名为 `www.baidu.com` 的计算机的 IP 地址的呢？答案是，`getaddrinfo` 会调用操作系统提供的域名查询服务，在 DNS 服务器上查找 `www.baidu.com` 的 IP 地址集。

用主机名或域名指代一台网络上的计算机，是因为文字形式的 IP 地址并不便于人类记忆。普通人，勉强能记住几个 IP v4 地址，但是要记住一个 IP v6 可能就很难了。主机名和域名的主要区别是，前者无人管理，只能在局域网中使用，而后者会有网络上专门的计算机系统予以管理并提供查询服务，故而可用于指代某台远程的计算机。

上述网络基础知识，现在可以知道，也可以不知道，`getaddrinfo` 已经将所有细节封装了。总之，我们已经可以轻易做到将文字形式的套接字地址转化为数字形式了，接下来便是如何利用后者，将信息发送出去。现在，倘若你使用的是 Linux 系统，不妨使用以下命令，再回顾一下 `getaddrinfo` 的一切：

```console
$ man 3 getaddrinfo
```

# 反函数

下面额外介绍一个与 `getaddrinfo` 相反的函数 `getnameinfo` 的用法。`getnameinfo` 可将结构体 `addrinfo` 中存储的数字化的套接字地址转换为文本形式。对于我们而言，文本形式的套接字地址是已知的，故而 `getnameinfo` 似乎没什么用处。不过，对于上述 foo 程序输出的 www.baidu.com 的 IP 地址，也许你更希望它是文本形式，所以 `getnameinfo` 还是有用的。

`getnameinfo` 的声明如下：

```c
#include <sys/socket.h>
#include <netdb.h>

int getnameinfo(const struct sockaddr *addr, socklen_t addrlen,
                char *host, socklen_t hostlen,
                char *serv, socklen_t servlen,
                int flags);
```

你可能又开始恐慌了……我也是。`getnameinfo` 接受 `socketaddr` 结构体，然后将 IP 地址转换为文本存储于长度为 `hostlen` 的缓冲区 `host`，并将端口转换为文本存于长度为 `servlen` 的缓冲区 `serv`，该数组的长度是 `servlen`。最后一个参数 `flags` 用于组合一些标识，让 `getnameinfo` 输出我们所需要的结果，例如如果希望输出的是文本形式的 IP 地址和端口，而不是主机名或域名以及服务名（服务名是为端口取的便于人类记住的名字，例如端口 80，对应的服务名是 http），只需将 `flags` 的值设定为

```c
NI_NUMERICHOST | NI_NUMERICSERV
```

下面是 `getnameinfo` 的用法示例：

```c
/* 输出 IP 地址和端口 */
char host[NI_MAXHOST], port[NI_MAXSERV];
for (struct addrinfo *it = res; it; it = it->ai_next) {
        getnameinfo(it->ai_addr, it->ai_addrlen,
                    host, sizeof(host),
                    port, sizeof(port),
                    NI_NUMERICHOST | NI_NUMERICSERV);
        printf("%s:%s\n", host, port);
}
```

可使用这段代码替换前文最后一个版本的 foo.c 中遍历 `getaddrinfo` 生成的套接字地址列表的那部分代码。

若你想更加了解 `getnameinfo`，可以

```console
$ man 3 getnameinfo
```

# 一网打尽

上文不断修改的 foo.c，所实现的皆为文字形式的 IP v4 地址及端口构成的套接字地址的数字化转换，虽然提及了 IP v6 地址，但是并未给出相关代码将其转换为数字。实际上，非常简单，只需将上文最后实现的 foo.c 中一处代码

```c
hints.ai_family = AF_INET;
```

修改为

```c
hints.ai_family = AF_INET6;
```

便可实现将文字形式的 IP v6 地址和端口构成的套接字地址转换为数字化套接字地址。

事实上，`getaddrinfo` 走得更远，胸怀更为宽广，它能够同时支持 IP v4 和 IP v6，只需将 `hints.ai_family` 的值设成 `AF_UNSPEC`。完成此修改，重新编译 foo.c，执行新生成的 foo 程序，获取 `www.baidu.com` 对应的套接字地址：

```console
$ ./foo www.baidu.com 80
2409:8c00:6c21:118b:0:ff:b0e8:f003:80
2409:8c00:6c21:11eb:0:ff:b0bf:59ca:80
39.156.70.239:80
39.156.70.46:80
```

# 总结

不知不觉，我们已经基本学会了 4 个套接字 API 函数了，`getaddrinfo`、`gai_strerror`、`freeaddrinfo` 以及 `getnameinfo`。没什么难的，我甚至有些过度讲解了。倘若你足够聪明，借助 man 命令便可获得这些函数最为权威的讲解，然后迅速学会它们。只是 man 命令无法告诉你，学习网络编程居然可以直接从学习 `getaddrinfo` 开始。
