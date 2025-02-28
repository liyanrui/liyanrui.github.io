---
title: 「IP 地址:端口」的数字化
abstract: 用数字表示 IP 地址和端口，是网络编程的核心问题。
date: 02 月 27 日
...

可能没有任何一本网络编程方面的书籍会告诉你，网络编程真正重要的问题从来都不是一台机器上的进程如何与另一台机器上的进程如何连接以及数据如何传递，而是这两台机器的网络地址如何表示。

倘若你对上述说法觉得奇怪甚至荒谬，那么请思考这样一个问题，当你打算给一个人写信的时候，最重要的问题是什么？他的收信地址以及其邮政编码。倘若你需要对方回复，那么你还需要知道你的收信地址及其邮政编码。将一个文字形式的网络地址转换为类似邮政编码一样的数字，这便是网络编程的核心问题。

邮局是如何建立的，邮寄的物品如何分拣、传输和派送，这些问题当然更为重要，但是写信和收信的人可无需关心。编写网络程序，自然是以网络已经存在为前提，可以无需关心网络体系的任何内幕，亦即你无需知道 OSI 模型七层结构是什么，Internet 四层结构是什么，每层结构都有哪些协议，在计算机操作系统中如何实现这些协议，TCP 协议需要几次握手和几次挥手等。

多学一些知识，必定有益，但是在刚开始学习网络编程时，这些知识可能会让你难以进行下去。试想你刚开始学习 C 语言，有人告诉你需要先了解计算机体系组成结构，汇编语言，操作系统原理，编译器原理……然后你才能开始写一个 Hello world 程序：

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

这个人也许是善意的，真心想给你一些非常有用的建议，可是他很容易让你没有机会写出上述的 Hello world 程序。我看过的网络编程书籍，每本书的作者都是这个人。

Linux 或任何一种还活着的 Unix，还有我不怎么使用的 Windows，它们都为操作 socket 提供了一组 C 函数。通常将这些函数统称为 socket API（Application Program Interface，应用程序接口）。不妨将这些 API 理解为邮局系统为寄信和收信的人提供的服务人员和服务设施，例如邮递员，邮筒等。

学习 socket 编程，第一个需要学会的 API 是 `getaddrinfo`，它可以将文字形式的 IP 地址和端口转换为一个数字，类似于你为某个寄信或收信的地址查找与之对应的邮政编码。`getaddrinfo` 的形式如下：

```c
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int getaddrinfo(const char *restrict node,
                const char *restrict service,
                const struct addrinfo *restrict hints,
                struct addrinfo **restrict res);

void freeaddrinfo(struct addrinfo *res);

const char *gai_strerror(int errcode);
```

现在，你可能已经开始有些恐慌，如同此刻的我。请自信一些，上述代码给出了 `getaddrinfo` 以及两个辅助函数的声明，以及使用它们时需要包含的三个头文件，当然理解这些，需要你熟悉一些 C 语言。倘若你不熟悉 C，这份文档所讲述的学习 socket 网络编程的思路依然适用于学习其他编程语言封装的 socket 模块或库。李白曾为 C socket API 写过一句诗，莫道前路无知己，天下谁人不识君。

首先，需要大致了解 `getaddrinfo`，它接受 4 个参数，若它运行成功，则返回 0，否则返回一个非 0 的错误码。`node` 参数表示 IP 地址，`service` 表示端口，`hints` 可以让我们对 `getaddrinfo` 的工作过程施加一些干扰，`res` 存储着 `getaddrinfo` 对文字形式的 IP 地址和端口的转换结果。已经熟悉 socket API 用法的人看到这里，或许会立刻指出我的说法不严谨，例如 `node` 也可以是主机名。的确如此，但是学习任何东西，都应该从不严谨出发，再逐步严谨，不是吗？若一开始就追求严谨，我们可能到现在还学不会走路。

可能熟悉 C 语言的人，也是在这里第一次看到 `restrict` 修饰的指针，并且又有些忘记 `const` 修饰的指针是指针常量还是常量指针，例如

```c
const char *restrict node
```

表示的是，指针 `node` 指向的对象是一个字符（`char`），该对象只能通过 `node` 访问（这就是 `restrict` 的限制意义），并且这个对象的内容不可被修改（`const` 的意义所在），亦即 `node` 是一个受限的常量指针。对于我们而言，`node` 是指向用于表示 IP 地址的文本的开始，例如指向 `127.0.0.1` 中开头的字符 `1` 所在的位置。同理，`service` 指向用于表示端口的文本的开始，例如指向 `8080` 中开头的 `8` 所在的位置。

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

现在，我们已经可以很轻松地向 `getaddrinfo` 提供 IP 地址和端口了，例如

```c
int a = getaddrinfo("192.168.1.10", "8080", 第 3 个参数, 第 4 个参数);
```

接下来看 `getaddrinfo` 的第 3 个参数 `hints`，其类型为 `const struct addrinfo *restrict`，依然是一个受限的常量指针，指向类型为 `struct addrinfo` 的对象。结构体 `addrinfo` 的定义如下：

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

不要恐慌，通常情况下，我们真正需要关心的只有 `ai_family` 和 `ai_socktype` 这两个成员（或字段），其他成员皆可清零。例如

```c
struct addrinfo hints;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_INET;
hints.ai_socktype = SOCK_STREAM;
```

`memset` 可将指定的内存空间清零，即以 0 值填充该空间。`ai_family` 用于设定 IP 地址是第 4 代（AF_INET）还是第 6 代（AF_INET6）。`ai_socktype`，可以暂且理解为用于设定在网络通信时数据传输过程的可靠程度，`SOCK_STREAM` 表示相当可靠。还有一种不太可靠的设定，即 `SOCK_DATAGRAM`。这两种可靠程度分别对应着两种通信方式（协议），`SOCK_STREAM` 对应 TCP 传输方式 `IPPROTO_TCP`，而 `SOCK_DATAGRAM` 对应 UDP 传输方式 `IPPROTO_UDP`，就像我们的生活中，用邮局寄信总比找个顺路的熟人将信捎过去会更靠谱一些。TCP 协议和 UPD 协议，可以通过 `ai_protocol` 进行设定，但是 `ai_socktype` 已经决定了它的值，故而可无需设定。

为 `hints` 设定的值有什么用处呢？这些值是在告诉 `getaddrinfo` 一些事情，诸如 `192.168.1.10` 是 IP v4 地址，数据传输是要用要用靠谱的 `SOCK_STREAM` 对应的传输方式，即 `IPPROTO_TCP`。不可靠的传输方式并非无用，其优势在于传输速度更快，适用于传输一些不太重要的信息，例如网络上聊天时的信息。

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

可能你对 C 语言的熟悉程度不足以理解上述关于双指针的讨论，不必为此羞愧，毕竟并没有任何人要求必须对 C 语言了如指掌方可学习网络编程。假设需要定义一个函数 `foo`，在该函数中动态分配了一处内存，用于存储某个对象，该如何将这个对象作为函数的返回值呢？下面是一个简单的示例：

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

若 `p` 是普通指针，例如

```c
int foo(int *p) {
        int *a = malloc(sizeof(int));
        if (a) {
                *a = 3;
                p = a;
                return 0;
        } else return -1;
}
```

则无法满足要求，因为上述函数如同

```c
void bar(int a) {
        a = 3;
}
```

一样无意义。函数的参数相当于函数内部的临时变量，它们只能用于接收来自函数外部的值，而无法通过修改它们的值以求对函数外部产生影响。例如以下代码，调用了上述的 `bar` 函数：

```c
int a = 1;
bar(a);
printf("a = %d\n", a);
```

结果会输出 `a = 1`，而不是 `a = 3`。

希望上述简单的 C 代码示例能够让你明白为什么 `getaddrinfo` 的第 4 个参数是双重指针，并且你也学会了如何在函数的参数中通过灵活地使用指针取出函数内部的数据。若你对 C 的指针依然有些畏惧，不妨阅读许多年前我写的一份文档「[面向指针编程](https://segmentfault.com/a/1190000004200708)」，然后也许你会就此爱上 C 的指针，像下面这段呓语所描述的那样：

> 这是我的指针，虽有很多与之相似的，但这个是我的。我的指针是我的挚友，如同我的生命。我将运用它如同运用我的生命。指针没了我便是废物，我没了指针便成为废人。我会准确无误地使用我的指针，比敌人用得更好。我会在他的程序速度超过我之前超过他，我会超过他的。

现在我们已经能够让 `getaddrinfo` 运行起来了，即

```c
struct addrinfo hints, *res;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_INET;
hints.ai_socktype = SOCK_STREAM;
int a = getaddrinfo("192.168.1.10", "8080", &hints, &res);
```

`getaddrinfo` 将文字形式的 `192.168.1.10:8080` 这样的网络地址转换成的数字藏于 `res` 所指向的结构体中的何处呢？在 `struct addrinfo` 结构体的 `ai_addr` 成员所指向的结构体对象之中。

`ai_addr` 的类型是 `struct sockaddr *`，而 `sockaddr` 的定义是

```c
struct sockaddr {
    sa_family_t     sa_family;      /* Address family */
    char            sa_data[];      /* Socket address */
};
```

即使不知道 `sa_family_t` 是无符号的整型类型，应该也能看出，结构体 `sockaddr` 并无网络地址信息，原因是 `sockaddr` 只是一个通用的类型，它几乎无意义，真正有用的是它的一个「派生」类型 `sockaddr_in`：

```c
struct sockaddr_in {
    sa_family_t     sin_family;     /* AF_INET */
    in_port_t       sin_port;       /* Port number */
    struct in_addr  sin_addr;       /* IPv4 address */
};
```

`sockaddr_in` 描述的是文本形式的 IP v4 和端口号描述的网络地址转化为数字的结果，`sin_port` 是无符号的 16 位整型类型，表示端口，IP v4 的地址在 `sin_addr` 中。`sin` 不是正弦的意思，其中的 `s` 表示 socket，`in` 表示 Internet……真是鬼画符一样的缩写。`sin_addr` 类型又是一个结构体，但这个结构体跟没有差不多：

```c
struct in_addr {
    in_addr_t s_addr;
};
```

`s_addr` 即文本形式的 IP v4 地址转换而成的数字，一个 32 位的无符号整型数。

之所以说结构体类型 `sockaddr_in` 是结构体 `sockaddr` 的派生类型，是因为一个 `struct sockaddr *` 类型的指针可以指向任何一个 `struct sockaddr_in` 类型的对象（IP v4 地址），也可以指向任何一个 `struct sockaddr_in6` 类型的对象（IP v6 地址）。之所以需要这么多结构体，就一个原因，socket API 为了兼容 IP v4 和 IP v6 地址在设计上尽了自己最大的努力。

倘若你对 C 的指针足够熟悉，应该能看出实际上 socket API 无需如此努力，因为 `void *` 类型的指针能够指向任何类型的对象。你的认识是正确的，socket API 在这方面的繁琐，是因为它诞生的时候，C 语言还没有 `void *` 指针。

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

关于 `getaddrinfo`，还有一些事情需要探索，例如直到现在，我们似乎未曾真正关心过它的返回值，亦即上述代码中的 `int a`。前面说过，`a` 的值若为 0，则表示 `getaddrinfo` 运行正确，否则运行失败。如何处理失败的情况呢？基本的做法是，打印错误信息，让程序退出：

```c
if (a != 0) {
        fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
        exit(-1); /* 让程序以非 0 值退出 */
}
```

还记得本文的开始伴随 `getaddrinfo` 的声明一同出现的 `gai_strerror` 函数吗？它的任务就是将 `getaddrinfo` 返回的错误码「翻译」为人类可理解的文本信息。

还有一个问题，`getaddrinfo` 用于保存数字化的网络地址信息，即上述代码 `res` 所指向的结构体对象，其类型为 `struct addrinfo`，它是在 `getaddrinfo` 内部创建的，亦即它占用了一段动态分配的内存空间，在不再使用它时，需要进行释放，以防程序出现内存泄漏。本文的开始伴随 `getaddrinfo` 的声明一同出现的另一个函数 `freeaddrinfo`，便是用于释放动态分配的类型为 `struct addrinfo` 的对象。

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

不过，还有一个问题，前文提到过，`getaddrinfo` 将得到的数字化的 IP 地址和端口存于一个链表中，上述代码中的 `res` 指向的是该链表的首结点。也许对此你会觉得奇怪，文本形式的 IP 地址和端口描述的一个网络地址，怎么可能会被转换成多个数字化的网络地址呢？一个文本形式的网络地址的确只能转换为一个数字化的网络地址，只是 `getaddrinfo` 所做的工作比我们想象得更多，它不仅可将文本形式的网络地址转换为数字形式，也可以将主机名或域名转换为数字形式的网络地址。

一个主机名或域名，指代的是网络上的一台计算机，而后者可以拥有多个 IP 地址。这并不奇怪，因为一台计算机上可以装配多个网卡，每个网卡都可以配置一个 IP 地址。因此，稳妥起见，需要在遍历 `getaddrinfo` 所生成的地址列表的过程中访问数字化的网络地址，以免有所疏漏：

```c
for (struct addrinfo *it = res; it; it = it->ai_next) {
        struct sockaddr_in *my_addr = (void *)it->ai_addr;
        printf("%u:%hu\n", my_addr->sin_addr.s_addr, my_addr->sin_port);
}
```

现在，对上述的 foo.c 略作修改，除了增加网络地址列表的遍历访问过程，将 `getaddrinfo` 所需的文字形式 IP 地址和端口由硬性设定改为命令行参数传入：

```c
/* 源文件 foo.c */
#include <stdio.h>        /* printf 需要它 */
#include <string.h>       /* memset 需要它 */
#include <stdlib.h>       /* exit 需要它 */
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

`localhost` 和 `127.0.0.1` 的数字转换结果相同，都是 `16777343`。`localhost` 表示本地计算机，即运行 foo 程序的这台计算机，而 `127.0.0.1` 表示网络回环地址，任何一台计算机都有这个地址，即使它没有安装网卡。可以将 `localhost` 和 `127.0.0.1` 理解成「自我」。一个人，可以不与整个社会发生任何关系，但是只要他还活着，必定会有「自我」。

可以试着用 foo 程序访问域名 www.baidu.com 的 IP 地址：

```console
$ ./foo www.baidu.com 80
776379431:20480
4014382119:20480
```

域名 www.baidu.com 指向的计算机，它拥有两个 IP 地址。事实上，许多知名的网页服务器（端口默认为 80），都有多个 IP。这便是 `getaddrinfo` 生成的是一个地址列表，而不是单个地址的原因。也许你会觉得有些不可思议，`getaddrinfo` 是得知域名为 www.baidu.com 的计算机的 IP 的呢？原因是，`getaddrinfo` 会调用操作系统提供的域名查询服务，在 DNS 服务器上查找 www.baidu.com 的 IP 集。

用主机名或域名指代一台网络上的计算机，是因为文字形式的 IP 地址并不便于人类记忆。普通人，勉强能记住几个 IP v4 地址，但是要记住一个 IP v6 可能就很难了。主机名和域名的主要区别是，前者无人管理，只能在局域网中使用，而后者会有网络上专门的计算机系统予以管理并提供查询服务，故而可用于指代某台远程的计算机。

上述网络基础知识，现在可以知道，也可以不知道，`getaddrinfo` 已经将所有细节封装了。总之，我们已经可以轻易做到将文字形式的网络地址转化为数字形式了，接下来便是如何利用后者，将信息发送出去。现在，倘若你使用的是 Linux 系统，不妨使用以下命令，再回顾一下 `getaddrinfo` 的一切：

```console
$ man 3 getaddrinfo
```
