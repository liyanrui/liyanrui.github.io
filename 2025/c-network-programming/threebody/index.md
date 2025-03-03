---
title: 我是三体人
abstract: 但是我却不是叶文洁想要找的人。
date: 2025 年 03 月 03 日
...

# 前言

在「[有一封信，无处可寄](simple-client/index.html)」中，由程序 ywj 发往 www.threebody.com 的信息以该域名对应的计算机不存在而失败。

不存在的，可以创建，然后欺骗 ywj。若是在 Linux 系统中，可使用超级用户权限编辑 /etc/hosts 文件，为其添加以下内容

```plain
127.0.0.1       www.threebody.com
```

也许你已经识破我的诡计了，我是给这个域名与本机网络回环地址绑定，我只需要写一个程序，接收 ywj 发来的信息并予以回复，便可完成欺骗。

这一瞒天过海之计能否奏效，我心里也没底，试试看！

# 我的地址

ywj 给我发送信息，她需要知道我的域名或 IP 地址。为了能收到她发来的信息，我需要为自己构造一个网络地址：

```c
struct addrinfo hints, *res;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;
int a = getaddrinfo("127.0.0.1", "8080", &hints, &res);
if (a != 0) {
        fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
        exit(-1);
}
```

与 ywj 构造收信人地址所用代码唯一的区别是，`getaddrinfo` 的第一个参数，在这里是 `127.0.0.1`。我只需要知道其他计算机上的某个进程试图访问「我所在的计算机」的 `8080` 端口。何谓「我所在的计算机」？答案是 `localhost` 或 `127.0.0.1`，即网络回环地址。

一定要清楚，ywj 构造的收信人地址与我在这里构造的地址是同一个地址，只是在 ywj 看来，它是 `www.threebody.com:8080`，而在我看来，它是 `127.0.0.1:8080`。我说的如此细致，也许你还是不明白。不妨想象一下，假设我楼下有个信箱，你要往这个信箱里寄信，那么你需要知道这个信箱在中国地图上的位置，而我呢？我只需要知道，它就在我楼下。

# 信封

我是三体人，若有外太空有消息发送给我，为了让我的同胞们永远不再受乱纪元脱水之苦，我必须有所回复，所以我也必须有一个信封，而且一开始它也是空的：

```c
int envelop = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
```

现在，我需要先在信封上写上我的地址，我楼下的那个信箱……由于这个地址如此随意，以致将这个地址写在信封上，也需要一种特别的方式，ywj 用的是 `connect` 函数——能一举写上通信双方的地址，而我用的是 `bind` 函数，只能先写上我的地址，原因是，此时我还不知道谁写了一封信寄给了我。

`bind` 函数的形式如下：

```c
#include <sys/socket.h>

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

现在，调用它的条件已经都满足了，即

```c
int a = bind(envelop, res->ai_addr, res->ai_addrlen);
```

同之前遇到过的那些 socket API 函数类似，`bind` 运行成功，返回 0，否则返回 -1。

若 `bind` 运行成功，现在的 `envelop` 还缺少给我写信的那个人的地址，它依然无法使用。

# 纷至沓来

只有在我收到信的时候，我才能知道给我写信的人的地址。不过，我担心的并非无人给我写信，而是万一有无数人给我写信，我该怎么办？当然是一封一封处理，或一批一批处理。

不过，现在已经很难用简单地用现实中书信通信的方式进行比喻了。ywj 用的信封，跟我用的信封实际上是不同的，因为 ywj 只需要考虑和我一个人的通信，而我需要考虑与可能无数个人通信的问题。换言之，ywj 的信封是「主动」的——她知道我的地址和她自己的地址，我的信封则是「被动」的——我只知道我的地址，与我通信的人的地址我需要先收到他的来信之后才知道，但是在 socket API 网络编程中，默认的信封形式是 ywj 的这种主动形式的信封。

被动形式的信封，需要用 `listen` 函数予以标记，以告诉操作系统这个信封需要等待我收到信之后才能构造出来。`listen` 函数的声明如下：

```c
#include <sys/socket.h>

int listen(int sockfd, int backlog);
```

它的作用是将一个还不完整的信封标记为被动形式，并声明该信封一次性最多能支持多少封来信。`listen` 函数实际上是在驱动操作系统为被动形式的信封构造两个序列，一个用于存储已经进入排队状态的来信，另一个用于存储暂时尚未获得排队资格的来信，前者最大的长度是 `backlog`。另外，`listen` 也会驱动操作系统监听 `sockfd` 所绑定的端口，将到来的信件塞入排队序列。若排队序列已满，则将未能获得排队资格的信件加入待排队序列，等到排队序列出现空位时，再将其填入。若待排队序列也满了，后续发来的信件会被丢弃。

想必你已经对上述这枯燥乏味且语焉难详的讲解表示抗拒了。我们可以继续比喻。之前我们将寄信和收信的过程想得太多于简单了。现在，需要假设你是一个大人物或者明星，可能每天都会收到大量的信件，假设你专门成立了一个部门负责接收这些信件——你肯定没时间处理这些琐事。这个部门可能只有 1 个人在处理这一事务，这个人叫 `listen`。在每一天里，`listen` 能接收的信件必定有一个数量限制，这个数量限制即 `backlog`，他接收的信件则是进入了排队序列，等待你接下来的处理，他未接收的信件则进入待排队序列。若待排队序列也满了，例如 `listen` 所在的房间已经无容身之地了，他会将后续送来的信件扔在门外不予理睬。呜呼……总算将 `listen` 的工作讲得有些清楚了。

与`listen` 函数的内涵相比，它的调用则非常简单：

```c
int b = listen(envelop, 10); /* 排队序列最大长度为 10 */
```

若 `listen` 成功运行，上述它的返回值 `b` 为 0，否则为 -1。

# 用于回信的信封

经过 `listen` 标记的信封，它实际上只是一个范本，需要将它传递于 `accept` 函数，由后者根据该范本（有我的地址）以及来信队列中第一个可获得的信件（有与我通信的人的地址）构造一个真正可用的信封——它含有通信双方的地址。

`accetp` 函数的声明如下：

```c
#include <sys/socket.h>

int accept(int sockfd, struct sockaddr *_Nullable restrict addr,
           socklen_t *_Nullable restrict addrlen);
```

它所需要的所有参数，我都有：

```c
int real_envelop = accept(envelop, NULL, NULL);
```

`accept` 的返回值是一个真正可用的信封，通过它，我可以接收来自 ywj 的信息，也能通过它向 ywj 发送信息。若 `accept` 运行失败，会返回 -1。注意，`accept` 的第 2 个和第 3 个参数，用于存储 `accept` 从来信排队序列中获得的来信地址以及该地址的长度——根据地址长度可判断该地址是 IP v4 还是 IP v6 地址），不过这两个参数可以皆为 `NULL`，表示我并不需要记录给我写信的人的地址——以后可能会需要，届时再细究。

基于同一个信封，可以收信，也可以发信，在现实中是不可能的，但在计算机网络系统中却可以，亦即 socket 具有对称性，同一个 socket 即可以用于接收信息，也可以用于发送信息，关键在于这个 socket 含有通信双方的地址，操作系统会根据信息是发送还是接收这两种不同情况使用这对地址，至于它是如何使用的，在 socket API 编程的层面上可不必关心。

# 读信和回信

使用 `recv` 函数可以从上一节构造的信封 `real_envelop` 中读取 ywj 给我发来的信息：

```c
const size_t buffer_size = 1024;
char buffer[buffer_size];
ssize_t n = recv(real_envelop, buffer, buffer_size - 1, 0);
if (n == -1) {
        fprintf(stderr, "recv error!\n");
        exit(-1);
} else if (n == 0) {
        printf("Connection is closed!\n");
} else {
        buffer[n] = '\0';
        printf("Received %zd bytes: \n%s\n", n, buffer);
}
```

无需关心 `recv` 的声明，它的用法与 ywj 使用的 `send` 相似，只需在内存中准备一个确定长度的空间，用于存放受到的信息。若 `recv` 的返回值非 -1 且非 0，则意味着已从 `real_envelop` 中获得了 `n` 个字节的信息，但是若将 `buffer` 中的内容作为字符串打印出来，需要在其末尾增加字符串结束符 `\0`，否则 `printf` 不知道字符串的截止位置，从而造成内存越界访问。

若我也需要向 ywj 这样的来信着发送信息，只需使用 `send` 函数向 `real_envelop` 写信息即可，例如

```c
char *other_buffer = "Hi, I am a threebody human!";
ssize_t m = send(real_envelop, other_buffer, strlen(other_buffer), 0);
```

信封用完后，需要关闭……不过，从现在开始，我们无需再使用信封作为 socket 的喻体了。socket 是一个能接受信息，也能发送信息的文件。既然是文件，不再用它的时候，需要使用 `close` 函数予以关闭：

```c
close(real_envelop);
close(envelop);
```

# 完整的程序

将上文所述的各个步骤融合为一个可以与 ywj 作一次应答的程序：

```c
/* threebody.c */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>  /* close 函数需要它 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int main(void) {
        struct addrinfo hints, *addr_list;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo("localhost", "8080", &hints, &addr_list);
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        /* 从 envelop 到 real_envelop 构造可用的信封 */
        int envelop = -1;
        char host[NI_MAXHOST], port[NI_MAXSERV];
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                envelop = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (envelop == -1) continue;
                int a = bind(envelop, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(envelop);
                        continue;
                }
                getnameinfo(it->ai_addr, it->ai_addrlen,
                            host, sizeof(host),
                            port, sizeof(port),
                            NI_NUMERICHOST | NI_NUMERICSERV);
                printf("%s:%s\n", host, port);
                break;
        }
        freeaddrinfo(addr_list);
        if (envelop == -1) {
                fprintf(stderr, "failed to bind!\n");
                exit(-1);
        }
        if (listen(envelop, 10) == -1) {
                fprintf(stderr, "failed to listen!\n");
                exit(-1);
        }
        int real_envelop = accept(envelop, NULL, NULL);
        if (real_envelop == -1) {
                fprintf(stderr, "failed to accept!\n");
                exit(-1);
        }
        /* 接收信息 */
        const size_t buffer_size = 1024;
        char buffer[buffer_size];
        ssize_t n = recv(real_envelop, buffer, buffer_size - 1, 0);
        if (n == -1) {
                fprintf(stderr, "recv error!\n");
                exit(-1);
        } else if (n == 0) {
                printf("Connection is closed!\n");
        } else {
                buffer[n] = '\0';
                printf("Received %zd bytes: \n%s\n", n, buffer);
        }
        /* 发送信息 */
        char *other_buffer = "Hi, I am a threebody human!";
        ssize_t m = send(real_envelop, other_buffer, strlen(other_buffer), 0);
        /* 善后工作 */
        close(real_envelop);
        close(envelop);
        return 0;
}
```

编译上述源码并执行：

```console
$ gcc threebody.c -o threebody
$ ./threebody
```

然后在 threebody 进程所在的同一台计算机上运行 ywj 程序，threebody 进程会打印以下内容

```console
I am here!
```

然后退出。threebody 打印的信息，正是 ywj 进程发送的，但是 threebody 发送给 ywj 的信息，后者收不到，因为后者尚未实现接收回信的功能。

# 总结

socket 是一种对称的信封，它可以用于发送信息，也能用于接收信息。
