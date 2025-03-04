---
title: 我是三体人
abstract: 却并非叶文洁想要找的人。
date: 2025 年 03 月 03 日
...

# 前言

在「[有一封信，无处可寄](simple-client/index.html)」中，程序 ywj 向 www.threebody.com:8080 发送信息，该过程因目标计算机不存在而以失败告终。

不存在的，可以创建，然后欺骗 ywj，让她以为它存在。若是在 Linux 系统中，可使用超级用户权限编辑 /etc/hosts 文件，为其添加以下内容

```plain
127.0.0.1       www.threebody.com
```

也许你已经识破我的诡计了，我是给这个域名与本机网络回环地址绑定，我只需要写一个程序，接收 ywj 发来的信息并予以回复，便可完成欺骗。

这一瞒天过海之计能否奏效，我心里也没底，试试看！

# 我的地址

ywj 给我发送信息，她需要知道我的域名或 IP 地址。为了能收到她发来的信息，我需要为自己构造一个网络地址：

```c
struct addrinfo hints, *addr_list;
memset(&hints, 0, sizeof(struct addrinfo));
hints.ai_family = AF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;
int a = getaddrinfo("127.0.0.1", "8080", &hints, &addr_list);
if (a != 0) {
        fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
        exit(-1);
}
for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
        /* ... 想必你已很熟悉地址列表遍历过程了 ... */
}
```

与 ywj 构造收信人地址所用代码唯一的区别是，`getaddrinfo` 的第一个参数，在这里是 `127.0.0.1`。我只需要知道其他计算机上的某个进程试图访问「我所在的计算机」的 `8080` 端口。

何谓「我所在的计算机」？答案是 `localhost` 或 `127.0.0.1`，即网络回环地址。一定要清楚，ywj 构造的收信人地址与我在这里构造的地址是同一个地址，只是在 ywj 看来，它是 `www.threebody.com:8080`，而在我看来，它是 `127.0.0.1:8080`。

我说的如此细致，也许你还是不明白。不妨想象一下，假设我楼下有个信箱，你要往这个信箱里寄信，那么你需要知道这个信箱在中国地图上的位置，而我呢？我只需要知道，它就在我楼下。

# 信封

我是三体人，若有外太空有消息发送给我，为了让我的同胞们永远不再受乱纪元脱水之苦，我必须有所回复，所以我也必须有一个信封，而且一开始它也是空的。对地址列表的遍历过程略作修改，便可创建一个可用的空信封：

```c
int envelop = -1;
for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
        envelop = socket(it->ai_family,  it->ai_socktype, it->ai_protocol);
        if (envelop == -1) continue;
        break;
}
```

得到可用的新信封后，我需要先在它上面写上我的地址——我楼下的那个信箱。这个地址有些随意，也只有我和我的计算机操作系统知道它的含义，故而将这个地址写在信封上，需要一种特别方式。ywj 用的是 `connect` 函数，该函数能一举写上通信双方的地址，而我用的是 `bind` 函数，只能先写上我的地址，原因是此时我还不知道谁写了一封信寄给了我。

`bind` 函数的形式如下：

```c
#include <sys/socket.h>

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

现在，调用它的条件已经都满足了，即

```c
int envelop = -1;
for (struct *addrinfo it = addr_list; it; it = it->ai_next) {
        envelop = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
        if (envelop == -1) continue;
        int a = bind(envelop, res->ai_addr, res->ai_addrlen);
        if (a == -1) {
                close(envelop);
                continue;
        }
        break;
}
freeaddrinfo(addr_list); /* 地址列表完成使命 */
if (envelop == -1) {
        fprintf(stderr, "failed to bind!\n");
        exit(-1);
}
```

同之前遇到过的那些 socket API 函数类似，`bind` 运行成功，返回 0，否则返回 -1。不过，即使 `bind` 运行成功，所得的 `envelop` 依然因缺少给我写信的那个人的地址而无法使用。

# 纷至沓来

只有收到信的时候，我才能知道给我写信的人的地址。不过，我担心的并非是无人给我写信，而是若有无数人给我写信，我该当如何是从？当然是逐一处理，或逐批处理。

不过，现在已经很难用简单地用现实中书信通信的方式进行比喻了。ywj 用的信封，跟我用的信封实际上是不同的。因为 ywj 只需要考虑和我一个人的通信，而我需要考虑与可能无数个人通信的问题，而在 socket API 网络编程中，默认的信封形式是 ywj 那种。

与多人通信用的信封，需要用 `listen` 函数予以标定，告诉操作系统该信封需待我收到他人来信信之后方能完全地构造出来。`listen` 函数的声明如下：

```c
#include <sys/socket.h>

int listen(int sockfd, int backlog);
```

若理解 `listen` 函数的作用，需要假设你是一个大人物。你可能每天都能收到大量信件，而你根本没时间操心收信这样的琐事。假设你专门成立了一个部门负责接收信件，并且这个部门可能只有 1 个人处理这一事务。在每一天里，他接收的信件必定有一个数量限制，这个数量限制即 `backlog`。他将接收的信件整齐地叠放成一摞，这摞信件最多有 `backlog` 封，而且先收到的信总是放在后收到的信的上面，不妨将这摞信件称为 A 摞。你要读信，每次只需拿 A 摞最上面的那封。同时，他每次将最新接到的信塞到 A 摞的底部，一旦 A 摞拥有信件的数量达到 `backlog`，并且此时，还有新的信件陆续到来，他便另起一摞——姑且成为 B 摞，同 A 摞，B 摞也总是将先到的信件放在上面，后到的信件放在下面。当他一旦发现 A 摞信件的数量减少，便从 B 摞的上面取走一些，塞到 A 摞下面，补充 A 摞所缺。同样，B 摞也有一个最大信件数量限制，只是我们不太清楚，但负责收信的这个人知道，在 B 摞的信件数量超出限制，他便拒绝再接收新的信件了。这个收信人，并非 `listen` 函数，后者只是用于通知他 A 摞的信件数量限制，他实际上是操作系统中的某个底层网络子系统，在 socket API 层面上，他是一个无名英雄。呜呼……总算将 `listen` 的工作讲得有些清楚了。

与`listen` 函数的内涵相比，它的调用则非常简单：

```c
int b = listen(envelop, 10); /* 排队序列最大长度为 10 */
```

若 `listen` 成功运行，上述它的返回值 `b` 为 0，否则为 -1。

# 信封的对称性

经过 `listen` 标记的信封，需要将它传递于 `accept` 函数，由后者根据该范本（有我的地址）以及来信队列中第一个可获得的信件（有与我通信的人的地址）构造一个真正可用的信封——它含有通信双方的地址。

`accept` 函数的声明如下：

```c
#include <sys/socket.h>

int accept(int sockfd, struct sockaddr *_Nullable restrict addr,
           socklen_t *_Nullable restrict addrlen);
```

它所需要的所有参数，我现在都有：

```c
int real_envelop = accept(envelop, NULL, NULL);
```

`accept` 的返回值是一个真正可用的信封，通过它，我可以接收来自 ywj 的信息，也能通过它向 ywj 发送信息。若 `accept` 运行失败，会返回 -1。注意，`accept` 的第 2 个和第 3 个参数，用于存储 `accept` 从来信排队序列中获得的来信地址以及该地址的长度——根据地址长度可判断该地址是 IP v4 还是 IP v6 地址），倘若不关心它，可将这两个参数皆设为 `NULL`。

基于同一个信封，可以收信，也可以发信，在现实中是不可能的，但在计算机网络系统中却可以，亦即 socket 具有对称性，同一个 socket 即可以用于接收信息，也可以用于发送信息，关键在于这个 socket 含有通信双方的地址，操作系统会根据信息是发送还是接收这两种不同情况使用这对地址，至于它是如何使用的，在 socket API 编程的层面上可不必关心。

# 信是谁写的？

上一节未深究 `accept` 的第 2 个和第 3 个参数的用途，倘若我想弄清楚是谁给我写了这封信，那么就需要在这两个参数上做文章。

`accetp` 函数的第 2 个参数 `addr`，其类型为 `struct sockaddr *`，是网络地址指针。在「[网络地址](getaddrinfo/index.html)」中的「[迷雾重重](getaddrinfo/index.html#迷雾重重)」一节里，介绍了 `sockaddr` 结构体及其「派生」类型 `sockaddr_in`（IP v4 地址）和 `sockaddr_in6`（IP v6 地址），特别是可使用 `sockaddr` 类型的指针指向后两者的对象，但是在类型兼容方面，`sockaddr` 无法兼容 `sockaddr_in` 和 `sockaddr_in6`。

为了让 `accept` 函数能够不关心 `sockaddr_in` 和 `sockaddr_in6` 的区别，需要使用 `sockaddr_storage` 类型作为 `accetp` 第 2 个参数通用的类型，例如

```c
struct sockaddr_storage addr;
socklen_t addr_len;
int real_envelop = accept(envelop, (struct sockaddr *)&addr, &addr_len);
```

若只是想查看给我发送信息的网络地址的文字形式，可以直接将 `accept` 获取的网络地址传递给 `getinfoname`，由后者转化成文字形式的 IP 地址和端口：

```c
char host[NI_MAXHOST], port[NI_MAXSERV];
getnameinfo((struct sockaddr *)&addr, addrlen,
            host, sizeof(host),
            port, sizeof(port),
            NI_NUMERICHOST | NI_NUMERICSERV);
```

`host` 存储来信者的 IP 地址，`port` 存储他所用的端口。

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

信封用完后，需要关闭。从现在开始，我们无需再使用信封作为 socket 的喻体了。socket 是一个能够用于接受信息，也能用于发送信息的文件。既然是文件，不再用它的时候，需使用 `close` 函数予以关闭：

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
#include <unistd.h>
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
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                envelop = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (envelop == -1) continue;
                int a = bind(envelop, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(envelop);
                        continue;
                }
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
        struct sockaddr_storage addr;
        socklen_t addr_len;
        int real_envelop = accept(envelop, (struct sockaddr *)&addr, &addr_len);
        if (real_envelop == -1) {
                fprintf(stderr, "failed to accept!\n");
                exit(-1);
        }
        /* 获取来信者的 IP 地址和端口 */
        char host[NI_MAXHOST], port[NI_MAXSERV];
        getnameinfo((struct sockaddr *)&addr, addr_len,
                    host, sizeof(host),
                    port, sizeof(port),
                    NI_NUMERICHOST | NI_NUMERICSERV);
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
                printf("Received %zd bytes: \n%s from %s:%s\n",
                       n, buffer, host, port);
        }
        /* 发送信息 */
        char *other_buffer = "Hi, I am a threebody human!";
        ssize_t m = send(real_envelop, other_buffer, strlen(other_buffer), 0);
        /* 关闭 socket */
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
Received 10 bytes: 
I am here! from 127.0.0.1:38066
```

然后退出。threebody 打印的信息，正是 ywj 进程发送的，但是 threebody 发送给 ywj 的信息，后者收不到，因为后者尚未实现接收回信的功能。

# 总结

socket 是一种对称的信封，它可以用于发送信息，也能用于接收信息。
