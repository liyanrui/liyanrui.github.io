---
title: 我是三体人
abstract: 却并非叶文洁想要找的人。
date: 2025 年 03 月 03 日
...

# 前言

在「[我在这里！](../simple-client/index.html)」中，程序 ywj 向 www.threebody.com:8080 发送信息，该过程因目标计算机不存在而以失败告终。

不存在的，可以创建，然后欺骗 ywj，让她以为它存在。若是在 Linux 系统中，可使用超级用户权限编辑 /etc/hosts 文件，为其添加以下内容

```plain
127.0.0.1       www.threebody.com
```

也许你已识破我的诡计了，我是给这个域名与本机网络回环地址绑定，我只需要写一个程序，接收 ywj 发来的信息并予以回复，便可完成欺骗。这一瞒天过海之计能否奏效，我心里也没底，试试看。

现在，我是计算机里的三体人了。

# 我的套接字地址

ywj 给我发送信息，她需要知道我的套接字地址——文字形式的，即我所在的计算机的域名（`www.threebody.com`）和端口（`8080`）。这个套接字地址，对我而言，便是 `127.0.0.1:8080`，我也需将其转化为数字形式：

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

# 我的套接字

我也需先创建一个空的套接字：

```c
int server = -1;
for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
        server = socket(it->ai_family,  it->ai_socktype, it->ai_protocol);
        if (server == -1) continue;
        break;
}
```

既然 ywj 认为我是 `server`，那我就用它命名我的套接字，但是为这个空的套接字赋予地址时，我不能像 ywj 那样使用 `connect`，因为我无需主动连接任何人。我需要使用 `bind` 为空套接字赋予地址：

`bind` 函数的形式如下：

```c
#include <sys/socket.h>

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
```

调用它的条件实际上已经都满足了，即

```c
int server = -1;
for (struct *addrinfo it = addr_list; it; it = it->ai_next) {
        server = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
        if (server == -1) continue;
        int a = bind(server, res->ai_addr, res->ai_addrlen);
        if (a == -1) {
                close(server);
                continue;
        }
        break;
}
freeaddrinfo(addr_list); /* 地址列表完成使命 */
if (server == -1) {
        fprintf(stderr, "failed to bind!\n");
        exit(-1);
}
```

同之前遇到过的那些套接字 API 函数类似，`bind` 运行成功，返回 0，否则返回 -1。

# 监听状态

我要想接收到来自网络上其他套接字的数据，前提是，我的套接字需要处于监听状态。`listen` 函数可为已赋予地址的套接字设置监听态，其形式如下：

```c
#include <sys/socket.h>

int listen(int sockfd, int backlog);
```

若理解 `listen` 函数的作用，需要假设你是一个大人物。你可能每天都能收到大量信件，而你根本没时间操心收信这样的琐事。假设你专门成立了一个部门负责接收信件，并且这个部门可能只有 1 个人处理这一事务，我们可称其为收信员。在每一天里，他接收的信件必定有一个数量限制，这个数量限制即 `backlog`。

收信员将接收的信件整齐地叠放成一摞，这摞信件在数量上最多有 `backlog`，先收到的信总是放在后收到的信的上面，不妨将这摞信件称为 A 摞。你要读信，每次只需拿 A 摞最上面的那封。同时，若有新的信件到来，收信员会将其塞入 A 摞最底部。

当 A 摞信件数量达到 `backlog` 时，若还有新的信件陆续到来，收信员就会另起一摞，姑且称之为 B 摞。同 A 摞，B 摞也总是先到的信件在上面，后到的信件在下面。当收信员一旦发现 A 摞信件的数量减少了，即不满 `backlog` 时，便从 B 摞的上面取走一些，塞到 A 摞下面，以补 A 摞所缺。同样，B 摞也有一个最大信件数量限制，只是我们不太清楚，但收信员知道。当 B 摞的信件数量超出限制时，他便拒绝接收新的信件了。

这个收信员，并非 `listen` 函数，后者只是用于通知他 A 摞的信件数量限制，他实际上是操作系统中的某个底层网络子系统，他是套接字 API 层面幕后的无名英雄。

与 `listen` 函数的内涵相比，其调用则非常简单：

```c
int b = listen(server, 10); /* 设上述 A 摞新建最大数量为 10 */
```

若 `listen` 成功运行，上述它的返回值 `b` 为 0，否则为 -1。

# 客户端套接字

经过 `listen` 函数标记的套接字，需要将它传递于 `accept` 函数，由后者构造一个可用的通信连接。还记得连接的概念吗？一对套接字。

`accept` 函数的声明如下：

```c
#include <sys/socket.h>

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
```

它所需要的所有参数，我现在都有：

```c
int client = accept(server, NULL, NULL);
```

若 `accpet` 成功运行，它的返回值是操作系统悄悄地为我构造的一个套接字，它表示客户端。当 ywj 向我请求连接，那么当我接收了她的请求，操作系统便为我构造一个客户端套接字，通过这个套接字，我可以接收 ywj 发来的数据，也可以给她一些回复。若 `accept` 运行失败，会返回 -1。

注意，`accept` 的第 2 个和第 3 个参数，用于存储 `accept` 从来信排队序列中获得的来信地址以及该地址的长度——根据地址长度可判断该地址是 IP v4 还是 IP v6 地址），倘若不关心它，可将这两个参数皆设为 `NULL`。

ywj 持有两个套接字，一个表示服务端，一个表示她自己。现在，我也有两个套接字，一个表示我自己，另一个表示客户端。当 ywj 的服务端是我，而我的客户端是 ywj，那么便意味我与她建立了连接。我们双方各自持有的两个套接字，本质上都是我们机器上的两份文件。ywj 向她的服务端套接字指代的文件写数据，网络系统会将这个数据复制到我的客户端套接字指代的文件里。

网络系统是什么？它是由不计其数的的计算机在遵守某种信息传输协议的前提下以有线或无线的方式连接起来的系统，客户端和服务端不过是这个系统的末端而已。在数字通信技术诞生之前，我们使用的网络系统就是邮政局，由不计其数的邮政工作人员和邮递员在遵守某种商业契约的前提下构成的系统。

# 谁寄来的信？

上一节未深究 `accept` 的第 2 个和第 3 个参数的用途。倘若我想弄清楚我收到的信件从何处寄来，亦即客户端的套接字地址，就需要在这两个参数上做文章。

`accetp` 函数的第 2 个参数 `addr`，其类型为 `struct sockaddr *`，是网络地址指针。在「[套接字地址](../getaddrinfo/index.html)」中的「[迷雾重重](../getaddrinfo/index.html#迷雾重重)」一节里，介绍了 `sockaddr` 结构体以及 `sockaddr_in`（IP v4 地址）和 `sockaddr_in6`（IP v6 地址）结构体，特别是可使用 `sockaddr` 类型的指针指向后两者的对象，但是在类型兼容方面，`sockaddr` 无法兼容 `sockaddr_in` 和 `sockaddr_in6`，原因是 `sockaddr` 指针是在 C 语言还没有 `void` 指针的时代发明出来的替代品。`void` 指针能够指向任何类型的实例（对象），但是却无法用 `void` 类型容纳任何数据。

结构体 `sockaddr_storage` 类型，不仅其指针形式可以指向 `sockaddr_in` 或 `sockaddr_in6` 的对象，其数据结构也能兼容它们，故而为了让 `accept` 函数能够不关心 `sockaddr_in` 和 `sockaddr_in6` 的区别，可使用 `sockaddr_storage` 类型的指针作为 `accetp` 第 2 个参数的类型。例如

```c
struct sockaddr_storage addr;
socklen_t addr_len = sizeof(addr);  /* 务必将其初始化为 addr 的大小 */
int client = accept(server, (struct sockaddr *)&addr, &addr_len);
```

注意，`accept` 的第 3 个参数 `addr_len` 必须初始化为第 2 个参数所指向的结构体对象占用的内存大小（字节数）。当 `accept` 返回后， 第 2 个参数所指向的结构体对象实际占用的内存大小会保存在第 3 个参数中。

若只是想查看客户端套接字地址的文字形式，可以直接将 `accept` 获取的网络地址传递给 `getinfoname`，由后者转化成文字形式的套接字地址：

```c
char host[NI_MAXHOST], port[NI_MAXSERV];
getnameinfo((struct sockaddr *)&addr, addrlen,
            host, sizeof(host),
            port, sizeof(port),
            NI_NUMERICHOST | NI_NUMERICSERV);
```

`host` 存储客户端的 IP 地址，`port` 存储他使用的端口。

# 读信和回信

使用 `recv` 函数可以从上一节构造的套接字 `client` 中读取 ywj 给我发来的数据：

```c
const size_t buffer_size = 1024;
char buffer[buffer_size];
ssize_t n = recv(client, buffer, buffer_size - 1, 0);
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

无需关心 `recv` 的声明，它的用法与 ywj 使用的 `send` 相似，只需在内存中准备一个确定长度的空间，用于存放受到的信息。若 `recv` 的返回值非 -1 且非 0，则意味着已从 `client` 中获得了 `n` 个字节的信息，但是若将 `buffer` 中的内容作为字符串打印出来，需要在其末尾增加字符串结束符 `\0`，否则 `printf` 不知道字符串的截止位置，从而造成内存越界访问。

若我也需要向客户端 ywj 发送信息，只需使用 `send` 函数向 `client` 写信息即可，例如

```c
char *other_buffer = "Hi, I am a threebody human!";
ssize_t m = send(client, other_buffer, strlen(other_buffer), 0);
```

套接字用完后，需要使用 `close` 函数予以关闭：

```c
close(client);
close(server);
```

原因是，在 Unix 或 Linux 系统中，套接字是文件描述符，而文件描述符是系统资源。凡是使用了系统资源，都需要归还，如同使用 `malloc` 分配一块内存，用完后，需要使用 `free` 归还给系统。

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
        /* 从 server 到 client 构造可用的连接 */
        int server = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                server = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (server == -1) continue;
                int a = bind(server, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(server);
                        continue;
                }
                break;
        }
        freeaddrinfo(addr_list);
        if (server == -1) {
                fprintf(stderr, "failed to bind!\n");
                exit(-1);
        }
        if (listen(server, 10) == -1) {
                fprintf(stderr, "failed to listen!\n");
                exit(-1);
        }
        struct sockaddr_storage addr;
        socklen_t addr_len = sizeof(addr);
        int client = accept(server, (struct sockaddr *)&addr, &addr_len);
        if (client == -1) {
                fprintf(stderr, "failed to accept!\n");
                exit(-1);
        }
        /* 获取客户端套接字地址 */
        char host[NI_MAXHOST], port[NI_MAXSERV];
        getnameinfo((struct sockaddr *)&addr, addr_len,
                    host, sizeof(host),
                    port, sizeof(port),
                    NI_NUMERICHOST | NI_NUMERICSERV);
        /* 接收信息 */
        const size_t buffer_size = 1024;
        char buffer[buffer_size];
        ssize_t n = recv(client, buffer, buffer_size - 1, 0);
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
        ssize_t m = send(client, other_buffer, strlen(other_buffer), 0);
        /* 关闭套接字 */
        close(client);
        close(server);
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

在 Unix 或 Linux 系统中，套接字实际上是文件。基于它实现的网络通信，本质是文件的读写操作。ywj 和 threebody 都是在各自的计算机中创建了两份文件，其中一份用于建立连接，另一份用于收发数据。

ywj 使用 `connect` 为服务端套接字赋予地址的过程中，操作系统暗自为她构造的套接字，其用途是向 threebody 发送连接请求，它与 threebody 构造的用于监听的套接字遥相呼应。要实现网络通信，ywj 只需读写她构造的服务端套接字，同时 threebody 也只需读写它使用 `accept` 构造的客户端套接字。

正在运行的网络系统能够帮助 ywj 和 threebody 将这些套接字连接起来，从而将网络通信变成了简单的的文件读写，这就是网络编程的本质。此刻，若你理解上述所说的，你实际上已经学会了它。
