---
title: 长缨在手
abstract: 何日缚住苍龙？
date: 04 月 07 日
...

# 前言

我们已经初步掌握了 `select` 函数的用法，基于它能够监控多个文件，从中选出所有可读、可写以及出现异常的文件，此即同步 I/O 多路复用。这个术语中的「同步」，表现在 `select` 是阻塞的。`select` 将一组文件提交给操作系统后，需要等待操作系统的「审批」，原因是只有操作系统能够最快知道文件是可读、可写还是存在异常，因为所有文件都是它负责管理。至于「多路复用」，「多路」指的是 `select` 选出的一组文件，每个文件都是一条 I/O 通道，它们在「复用」当前这个进程对它们的调度，以完成数据传输。

在网络编程中，套接字也是文件，亦即它也是 I/O 通道。在编写服务端程序时，我们可以将一组套接字通过 `select` 提交给操作系统审批，从中选出可读、可写或有异常者，由它们复用服务端进程。这种机制能提高服务端进程的并发能力，亦即能提高服务端进程面对多个客户端同时发起连接这类情况的处理能力吗？

之前，我们实现的 threebody 程序，它只能逐一接受客户端的连接，并与之通信。它无法知道当前接受了一个客户端的连接之后，还有没有其他客户端在排队等待。同步 I/O 多路复用机制，能够让 threebody 知晓当前有多少客户端与它建立了连接，并知晓这些客户端有哪些发来了数据，以及有哪些在等待接收数据。这就是计算机世界里的广义相对论，在一个假想的时空里，有的客户端的时间很慢，有的客户端的时间很快，但是服务端与所有接入的客户端通信消耗的总时间，可能远小于每个客户端与服务端通信时长的总和。

时间，是一种设备。之所以发明它，是为了让所有事情不同时发生。

为了清晰地展现基于同步 I/O 多路复用机制的服务端并发能力的改进策略，我们暂不使用或改造「[封装](../wrapper/index.html)」中的 `SimServer` 类，而是继续沿袭「[我是三体人](../threebody/index.html)」中琐碎但直白的套接字函数以及普通的 C 字符串操作。待彻底熟悉这一改进策略后，再令其与 `SimServer` 类发生化学反应。

# 客户端集

为了更快抓住问题的本质，我们需要忽略一些细节，诸如将文字形式的套接字地址转化为数字，用 `socket` 和 `bind` 函数构造用于监听客户端连接的套接字等内容。现在，假设已存在非阻塞的监听套接字 `listener`，在 `select` 的帮助下，与客户端建立连接的过程可实现为以下代码：

```c
/* 最多可同时与 100 个客户端建立连接 */
#define MAX_CLIENTS 100
/* 用于存储与服务端连接的客户端 */
int clients[MAX_CLIENTS] = {0}; /* 数组所有元素初始化为 0 */

fd_set read_fds;
fd_set write_fds;
int fd_max = listener;

while (1) {
        FD_ZERO(&read_fds);
        FD_ZERO(&write_fds);
        
        /* 将 listener 和 clients 中的文件加入监视集 */
        FD_SET(listener, &read_fds);
        for (int i = 0; i < MAX_CLIENTS; i++) {
                int client = clients[i];
                if (client > 0) {
                        FD_SET(client, &read_fds);
                        FD_SET(client, &write_fds);
                        /* 更新文件描述符最大值 */
                        if (client > fd_max) fd_max = client;
                }
        }
        
        /* 轮询套接字集合 */
        if (select(fd_max + 1, &read_fds, &write_fds, NULL, NULL) == -1) {
                if (errno != EINTR) {
                        fprintf(stderr, "select error!\n");
                        continue;
                }
        }
        
        /* 建立新连接 */
        int new_client;
        if (FD_ISSET(listener, &read_fds)) {
                new_client = accept(listener, NULL, NULL);
                if (new_client == -1) {
                        fprintf(stderr, "accetp error!\n");
                        continue;
                }
                /* 将 new_client 设为非阻塞 */
                socket_nonblock(new_client);
                
                /* 将 new_client 加入 clients */
                for (int i = 0; i < MAX_CLIENTS; i++) {
                        if (clients[i] == 0) {
                                clients[i] = new_client;
                                printf("连接客户端 %d\n", new_client);
                                break;
                        }
                }
        }
        /* 从客户端接收数据 */
        ... ... ... ... ...
        /* 向客户端发送数据 */
        ... ... ... ... ...
}
```

这段代码所实现的主要功能是，在一个循环里，用 `clients` 数组不断收集新的客户端套接字，所有的技术细节在「[我是三体人！](../threebody/index.html)」和「[同步 I/O 多路复用](../io-multiplexing/index.html)」里皆有阐述。`clients` 最多能容纳 `MAX_CLIENTS` 个客户端套接字，这意味着基于上述代码实现的服务端，它最多能同时接受 100 个客户端发起的连接。需要注意的一点是，`listener` 先被加入到 `read_fds`，然后经过 `select` 的挑选，若它还在 `read_fds`，则意味着可以用它与客户端建立连接。

上述代码的末尾，暂缺「从客户端接收数据」和「向客户端发送数据」的实现，下文予以补全。

# 接收数据

在上一节的 `while (1) {...}` 循环中，若 `clients` 中的任一客户端属于集合 `read_fds`，则意味着服务端可从该客户端接收数据，该过程如下：

```c
/* 从客户端接收数据 */
#define BUF_SIZE 1024
char buffer[BUF_SIZE];

for (int i = 0; i < MAX_CLIENTS; i++) {
        ind client = clients[i];
        if (client == 0) continue;
        if (FD_ISSET(client, &read_fds)) {
                printf("从客户端 %d 接受数据\n", client);
                ssize_t n = recv(client, buffer, BUF_SIZE, 0);
                if (n > 0) {
                        if (n < BUF_SIZE) *(buffer + n) = '\0';
                        else *(buffer + BUF_SIZE - 1) = '\0';
                        printf("%s\n", buffer);
                } else {
                        /* 关闭客户端，并从 clients 中将其移除 */
                        close(client);
                        clients[i] = 0;
                        printf("数据接收失败。关闭客户端 %d\n", client); 
                }
        }
}
```

上述代码的数据接收部分，为了追求简明，未考虑安全性。若客户端发送的数据，字节数大于 `BUF_SIZE - 1`，则服务端收到的数据并不完整。此外，也未考虑处理「[可以挽救的 recv 错误](../blocking/index.html#可以挽救的错误)」。若追求服务端的实用性，这些情况皆需认真考虑。

# 发送数据

与接收数据过程相似，服务端只需逐一向 `clients` 中的套接字发送数据：

```c
/* 向客户端发送数据 */
char *msg = "threebody: Hi!";
size_t msg_len = strlen(msg);
for (int i = 0; i < MAX_CLIENTS; i++) {
        ind client = clients[i];
        if (client == 0) continue;
        if (FD_ISSET(client, &read_fds)) {
                printf("向客户端 %d 发送数据\n", client);
                ssize_t n = send(client, msg, msg_len, 0);
                if (n == -1) {
                        /* 关闭客户端，并从 clients 中将其移除 */
                        close(client);
                        clients[i] = 0;
                        printf("数据发送失败。关闭客户端 %d\n", client); 
                }
        }
}
```

同样为了追求代码的简明，使用 `send` 发送数据的过程中，未处理 「[可以挽救的 send 错误](../blocking/index.html#可以挽救的错误)」。

# 大胆假设

我们在「[两朵乌云](../blocking/index.html)」中写了一个犹豫迟疑的 ywj，即 other-ywj。它在与 threebody 建立连接后，并未急于向 threebody 发送数据，而是用 `sleep(15)` 在时间上阻塞了 15 秒。如果新的 threebody 是基于上述代码实现的，对于 other-ywj 建立的连接，它会如何处理呢？

一个大胆的答案是，服务端的 `select` 将 other-ywj 建立的这个连接对应的套接字提交给操作系统审批时，操作系统会拒绝它通过，除非在 15 秒后，other-ywj 开始向服务端发送数据和接收数据。这就是 `select` 函数之所以能提高服务端并发能力的根本原因。

结合上文代码，other-ywj 发起的连接，在服务端会表现为一个客户端套接字，假设为 `x`，它会被存入 `clients`，但是在服务端每一轮的运行中，`select` 会将 `x` 从 `read_fds` 和 `write_fds` 中剔除，故而 `x` 没有机会参与后续的数据接收和发送过程。

至于 ywj，我们也要敢于猜测，它与 threebody 建立连接后，便开始了通信。threebody 中的 `select` 会将 ywj 对应的套接字保留在 `read_fds` 和 `write_fds` 中，从而该套接字可直接用于 threebody 与 ywj 的通信。最终结果是，实现 ywj 无需等待 other-ywj 与 threebody 的通信结束后方能与 threebody 通信。

# 小心求证

为了验证上一节我们的猜想，需要基于同步 I/O 多路复用机制构建一个完整的 threebody：

```c
/* threebody.c */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <fcntl.h>
#include <sys/select.h>

#define MAX_CLIENTS 100
int clients[MAX_CLIENTS] = {0};

fd_set read_fds;
fd_set write_fds;
int fd_max;

#define BUF_SIZE 1024
char buffer[BUF_SIZE];

static int socket_nonblock(int x) {
    return fcntl(x, F_SETFL, fcntl(x, F_GETFL) | O_NONBLOCK);
}

int main(void) {
        /* 构造数字化的套接字地址 */
        struct addrinfo hints, *addr_list;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo("localhost", "8080", &hints, &addr_list);
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        /* 构造监听套接字 listener */
        int listener = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                listener = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (listener == -1) continue;
                int a = bind(listener, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(listener);
                        continue;
                }
                break;
        }
        freeaddrinfo(addr_list);
        socket_nonblock(listener);
        if (listener == -1) {
                fprintf(stderr, "failed to bind!\n");
                exit(-1);
        }
        if (listen(listener, 10) == -1) {
                fprintf(stderr, "failed to listen!\n");
                exit(-1);
        }
        /* 服务端运行 */
        while (1) {
                FD_ZERO(&read_fds);
                FD_ZERO(&write_fds);
                /* 将 listener 和 clients 中的文件加入监视集 */
                FD_SET(listener, &read_fds);
                fd_max = listener;
                for (int i = 0; i < MAX_CLIENTS; i++) {
                        int client = clients[i];
                        if (client > 0) {
                                FD_SET(client, &read_fds);
                                FD_SET(client, &write_fds);
                                /* 更新文件描述符最大值 */
                                if (client > fd_max) fd_max = client;
                        }
                }
                
                /* 轮询套接字集合 */
                if (select(fd_max + 1, &read_fds, &write_fds, NULL, NULL) == -1) {
                        fprintf(stderr, "select error!\n");
                }
                
                /* 建立新连接 */
                int new_client;
                if (FD_ISSET(listener, &read_fds)) {
                        new_client = accept(listener, NULL, NULL);
                        if (new_client == -1) {
                                fprintf(stderr, "accetp error!\n");
                                continue;
                        }
                        /* 将 new_client 设为非阻塞 */
                        socket_nonblock(new_client);
                        
                        /* 将 new_client 加入 clients */
                        for (int i = 0; i < MAX_CLIENTS; i++) {
                                if (clients[i] == 0) {
                                        clients[i] = new_client;
                                        printf("连接客户端 %d\n", new_client);
                                        break;
                                }
                        }
                }

                /* 从客户端接收数据 */
                for (int i = 0; i < MAX_CLIENTS; i++) {
                        int client = clients[i];
                        if (client == 0) continue;
                        if (FD_ISSET(client, &read_fds)) {
                                printf("从客户端 %d 接受数据\n", client);
                                ssize_t n = recv(client, buffer, BUF_SIZE, 0);
                                if (n > 0) {
                                        if (n < BUF_SIZE) *(buffer + n) = '\0';
                                        else *(buffer + BUF_SIZE - 1) = '\0';
                                        printf("%s\n", buffer);
                                } else {
                                        /* 关闭客户端，并从 clients 中将其移除 */
                                        close(client);
                                        clients[i] = 0;
                                        printf("数据接收失败。关闭客户端 %d\n", client); 
                                }
                        }
                }
                
                /* 向客户端发送数据 */
                char *msg = "threebody: Hi!";
                size_t msg_len = strlen(msg);
                for (int i = 0; i < MAX_CLIENTS; i++) {
                        int client = clients[i];
                        if (client == 0) continue;
                        if (FD_ISSET(client, &read_fds)) {
                                printf("向客户端 %d 发送数据\n", client);
                                ssize_t n = send(client, msg, msg_len, 0);
                                if (n == -1) {
                                        /* 关闭客户端，并从 clients 中将其移除 */
                                        close(client);
                                        clients[i] = 0;
                                        printf("数据发送失败。关闭客户端 %d\n", client); 
                                }
                        }
                }
        }
        close(listener);
        return 0;
}
```

编译 threebody.c：

```console
$ gcc threebody.c -o threebody
```

运行 threebody：

```console
$ ./threebody
```

然后再打开两个终端窗口，分别用于运行「[两朵乌云](../blocking/index.html)」中的 other-ywj 和 ywj。先运行 other-ywj，它与 threebody 取得连接，threebody 会输出：

```plain
连接客户端 4
```

运行 other-ywj 后，快速在另一个终端窗口运行 ywj，而后 threebody 会输出

```plain
连接客户端 5
从客户端 5 接受数据
ywj: I am here!
从客户端 5 接受数据
数据接收失败。关闭客户端 5

```

同时，ywj 也得到了 threebody 的回复：

```plain
threebody: Hi!
```

而 other-ywj 在运行大约 15 秒后，得到了 threebody 的回复：

```plain
threebody: Hi!
```

threebody 的输出是

```plain
从客户端 4 接受数据
other ywj: I am here!
向客户端 4 发送数据
从客户端 4 接受数据
数据接收失败。关闭客户端 4
```

以上试验结果完美的验证了上一节我们大胆的猜想都是正确的！

现在先不要用 Ctrl + C 组合键关闭 threebody，使用以下命令查看 threebody 的 CPU 占用情况：

```console
$ top -p $(pgrep threebody)
```

可得到类似以下结果：

```plain
    PID ...  %CPU  %MEM   ...  COMMAND
  13291 ...   0.0   0.0   ...  threebody 
```

threebody 现在也不再是疯狂吞噬 CPU 的怪物了！

现在，可以用 Ctrl + C 关闭 threebody 了。

# 你上当了

实际上，只要你认真观察「[发送数据](#发送数据)」一节的代码，应该能发现以下语句是有问题的：

```c
if (FD_ISSET(client, &read_fds))
```

由于是向套接字 `cient` 发送数据，我们应该判断 `client` 是否出现于 `write_fds`，而非 `read_fds`。倘若你将上述的条件语句修改为

```c
if (FD_ISSET(client, &write_fds))
```

那么上一节的试验便只能成功一半，即 ywj 能很快收到 threebody 的回复，但是 other-ywj 在迟疑的 15 秒的时间里，threebody 会不断给她发送数据。15 秒之后，other-ywj 收到的是不计其数条 `threebody: Hi!`。

实际上，`select` 在将 `read_fds` 和 `write_fds` 提交给操作系统审批时，threebody 用于同 other-ywj 通信的套接字，假设它为 `x`，会被 `select` 从 `read_fds` 中剔除，但它仍旧保留在 `write_fds`。因为在 `select` 时，若 other-ywj 尚未给 threebody 发送数据，此时 `x` 无数据，故而 `select` 认为它是不可读的，但是此时操作系统为 `x` 准备的用于写数据的缓冲区也是空的，故而 `select` 认为它是可写的。

为了保证上一节的试验能够完全成功，最稳妥的办法是，将服务端接收数据和发送数据所用的条件语句都改为

```c
if (FD_ISSET(client, &read_fds) && FD_ISSET(client, &write_fds))
```

即套接字必须 `client` 既可读，又可写，方能用于通信。


# 结语

盘桓在我们头上的两朵乌云终于散去。阳光普照服务端，客户端在阳光下自由奔跑。唯一打破这美好意向的是 threebody.c 的代码，步履蹒跚，我们还需要再作一些努力，让它能像「[封装](../wrapper/index.html)」中的 threebody.c 那样清爽，富有生气。
