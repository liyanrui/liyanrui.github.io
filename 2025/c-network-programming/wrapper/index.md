---
title: 封装
abstract: 收纳桌子上那些乱七八糟的电线。
date: 2025 年 04 月 05 日
...

# 前言

sim 项目的字符串类获得了业界好评，ywj 和 threebody 也受到了鼓舞，希望加入 sim 的阵营。

# 客户端

新建一份文件 sim-network.h，除了防碰头设计，还包含了 sim-str.h：

```c
/* sim-network.h */
#ifndef SIM_NETWORK_H
#define SIM_NETWORK_H
#include <sim-str.h>

#endif
```

之后我用以下示意标记

```c
/* sim-network.h ++ */
... ... 代码片段 ... ...
```

我希望你能理解成，代码片段是添加在 `#define SIM_NETWORK_H` 和 `#endif` 之间的区域。

创建 sim-network.c 文件，先让它包含 sim-network.h：

```c
/* sim-network.c */
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include "sim-network.h"
```

客户端程序 ywj 较为简单，我们不妨先从客户端类 `SimClient` 的定义开始：

```c
/* sim-network.h ++ */
typedef struct sim_client SimClient;
```

```c
/* sim-network.c ++ */
struct sim_client {
        int server;   /* 服务端套接字 */
        const char *error;
};
```

与 `SimStr` 相似，`SimClient` 对象也有 `error` 成员，用于记录它的生命周期内可能的出错信息。我会努力让 sim 项目中所有的对象，都拥有 `error` 成员。

`SimClient` 对象的构造函数的声明如下：

```c
/* sim-network.h ++ */
SimClient *sim_client(const char *host, const char *port);
```

在实现 `sim_client` 函数之前，需要先将套接字地址由文字化转化为数字化的过程封装为一个仅在 sim-network.c 中使用的函数。不过，在定义这个函数之前，我们先定义一个函数指针：

```c
/* sim-network.c ++ */
typedef int (*AddrSelector)(int sockfd, 
                            const struct sockaddr *addr, 
                            socklen_t addrlen);
```

函数指针 `AddSelector` 指向的函数，用于 `getaddrinfo` 返回的地址列表中挑选可用地址。倘若你还没有对套接字 API 函数中的 `connect` 和 `bind` 的用法过于淡忘，想必应该明白了，它们就是这样的函数。基于这个函数指针，便可定义一个函数，从文字化套接字地址转化的数字化套接字地址列表中选择第一个可用的地址，如下：

```c
/* sim-network.c ++ */
static int first_valid_address(const char *host, 
                               const char *port,
                               AddrSelector selector) {
        struct addrinfo hints, *addr_list;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        if (getaddrinfo(host, port, &hints, &addr_list) != 0) {
                return -1;
        }
        int fd = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                fd = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (fd == -1) continue;
                if (selector(fd, it->ai_addr, it->ai_addrlen) == -1) {
                        close(fd);
                        fd = -1;
                        continue;
                }
                break;
        }
        freeaddrinfo(addr_list);
        return fd;
}
```

在 `first_valid_address` 的辅助下，可以很简单的实现 `SimClient` 对象的构造函数：

```c
/* sim-network.c ++ */
SimClient *sim_client(const char *host, const char *port) {
        int fd = first_valid_address(host, port, connect); 
        if (fd == -1) return NULL;

        SimClient *client = malloc(sizeof(SimClient));
        if (!client) {
                fprintf(stderr, "sim_client error: failed to malloc!\n");
                return NULL;
        }
        client->server = fd;
        client->error = NULL;
        return client;
}
```

如果你对 C 语言的函数指针的用法不熟悉，那么上述代码可以作为一个很实用的示例。

`SimClient` 对象的析构函数的声明和定义如下：

```c
/* sim-network.h ++ */
void sim_client_free(SimClient *client);
```

```c
/* sim-network.c ++ */
void sim_client_free(SimClient *client) {
        if (client) {
                close(client->server);
                free(client);
        } else {
                fprintf(stderr, "sim_client_free error: NULL pointer!\n");
        }
}
```

上述代码，若有不解之处，请及时回顾「[我在这里！](../simple-client/index.html)」。

# 服务端

服务端类的声明和定义如下：

```c
/* sim-network.h ++ */
typedef struct sim_server SimServer;
```

```c
/* sim-network.c ++ */
struct sim_server {
        int listener;  /* 用于监听的套接字 */
        int client;    /* 客户端套接字 */
        const char *error;
};
```

`SimServer` 对象的构造函数的声明和定义如下：

```c
/* sim-network.h ++ */
SimServer *sim_server(const char *host, const char *port);
```

```c
/* sim-network.h ++ */
SimServer *sim_server(const char *host, const char *port) {
        int fd = first_valid_address(host, port, bind); 
        if (fd == -1) return NULL;
        if (listen(fd, 10) == -1) return NULL;
        
        SimServer *server = malloc(sizeof(SimServer));
        if (!server) {
                fprintf(stderr, "sim_server error: failed to malloc!\n");
                return NULL;
        }
        server->listener = fd;
        server->client = -1;
        server->error = NULL;
        return server;
}
```

`SimServer` 对象的析构函数声明如下：

```c
/* sim-network.h ++ */
void sim_server_free(SimServer *server);
```

`sim_server_free` 只需关闭用于 `SimServer` 对象的用于监听的套接字：

```c
/* sim-network.c ++ */
void sim_server_free(SimServer *server) {
        if (server) {
                close(server->listener);
                free(server);
        } else {
                fprintf(stderr, "sim_server_free error: NULL pointer!\n");
        }
}
```

`SimServer` 对象需要一个 `run` 方法，表达服务端开始运转，接受客户端的连接，该方法声明和定义如下：

```c
/* sim-network.h ++ */
void sim_server_run(SimServer *self);
```

```c
/* sim-network.c ++ */
void sim_server_run(SimServer *self) {
        int fd = accept(self->listener, NULL, NULL);
        if (fd == -1) {
                self->error = "sim_server_run error: failed to accept!";
        } else {
                /* 恢复 self 无错状态 */
                if (self->error) self->error = NULL;
        }
        self->client = fd;
}
```

至于 `SimServer` 对象的 `client` 套接字，每次当 `SimServer` 对象处理完一个客户端连接后，可使用 `close` 方法关闭，该方法的声明与定义如下：

```c
/* sim-network.h ++ */
void sim_server_close(SimServer *server);
```

```c
/* sim-network.c ++ */
void sim_server_close(SimServer *self) {
        if(self) {
                close(self->client);
                self->client = -1;
        } else {
                fprintf(stderr, "sim_server_close error: NULL pointer!\n");
        }
}
```

上述代码，若有不解之处，请及时回顾「[我是三体人](../threebody/index.html)」。

# 数据发送与接收

`SimClient` 对象和 `SimServer` 对象之间的通信方法本质相同，区别只是使用的套接字不同，`SimClient` 用它的服务端套接字，而 `SimServer` 用它的客户端套接字。我们可以先实现两个通用的函数，对所有套接字通信一视同仁，然后基于它们为 `SimClient` 对象和 `SimServer` 对象定义数据发送和接收方法。

首先，实现向一个套接字写入数据。在「[我在这里！](../simple-client/index.html)」中，我们使用套接字 API 函数 `send` 向套接字写入数据。若 `send` 运行成功，其返回值是已写入数据的字节数。可能当时你并未深想这句话意味着什么。事实上，`send` 有时未必能将数据一次性完全写入套接字，会有一些剩余。网络状况不稳定或信息接收方处理速度较慢，都有可能导致这种情况出现。

解决上述问题的方法很简单，`send` 函数能够将剩余数据继续写入套接字，只需重复这个过程，直至数据没有剩余。该过程的实现如下：

```c
/* sim-network.c ++ */
/* 将字符串对象中的数据发送到套接字 x */
static int send_robustly(int x, SimStr *str) {
        const char *buffer = sim_str_share(str);
        size_t n = sim_str_size(str); /* buffer 字节数 */
        size_t m = 0; /* 发送了多少字节 */
        size_t remaining = n; /* 还有多少字节未发送 */
        while (m < n) {
                ssize_t t = send(x, buffer + m, remaining, 0);
                if (t == -1) break;
                m += t;
                remaining -= t;
        }
        return (m < n) ? -1 : 0;
}
```

同理，套接字 API 函数 `recv` 函数也存在类似 `send` 这样的问题。从套接字读取数据时，有可能缓冲区太小，无法一次性读取所有数据，必须多次读取，直至无剩余数据为止。该过程的实现如下：

```c
/* 从套接字 x 接收数据，返回值为接受到的数据 */
SimStr *recv_robustly(int x) {
        size_t m = 1024;
        char *data = malloc(m * sizeof(char));
        size_t n = 0; /* 已接收的字节数 */
        while (1) {
                if (n == m) { /* 扩容 */
                        m *= 2;
                        data = realloc(data, m);
                }
                size_t remaining = m - n; /* 剩余空间长度 */
                ssize_t h = recv(x, data + n, remaining, 0);
                if (h == -1) {
                        free(data);
                        return NULL;
                } else if (h == 0) break;
                else {
                        n += h;
                        if (h < remaining) break;
                }
        }
        if (n == 0) {
                free(data);
                return NULL;
        } else return sim_str_absorb(data, m, n);
}
```

注意，`recv_robustly` 在接收数据的过程中，发现缓冲区 `data` 已满，便将其扩容一倍，使得缓冲区空间总是够用。当发现接收数据的字节数 `h` 小于 `data` 剩余空间长度 `rest` 时，便意味着数据接收完毕。

基于上述的 `send` 和 `recv` 稳健版，可以为 `Client` 和 `Server` 对象定义数据发送和接收方法：

```c
/* sim-network.h ++ */
void sim_client_send(SimClient *self, SimStr *str);
SimStr *sim_client_receive(SimClient *self);
```

```c
/* sim-network.c ++ */
#define SEND_COMMON(member, error_info)  do {         \
        if (send_robustly(self->member, str) == -1) { \
                self->error = error_info;             \
        } else {                                      \
                /* 恢复 self 正常 */                   \
                if (self->error) self->error = NULL;  \
        }                                             \
} while (0)

void sim_client_send(SimClient *self, SimStr *str) {
        SEND_COMMON(server, "sim_client_send error!");
}
void sim_server_send(SimServer *self, SimStr *str) {
        SEND_COMMON(client, "sim_server_send error!");
}
```

```c
/* sim-network.h ++ */
void sim_server_send(SimClient *self, SimStr *str);
SimStr *sim_server_receive(SimClient *self);
```

```c
/* sim-network.c ++ */
#define RECEIVE_COMMON(member, error_info) do {       \
        SimStr *str = recv_robustly(self->member);    \
        if (!str) {                                   \
                self->error = error_info;             \
        } else {                                      \
                /* 恢复 self 正常 */                   \
                if (self->error) self->error = NULL;  \
        }                                             \
        return str;                                   \
} while (0)

SimStr *sim_client_receive(SimClient *self) {
        RECEIVE_COMMON(server, "sim_client_receive error!");
}

SimStr *sim_server_receive(SimServer *self) {
        RECEIVE_COMMON(client, "sim_server_receive error!");
}
```

上述代码，为了消除重复代码，我用了宏，但愿不会吓到你。它们其实很简单，你只需用宏调用语句中的实际参数去替换宏定义中的名义参数即可。例如

```c
SEND_COMMON(server, "sim_client_send error!");
```

这个宏调用语句的展开结果是

```c
do {
        SimStr *str = recv_robustly(self->server);
        if (!str) {
                self->error = sim_client_send error!;
        } else {
                if (self->error) self->error = NULL;
        }
        return str;
} while (0);
```

应该有更好的避免代码重复的方法，待日后再予深究。

# 安全监测

上述 `SimClient` 和 `SimServer` 对象的一些方法的实现，利用对象的 `error` 记录了错误信息。我们为这两种对象的安全性提供了检测方法：

```c
/* sim-network.h ++ */
bool sim_client_safe(SimClient *self);
bool sim_server_safe(SimServer *self);
```

```c
/* sim-network.h ++ */
#define SIM_OBJ_SAFE(error_info) do {          \
        if (self) {                            \
                /* 不太致命的错误 */             \
                if (self->error) return false; \
        } else {                               \
                /* 致命错误 */                  \
                fprintf(stderr, error_info);   \
                return false;                  \
        }                                      \
        return true;                           \
} while (0)

bool sim_client_safe(SimClient *self) {
        SIM_OBJ_SAFE("sim_client_safe error: NULL pointer!\n");
}

bool sim_server_safe(SimServer *self) {
        SIM_OBJ_SAFE("sim_server_safe error: NULL pointer!\n");
}
```

上述的 `SIM_SAFE` 宏，对 `sim_str_safe` 也适用，以后我们应该专门建立一个 sim-macro.h 文件，集中存放一些通用的宏。

# 重写 ywj

ywj 说，我期待这一天已经很久了！

```c
/* ywj.c */
#include "sim-network.h"
int main(void) {
        SimClient *ywj = sim_client("www.threebody.com", "8080");
        if (!ywj) {
                fprintf(stderr, "sim_client failed!\n");
                exit(-1);
        }
        /* 发送数据 */
        SimStr *msg_to = sim_str("ywj: I am here!");
        sim_client_send(ywj, msg_to);
        sim_str_free(msg_to);
        /* 从 www.threebody.com:8080 接收信息 */
        SimStr *msg_from = sim_client_receive(ywj);
        if (sim_str_safe(msg_from)) {
                printf("%s\n", sim_str_raw(msg_from));
        }
        sim_str_free(msg_from);
        /* 析构，退出 */
        sim_client_free(ywj);
        return 0;
}
```

用 gcc 编译上述 ywj.c，需要联编 sim-str.c 和 sim-network.c：

```console
$ gcc -I. sim-str.c sim-network.c ywj.c -o ywj
```

# 重写 threebody

threebody 说，终于轮到我了！

```c
/* threebody.c */
#include "sim-network.h"
int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        sim_server_run(threebody);
        /* 从客户端读取信息 */
        SimStr *msg_from = sim_server_receive(threebody);
        if (sim_str_safe(msg_from)) {
                printf("%s\n", sim_str_raw(msg_from));
        }
        sim_str_free(msg_from);
        /* 向客户端发送信息 */
        SimStr *msg_to = sim_str("threebody: Hi!");
        sim_server_send(threebody, msg_to);
        sim_str_free(msg_to);
        sim_server_close(threebody);
        /* 析构，退出 */
        sim_server_free(threebody);
        return 0;
}
```

编译：

```console
$ gcc -I. sim-str.c sim-network.c threebody.c -o threebody
```

# 总结

如果你依然不想逐一复制本文所有代码片段，拼凑成完整的 .h 和 .c 文件，你依然可以不劳而获……

* [sim-str.h](sim-str.h)
* [sim-str.c](sim-str.c)
* [sim-network.h](sim-network.h)
* [sim-network.h](sim-network.c)
* [ywj.c](ywj.c)
* [threebody.c](threebody.c)

我们的 sim 项目，现已初具规模，它有两个模块了。此刻，你应该也基本熟悉如何用 C 编写简单的面向对象范式的程序了。面向对象编程已在业界独领风骚三十余年，自然是博大精深的，不过，恐龙也是博大精深的，统治地球上亿年。现有的面向编程技巧对于我们而言，似乎已经足够了。持而盈之，不若其已。
