---
title: 封装
abstract: 收纳桌子上那些乱七八糟的电线。
date: 2025 年 03 月 05 日
...

# 前言

现在，你已经实现了一个简单的 C/S 架构的程序，这个程序分为两个部分，一个是 ywj，它是客户端（Client），另一个是 threebody，它是服务端（Server），这两部分通过 socket 合为一体。不妨再勇敢一些，Internet 也不过是使用不计其数的 socket 将各个部分连接起来的一个程序。你想到了……量子力学么？通信即测量，网络即量子。

不过，还是尽快从无尽的遐想中回归，看一看 ywj.c 和 threebody.c 中的 `main` 函数，它们的代码已经有些不清晰了，像桌面上连接计算机的电源线以及乱七八糟的信号线和数据线，我们需要尝试用面向对象的办法收纳这些代码。

# 类与方法

C 语言在语法上不能像 C++、Java、Java 等语言那样支持面向对象编程，但是若将面向对象编程视为一种编程范式，C 在一定程度上是可以实现的。与 threebody.c 相比，ywj.c 的代码更为简单，我们可以从后者着手，将其改造为面向对象形式。

可以用结构体模拟类的概念，且用类名作为一些函数名以模拟类的方法。

```c
/* 类 */
struct client {
        struct addrinfo *address_list;
        int connection;
};
/* 类名 */
typedef struct client Client;

/* 方法 */
Client *client_new(const char *host, const char *port);
void client_send(Client *self, const char *message);
char *client_recieve(Client *self);
void client_free(Client *self);
```

# 实现

现在着手实现上一节为 `Client` 类声明的 4 个方法。首先是 `client_new`，它只需要构造一个网络地址列表和一个 socket——从现在开始，我不再对 socket 作任何比喻，因为你已经在直觉上知道它是什么了。

```c
Client *client_new(const char *host, const char *port) {
        Client *client = malloc(sizeof(Client));
        /* 构造用于接收信息的网络地址列表 */
        struct addrinfo hints;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo(host, port, &hints, &(client->address_list));
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        /* 选择第一个可用的地址，用于构造 socket */
        int fd = -1;
        for (struct addrinfo *it = client->address_list; it; it = it->ai_next) {
                fd = socket(client->address_list->ai_family,
                            client->address_list->ai_socktype,
                            client->address_list->ai_protocol);
                if (fd == -1) continue;
                int a = connect(fd, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(fd);
                        continue;
                }
                break;
        }
        if (fd == -1) {
                fprintf(stderr, "failed to connect!\n");
                exit(-1);
        }
        client->connection = fd;
        return client;
}
```

`client_send` 函数向 `Client` 的 `connection` 发送信息：

```c
void client_send(Client *self, const char *message) {
        ssize_t b = send(self->connection, message, strlen(message), 0);
        if (b == -1) {
                fprintf(stderr, "send error!\n");
                exit(-1);
        }
}
```

`client_recive` 函数从 `Client` 的 `connection` 接收信息：

```c
char *client_recieve(Client *self) {
        size_t m = 1024;
        char *buffer = malloc(m * sizeof(char));
        size_t n = 0;
        char *total = NULL;
        while (1) {
                ssize_t h = recv(self->connection, buffer, m, 0);
                if (h == -1) {
                        fprintf(stderr, "recv error!\n");
                        exit(-1);
                } else if (h == 0) break;
                else {
                        total = realloc(total, n + h);
                        memcpy(total + n, buffer, h);
                        n += h;
                        if (h < m) break;
                }
        }
        if (total) { /* 为字符串增加终止符 */
                total = realloc(total, n + 1);
                *(total + n) = '\0';
                n += 1;
        }
        free(buffer);
        return total;
}
```

注意，`client_recive` 返回的字符串对象，不用时，需手动 `free`。

`client_free` 用于释放 `Client` 对象，并回收 socket 占用的文件资源：

```c
void client_free(Client *self) {
        freeaddrinfo(self->address_list);
        close(self->connection);
        free(self);
}
```

注意，`client_free` 不检验参数的有效性，使用它时需要用户保证 `self` 指针是有效的。

# 重写 ywj 程序


基于上述实现的 `Client` 类和方法——完整的源码见[附录](#附录)，重写 ywj 程序。由于复杂的线索都已经封装了起来，ywj.c 非常简单：

```c
#include "client.h"

int main(void) {
        Client *earth = client_new("www.threebody.com", "8080");
        client_send(earth, "I am here!");
        char *reply = client_recieve(earth);
        printf("%s\n", reply);
        free(reply);
        client_free(earth);
        return 0;
}
```

编译 ywj.c：

```console
$ gcc client.c ywj.c -o ywj
```

# 练习题

终于，我也可以给别人布置习题了。

将 threebody.c 中的源码面向对象化吧！

# 附录

上述实现的类和方法，现在给出完整的实现。头文件 client.h 定义了 `Client` 类并声明了相关方法：

```c
/* client.h */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

/* 类 */
struct client {
        struct addrinfo *address_list;
        int connection;
};
/* 类名 */
typedef struct client Client;

/* 方法 */
Client *client_new(const char *host, const char *port);
void client_send(Client *self, const char *message);
char *client_recv(Client *self);
void client_free(Client *self);
```

client.c 实现了 `Client` 的所有方法：

```c
/* client.c */
#include "client.h"

Client *client_new(const char *host, const char *port) {
        Client *client = malloc(sizeof(Client));
        /* 构造用于接收信息的网络地址列表 */
        struct addrinfo hints;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo(host, port, &hints, &(client->address_list));
        if (a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        /* 选择第一个可用的地址，用于构造 socket */
        int fd = -1;
        for (struct addrinfo *it = client->address_list; it; it = it->ai_next) {
                fd = socket(client->address_list->ai_family,
                            client->address_list->ai_socktype,
                            client->address_list->ai_protocol);
                if (fd == -1) continue;
                int a = connect(fd, it->ai_addr, it->ai_addrlen);
                if (a == -1) {
                        close(fd);
                        continue;
                }
                break;
        }
        if (fd == -1) {
                fprintf(stderr, "failed to connect!\n");
                exit(-1);
        }
        client->connection = fd;
        return client;
}

void client_send(Client *self, const char *message) {
        ssize_t b = send(self->connection, message, strlen(message), 0);
        if (b == -1) {
                fprintf(stderr, "send error!\n");
                exit(-1);
        }
}

char *client_recieve(Client *self) {
        size_t m = 1024;
        char *buffer = malloc(m * sizeof(char));
        size_t n = 0;
        char *total = NULL;
        while (1) {
                ssize_t h = recv(self->connection, buffer, m, 0);
                if (h == -1) {
                        fprintf(stderr, "recv error!\n");
                        exit(-1);
                } else if (h == 0) break;
                else {
                        total = realloc(total, n + h);
                        memcpy(total + n, buffer, h);
                        n += h;
                        if (h < m) break;
                }
        }
        if (total) { /* 为字符串增加终止符 */
                total = realloc(total, n + 1);
                *(total + n) = '\0';
                n += 1;
        }
        free(buffer);
        return total;
}

void client_free(Client *self) {
        freeaddrinfo(self->address_list);
        close(self->connection);
        free(self);
}
```
