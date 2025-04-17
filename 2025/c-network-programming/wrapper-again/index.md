---
title: 再封装
abstract: sim 项目的进化。 
date: 2025 年 04 月 11 日
...

# 前言

我们基于 `select` 显著提升了服务端的并发性能。在高兴之余，一定会有更擅长网络编程的朋友告诉我们，`select` 过时了，`poll` 更好，最好的是 `epoll`……他们是正确的，但是现在我们需要的，还不是这种正确。`select` 的缺陷是，通常情况下最多只能支持 1024 个文件描述符，亦即基于它实现的服务端同时最多只能支持 1024 个客户端。然而，我们的服务端能同时支持这么多个客户端，已经非常了不起了，如同你经营一家超市，每个时间段都能有 1024 个顾客在店内游逛。以后，待我们需要更了不起的时候，再研究 `poll` 甚至 `epoll` 也不迟，而现在我们要做的是，将基于 `select` 的同步 I/O 多路复用机制融入「[封装](../wrapper/index.html)」中所实现的 Sim 项目。

# 单向链表

在「[长缨在手](../socket-and-select/index.html)」中，我们用了一个固定长度的数组存储了一组客户端套接字。我们需要实现一个较固定长度的数组更为灵巧的数据结构，将其用于存储客户端套接字。该数据结构应当由一些称为结点的数据单元构成，每个结点可存储 1 个套接字。这种数据结构通常有两种实现，一种是动态数组，一种是链表。我更喜欢链表。

以下代码定义了一个非常简单的单向链表类 `SimList` 并声明了其析构函数和 `add` 方法：

```c
/* sim-list.h */
#ifndef SIM_LIST_H
#define SIM_LIST_H
#include <stdio.h>

typedef struct sim_list SimList;
struct sim_list {
        void *data;
        struct sim_list *next;
};
void sim_list_free(SimList *list);

/* 获取链表长度 */
size_t sim_list_size(SimList *list);
/* 向链表添加一条数据 */
SimList *sim_list_add(SimList *self, void *data);
/* 从链表删除一个单元（结点） */
SimList *sim_list_delete(SimList *self, SimList *node);
#endif
```

我没有为 `SimList` 对象提供构造函数，原因是我已将 `SimList` 的定义公布于 sim-list.h 里了，这意味着任何人都可以根据它的定义创建 `SimList` 对象。

以下代码实现了 `SimList` 对象的析构函数和所有方法：

```c
/* sim-list.c */
#include <stdlib.h>
#include "sim-list.h"

void sim_list_free(SimList *list) {
        if (list) {
                SimList *it = list;
                while (it) {
                        SimList *next = it->next;
                        free(it);
                        it = next;
                }
        }
}

size_t sim_list_size(SimList *list) {
        if (list) {
                size_t n = 0;
                for (SimList *it = list; it; it = it->next) n++;
                return n;
        } else return 0;
}

SimList *sim_list_add(SimList *self, void *data) {
        SimList *new_node = malloc(sizeof(SimList));
        if (new_node) {
                new_node->data = data;
                if (self) new_node->next = self;
                else new_node->next = NULL;
        } else fprintf(stderr, "sim_list_add error!\n");
        return new_node;
}

SimList *sim_list_delete(SimList *self, SimList *node) {
        if (!node) return self;
        SimList *prev = NULL;
        for (SimList *it = self; it; it = it->next) {
                if (it == node) break;
                if (it->next == node) {
                        prev = it;
                        break;
                }
        }
        if (prev) prev->next = node->next;
         else self = node->next;
        free(node);
        return self;
}

```

作为 `SimList` 类的用法示例，将三个整型变量存储于 `SimList` 对象：

```c
int a = 4, b = 5, c = 6;

SimList *list = NULL;
list = sim_list_add(list, &a);
list = sim_list_add(list, &b);
list = sim_list_add(list, &c);
```

以下代码遍历 `list`，输出套接字，然后释放 `list`：

```c
for (SimList *it = list; it; it = it->next) {
        int *i = it->data;
        printf("socket %d\n", *i);
}
sim_list_free(list);
```

若上述 `a`、`b`、`c` 皆为局部变量，需要将它们的值存储于 `list`，只需像下面这样做：

```c
int a = 4, b = 5, c = 6;

SimList *list = NULL;
int *p;
p = malloc(sizeof(int));
*p = a;
list = sim_list_add(list, p);
p = malloc(sizeof(int));
*p = b;
list = sim_list_add(list, p);
p = malloc(sizeof(int));
*p = c;
list = sim_list_add(list, p);
```

要释放用于存储 `a`、`b`、`c` 值的内存，只需在对 `list` 析构前，遍历 `list`，逐一释放 `list->data`：

```c
for (SimList *it = list; it; it = it->next) {
        free(it->data);
}
sim_list_free(list);
```

不得不承认，用 `SimList` 存储套接字，有些笨，但终归可以摆脱固定长度的数组的桎梏了。实际上，我们可以为 `SimList` 实现一个更方便的 `add` 的方法，只不过该方法是一个宏：

```c
/* sim-list.h ++ */
#define SIM_LIST_ADD(self, value, type) do { \
        type *p = malloc(sizeof(type)); \
        *p = value; \
        self = sim_list_add(self, p); \
} while (0)
```

基于 `SIM_LIST_ADD` 宏，可将上述将局部变量 `a`、`b`、`c` 的值存储于 `list` 的过程简化为

```c
int a = 4, b = 5, c = 6;
SimList *list = NULL;

SIM_LIST_ADD(list, a, int);
SIM_LIST_ADD(list, b, int);
SIM_LIST_ADD(list, c, int);
```

一定要注意，`SIM_LIST_ADD` 的第一个参数必须是 `SimList *` 类型的变量。此外，无论是 `sim_list_add` 还是 `SIM_LIST_ADD`，新建的链表单元总是位于链表之首。这个 `add` 方法，它更准确的名字应该是 `prepend`。不过，我喜欢 `add`，这个名字短。

# 修改 SimServer 类

`SimServer` 对象原本只能记录一个客户端套接字，现在我们试图让它具备并发能力，这意味着它需要存储多个客户端套接字，具体数量未知。为此，我们可以用 `SimList` 对象作为 `SimServer` 对象存储多个客户端套接字的容器：


```c
/* sim-network.h ++ */
#include <sim-list.h>
```

```c
/* sim-network.c ++ */
#include <fcntl.h>
#include <sys/select.h>
```

```c
/* sim-network.c [改] */
struct sim_server {
        int listener;
        SimList *clients;
        fd_set read_fds;
        fd_set write_fds;
        const char *error;
};
```

`SimServer` 对象也许从未想过有一天自己会如此富有，以至于对自己的构造和析构函数都要更加小心翼翼了。

```c
/* sim-network.c [改] */
static int socket_nonblock(int x);

SimServer *sim_server(const char *host, const char *port) {
        int fd = first_valid_address(host, port, bind); 
        if (fd == -1) return NULL;
        if (listen(fd, 10) == -1) return NULL;
        socket_nonblock(fd); /* 将 fd 设为非阻塞状态 */

        SimServer *server = malloc(sizeof(SimServer));
        if (!server) {
                fprintf(stderr, "sim_server error!\n");
                return NULL;
        }
        server->listener = fd;
        server->clients = NULL;
        server->error = NULL;
        return server;
}

static int socket_nonblock(int x) {
    return fcntl(x, F_SETFL, fcntl(x, F_GETFL) | O_NONBLOCK);
}
```

```c
/* sim-network.c [改] */
void sim_server_free(SimServer *server) {
        if (server) {
                close(server->listener);
                if (server->clients) {
                        for (SimList *it = server->clients; it; it = it->next) {
                                if (it->data) free(it->data);
                        }
                        sim_list_free(server->clients);
                }
                free(server);
        }
}
```

需要注意的是，`sim_server` 函数中，将用于监听的套接字设成了非阻塞状态，这是实现服务端对 I/O 多路复用机制的支持所必须的。


# 修改 sim_server_run_once

将基于 `select` 的 I/O 多路复用机制纳入 `sim_server_run_once` 函数：

```c
/* sim-network.c [改] */
void sim_server_run_once(SimServer *self) {
        int fd_max;
         /* 尽量让 select 成功运行 */
        while (1) {
                FD_ZERO(&self->read_fds);
                FD_ZERO(&self->write_fds);
                FD_SET(self->listener, &self->read_fds);
                fd_max = self->listener;
                for (SimList *it = self->clients; it; it = it->next) {
                        int client = *(int *)(it->data);
                        FD_SET(client, &self->read_fds);
                        FD_SET(client, &self->write_fds);
                        if (fd_max < client) fd_max = client;
                }
                if (select(fd_max + 1, 
                           &self->read_fds, 
                           &self->write_fds, 
                           NULL, 
                           NULL) == -1) {
                        if (errno == EINTR) continue;
                        else {
                            self->error = "sim_server_run_once error!";
                            break;
                        }
                } else break;
        }

        /* 接纳新的连接 */
        if (FD_ISSET(self->listener, &self->read_fds)) {
                 /* 尽量让 accept 运行成功 */
                while (1) {
                        int fd = accept(self->listener, NULL, NULL);
                        if (fd == -1) {
                                if (errno == EINTR) continue;
                                else {
                                        self->error = "sim_server_run_once error!";
                                        break;
                                }
                        } else {
                                /* 将 fd 设为非阻塞状态 */
                                socket_nonblock(fd);
                                /* 将 fd 加入 self->clients */
                                SIM_LIST_ADD(self->clients, fd, int);
                                /* 恢复 self 无错状态 */
                                if (self->error) self->error = NULL;
                                break;
                        }
                }
        }
}
```

上述代码中，两处 `while (1)` 循环只是为了尽量保证 `select` 和 `accept` 不会受系统中断信号的干扰而出错，详见「[可以挽救的错误](../blocking/index.html#可以挽救的错误)」，在通常情况下，这两处 `while (1)` 的循环体只运行一次便可退出，故而不会无限循环。

正常情况下，每运行一次 `sim_server_run_once`，`SimServer` 对象的 `clients` 成员便可纳入一个新的客户端套接字。在下一次运行 `sim_server_run_once` 时，该套接字会被 `select` 提交给系统审批。若审批通过，该套接字会被保留在 `read_fds` 和 `write_fds` 中。

# 修改 sim_server_receive

`sim_server_receive` 依然基于「[可以挽救的错误](../blocking/index.html#可以挽救的错误)」中的 `recv_robustly` 从客户端套接字接收数据，只是现在需要遍历 `SimServer` 对象的 `clients` 中的每个客户端套接字，若该套接字在 `SimServer` 的 `read_fds` 中，则通过它接收数据。此外，由于可能有多个客户端套接字皆可读，从每个套接字读取的数据保存在一个 `SimStr` 对象里，故而我们需要用一个 `SimList` 对象存储这些 `SimStr `对象。

```c
/* sim-network.h [改] */
SimList *sim_server_receive(SimServer *self);
```

```c
/* sim-network.c [改] */
SimList *sim_server_receive(SimServer *self) {
        SimList *msgs = NULL;
        SimList *it = self->clients;
        while (it) {
                int client = *(int *)(it->data);
                if (FD_ISSET(client, &self->read_fds)
                    && FD_ISSET(client, &self->write_fds)) {
                        SimStr *msg = recv_robustly(client);
                        if (msg) {
                                msgs = sim_list_add(msgs, msg);
                        } else {
                                /* 客户端套接字无法读取，可能对端已经关闭了连接 */
                                /* 我们需要从 self->clients 移除 it，并关闭 client */
                                SimList *next = it->next;
                                free(it->data);
                                self->clients = sim_list_delete(self->clients, it);
                                it = next;
                                close(client);
                                continue;
                        }
                }
                it = it->next;
        }
        return msgs;
}
```

# 修改 sim_server_send

`sim_server_send` 需要遍历 `SimServer` 对象的 `clients` 中的每个客户端套接字，若该套接字在 `SimServer` 的 `write_fds` 中，则通过它向客户端发送数据。

```c
/* sim-network.c [改] */
void sim_server_send(SimServer *self, SimStr *msg) {
        SimList *it = self->clients;
        while (it) {
                int client = *(int *)(it->data);
                if (FD_ISSET(client, &self->read_fds)
                    && FD_ISSET(client, &self->write_fds)) {
                        if (send_robustly(client, msg) == -1) {
                                /* 客户端套接字无法写入数据，可能对端已经关闭了连接 */
                                /* 我们需要从 self->clients 移除 it，并关闭 client */
                                SimList *next = it->next;
                                free(it->data);
                                self->clients = sim_list_delete(self->clients, it);
                                it = next;
                                close(client);
                                continue;
                        }
                }
                it = it->next;
        }
}
```

# threebody 并发

现在，可以重写 threebody.c，欺骗 ywj 和 other-ywj，让她们都以为自己得到了 threebody 的眷顾，前者很快得到了 threebody 的回复，后者以为 threebody 颇有耐心，容许她的迟疑……

```c
/* threebody.c */
#include "sim-network.h"
int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        /* 服务端程序运转 */
        while (1) {
                sim_server_run_once(threebody);
                if (sim_server_safe(threebody)) {
                        /* 从客户端接收信息 */
                        SimList *msgs = sim_server_receive(threebody);
                        if (msgs) {
                                for (SimList *it = msgs; it; it = it->next) {
                                        SimStr *msg = it->data;
                                        printf("%s\n", sim_str_raw(msg));
                                        sim_str_free(msg);
                                }
                                sim_list_free(msgs);
                        }
                        /* 向客户端发送信息 */
                        SimStr *msg = sim_str("threebody: Hi");
                        sim_server_send(threebody, msg);
                        sim_str_free(msg);
                }
        }
        sim_server_free(threebody);
        return 0;
}
```

# 总结

本文最突出的工作并非营造一个具备并发能力的 threebody，而是定义了 `SimList` 这个单向链表类，没有它的支持，服务端便无法以一种可变长度的数据结构存储数量不固定的客户端套接字。单向链表的实现较为简单，但弊端也是有的。例如，获取链表长度，时间复杂度为 O(n)，因为要遍历所有结点。删除链表中的某个结点，时间复杂度也为 O(n)，因为需要从链表中找到该结点的前一个结点，方能完成删除过程。不过，由于 `select` 最多只能支持 1024 个客户端并发访问，单向链表的性能并非不可接受。

下面是一些你可以不劳而获的成果：

* 单向链表类：[sim-list.h](sim-list.h) 和 [sim-list.c](sim-list.c)
* 字符串类：[sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
* 网络类：[sim-network.h](sim-network.h) 和 [sim-network.c](sim-network.c)
* 测试程序：[threebody.c](threebody.c)
