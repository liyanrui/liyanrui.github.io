---
title: select 不负重望
abstract: 我们有了初步拥抱这个世界的能力。
date: 04 月 07 日
...

# 前言

「[两朵乌云](blocking/index.html)」中修改的 sim-network.c 余温尚存，基于 `select` 实现的 I/O 多路复用机制炉火正旺，二者交融，会发生什么神奇的化学反应呢？

# 修改 SimServer

首先需要在 sim-network.c 中添加文件描述符集合以及 `select` 函数所需的头文件：

```c
/* sim-network.c ++ */
#include <sys/select.h>
```

然后在 `SimServer` 的定义中增加三个文件描述符集合，分别用于监视文件是否可读、可写以及异常等状态：

```c
/* sim-network.c [改] */
struct sim_server {
        int listener;       /* 用于监听的套接字 */
        fd_set read_fds;    /* 可读文件描述符集 */
        fd_set write_fds;   /* 可写文件描述符集 */
        int fd_max;         /* 监控的文件描述符最大值 */
        const char *error;
};
```

需要注意的是，之前 `SimServer` 有一个 `client` 套接字，现在不需要它了，因为 `SimServer` 对象从本文开始就不需要与客户端一对一的通信了。所有可能的客户端套接字都在 `read_fds` 和 `write_fds` 集合里。

`SimServer` 对象也许从未想过，会有一天，自己竟然如此阔绰，以至于不得不小心翼翼地构造自己：

```c
/* sim-network.c [改] */
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
        server->error = NULL;
        FD_ZERO(&server->read_fds);
        FD_ZERO(&server->write_fds);
        FD_SET(fd, &server->read_fds);
        server->fd_max = fd;
        return server;
}
```

# 修改 sim_server_run

```c
/* sim-network.c [改] */
void sim_server_run(SimServer *self) {
        fd_set read_fds = self->read_fds;
        fd_set write_fds = self->write_fds;
        while (1) {
                read_fds = self->read_fds;
                write_fds = self->write_fds;
                if (select(self->fd_max + 1, 
                           &read_fds, 
                           &write_fds, 
                           NULL, 
                           NULL) == -1) {
                        if (errno == EINTR) continue;
                        else {
                            self->error = "sim_server_run error!";
                            break;
                        }
                } else break;
        }
        if (FD_ISSET(self->listener, &read_fds)) {
                /* 建立连接 */
                int fd;
                while (1) {
                        fd = accept(self->listener, NULL, NULL);
                        if (fd == -1) {
                                if (errno == EAGAIN || errno == EINTR) continue;
                                else {
                                        self->error = "sim_server_run error!";
                                        break;
                                }
                        } else {
                                /* 将 fd 设为非阻塞状态 */
                                socket_nonblock(fd); 
                                /* 恢复 self 无错状态 */
                                if (self->error) self->error = NULL;
                                break;
                        }
                }
                /* 将 fd 加入文件描述符监视集 */
                FD_SET(fd, &self->read_fds);
                FD_SET(fd, &self->write_fds);
                /* 更新 fd_max */
                if (fd > self->fd_max) {
                        self->fd_max = fd;
                }
        }
}
```

修改后的 `sim_server_run`，每一次运行时，运气好的话，能建立一个新的客户端套接字，并将添加到集合 `read_fds` 和 `write_fds` 里。上述代码看上去已颇为复杂，但大多数代码是为了提高 `select` 和 `accept` 的容错性，能正确应对 `EAGAIN` 和 `EINTR` 这些错误。

# 修改 sim_server_send

向客户端发送数据，只需遍历 `SimServer` 的 `write_fds` 中的所有套接字，逐一发送数据：

```c
/* sim-network.c [改] */
static void update_fd_max(SimServer *self);

void sim_server_send(SimServer *self, SimStr *str) {
        bool changed = false;
        for (int i = 0; i < self->fd_max + 1; i++) {
                if (FD_ISSET(i, &self->write_fds)) {
                        if (send_robustly(i, str) == -1) {
                                FD_CLR(i, &self->write_fds);
                                close(i);
                                self->error = "invalid socket!";
                                changed = true;
                        } else {
                                if (self->error) self->error = NULL;
                        }
                }
        }
        if (changed) update_fd_max(self);
}

static void update_fd_max(SimServer *self) {
        int n = self->fd_max; 
        self->fd_max = -1;
        for (int i = 0; i < n + 1; i++) {
                if (FD_ISSET(i, &self->write_fds)) {
                        if (i > self->fd_max) self->fd_max = i;
                }
        }
}
```

若 `send_robustly` 向套接字 `i` 发送数据失败，返回 -1，此时需要从 `write_fds` 中删除 `i`，并将其关闭，然后记录出错信息。

# 修改 sim_server_receive

从客户端读取数据，只需遍历 `SimServer` 的 `read_fds` 中的所有套接字，逐一读取数据……你应该很快发现了困难之处，之前的 `sim_server_receive` 版本返回的是从 1 个客户端套接字读取的数据，而现在可能有多个客户端套接字要读取，故而 `sim_server_receive` 返回的应该是一个数据集，而且这个数据集的规模无法预先确定。

这意味着什么？意味着我们需要有一个链表或动态数组之类的数据容器，存放这个数据集。现在，先假设我们有一个单链表形式的数据容器，其类型为 `SimList`，它有 `append` 方法，然后便可修改 `sim_server_receive`。

```c
/* sim-network.h [改] */
SimList *sim_server_receive(SimServer *self);
```

```c
/* sim-network.c [改] */
SimList *sim_server_receive(SimServer *self) {
        bool changed = false;
        SimList *strs = NULL;
        for (int i = 0; i < self->fd_max + 1; i++) {
                if (FD_ISSET(i, &self->read_fds)) {
                        if (i == self->listener) continue;
                        SimStr *str = recv_robustly(i);
                        if (str) {
                                strs = sim_list_add(strs, str);
                                if (self->error) self->error = NULL;
                        } else {
                                FD_CLR(i, &self->read_fds);
                                close(i);
                                self->error = "invalid socket!";
                                changed = true;
                        }
                }
        }
        if (changed) update_fd_max(self);
        return strs;
}
```

# 单链表容器

`sim_server_receive` 需要单链表容器，现在实现一个不严肃的版本，以其功能够用即可。

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

SimList *sim_list_add(SimList *self, void *data);
#endif
```

`SimList` 对象没有构造函数，原因是我已将它的数据结构完整地暴露在 sim-list.h 里了，任何人皆可由该数据结构构造 `SimList` 对象。

以下是 `SimList` 类的实现：

```c
/* sim-list.c */
#include <stdlib.h>
#include "sim-list.h"

void sim_list_free(SimList *list) {
        if (list) {
                SimList *it = list;
                while (1) {
                        if (!it) break;
                        SimList *next = it->next;
                        free(it);
                        it = next;
                }
        } else {
                fprintf(stderr, "sim_list_free error!\n");
        }
}
```

```c
/* sim-list.c ++ */
SimList *sim_list_add(SimList *self, void *data) {
        SimList *new_node = malloc(sizeof(SimList));
        if (self) {
                new_node->data = data;
                new_node->next = self;
        } else {
                new_node->data = data;
                new_node->next = NULL;
        }
        return new_node;
}
```

然后，在 sim-network.c 文件中包含 sim-list.h，便可令 `sim_server_receive` 不再形同虚设。

```c
/* sim-network.h ++ */
#include <sim-list.h>
```

# 消除时间阻塞

不知你是否还记得，在「[两朵乌云](blocking/index.html)」中，`SimServer` 对象的 `listener` 和 `client` 套接字都设为非阻塞状态后，为了让 threebody 能够收到 other-ywj 和 ywj 发送的数据，我修改了 `sim_server_run`、`recv_robustly` 和 `send_robustly` 函数，让它们在遇到 `EAGAIN` 错误时，能重新运行 `accept`、`recv` 和 `send` 函数，这种做法是为访问非阻塞的套接字的过程增加的时间阻塞，本质上依然是阻塞的。

现在，我们已基于 `select` 实现的 I/O 多路同步复用机制修改了 `sim_server_run`。实际上，`select` 函数也是 I/O 阻塞的函数，因为它要轮询套接字集合，从中挑选可读、可写或存在异常的子集，只是这个过程的阻塞时间较短，通常可忽略不计。最重要的是，`select` 选出的套接字子集，将它们用于数据传输，几乎不可能出现

# 高性能的 threebody

现在，我们几乎有了一个全新的 `SimServer` 类了，基于它重新实现 threebody：

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
                sim_server_run(threebody);
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
                        msg = sim_str("threebody: Hi");
                        sim_server_send(threebody, msg);
                        sim_str_free(msg);
                        sim_server_close(threebody);
                }
        }
        sim_server_free(threebody);
        return 0;
}
```

得益于面向对象编程思想，我们对 sim-network.c 大刀阔斧的修改，对 threebody.c 的影响却微乎其微。

重新编译 threebody.c，像之前那样，只是现在还要联编 sim-list.c：

```console
$ gcc -I. sim-str.c sim-list.c sim-network.c threebody.c -o threebody
```

# 总结

我们胜利了！胜利的成果已全部写在最新的 [network.h](network.h)、[network.c](network.c) 和 [threebody.c](threebody.c) 中了。
