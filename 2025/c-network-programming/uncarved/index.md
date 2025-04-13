---
title: 返朴
abstract: 朴散则为器。
date: 2025 年 04 月 13 日
...

# 前言

一路行来，甚为艰难，所幸见识和遐想颇多。进程、线程及至协程，我并无计划将这些技术融入 Sim 项目。这些技术都很有用处，可它们过于聪明了。驾驭它们，如同驯服野马，这与 Sim 项目的本意是矛盾的。Sim 项目只能在你企图驯服这些野马时，为你提供马鞍和缰绳。

实际上现在的 sim-network.c 里已经有一些聪明的技术了，例如「[再封装](../wrapper-again/index.html)」里为 `SimServer` 对象实现的 `receive` 和 `send` 方法，它们不该设计成内部已有驯服的野马在奔跑的样子。我们应该删除这些聪明之处，不然此时的聪明，在未来可能是自作聪明的绊脚石。

本文以「[再封装](../wrapper-again/index.html)」中的 Sim 项目的实现为底本，清理一些代码，并增加一些方法和宏，让 Sim 项目趋向于提供简洁的机制，将如何使用这套机制编写可用的网络应用程序的自由交给它的用户。即便用户只有我一个人。

# 简化


首先，将 `SimServer` 对象的 `receive` 和 `send` 方法修改为面向给定的客户端套接字的数据收发过程。

```c
/* sim-network.h [改] */
SimStr *sim_server_receive(SimServer *self, int client);
void sim_server_send(SimServer *self, int client, SimStr *data);
```

```c
/* sim-network.c [改] */
SimStr *sim_server_receive(SimServer *self, int client) {
        if (client < 0) return NULL;
        if (FD_ISSET(client, &self->read_fds)) {
                SimStr *msg = recv_robustly(client);
                if (msg) return msg;
                else {
                        /* 客户端套接字无法读取，可能对端已经关闭了连接 */
                        /* 先将其标记为无效的客户套接字 */
                        sim_server_invalid_client(self, client);
                        return NULL;
                }
        }
}

void sim_server_send(SimServer *self, int client, SimStr *data) {
        if (FD_ISSET(client, &self->write_fds)) {
                if (send_robustly(client, data) == -1) {
                        sim_server_invalid_client(self, client);
                }
        }
}
```

上述代码中使用的 `SimServer` 的 `invalid_client` 方法尚未定义，现在你只需假设它存在。

在之前的 `receive` 和 `send` 方法的实现中，遇到无效的客户端套接字，我们是当场将从 `SimServer` 对象的 `clients` 链表中将其删除，这个过程不该交由一个仅负责数据接收或发送的函数完成，如同一个快递员给我送货，他不能因为没能联系到我，便自作聪明，认为我这个人并不存在。网络通信中，收发数据的函数，将无效的套接字暂且记录下来，最终交由 `SimServer` 对象在适当的时机予以处理，更为妥当。

还需要注意，「[再封装](../wrapper-again/index.html)」中实现的 `receive` 和 `send` 方法，判断客户端套接字是否有效时，用的是严格的条件，即要求客户端套接字能同时满足可读和可写，即

```c
if (FD_ISSET(client, &read_fds) && FD_ISSET(client, &write_fds)) {
        /* 数据接收或发送过程 */
        ... ... ... ... ... ...
}
```

这个条件，当初在「[select 不负重望](../socket-and-select/index.html#你上当了)」中引入，现在我们放弃了。服务端从一个客户端套接字读数据，只能要求该套接字在 `read_fds` 之内，反之向该客户端套接字写入数据，只能要求它在 `write_fds` 之内。 

通过上述修改，你是否发现，我们之前实在是太过于追求聪明而创造了许多非自然的功能？

# 无效的客户端

`SimServer` 对象在收发数据的过程中会遇到一些无效的客户端套接字，上一节的处理方式是将其标记为无效。我们可以在 `SimServer` 类里新设一个单向链表对象 `invalid_clients`，出现于该链表中的客户端套接字即为无效。

```c
/* sim-network.c [改] */
struct sim_server {
        int listener;
        SimList *clients;
        SimList *invalid_clients;
        fd_set read_fds;
        fd_set write_fds;
        const char *error;
};
```

```c
/* sim-network.c [改] */
SimServer *sim_server(const char *host, const char *port) {
        /* [上文] server->clients = NULL; */
        server->invalid_clients = NULL;
        /* [下文] server->error = NULL; */
}
```

在上述修改中，我在「[C 面向对象变成](../coop/index.html)」创造的示意标记的基础上，又创造了一种新的示意标记，即在所修改的代码中，根据上述 `[上文]` 和 `[下文]` 标记中的代码找到相应的两个位置，在它们中间插入代码。我觉得，你应该能理解我在说什么，假如你也不想让我将函数的原有代码都复制过来，然后在其中悄悄插入一行代码。

`SimServer` 的析构函数也要修改，如下：

```c
/* sim-network.c [改] */
static void clients_free(SimList *clients);

void sim_server_free(SimServer *server) {
        if (server) {
                close(server->listener);
                clients_free(self->clients);
                clients_free(self->invalid_clients);
                free(server);
        }
}

static void clients_free(SimList *clients) {
        if (clients) {
                for (SimList *it = server->clients; it; it = it->next) {
                        if (it->data) free(it->data);
                }
                sim_list_free(clients);
        }
}
```

`sim_server_invalid_client` 的声明和实现如下：

```c
/* sim-network.h ++ */
void sim_server_invalid_client(SimServer *self, int client);
```

```c
/* sim-network.c ++ */
void sim_server_invalid_client(SimServer *self, int client) {
        bool existed = false;
        for (SimList *it = self->invalid_clients; it; it = it->next) {
                int fd = *(int *)it->data;
                if (fd == client) {
                        existed = true;
                        break;
                }
        }
        if (!existed) {
                SIM_LIST_ADD(self->invalid_clients, client, int);
        }
}
```

在每一次运行 `SimServer` 的 `run` 方法时，首先需要基于 `invalid_clients`，将 `clients` 中的无效客户端套接字删除，这个过程需要一个专用的函数：

```c
/* sim-network.h ++ */
static SimList *find_node(SimList *list, int fd);

static SimList *clean_clients(SimList *clients, SimList *invalid_clients) {
        for (SimList *it = invalid_clients; it; it = it->next) {
                int client = *(int *)it->data;
                SimList *t = find_node(clients, client);
                if (t) clients = sim_list_delete(clients, t);
                close(client);
        }
        return clients;
}
```

其中，`find_node` 可从链表中寻找含有指定套接字的结点：

```c
/* sim-network.h ++ */
static SimList *find_node(SimList *list, int fd) {
        SimList *t = NULL;
        for (SimList *it = list; it; it = it->next) {
                int a = *(int *)it->data;
                if (a == fd) {
                        t = it;
                        break;
                }
        }
        return t;
}
```

上述一系列操作，体现了单向列表在查找和删除某个结点时过于耗费时间。不过，现在依然可以容忍。

现在，可以修改 sim-network.c 中的 `sim_server_run` 的定义了，在其开头增加以下代码片段：

```c
/* sim-network.c ++ */
void sim_server_run(SimServer *self) {
        /* 清理无效的套接字 */
        SimList *invalid_clients = self->invalid_clients;
        self->clients = clean_clients(self->clients, invalid_clients);
        
        /* 释放 invalid_clients */
        for (SimList *it = invalid_clients; it; it = it->next) {
                free(it->data);
        }
        sim_list_free(invalid_clients);
        self->invalid_clients = NULL;
        
        /* [下文] 
        int fd_max;
        while (1) {
                FD_ZERO(&self->read_fds); */
}
```

# 公之于众

由于 `SimServer` 对象的 `recieve` 和 `send` 方法需要给定客户端套接字方能使用，我们必须将 `SimServer` 对象的 `clients` 公开给用户：

```c
/* sim-network.h ++ */
SimList *sim_server_clients(SimServer *self);
```

```c
/* sim-network.c ++ */
SimList *sim_server_clients(SimServer *self) {
        if (self) return self->clients;
        else return NULL;
}
```

用户需要记住，在遍历客户端链表过程中，对于任一结点 `it`，需要使用类似以下语句，提取客户端套接字：

```c
int client = *(int *)it->data;
```

也许会有无数软件工程爱好者会告诫我，这样暴露了太多细节，程序会非常不安全。我承认，事实的确如此，但我还是愿意相信 Sim 库的用户都希望自己所写的代码是安全的，毕竟他们在生活中的大多数时间是不会闯红灯的。

# 新的 threebody

现在，为了验证上述修改是否正确并且演示新的 `SimServer` 类的用法，我们需要重写 threebody 程序，如下

```c
#include "sim-network.h"
int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        /* 服务端程序运转 */
        SimStr *hi = sim_str("threebody: hi!");
        while (1) {
                sim_server_run(threebody);
                if (!sim_server_safe(threebody)) continue;
                
                SimList *clients = sim_server_clients(threebody);
                printf("%lu clients!\n", sim_list_size(clients));
                for (SimList *it = clients; it; it = it->next) {
                        int client = *(int *)it->data;
                        /* 从客户端接收信息 */
                        SimStr *msg = sim_server_receive(threebody, client);
                        if (msg) {
                                printf("%s\n", sim_str_raw(msg));
                                sim_str_free(msg);
                        } else continue;
                        /* 向客户端发送信息 */
                        sim_server_send(threebody, client, hi);
                }
        }
        sim_str_free(hi);
        sim_server_free(threebody);
        return 0;
}
```

编译 threebody.c：

```console
$ gcc -I. sim-str.c sim-list.c sim-network.c threebody.c -o threebody
```

运行 threebody：

```console
$ ./threebody
```

然后运行「[两朵乌云](../blocking/index.html)」中的 ywj 和 other-ywj，除了 threebody 能够正常与她们应答之外，也能看到 threebody 输出的当前客户端连接数量，无论运行多少次 ywj，这个数量绝大多数时间是 1，说明我们为 `SimServer` 对象增加的清除无效客户端套接字的功能是有效的。

# 宏

我们需要为 Sim 项目新建一个头文件 sim-macros.h，用于定义一些常用的宏，这些宏有一些是只在 Sim 项目内部使用的，也有一些供 Sim 库的用户使用，例如协程宏。

我们先为 `sim_str_safe`，`sim_client_safe`，`sim_server_safe` 以及未来一些新的类，定义一个通用的安全检测宏：

```c
/* sim-macros.h */
#ifndef SIM_MACROS_H
#define SIM_MACROS_H

/* 只在 Sim 项目内部使用 */
#define SIM_OBJECT_SAFE(object) do { \
       if (self) { \
                /* 不太致命的错误 */ \
                if (self->error) { \
                        fprintf(stderr, "%s error: %s\n", \
                                __func__, self->error); \
                        return false; \
                } \
        } else { \
                /* 致命错误 */ \
                fprintf(stderr, "%s error: NULL pointer!", __func__); \
                return false; \
        } \
        return true; \
} while (0)

#endif
```

注意，上述代码所用的 `__func__` 是 C99 标准引入的标识符，它是一个字符串常量，表示当前代码所属函数的名字。例如，若在 `sim_str_safe` 中使用 `SIM_OBJECT_SAFE`，则 `__func__` 便是 `sim_str_safe`。

现在修改 Sim 项目中现有的全部 `safe` 方法：

```c
/* sim-str.c ++ */
#include "sim-macros.h"
```

```c
/* sim-str.c [改] */
bool sim_str_safe(SimStr *self) {
        SIM_OBJECT_SAFE(self);
}
```

```c
/* sim-network.c ++ */
#include "sim-macros.h"
```

```c
/* sim-network.c [改] */
bool sim_client_safe(SimStr *self) {
        SIM_OBJECT_SAFE(self);
}

bool sim_server_safe(SimServer *self) {
        SIM_OBJECT_SAFE(self);
}
```

这个宏，现在已经让我们可以少写大约 30 行非常无趣的代码了。要善用宏！

# 协程

我们将基于 duff 设备的协程宏也放在 sim-macros.h 中。在 Sim 项目中，我们不使用协程，在基于 Sim 库编写网络应用程序时，我们以及其他用户，可以使用协程。

在「[天地一指](../coroutine/index.html)」中指出，若让协程具备可重入性，它只能以闭包的形式出现，而所谓闭包，就是一个函数，如果它的参数是指针，在其内部能够通过这个指针修改外部变量的值。在 C 语言编程中，这原本是一种极为常见函数形式，理论家们将其命名为闭包，反而非常难以理解了。

闭包形式的协程，其参数通常至少需要 3 个指针，例如

```c
void foo(int *state, int *i, void *data) {
        /* 闭包形式的协程 */
        ... ... ... ... ...
}
```

倘若 `foo` 函数还需要其他参数，因参数过多，便会导致该函数的意义难以被他人理解。我们可以将与协程有关的参数聚拢为一个结构体对象：

```c
/* sim-macros.h ++ */
typedef struct sim_cr SimCr;
```

`struct sim_cr` 可以根据自己的需要予以定义，例如针对上述协程 `foo` 的参数结构体可定义为 

```c
struct sim_cr {
        int state;
        int i;
        void *data;
        bool done;
};
```

其中 `state` 用于控制协程状态，`done` 用于表示协程是否终止。在使用上述结构体时，只需构造一个参数结构体对象，然后将该对象的地址传给 `foo`：

```c
/* 定义协程 foo */
void foo(SimCr *cr) {
        /* 闭包形式的协程 */
        ... ... ... ... ...
        /* 协程结束 */
        cr->done = true;
}

/* 调用协程 */
SimCr cr = {0, 0, NULL, false};
foo(&cr);
```

定义结构体 `sim_cr` 时，结构体一定要有 `state` 和 `done`，切记，切记，切记！至于其他成员，要根据具体需求而定，例如

```c
struct sim_cr {
        int state;
        SimList *it;
        SimStr *str;
        bool done;
};
```

基于 `SimCr` 类型，便可定义闭包形式的协程宏：

```c
/* sim-macros.h ++ */
#define sim_cr_begin(cr) switch (cr->state) { case 0:

#define sim_cr_yield(cr,...) do {cr->state = __LINE__; \
                                 return __VA_ARGS__; \
                                 case __LINE__: ; } while (0)

#define sim_cr_end(cr) } cr->done = true;
```

# 叮咚或咚叮

对于阻塞的套接字，我们曾试验过基于进程和线程的并发，即为服务端接受的每个客户端分配一个进程或线程与之通信。现在我们已经基本理解了协程，不妨大胆构想，在基于非阻塞的套接字和同步 I/O 多路复用机制实现的服务端中，若为每个客户端分配一个协程，结果会如何？

我们再次以 threebody 为例，试试这个想法。

```c
/* threebody.c */
#include "sim-macros.h"
#include "sim-network.h"
```

我们将协程必须的参数定义为

```c
/* threebody.c ++ */
struct sim_cr {
        int state;
        bool done;
        SimStr *hi;
        char *ding;
        char *dong;
};
```

然后定义一个可与客户端通信的协程：

```c
/* threebody.c ++ */
static void handle_client(SimCr *cr, SimServer *server, int client) {
        sim_cr_begin(cr);
        while (!cr->ding || !cr->dong) {
                if (!cr->ding) {
                        SimStr *msg = sim_server_receive(server, client);
                        if (msg) {
                                cr->ding = "叮";
                                printf("%s\n", sim_str_raw(msg));
                                sim_str_free(msg);
                        }
                }
                if (!cr->dong) {
                        sim_server_send(server, client, cr->hi);
                        if (sim_server_safe(server)) cr->dong = "咚";
                }
                sim_cr_yield(cr);
        }
        sim_cr_end(cr);
}
```

在 `main` 函数的一开始，我们定义一个指针数组 `coroutines`，并将其所有元素初始化为 `NULL`。在服务端运转过程中，每次若出现新的客户端套接字，便在 `coroutines` 的对应槽位为其分配协程参数，然后运行协程 `handle_client`。若某个协程结束，便将其槽位清空。

```c
/* threebody.c ++ */
int main(void) {
        SimCr **coroutines = malloc(1024 * sizeof(SimCr *));
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        /* 服务端程序运转 */
        SimStr *hi = sim_str("threebody: hi!");
        SimCr cr_init = {0, false, hi, NULL, NULL};
        while (1) {
                sim_server_run(threebody);
                if (sim_server_safe(threebody)) {
                         /* 协程调度 */
                        SimList *clients = sim_server_clients(threebody);
                        for (SimList *it = clients; it; it = it->next) {
                                int client = *(int *)it->data;
                                SimCr *cr = coroutines[client];
                                if (cr) {
                                        if (cr->done) {
                                                /* 释放协程参数 */
                                                free(cr);
                                                coroutines[client] = NULL;
                                        } else {
                                                /* 运行协程 */
                                                handle_client(cr, threebody, client);
                                        }
                                } else {
                                        /* 分配协程参数 */
                                        cr = malloc(sizeof(SimCr));
                                        *cr = cr_init;
                                        coroutines[client] = cr;
                                        /* 运行协程 */
                                        handle_client(cr, threebody, client);
                                }
                        }
                }
        }
        sim_str_free(hi);
        for (int i = 0; i < 1024; i++) {
                SimCr *cr = coroutines[i];
                if (cr) free(cr);
        }
        free(coroutines);
        sim_server_free(threebody);
        return 0;
}
```

也许上述 threebody.c 是自古以来最为复杂的一个，它与之前的版本最显著的区别是，像 other-ywj 这样迟疑的客户端，即使她尚未给 threebody 发送数据，threebody 会先回复她，然后继续等她发送数据，于是协程参数里的 `dong` 先有值，而 `ding` 则后有值，亦即「咚叮」。像 ywj 这样毫不迟疑的客户端，则是「叮咚」。

若着实不理解这个协程版本的 threebody，也无妨，毕竟它过于聪明了……不过，它也非常愚蠢，用了有着 1024 个槽位的协程参数表。之所以是 1024，因为 `select` 实现的同步 I/O 多路复用机制最多能允许服务端接受 1024 个客户端的连接。若对其改进，可用链表或动态数组。

# 总结

作为一个创造者，主要任务不仅是消除作品中的愚蠢之处，同样也要消除其聪明之处。大器免成，大巧若拙，是设计者能致以用户的最大敬意。每个人都有其过人之处。若你觉得自己是个聪明的人，不过是因为你向来过于关心自己。

下面是最新的 Sim 项目的一切：

* [sim-macros.h](sim-macros.h)
* [sim-str.h](sim-str.h) 和 [sim-str.c](sim-str.c)
* [sim-list.h](sim-list.h) 和 [sim-list.c](sim-list.c)
* [sim-network.h](sim-network.h) 和 [sim-network.c](sim-network.c)
* [threebody.c 协程版本](threebody.c)
