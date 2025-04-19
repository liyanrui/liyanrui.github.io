#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/select.h>
#include "sim-macros.h"
#include "sim-list.h"
#include "sim-network.h"

struct sim_client {
        int server;   /* 服务端套接字 */
        const char *error;
};

struct sim_server {
        int listener;
        SimList *clients;
        SimList *invalid_clients;
        fd_set read_fds;
        fd_set write_fds;
        const char *error;
};

static int socket_nonblock(int x) {
    return fcntl(x, F_SETFL, fcntl(x, F_GETFL) | O_NONBLOCK);
}

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

static SimList *clean_clients(SimList *clients, SimList *invalid_clients) {
        for (SimList *it = invalid_clients; it; it = it->next) {
                int client = *(int *)it->data;
                SimList *t = find_node(clients, client);
                if (t) clients = sim_list_delete(clients, t);
                close(client);
        }
        return clients;
}

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

typedef int (*AddrSelector)(int sockfd, 
                            const struct sockaddr *addr, 
                            socklen_t addrlen);


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

void sim_client_free(SimClient *client) {
        if (client) {
                close(client->server);
                free(client);
        } else {
                fprintf(stderr, "sim_client_free error: NULL pointer!\n");
        }
}

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
        server->invalid_clients = NULL;
        server->error = NULL;
        return server;
}

static void clients_free(SimList *clients) {
        if (clients) {
                for (SimList *it = clients; it; it = it->next) {
                        if (it->data) free(it->data);
                }
                sim_list_free(clients);
        }
}

void sim_server_free(SimServer *server) {
        if (server) {
                close(server->listener);
                clients_free(server->clients);
                clients_free(server->invalid_clients);
                free(server);
        }
}

SimList *sim_server_clients(SimServer *self) {
        if (self) return self->clients;
        else return NULL;
}

void sim_server_run_once(SimServer *self) {
        /* 清理无效的套接字 */
        self->clients = clean_clients(self->clients,
                                      self->invalid_clients);
        
        /* 释放 invalid_clients */
        for (SimList *it = self->invalid_clients; it; it = it->next) {
                free(it->data);
        }
        sim_list_free(self->invalid_clients);
        self->invalid_clients = NULL;

        /* 基于 select 的同步 I/O 多路复用 */
        int fd_max;
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

static int send_robustly(int x, SimStr *str) {
        const char *buffer = sim_str_share(str);
        size_t n = sim_str_size(str); /* buffer 字节数 */
        size_t m = 0; /* 发送了多少字节 */
        size_t remaining = n; /* 还有多少字节未发送 */
        while (m < n) {
                ssize_t t = send(x, buffer + m, remaining, 0);
                if (t == -1) {
                        if (errno == EAGAIN || errno == EINTR) continue;
                        else break;
                }
                m += t;
                remaining -= t;
        }
        return (m < n) ? -1 : 0;
}

SimStr *recv_robustly(int x) {
        size_t m = 1024;
        char *data = malloc(m * sizeof(char));
        size_t n = 0; /* 已接收的字节数 */
        while (1) {
                if (n == m) { /* 扩容 */
                        m *= 2;
                        data = realloc(data, m);
                        if (!data) {
                                free(data);
                                return NULL;
                        }
                }
                size_t remaining = m - n; /* 剩余空间长度 */
                ssize_t h = recv(x, data + n, remaining, 0);
                if (h == -1) {
                        if (errno == EAGAIN || errno == EINTR) continue;
                        else {
                                free(data);
                                return NULL;
                        }
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

void sim_client_send(SimClient *self, SimStr *str) {
        if (send_robustly(self->server, str) == -1) {
                self->error = "sim_client_send error!";
        } else {
                /* 恢复 self 正常 */
                if (self->error) self->error = NULL;
        }
}

void sim_server_send(SimServer *self, int client, SimStr *data) {
        if (FD_ISSET(client, &self->write_fds)) {
                if (send_robustly(client, data) == -1) {
                        sim_server_invalid_client(self, client);
                        self->error = "sim_client_send error!";
                } else {
                        self->error = NULL;
                }
        }
}

SimStr *sim_client_receive(SimClient *self) {
        SimStr *str = recv_robustly(self->server);
        if (!str) {
                self->error = "sim_client_receive error!";
        } else {
                /* 恢复 self 正常 */
                if (self->error) self->error = NULL;
        }
        return str;
}

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

bool sim_client_safe(SimClient *self) {
        SIM_OBJECT_SAFE(self);
}

bool sim_server_safe(SimServer *self) {
        SIM_OBJECT_SAFE(self);
}

