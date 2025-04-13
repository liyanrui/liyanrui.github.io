#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/select.h>
#include "sim-network.h"

struct sim_client {
        int server;   /* 服务端套接字 */
        const char *error;
};

struct sim_server {
        int listener;       /* 用于监听的套接字 */
        fd_set read_fds;    /* 可读文件描述符集 */
        fd_set write_fds;   /* 可写文件描述符集 */
        int fd_max;         /* 监控的文件描述符最大值 */
        const char *error;
};

static int socket_nonblock(int x) {
    return fcntl(x, F_SETFL, fcntl(x, F_GETFL) | O_NONBLOCK);
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
        server->error = NULL;
        FD_ZERO(&server->read_fds);
        FD_ZERO(&server->write_fds);
        FD_SET(fd, &server->read_fds);
        server->fd_max = fd;
        return server;
}

void sim_server_free(SimServer *server) {
        if (server) {
                close(server->listener);
                free(server);
        } else {
                fprintf(stderr, "sim_server_free error: NULL pointer!\n");
        }
}

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
                                if (errno == EINTR) continue;
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
                        if (errno == EINTR) continue;
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
        return sim_str_absorb(data, m, n);
}

void sim_client_send(SimClient *self, SimStr *str) {
        if (send_robustly(self->server, str) == -1) {
                self->error = "sim_client_send error!";
        } else {
                /* 恢复 self 正常 */
                if (self->error) self->error = NULL;
        }
}

static void update_fd_max(SimServer *self) {
        int n = self->fd_max; 
        self->fd_max = -1;
        for (int i = 0; i < n + 1; i++) {
                if (FD_ISSET(i, &self->read_fds) 
                    || FD_ISSET(i, &self->write_fds)) {
                        if (i > self->fd_max) self->fd_max = i;
                }
        }
}

void sim_server_send(SimServer *self, SimStr *str) {
        bool changed = false;
        for (int i = 0; i < self->fd_max + 1; i++) {
                if (FD_ISSET(i, &self->write_fds)) {
                        if (send_robustly(i, str) == -1) {
                                FD_CLR(i, &self->write_fds);
                                FD_CLR(i, &self->read_fds);
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
                                FD_CLR(i, &self->write_fds);
                                close(i);
                                self->error = "invalid socket!";
                                changed = true;
                        }
                }
        }
        if (changed) update_fd_max(self);
        return strs;
}

bool sim_client_safe(SimClient *self) {
        if (self) {
                /* 不太致命的错误 */
                if (self->error) {
                        fprintf(stderr, "%s\n", self->error);
                        return false;
                }
        } else {
                /* 致命错误 */
                fprintf(stderr, "sim_client_safe error: NULL pointer!\n");
                return false;
        }
        return true;
}

bool sim_server_safe(SimServer *self) {
        if (self) {
                /* 不太致命的错误 */
                if (self->error) {
                        fprintf(stderr, "%s\n", self->error);
                        return false;
                }
        } else {
                /* 致命错误 */
                fprintf(stderr, "sim_server_safe error: NULL pointer!\n");
                return false;
        }
        return true;        
}

