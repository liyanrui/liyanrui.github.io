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
                        if (FD_ISSET(client, &read_fds) && FD_ISSET(client, &write_fds)) {
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
                        if (FD_ISSET(client, &read_fds) && FD_ISSET(client, &write_fds)) {
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
