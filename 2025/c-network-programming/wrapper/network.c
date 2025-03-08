#include "network.h"

static struct addrinfo *get_address_list(const char *host,
                                         const char *port) {
        struct addrinfo hints, *addr_list;
        memset(&hints, 0, sizeof(struct addrinfo));
        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        int a = getaddrinfo(host, port, &hints, &addr_list);
        if(a != 0) {
                fprintf(stderr, "getaddrinfo error: %s\n", gai_strerror(a));
                exit(-1);
        }
        return addr_list;
}

static Socket *socket_new(void) {
        Socket *result = malloc(sizeof(Socket));
        result->listen = -1;
        result->connection = -1;
        result->host_size = NI_MAXHOST * sizeof(char);
        result->host = malloc(result->host_size);
        result->port_size = NI_MAXSERV * sizeof(char);
        result->port = malloc(result->port_size);
        return result;
}

Socket *client_socket(const char *host, const char *port) {
        Socket *result = socket_new();
        struct addrinfo *addr_list = get_address_list(host, port);
        int fd = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                fd = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (fd == -1) continue;
                if (connect(fd, it->ai_addr, it->ai_addrlen) == -1) {
                        close(fd);
                        continue;
                }
                getnameinfo(it->ai_addr, it->ai_addrlen,
                            result->host, result->host_size,
                            result->port, result->port_size,
                            NI_NUMERICHOST | NI_NUMERICSERV);
                break;
        }
        freeaddrinfo(addr_list);
        
        result->connection = fd;
        return result;
}

Socket *server_socket(const char *host, const char *port) {
        struct addrinfo *addr_list = get_address_list(host, port);
        int fd = -1;
        for (struct addrinfo *it = addr_list; it; it = it->ai_next) {
                fd = socket(it->ai_family, it->ai_socktype, it->ai_protocol);
                if (fd == -1) continue;
                if (bind(fd, it->ai_addr, it->ai_addrlen) == -1) {
                        close(fd);
                        continue;
                }
                break;
        }
        freeaddrinfo(addr_list);
        
        if (fd == -1) {
                fprintf(stderr, "failed to bind!\n");
                exit(-1);
        }
        if (listen(fd, 10) == -1) {
                fprintf(stderr, "failed to listen!\n");
                exit(-1);
        }
        
        Socket *result = socket_new();
        result->listen = fd;
        return result;
}

void server_socket_accept(Socket *x) {
        struct sockaddr_storage addr;
        socklen_t addr_len = sizeof(addr);
        x->connection = accept(x->listen, (struct sockaddr *)(&addr), &addr_len);
        if (x->connection == -1) {
                fprintf(stderr, "failed to accept!\n");
                exit(-1);
        }
        getnameinfo((struct sockaddr *)&addr, addr_len,
                    x->host, x->host_size,
                    x->port, x->port_size,
                    NI_NUMERICHOST | NI_NUMERICSERV);
}

void socket_send(Socket *x, const char *message) {
        if (send(x->connection, message, strlen(message), 0) == -1) {
                fprintf(stderr, "send error!\n");
                exit(-1);
        }
}

char *socket_recieve(Socket *x) {
        size_t m = 1024;
        char *buffer = malloc(m * sizeof(char));
        size_t n = 0;
        char *total = NULL;
        while (1) {
                ssize_t h = recv(x->connection, buffer, m, 0);
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

void socket_free(Socket *x) {
        free(x->host);
        free(x->port);
        free(x);
}

