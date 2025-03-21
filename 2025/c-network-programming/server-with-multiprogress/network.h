#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/wait.h>

void observer_init(void);

typedef struct {
        int listen;
        int connection;
        char *host;
        size_t host_size;
        char *port;
        size_t port_size;
} Socket;

Socket *client_socket(const char *host, const char *port);
Socket *server_socket(const char *host, const char *port);

int socket_nonblock(int x);
void server_socket_accept(Socket *x);

void socket_send(Socket *x, const char *message);
char *socket_receive(Socket *x);

void socket_free(Socket *x);

