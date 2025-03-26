#include "network.h"

int main(void) {
        Socket *x = server_socket("localhost", "8080");
        /* 接受无数个连接 */
        while (1) {
                server_socket_accept(x);
                { /* 从 x 读取信息 */
                        char *msg = socket_receive(x);
                        printf("%s:%s said: %s\n", x->host, x->port, msg);
                        free(msg);
                }
                socket_send(x, "Hi, I received your message!");
                close(x->connection);
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
