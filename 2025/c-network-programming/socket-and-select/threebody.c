#include "network.h"

void with_ywj(Socket *x, void *user_data) {
        /* 读取客户端发来的信息 */
        char *msg = socket_receive(x);
        printf("%s:%s said: %s\n", x->host, x->port, msg);
        free(msg);
        /* 向客户端发送信息 */
        socket_send(x, "Hi, I received your message!");
}

int main(void) {
        /* 构造用于监听的 socket */
        Socket *x = server_socket("localhost", "8080");
        /* 通信 */
        while (1) {
                socket_communicate(x, with_ywj, NULL);
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
