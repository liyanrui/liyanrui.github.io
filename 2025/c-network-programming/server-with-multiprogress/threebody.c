#include "network.h"

int main(void) {
        Socket *x = server_socket("localhost", "8080");
        observer_init();
        while (1) {
                server_socket_accept(x);
                if (fork() == 0) {
                        close(x->listen); /* 子进程不需要该 socket */
                        /* 从 x 读取信息 */
                        char *msg = socket_receive(x);
                        printf("%s:%s said: %s\n", x->host, x->port, msg);
                        free(msg);
                        /* 从 x 发送信息 */
                        socket_send(x, "Hi, I received your message!");
                        sleep(10);
                        exit(0);
                }
                close(x->connection); /* 父进程不需要该 socket */
        }
        /* 关闭监听 socket 并释放 x */
        close(x->listen);
        socket_free(x);
        return 0;
}
