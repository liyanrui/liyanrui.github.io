#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#include "sim-network.h"

void handler(int signal) {
        wait(NULL);
}

int main(void) {
        /* SIGCHLD 信号处理 */
        struct sigaction sa;
        sa.sa_handler = handler;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = SA_RESTART|SA_NOCLDSTOP;
        int a = sigaction(SIGCHLD, &sa, NULL);
        if (a == -1) {
                fprintf(stderr, "sigaction error!\n");
                exit(-1);
        }
        /* 服务端 */
        SimServer *threebody = sim_server("localhost", "8080");
        while (1) {
                sim_server_run(threebody);
                if (fork() == 0) {
                         /* 子进程不需要监听套接字 */
                        sim_server_close_listener(threebody);
                        /* 从客户端接收数据 */
                        SimStr *msg_from = sim_server_receive(threebody);
                        if (sim_str_safe(msg_from)) {
                                printf("%s\n", sim_str_raw(msg_from));
                        }
                        sim_str_free(msg_from);
                        /* 向客户端发送信息 */
                        SimStr *msg_to = sim_str("threebody: Hi!");
                        sim_server_send(threebody, msg_to);
                        sim_str_free(msg_to);
                        sim_server_close_client(threebody);
                        exit(0);
                }
                /* 父进程不需要客户端套接字 */
                sim_server_close_client(threebody);
        }
        sim_server_free(threebody);
        return 0;
}
