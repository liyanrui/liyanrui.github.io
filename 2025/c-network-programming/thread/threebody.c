#include <unistd.h>
#include <pthread.h>
#include "sim-network.h"

void *handler(void *arg) {
        /* 类型转换 */
        SimServer *threebody_copy = arg;

        /* 从客户端接收数据 */
        SimStr *msg_from = sim_server_receive(threebody_copy);
        if (sim_str_safe(msg_from)) {
                printf("%s\n", sim_str_raw(msg_from));
        }
        sim_str_free(msg_from);
        
        /* 向客户端发送信息 */
        SimStr *msg_to = sim_str("threebody: Hi!");
        sim_server_send(threebody_copy, msg_to);
        sim_str_free(msg_to);
        
        /* 关闭客户端，释放 threebody 副本 */
        sim_server_close(threebody_copy);
        free(threebody_copy);
        return NULL;
}

int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        while (1) {
                sim_server_run_once(threebody);
                /* 创建新的线程 */
                pthread_t t;
                SimServer *threebody_copy = sim_server_copy(threebody);
                pthread_create(&t, NULL, handler, threebody_copy);
                pthread_detach(t);
        }
        sim_server_free(threebody);
        return 0;
}
