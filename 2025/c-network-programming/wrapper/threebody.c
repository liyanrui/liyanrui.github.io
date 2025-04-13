#include "sim-network.h"

int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        sim_server_run(threebody);
        
        /* 从客户端读取信息 */
        SimStr *msg_from = sim_server_receive(threebody);
        if (sim_str_safe(msg_from)) {
                printf("%s\n", sim_str_raw(msg_from));
        }
        sim_str_free(msg_from);
        
        /* 向客户端发送信息 */
        SimStr *msg_to = sim_str("threebody: Hi!");
        sim_server_send(threebody, msg_to);
        sim_str_free(msg_to);
        sim_server_close(threebody);
        
        /* 析构，退出 */
        sim_server_free(threebody);
        return 0;
}
