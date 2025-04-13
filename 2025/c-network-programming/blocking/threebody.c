#include "sim-network.h"
int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        /* 服务端程序运转 */
        while (1) {
                sim_server_run(threebody);
                if (sim_server_safe(threebody)) { 
                        /* 从客户端接收信息 */
                        SimStr *msg = sim_server_receive(threebody);
		        if (msg) {
                                printf("%s\n", sim_str_raw(msg));
                                sim_str_free(msg);
			}
                        /* 向客户端发送信息 */
                        msg = sim_str("threebody: Hi");
                        sim_server_send(threebody, msg);
                        sim_str_free(msg);
                        sim_server_close(threebody);
                }
        }
        sim_server_free(threebody);
        return 0;
}
