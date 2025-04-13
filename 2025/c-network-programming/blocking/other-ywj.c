#include <unistd.h>
#include "sim-network.h"
int main(void) {
        SimClient *ywj = sim_client("www.threebody.com", "8080");
        if (!ywj) {
                fprintf(stderr, "sim_client failed!\n");
                exit(-1);
        }
        /* 延迟 15 秒，模拟存在许多运算 */
        sleep(15);
        /* 发送数据 */
        SimStr *msg_to = sim_str("other ywj: I am here!");
        sim_client_send(ywj, msg_to);
        sim_str_free(msg_to);
        /* 从 www.threebody.com:8080 接收信息 */
        SimStr *msg_from = sim_client_receive(ywj);
        if (sim_str_safe(msg_from)) {
                printf("%s\n", sim_str_raw(msg_from));
        }
        sim_str_free(msg_from);
        
        /* 析构，退出 */
        sim_client_free(ywj);
        return 0;
}
