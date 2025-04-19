#include "sim-macros.h"
#include "sim-network.h"

struct sim_cr {
        int state;
        bool done;
        SimStr *hi;
        char *ding;
        char *dong;
};

static void handle_client(SimCr *cr, SimServer *server, int client) {
        sim_cr_begin(cr);
        while (!cr->ding || !cr->dong) {
                if (!cr->ding) {
                        SimStr *msg = sim_server_receive(server, client);
                        if (msg) {
                                cr->ding = "叮";
                                printf("%s\n", sim_str_raw(msg));
                                sim_str_free(msg);
                                printf("叮\n");
                        }
                }
                if (!cr->dong) {
                        sim_server_send(server, client, cr->hi);
                        if (sim_server_safe(server)) {
                                cr->dong = "咚";
                                printf("咚\n");
                        }
                }
                sim_cr_yield(cr);
        }
        sim_cr_end(cr);
}

int main(void) {
        SimCr **coroutines = malloc(1024 * sizeof(SimCr *));
        SimServer *threebody = sim_server("localhost", "8080");
        if (!threebody) {
                fprintf(stderr, "sim_server failed!\n");
                exit(-1);
        }
        /* 服务端程序运转 */
        SimStr *hi = sim_str("threebody: hi!");
        SimCr cr_init = {0, false, hi, NULL, NULL};
        while (1) {
                sim_server_run_once(threebody);
                if (sim_server_safe(threebody)) {
                         /* 协程调度 */
                        SimList *clients = sim_server_clients(threebody);
                        for (SimList *it = clients; it; it = it->next) {
                                int client = *(int *)it->data;
                                SimCr *cr = coroutines[client];
                                if (cr) {
                                        if (cr->done) {
                                                /* 释放协程参数 */
                                                free(cr);
                                                coroutines[client] = NULL;
                                        } else {
                                                /* 运行协程 */
                                                handle_client(cr, threebody, client);
                                        }
                                } else {
                                        /* 分配协程参数 */
                                        cr = malloc(sizeof(SimCr));
                                        *cr = cr_init;
                                        coroutines[client] = cr;
                                        /* 运行协程 */
                                        handle_client(cr, threebody, client);
                                }
                        }
                }
        }
        sim_str_free(hi);
        for (int i = 0; i < 1024; i++) {
                SimCr *cr = coroutines[i];
                if (cr) free(cr);
        }
        free(coroutines);
        sim_server_free(threebody);
        return 0;
}
