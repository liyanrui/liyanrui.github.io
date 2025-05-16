#ifndef SIM_ERR2_H
#define SIM_ERR2_H
#include <sim-str.h>

typedef SimStr * SimErr2;
extern SimErr2 sim_err2;

#define SIM_ERR2(v) do { \
        if (sim_err2) { /* 清空 sim_err2 */ \
                sim_str_del(sim_err2, 0, sim_str_size(sim_err2)); \
        } else {  /* 初始化 sim_err2 */ \
                sim_err2 = sim_str(NULL); \
        } \
        sim_str_suffix(sim_err2, __func__); \
        sim_str_suffix(sim_err2, " error: "); \
        sim_str_suffix(sim_err2, (v)); \
} while (0)

#define SIM_MER2 if (sim_str_size(sim_err2) > 0) { \
        if (sim_err2) { \
                fprintf(stderr, "%s\n", sim_str_raw(sim_err2)); \
                sim_str_del(sim_err2, 0, sim_str_size(sim_err2)); \
        } else { /* 初始化 sim_err2 */ \
                sim_err2 = sim_str(NULL); \
        } \
} else
#define SIM_ASK2 SIM_MER2{}

#define SIM_PUN2 if (sim_str_size(sim_err2) > 0) { \
        if (sim_err2) { \
                fprintf(stderr, "%s\n", sim_str_raw(sim_err2)); \
                sim_str_free(sim_err2); \
        } \
        exit(-1); \
}
#endif
