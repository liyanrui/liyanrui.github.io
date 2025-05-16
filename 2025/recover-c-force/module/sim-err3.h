#ifndef SIM_ERR3_H
#define SIM_ERR3_H
#include <sim-array.h>

typedef SimArray * SimErr3;
extern SimErr3 sim_err3;

#define SIM_ERR3(v) do { \
        if (!sim_err3) sim_err3 = sim_array(SimStr *); \
        SimStr *e = sim_str(__func__); \
        sim_str_suffix(e, " error: "); \
        sim_str_suffix(e, (v)); \
        sim_array_add(sim_err3, e, SimStr *); \
} while (0)

#define SIM_ASSERT_RET(bool_exp, ...) do { \
        if (!(bool_exp)) { \
                SIM_ERR3("invalid assertion!"); \
                return __VA_ARGS__; \
        } \
} while (0)

#define SIM_PRINT_ERR_AND_RESET do { \
        for (size_t i = 0; i < sim_err3->n; i++) { \
                SimStr *e = sim_array_raw(sim_err3, i, SimStr *); \
                printf("%s\n", sim_str_raw(e)); \
                sim_str_free(e); \
        } \
        sim_array_free(sim_err3); \
        sim_err3 = NULL; \
} while (0)

#define SIM_MER3 if (sim_err3) { \
                SIM_PRINT_ERR_AND_RESET; \
        } else

#define SIM_ASK3 SIM_MER3{}

#define SIM_PUN3 if (sim_err3) { \
                SIM_PRINT_ERR_AND_RESET; \
        } \
        exit(-1);
#endif
