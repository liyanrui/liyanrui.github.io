#include "sim-val.h"

struct sim_val {
        size_t u;
        char data[];
};

SimVal *SIM_VAL(void *data, size_t u) {
        if (!data || u == 0) {
                SIM_ERR3("invalid data!");
                return NULL;
        }
        SimVal *v = malloc(sizeof(SimVal) + u);
        if (!v) {
                SIM_ERR3("failed to allocate memory for SimVal object!");
                return NULL;
        }
        memcpy(v->data, data, u);
        v->u = u;
        return v;
}

void sim_val_free(SimVal *val) {
        if (val) free(val);
        else SIM_ERR3("invalid SimVal object!");
}

const void *SIM_VAL_RAW(SimVal *val, size_t u) {
        if (!val) {
                SIM_ERR3("invalid SimVal object!"); SIM_ASK3;
                return NULL;
        }
        if (val->u != u) {
                SIM_ERR3("type not mached!"); SIM_ASK3;
                return NULL;
        }
        return val->data;
}
