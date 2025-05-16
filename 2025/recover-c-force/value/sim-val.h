#ifndef SIM_VAL_H
#define SIM_VAL_H
#include <sim-err3.h>

typedef struct sim_val SimVal;
SimVal *SIM_VAL(void *data, size_t u);
#define sim_val(anyone, type) SIM_VAL(&anyone, sizeof(type))
void sim_val_free(SimVal *val);

const void *SIM_VAL_RAW(SimVal *val, size_t u);
#define sim_val_raw(val, type) \
                *(type *)SIM_VAL_RAW(val, sizeof(type))
#endif
