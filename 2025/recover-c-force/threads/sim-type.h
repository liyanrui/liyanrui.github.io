#ifndef SIM_TYPE_H
#define SIM_TYPE_H
#include <sim-err3.h>
#include <sim-array.h>
#include <sim-val.h>

extern SimArray *sim_types;
typedef size_t SimType;

typedef struct {
        SimType type;
        SimVal *value;
} SimVar;

SimType sim_type_reg(const char *name);

#define sim_var(type, anyone, raw_type) \
        (SimVar){(type == 0) ? (type = sim_type_reg(#type)) : type, \
                  sim_val(anyone, raw_type)}
#define sim_var_free(var) sim_val_free(var.value)

#define sim_var_raw(var, raw_type) \
        sim_val_raw(var.value, raw_type)

const char *SIM_TYPE_NAME(void *obj);
#define sim_type_name(obj) SIM_TYPE_NAME(&obj)
#endif
