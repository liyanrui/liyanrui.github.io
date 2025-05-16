#include "sim-type.h"
SimArray *sim_types = NULL;

typedef struct sim_type_info {
        SimType type;
        const char *name;
} SimTypeInfo;

SimType sim_type_reg(const char *name) {
        if (!sim_types) sim_types = sim_array(sizeof(SimTypeInfo));
        if (!name) {
                SIM_ERR3("invalid type name");
                return 0;
        }
        SimType new_type;
        if (sim_types->n == 0) new_type = 1;
        else {
                SimTypeInfo tail = sim_array_raw(sim_types,
                                                 sim_types->n - 1, SimTypeInfo);
                new_type = tail.type + 1;
        }
        SimTypeInfo new_type_info = {new_type, name};
        sim_array_add(sim_types, new_type_info, SimTypeInfo);
        return new_type;
}

const char *SIM_TYPE_NAME(void *obj) {
        if (!sim_types) {
                SIM_ERR3("sim_types not existed!");
                return "";
        }
        if (obj == NULL) {
                SIM_ERR3("invalid type!");
                return "";
        }
        SimType type = *(SimType *)obj;
        SimTypeInfo t = sim_array_raw(sim_types, type - 1, SimTypeInfo);
        return t.name;
}
