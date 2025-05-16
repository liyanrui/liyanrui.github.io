#include "sim-mod.h"
_Thread_local SimArray *sim_module_table = NULL;

static SimMod *this_mod = NULL;
SimMod *sim_mod(void) {
        SIM_MOD_REG(sim_module_table, this_mod, SimMod);
        return this_mod;
}

bool SimMod_is(SimMod *mod) {
        SIM_MOD_IS_COMMON(mod, this_mod);
}
