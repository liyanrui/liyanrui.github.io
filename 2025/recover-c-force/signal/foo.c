#include "foo.h"

SimVal *foo_hi(void) {
        int a = 3;
        return sim_val(a, int);
}

SimVal *foo_router(SimSig sig, SimVal *v) {
        if (sim_sig_is(sig, "hi")) {
                return foo_hi();
        }
        return NULL;
}
