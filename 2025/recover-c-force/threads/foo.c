#include "foo.h"
void foo_hi(void) {
        printf("hi, i am foo!\n");
}
SimVal *foo_router(SimSig sig, SimVal *v) {
        if (sim_sig_is(sig, "hi")) {
                foo_hi();
                return NULL;
        }
        return NULL;
}
