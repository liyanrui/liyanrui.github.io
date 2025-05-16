#include "foo.h"
SIM_MOD_DEFINE(FooMod, foo_mod, SimMod, sim_mod);

Foo *foo(const char *anything) {
        Foo *a = malloc(sizeof(Foo));
        SIM_ASSERT_RET(a, NULL);
        a->mod = foo_mod();
        a->anything = anything;
        return a;
}
