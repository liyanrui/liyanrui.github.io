#include "bar.h"
SIM_MOD_DEFINE(BarMod, bar_mod, FooMod, foo_mod);
FooModTraits bar_traits = {
        (SimTrait)bar_zero,
        (SimTrait)bar_add
};

BarMod *bar_mod_init(void) {
        BarMod *mod = bar_mod();
        mod->traits = &bar_traits;
        return mod;
}

Bar *bar(double data) {
        Bar *a = malloc(sizeof(Bar));
        SIM_ASSERT_RET(a, NULL);
        a->mod = bar_mod_init();
        a->data = data;
        return a;
}

Bar *bar_zero(void) {
        Bar *a = bar(0);
        return a;
}

void bar_add(Bar *a, Bar *b) {
        SIM_ASSERT_RET(a && b);
        a->data += b->data;
}
