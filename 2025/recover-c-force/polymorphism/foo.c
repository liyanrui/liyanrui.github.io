#include "foo.h"
SIM_MOD_DEFINE(FooMod, foo_mod, SimMod, sim_mod);

Foo *foo_sum(SimArray *objects) {
        /* 从 objects 任取一个对象，获取特性表，并按协议转换 */
        Foo *sample = sim_array_raw(objects, 0, Foo *);
        SIM_ASSERT_RET(foo_mod_is(sample->mod), NULL);
        FooModProto *proto = (FooModProto *)sample->mod->traits;
        /* 构造零值 */
        Foo *res = proto->zero();
        SIM_ASSERT_RET(foo_mod_is(res->mod), NULL);
        /* 求和 */
        for (size_t i = 0; i < objects->n; i++) {
                Foo *a = sim_array_raw(objects, i, Foo *);
                SIM_ASSERT_RET(foo_mod_is(a->mod), NULL);
                proto->add(res, a);
        }
        return res;
}
