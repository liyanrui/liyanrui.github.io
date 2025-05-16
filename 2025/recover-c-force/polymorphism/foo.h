#ifndef FOO_H
#define FOO_H
#include <sim-mod.h>
/* FooMod 的特性表 */
typedef struct foo_mod_traits FooModTraits;
/* 模块结构体 */
typedef struct foo_mod {
        SimMod *parent;
        const char *name;
        FooModTraits *traits; /* 特性表 */
} FooMod;

/* FooMod 的核心类型 */
typedef struct foo {
        FooMod *mod;
} Foo;

struct foo_mod_traits {
        SimTrait zero;
        SimTrait add;
};

typedef struct foo_mod_proto {
        Foo *(*zero)(void);
        void (*add)(Foo *, Foo *);
} FooModProto;

SIM_MOD_DECLARE(FooMod, foo_mod);
#define FOO_MOD(mod) ((FooMod *)(mod))
#define foo_mod_is(mod) FooMod_is(FOO_MOD(mod))

Foo *foo_sum(SimArray *objects);
#endif
