#ifndef FOO_H
#define FOO_H
#include <sim-mod.h>
/* 模块结构体 */
typedef struct foo_mod {
        SimMod *parent;
        const char *name;
} FooMod;

/* 隶属于 Foo 模块的数据类型 */
typedef struct foo {
        FooMod *mod;
        const char *anything;
} Foo;

SIM_MOD_DECLARE(FooMod, foo_mod);
#define FOO_MOD(mod) ((FooMod *)(mod)) /* 类型转换 */
#define foo_mod_is(mod) FooMod_is(FOO_MOD(mod)) /* 自省 */

Foo *foo(const char *anything); /* Foo 对象的构造函数 */
#define foo_free(a) free(a)  /* Foo 对象释放宏 */
#endif
