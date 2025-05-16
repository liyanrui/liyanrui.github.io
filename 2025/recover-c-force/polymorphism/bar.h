#ifndef BAR_H
#define BAR_H
#include <foo.h>
/* Bar 模块类型 */
typedef struct bar_mod {
        FooMod *parent;
        const char *name;
        FooModTraits *traits;
} BarMod;

/* Bar 模块的核心类型 */
typedef struct bar {
        BarMod *mod;
        double data;
} Bar;

SIM_MOD_DECLARE(BarMod, bar_mod);
#define BAR_MOD(mod) ((BarMod *)(mod))
#define bar_mod_is(mod) BarMod_is(BAR_MOD(mod))

/* Bar 模块对象初始化函数 */
BarMod *bar_mod_init(void);
/* Bar 模块核心对象构造函数和释放宏 */
Bar *bar(double data);
#define bar_free(obj) free(obj)

/* zero 和 add 特性的实现 */
Bar *bar_zero(void);
void bar_add(Bar *a, Bar *b);
#endif
