#ifndef SIM_MOD_H
#define SIM_MOD_H
#include <stdbool.h>
#include <sim-err3.h>

extern _Thread_local SimArray *sim_module_table;

typedef struct sim_mod SimMod;
struct sim_mod {
        SimMod *parent;
        const char *name;
};

typedef void(*SimTrait)(void);

#define SIM_MOD_REG(modules, mod, type) do { \
        static size_t id = 0; \
        if (id == 0) { \
                if (!modules) { \
                        modules = sim_array(SimMod *); \
                } \
                mod = malloc(sizeof(type)); \
                if (!mod) { \
                        SIM_ERR3("failed to allocate memory for Module object!"); \
                        return NULL; \
                } \
                mod->parent = NULL; \
                mod->name = #type; \
                sim_array_add(modules, mod, type *); \
                id = modules->n; \
        } \
        mod = sim_array_raw(modules, id - 1, type *); \
} while (0)

SimMod *sim_mod(void);

#define SIM_MOD(mod) ((SimMod *)(mod))

#define SIM_MOD_IS_COMMON(mod_a, mod_b) do { \
        SimMod *t = (SimMod *)(mod_a); \
        while (1) { \
                if (!t) { \
                        SIM_ERR3("invalid object!"); \
                        return false; \
                } \
                if ((void *)t == (void *)(mod_b)) return true; \
                t = t->parent; \
        } \
        return false; \
} while (0)
bool SimMod_is(SimMod *mod);
#define sim_mod_is(mod) SimMod_is(SIM_MOD(mod))

#define SIM_MOD_DECLARE(ModType, mod_type) \
        ModType *mod_type(void); \
        bool ModType##_is(ModType *mod); \
        ModType *mod_type##_this(ModType *mod)

#define SIM_MOD_DEFINE(ModType, mod_type, ParentModType, parent_mod_type) \
        static ModType *this_mod = NULL; \
        ModType *mod_type(void) { \
                ParentModType *parent = parent_mod_type(); \
                SIM_MOD_REG(sim_module_table, this_mod, ModType); \
                this_mod->parent = parent; \
                this_mod->name = #ModType; \
                return this_mod; \
        } \
        bool ModType##_is(ModType *mod) { SIM_MOD_IS_COMMON(mod, this_mod); } \
        ModType *mod_type##_this(ModType *mod) { \
                return mod_type##_is(mod) ? this_mod : NULL; \
        } \
        /* 下面这行代码可让宏调用语句后面允许出现分号而不引发编译器警告 */ \
        void foo_this_func_not_defined_forever(void)
#endif
