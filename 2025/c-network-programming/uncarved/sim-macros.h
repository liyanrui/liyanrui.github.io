#ifndef SIM_MACROS_H
#define SIM_MACROS_H

/* 只在 Sim 项目内部使用 */
#define SIM_OBJECT_SAFE(object) do { \
       if (self) { \
                /* 不太致命的错误 */ \
                if (self->error) { \
                        fprintf(stderr, "%s error: %s\n", \
                                __func__, self->error); \
                        return false; \
                } \
        } else { \
                /* 致命错误 */ \
                fprintf(stderr, "%s error: NULL pointer!", __func__); \
                return false; \
        } \
        return true; \
} while (0)

/* 协程，供用户使用。cr 的类型为 SimCr 指针 */
typedef struct sim_cr SimCr;
#define sim_cr_begin(cr) switch (cr->state) { case 0:

#define sim_cr_yield(cr,...) do {cr->state = __LINE__; \
                                 return __VA_ARGS__; \
                                 case __LINE__: ; } while (0)

#define sim_cr_end(cr) } cr->done = true;
#endif
