#ifndef SIM_ARRAY_H
#define SIM_ARRAY_H
#include <sim-err2.h>

typedef struct sim_array_priv SimArrayPriv;
typedef struct sim_array {
        size_t n; /* 数组长度复本 */
        SimArrayPriv *priv; /* 数组的隐私部分 */
} SimArray;

SimArray *SIM_ARRAY(size_t u);
#define sim_array(type) SIM_ARRAY(sizeof(type))
void sim_array_free(SimArray *array);

void SIM_ARRAY_ADD(SimArray *array, void *unit, size_t u);
#define sim_array_add(array, unit, type) do { \
        type tmp_unit = unit; \
        SIM_ARRAY_ADD(array, &tmp_unit, sizeof(type)); \
} while (0)

const void *SIM_ARRAY_RAW(SimArray *array, size_t i, size_t u);
#define sim_array_raw(x, i, type) \
        *(type *)SIM_ARRAY_RAW(x, i, sizeof(type))
#endif
