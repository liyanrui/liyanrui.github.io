#include "sim-array.h"

struct sim_array_priv {
        size_t n; /* 数组长度：即数组存储的单元个数 */
        size_t u; /* 单元字节数 */
        size_t m;
        void *data;
};

SimArray *SIM_ARRAY(size_t u) {
        SimArray *array = malloc(sizeof(SimArray));
        if (!array) {
                SIM_ERR2("failed to allocate memory for object!");
                return NULL;
        }
        array->priv = malloc(sizeof(SimArrayPriv));
        if (!array->priv) {
                free(array);
                SIM_ERR2("failed to allocate memory for private part!");
                return NULL;
        }
        array->priv->n = 0;
        array->priv->u = u;
        array->priv->m = 0;
        array->priv->data = NULL;
        array->n = array->priv->n;
        return array;
}

void sim_array_free(SimArray *array) {
        if (array) {
                if (array->priv) {
                        free(array->priv->data);
                        free(array->priv);
                } else SIM_ERR2("invalid private part!");
                free(array);
        } else {
                SIM_ERR2("invalid SimArray object!");
        }
}

void SIM_ARRAY_ADD(SimArray *array, void *unit, size_t u) {
        if (!array) {
                SIM_ERR2("invalid SimArray object!");
                return;
        }
        if (!unit) {
                SIM_ERR2("invalid unit!");
                return;
        }
        if (array->priv->data) {
                if (array->priv->u != u) {
                        SIM_ERR2("unit type not matched!");
                        return;
                }
                size_t s = array->priv->n * u;
                size_t t = s + u;
                if (t > array->priv->m) { /* 扩容 */
                        size_t m = 1.5 * t;
                        void *new_data = realloc(array->priv->data, m);
                        if (!new_data) {
                                m = t + u;
                                new_data = realloc(array->priv->data, m);
                                if (!new_data) {
                                        SIM_ERR2("failed to enlarge capacity!");
                                        return;
                                }
                        }
                        array->priv->data = new_data;
                        array->priv->m = m;
                }
                memcpy((char *)(array->priv->data) + s, unit, u);
                array->priv->n++;
                /* 主动修复用户层面的 n */
                array->n = array->priv->n;
        } else {
                array->priv->data = malloc(u);
                if (!array->priv->data) {
                        SIM_ERR2("failed to allocate memory for data member!");
                        return;
                }
                memcpy(array->priv->data, unit, u);
                array->priv->n = 1;
                array->priv->u = u;
                array->priv->m = u;
                /* 主动修复用户层面的 n */
                array->n = array->priv->n;
        }
}

const void *SIM_ARRAY_RAW(SimArray *array, size_t i, size_t u) {
        if (!array) {
                SIM_ERR2("invalid SimArray object!"); SIM_ASK2;
                return NULL;
        }
        /* 主动修复用户层面的 n */
        array->n = array->priv->n;
        /* 类型匹配检测 */
        if (array->priv->u != u) {
                SIM_ERR2("unit type not mached!"); SIM_ASK2;
                return NULL;
        }
        if (i >= array->n) {
                SIM_ERR2("out-of-bounds access!"); SIM_ASK2;
                return NULL;
        }
        return (char *)(array->priv->data) + i * u;
}

