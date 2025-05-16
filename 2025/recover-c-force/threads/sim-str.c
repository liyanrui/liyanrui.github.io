#include <stdint.h>
#include "sim-str.h"

struct sim_str {
        size_t n;
        size_t m;
        char *data;
};

SimStr *sim_str(const char *raw) {
        SimStr *str = malloc(sizeof(SimStr));
        if (!str) goto ERROR;
        if (raw) {
                size_t n = strlen(raw);
                size_t m = n + 1;
                str->data = malloc(m * sizeof(char));
                if (str->data) {
                        str->n = n; str->m = m;
                        memcpy(str->data, raw, n);
                        /* 为 str->data 增加 C 字符串结束符 */
                        str->data[n] = '\0';
                        return str;
                } else {
                        free(str);
                        goto ERROR;
                }
        } else {
                str->data = malloc(sizeof(char));
                if (str->data) {
                        str->n = 0;
                        str->m = 1;
                        str->data[0] = '\0';
                        return str;
                } else {
                        free(str);
                        goto ERROR;
                }
        }
ERROR: 
        SIM_ERR("sim_str error!"); 
        return NULL;
}

void sim_str_free(SimStr *str) {
        if (str) {
                if (str->data) free(str->data);
                free(str);
        } else {
                SIM_ERR("sim_str_free error: NULL pointer!");
        }
}

size_t sim_str_size(SimStr *str) {
        if (str) {
                return str->n;
        } else {
                SIM_ERR("sim_str_size error: invalid string!");
                return 0;
        }
}

const char *sim_str_raw(SimStr *str) {
        if (!str) {
                SIM_ERR("sim_str_raw error: invalid string!");
                SIM_ASK;
                return NULL;
        } else {
                if (str->data) return str->data;
                else {
                        SIM_ERR("sim_str_raw error: emppty string!");
                        SIM_ASK;
                        return NULL;
                }
        }
}

void sim_str_insert(SimStr *str, size_t index, const char *raw) {
        if (!str || !raw) {
                SIM_ERR("sim_str_insert error: invalid string!");
                return;
        }
        /* 判断 index 是否在 [0, str->n - 1] 之内 */
        if (index > str->n) {
                SIM_ERR("sim_str_insert error: overrange!");
                return;
        }
        /* 考虑是否对 str 扩容 */
        size_t n = strlen(raw);
        size_t m = str->n + 2 * n; /* 扩容时，不妨多扩一些 */
        if (str->m < m) {
                char *new_data = realloc(str->data, m);
                if (!new_data) {
                        SIM_ERR("sim_str_insert error: failed to realloc!");
                        return;
                }
                str->data = new_data;
                str->m = m;
        }
        /* 将 str->data 的 [index, str->n) 部分移到尾部 */
        size_t new_n = str->n + n;
        size_t d = str->n - index;
        if (d > 0) {
                memcpy(str->data + new_n - d, str->data + index, d);
        }
        /* 将 raw 插入 str->data */
        memcpy(str->data + index, raw, n);
        str->n = new_n;
        str->data[str->n] = '\0';
}

void sim_str_prefix(SimStr *str, const char *raw) {
        sim_str_insert(str, 0, raw);
}

void sim_str_suffix(SimStr *str, const char *raw) {
        sim_str_insert(str, str->n, raw);
}

size_t sim_str_find(SimStr *str, const char *target) {
        if (!str || !target) {
                SIM_ERR("sim_str_find error: invalid string!");
                return str->n + 1;
        }
        if (!str->data) {
                SIM_ERR("sim_str_find error: invalid string!");
                return str->n + 1;
        }
        char *where = strstr(str->data, target);
        if (where) {
                return where - str->data;
        } else {
                SIM_ERR("sim_str_find error: no such target!");
                return str->n + 1;
        }
}

void sim_str_del(SimStr *str, size_t begin, size_t n) {
        if (!str) {
                SIM_ERR("sim_str_insert error: invalid string!");
                return;
        }
        if (!str->data) {
                SIM_ERR("sim_str_insert error: invalid string!");
                return;
        }
        /* 检查 [begin, begin + n] 是否超范围，或者 begin 为无穷大 */
        size_t j = begin + n;
        if (begin >= str->n || j > str->n || begin == SIZE_MAX) {
                SIM_ERR("sim_str_del error: overrange!");
                return;
        }
        if (n == 0) return;
        /* 用 begin + n 之后的内容补位 */
        memcpy(str->data + begin, str->data + j, str->n - j);
        str->n -= n;
        str->data[str->n] = '\0';
        /* 缩容 */
        size_t new_m = str->n + 2 * n;
        if (new_m < str->m) {
                char *new_data = realloc(str->data, new_m);
                if (new_data) {
                        str->data = new_data;
                        str->m = new_m;
                } else {
                        SIM_ERR("sim_str_del error: failed to realloc!");
                        return;
                }
        }
}
