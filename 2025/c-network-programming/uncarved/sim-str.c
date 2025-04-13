#include "sim-macros.h"
#include "sim-str.h"

struct sim_str {
        char *data; /* 支持以 '\0' 为字符串结束标志，亦可不支持 */
        size_t n;   /* data 中存储的字符串的长度（字节数） */
        size_t m;   /* data 占用内存空间的字节数，m >= n */
        const char *error;
};

SimStr *sim_str(const char *raw) {
        SimStr *str = malloc(sizeof(SimStr));
        if (!str) {
                fprintf(stderr, "sim_str: malloc error!\n");
                return NULL;
        }
        str->n = 0;
        str->m = 0;
        if (raw) {
                size_t n = strlen(raw);
                str->data = malloc(n * sizeof(char));
                if (!str->data) {
                        str->error = "sim_str: failed to malloc 'data' member!";
                } else {
                        str->n = n;
                        str->m = n;
                        str->error = NULL;
                        memcpy(str->data, raw, n);
                }
        } else {
                str->data = NULL;
                str->error = NULL;
        }
        return str;
}

SimStr *sim_str_absorb(void *buffer, size_t m, size_t n) {
        if (!buffer || n == 0) return NULL;
        SimStr *str = malloc(sizeof(SimStr));
        if (!str) {
                fprintf(stderr, "sim_str: malloc error!\n");
                return NULL;
        }
        str->n = n;
        str->m = m;
        str->data = buffer;
        str->error = NULL;
        return str;
}

void sim_str_free(SimStr *str) {
        if (str) {
                if (str->data) free(str->data);
                free(str);
        } else {
                fprintf(stderr, "sim_str_free error!");
        }
}

bool sim_str_safe(SimStr *self) {
        SIM_OBJECT_SAFE(self);
}

size_t sim_str_size(SimStr *self) {
        return self->n;
}

const char *sim_str_share(SimStr *self) {
        return self->data;
}

const char *sim_str_raw(SimStr *self) {
        if (self->n == self->m) {
                /* 对 self->data 扩容 1 个字节 */
                /* 用于存储 C 字符串结束符 */
                void *p = realloc(self->data, self->m + 1);
                if (!p) return NULL;
                else {
                        self->m += 1;
                        self->data = p;
                }
        }
        /* 添加字符串结束符 */
        *(self->data + self->n) = '\0';
        return self->data;
}
