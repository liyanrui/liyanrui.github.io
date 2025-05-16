#ifndef SIM_STR_H
#define SIM_STR_H
#include <string.h>
#include <sim-err.h>

typedef struct sim_str SimStr;
SimStr *sim_str(const char *raw);
void sim_str_free(SimStr *str);

size_t sim_str_size(SimStr *str);
const char *sim_str_raw(SimStr *str);

void sim_str_insert(SimStr *str, size_t index, const char *raw);
void sim_str_prefix(SimStr *str, const char *raw);
void sim_str_suffix(SimStr *str, const char *raw);

size_t sim_str_find(SimStr *str, const char *target);
void sim_str_del(SimStr *str, size_t begin, size_t n);
#endif
