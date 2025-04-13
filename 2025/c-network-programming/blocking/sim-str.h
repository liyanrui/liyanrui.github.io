#ifndef SIM_STR_H
#define SIM_STR_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct sim_str SimStr;

SimStr *sim_str(const char *raw);
SimStr *sim_str_absorb(void *buffer, size_t m, size_t n);
void sim_str_free(SimStr *str);

bool sim_str_safe(SimStr *self);
size_t sim_str_size(SimStr *self);
const char *sim_str_share(SimStr *self);
const char *sim_str_raw(SimStr *self);
#endif
