#ifndef SIM_LIST_H
#define SIM_LIST_H
#include <stdio.h>

typedef struct sim_list SimList;
struct sim_list {
        void *data;
        struct sim_list *next;
};
void sim_list_free(SimList *list);

SimList *sim_list_add(SimList *self, void *data);

#define SIM_LIST_ADD(self, value, type) do { \
        type *p = malloc(sizeof(type)); \
        *p = value; \
        self = sim_list_add(self, p); \
} while (0)
#endif
