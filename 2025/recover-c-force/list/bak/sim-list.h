#ifndef SIM_LIST_H
#define SIM_LIST_H
#include <stdbool.h>
#include <sim-err3.h>

typedef struct sim_value SimValue;

typedef struct sim_link {
        struct sim_link *prev;
        struct sim_link *next;
        SimValue *value;
} SimLink;

typedef struct sim_list {
        SimLink *head;
        SimLink *tail;
        size_t n;
} SimList;

SimList *sim_list(void);
void sim_list_free(SimList *list);

SimLink *sim_list_insf(SimList *list, SimLink *here, void *data, size_t u);
SimLink *sim_list_insb(SimList *list, SimLink *here, void *data, size_t u);
#define sim_list_prepend(list, v, type) \
        sim_list_insf(list, list->head, &(type){v}, sizeof(type))
#define sim_list_append(list, v, type) \
        sim_list_insb(list, list->tail, &(type){v}, sizeof(type))

void *sim_link_data(SimLink *link, size_t u);
#define sim_link_val(link, type) \
        *(type *)sim_link_data(link, sizeof(type))

void sim_list_del(SimList *list, SimLink *target);
#endif
