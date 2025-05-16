#ifndef SIM_LIST_H
#define SIM_LIST_H
#include <stdbool.h>
#include <sim-err3.h>
#include <sim-val.h>

typedef struct sim_link {
        struct sim_link *prev;
        struct sim_link *next;
        SimVal *value;
} SimLink;

typedef struct sim_list {
        SimLink *head;
        SimLink *tail;
        size_t n;
} SimList;

SimList *sim_list(void);
void sim_list_free(SimList *list);

SimLink *SIM_LIST_INSF(SimList *list, SimLink *here, void *data, size_t u);
SimLink *SIM_LIST_INSB(SimList *list, SimLink *here, void *data, size_t u);
#define sim_list_prefix(list, v, type) \
        SIM_LIST_INSF(list, list->head, &(v), sizeof(type))
#define sim_list_suffix(list, v, type) \
        SIM_LIST_INSB(list, list->tail, &(v), sizeof(type))

const void *SIM_LINK_RAW(SimLink *link, size_t u);
#define sim_link_raw(link, type) \
        *(type *)SIM_LINK_RAW(link, sizeof(type))

void sim_list_del(SimList *list, SimLink *target);
#endif
