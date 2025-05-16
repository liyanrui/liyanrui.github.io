#include "sim-list.h"

SimList *sim_list(void) {
        SimList *list = malloc(sizeof(SimList));
        if (list) {
                list->head = NULL;
                list->tail = NULL;
                list->n = 0;
                return list;
        } else {
                SIM_ERR3("failed to allocate memory for SimList object!");
                return NULL;
        }
}

void sim_list_free(SimList *list) {
        if (!list) {
                SIM_ERR3("invalid list!");
                return;
        }
        SimLink *it = list->head;
        while (it) {
                SimLink *next = it->next;
                sim_val_free(it->value);
                free(it);
                it = next;
        }
        free(list);
}

static bool sim_list_boundry_check(SimList *list) {
        if (!list) {
                SIM_ERR3("invalid list!");
                return false;
        }
        if (list->n == 0) {
                if (list->head != NULL || list->tail != NULL) {
                        SIM_ERR3("invalid empty list!");
                        return false;
                }
                return true;
        }
        if (list->n > 0) {
                if (list->head == NULL || list->tail == NULL) {
                        SIM_ERR3("head and tail should not be NULL!");
                        return false;
                }
        }
        return true;
}

static SimLink *sim_link(void *data, size_t u) {
        SimLink *new_link = malloc(sizeof(SimLink));
        if (!new_link) {
                SIM_ERR3("failed to allocate memory for SimLink object!");
                return NULL;
        }
        new_link->prev = NULL;
        new_link->next = NULL;
        new_link->value = SIM_VAL(data, u);
        return new_link;
}

SimLink *SIM_LIST_INSF(SimList *list, SimLink *here, void *data, size_t u) {
        if (!sim_list_boundry_check(list)) return NULL;
        SimLink *new_link = sim_link(data, u);
        if (here) {
                new_link->prev = here->prev;
                new_link->next = here;
                here->prev = new_link;
        } else {
                if (list->n == 0) { /* 仅当 list 为空表时允许 here 为空 */
                        list->head = new_link;
                        list->tail = new_link;
                } else {
                        SIM_ERR3("invalid link!");
                        return NULL;
                }
        }
        if (!new_link->prev) list->head = new_link;
        list->n++;
        return new_link;
}

SimLink *SIM_LIST_INSB(SimList *list, SimLink *here, void *data, size_t u) {
        if (!sim_list_boundry_check(list)) return NULL;
        SimLink *new_link = sim_link(data, u);
        if (here) {
                new_link->prev = here;
                new_link->next = here->next;
                here->next = new_link;
        } else {
                if (list->n == 0) {
                        list->head = new_link;
                        list->tail = new_link;
                } else {
                        SIM_ERR3("invalid link!");
                        return NULL;
                }
        }
        if (!new_link->next) list->tail = new_link;
        list->n++;
        return new_link;
}

const void *SIM_LINK_RAW(SimLink *link, size_t u) {
        if (link) {
                return SIM_VAL_RAW(link->value, u);
        } else {
                SIM_ERR3("invalid link!"); SIM_ASK3;
                return NULL;
        }
}

void sim_list_del(SimList *list, SimLink *target) {
        if (!sim_list_boundry_check(list)) return;
        if (target) {
                SimLink *prev = target->prev;
                SimLink *next = target->next;
                if (prev) prev->next = next;
                else list->head = next;
                if (next) next->prev = prev;
                else list->tail = prev;
                /* 释放 target */
                sim_val_free(target->value);
                free(target);
                list->n--;
        } else SIM_ERR3("target is NULL!");
}
