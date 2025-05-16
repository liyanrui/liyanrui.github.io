#include "sim-list.h"

struct sim_value {
        void *data;
        size_t u;
};

SimList *sim_list(void) {
        SimList *list = malloc(sizeof(SimList));
        if (list) {
                list->head = NULL;
                list->tail = NULL;
                list->n = 0;
                return list;
        } else {
                SIM_ERR3("failed to malloc SimList object!");
                return NULL;
        }
}

static void sim_link_free(SimLink *link);
void sim_list_free(SimList *list) {
        if (!list) {
                SIM_ERR3("invalid list!");
                return;
        }
        SimLink *it = list->head;
        while (it) {
                SimLink *next = it->next;
                sim_link_free(it);
                it = next;
        }
        free(list);
}
static void sim_link_free(SimLink *link) {
        if (link->value) {
                free(link->value->data);
                free(link->value);
        }
        free(link);
}


static bool sim_list_boundry_check(SimList *list);
static SimValue *sim_value(void *data, size_t u);
static SimLink *sim_link(SimValue *v);

SimLink *sim_list_insf(SimList *list, SimLink *here, void *data, size_t u) {
        if (!sim_list_boundry_check(list)) return NULL;
        /* 复制数据 */
        SimValue *v = sim_value(data, u);
        /* 构造一个新节点，将其插入在 here 之后 */
        SimLink *new_link = sim_link(v);
        if (here) {
                new_link->prev = here->prev;
                new_link->next = here;
                here->prev = new_link;
        } else {
                if (list->n == 0) {
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
static SimValue *sim_value(void *data, size_t u) {
        if (!data || u == 0) {
                SIM_ERR3("invalid data!");
                return NULL;
        }
        SimValue *v = malloc(sizeof(SimValue));
        if (!v) {
                SIM_ERR3("failed to malloc SimValue object!");
                return NULL;
        }
        v->data = malloc(u);
        if (!v->data) {
                free(v);
                SIM_ERR3("failed to malloc data member of SimValue object!");
                return NULL;
        }
        memcpy(v->data, data, u);
        v->u = u;
        return v;
}

SimLink *sim_link(SimValue *v) {
        SimLink *new_link = malloc(sizeof(SimLink));
        if (!new_link) {
                SIM_ERR3("failed to malloc SimLink object!");
                return NULL;
        }
        new_link->prev = NULL;
        new_link->next = NULL;
        new_link->value = v;
        return new_link;
}

SimLink *sim_list_insb(SimList *list, SimLink *here, void *data, size_t u) {
        if (!sim_list_boundry_check(list)) return NULL;
        /* 复制数据 */
        SimValue *v = sim_value(data, u);
        /* 构造一个新节点，将其插入在 here 之后 */
        SimLink *new_link = sim_link(v);
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
                sim_link_free(target);
                list->n--;
        } else SIM_ERR3("target is NULL!");
}

void *sim_link_data(SimLink *link, size_t u) {
        if (link) {
                if (link->value) {
                        if (link->value->u != u) {
                                SIM_ERR3("value type not mached!");
                                return sim_invalid_value(u);
                        }
                        return link->value->data;
                }
                SIM_ERR3("no value!");
                return sim_invalid_value(u);
        } else {
                SIM_ERR3("invalid link!");
                return sim_invalid_value(u);
        }
}
