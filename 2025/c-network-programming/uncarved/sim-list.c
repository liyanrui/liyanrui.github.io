#include <stdlib.h>
#include "sim-list.h"

void sim_list_free(SimList *list) {
        if (list) {
                SimList *it = list;
                while (it) {
                        SimList *next = it->next;
                        free(it);
                        it = next;
                }
        }
}

size_t sim_list_size(SimList *list) {
        if (list) {
                size_t n = 0;
                for (SimList *it = list; it; it = it->next) n++;
                return n;
        } else return 0;
}

SimList *sim_list_add(SimList *self, void *data) {
        SimList *new_node = malloc(sizeof(SimList));
        if (new_node) {
                new_node->data = data;
                if (self) new_node->next = self;
                else new_node->next = NULL;
        } else fprintf(stderr, "sim_list_add error!\n");
        return new_node;
}

SimList *sim_list_delete(SimList *self, SimList *node) {
        if (!node) return self;
        SimList *prev = NULL;
        for (SimList *it = self; it; it = it->next) {
                if (it == node) break;
                if (it->next == node) {
                        prev = it;
                        break;
                }
        }
        if (prev) prev->next = node->next;
         else self = node->next;
        free(node);
        return self;
}
