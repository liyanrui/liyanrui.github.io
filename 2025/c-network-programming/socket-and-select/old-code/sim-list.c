#include <stdlib.h>
#include "sim-list.h"

void sim_list_free(SimList *list) {
        if (list) {
                SimList *it = list;
                while (1) {
                        if (!it) break;
                        SimList *next = it->next;
                        free(it);
                        it = next;
                }
        } else {
                fprintf(stderr, "sim_list_free error!\n");
        }
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

