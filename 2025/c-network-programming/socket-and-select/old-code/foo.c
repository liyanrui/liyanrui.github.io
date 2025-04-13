#include <stdlib.h>
#include <sim-list.h>

int main(void) {
        int a = 4, b = 5, c = 6;
        
        SimList *list = NULL;
        SIM_LIST_ADD(list, a, int);
        SIM_LIST_ADD(list, b, int);
        SIM_LIST_ADD(list, c, int);

        for (SimList *it = list; it; it = it->next) {
                int *i = it->data;
                printf("socket %d\n", *i);
                free(i);
        }
        sim_list_free(list);
}
