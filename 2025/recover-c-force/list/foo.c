#include <sim-list.h>
int main(void) {
        /* 构造链表 x */
        SimList *x = sim_list();
        int a[] = {1, 2, 3, 4, 5};
        int n = sizeof(a) / sizeof(int); /* 数组长度 */
        for (size_t i = 0; i < n; i++) {
                sim_list_suffix(x, a[i], int);
        }
        /* 遍历 x */
        printf("NULL <=> ");
        for (SimLink *it = x->head; it; it = it->next) {
                int t = sim_link_raw(it, int);
                printf("%d <=> ", t);
        }
        printf(" NULL\n");
        /* 释放 x，检错 */
        sim_list_free(x); SIM_ASK3;
        return 0;
}
