#include <sim-array.h>
int main(void) {
        SimArray *x = sim_array(int);
        /* 将 a 中元素复制到 x */
        int a[] = {0, 1, 2, 3, 4};
        size_t n = sizeof(a) / sizeof(int);
        for (size_t i = 0; i < n; i++) {
                sim_array_add(x, a[i], int);
        }
        /* 添加字面量 */
        sim_array_add(x, 5, int);
        /* 遍历 x */
        for (size_t i = 0; i < x->n; i++) {
                int t = sim_array_raw(x, i, int);
                printf("%d\n", t);
        }
        /* 越界访问，程序在此崩溃！ */
        int foo = sim_array_raw(x, x->n, int);
        printf("foo = %d\n", foo);
        /* 释放               验错 */
        sim_array_free(x); SIM_ASK2;
        return 0;
}
