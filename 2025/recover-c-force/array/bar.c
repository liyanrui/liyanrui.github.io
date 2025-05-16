#include <sim-array.h>
int main(void) {
        SimArray *x = sim_array(SimStr *);
        SimStr *a = sim_str("hello");
        SimStr *b = sim_str(" ");
        SimStr *c = sim_str("world");
        SimStr *d = sim_str("!");

        sim_array_append(x, a, SimStr *);
        sim_array_append(x, b, SimStr *);
        sim_array_append(x, c, SimStr *);
        sim_array_append(x, d, SimStr *);
        for (size_t i = 0; i < x->n; i++) {
                SimStr *str = sim_array_unit(x, i, SimStr *);
                printf("%s", sim_str_raw(str));
                sim_str_free(str);
        }
        sim_array_free(x);
        /* 检错 */
        SIM_ASK2;
        return 0;
}
