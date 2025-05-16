#include <sim-str.h>
void str_print(SimStr *a) {
        printf("%s: %lu bytes.\n",
               sim_str_raw(a), sim_str_size(a));
}

int main(void) {
        SimStr *a = sim_str(NULL); str_print(a);
        char *w = "world";
        sim_str_suffix(a, w); str_print(a); SIM_ASK;
        
        char *h = "hello";
        sim_str_prefix(a, h); str_print(a);
        sim_str_insert(a, 5, " "); str_print(a);
        sim_str_suffix(a, "!"); str_print(a); SIM_ASK;
        
        sim_str_del(a, sim_str_find(a, w), strlen(w));
        str_print(a);
        sim_str_del(a, strlen(h), 1); str_print(a);
        sim_str_del(a, 0, sim_str_size(a)); str_print(a);
        SIM_ASK;
        
        sim_str_free(a); SIM_ASK;
        return 0;
}
