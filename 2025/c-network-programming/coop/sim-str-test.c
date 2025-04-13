#include <sim-str.h>

int main(void) {
        const char *raw = "Hello world!";
        SimStr *foo = sim_str(raw);
        if (sim_str_safe(foo)) {
                printf("foo length: %lu\n", sim_str_size(foo));
                printf("foo content: %s\n", sim_str_raw(foo));
        }
        sim_str_free(foo);
        
        size_t n = strlen(raw);
        size_t m = 2 * n;
        void *buffer = malloc(m * sizeof(char));
        memcpy(buffer, raw, n);
        SimStr *bar = sim_str_absorb(buffer, m, n);
        if (sim_str_safe(bar)) {
                fwrite(sim_str_share(bar), 
                       sizeof(char),
                       sim_str_size(bar),
                       stdout);
                printf("\n");
                printf("%s\n", sim_str_raw(bar));
        }
        sim_str_free(bar);
        return 0;
}
