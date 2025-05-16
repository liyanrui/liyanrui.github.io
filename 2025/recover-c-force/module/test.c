#include <foo.h>
int main(void) {
        Foo *a = foo("i am foo!");
        if (foo_mod_is(a->mod)) {
                printf("%s\n", a->anything);
                printf("i belong to FooMod.\n");
        }
        if (sim_mod_is(a->mod)) {
                printf("i also belong to SimMod.\n");
        }
        foo_free(a); SIM_ASK3;
        return 0;
}
