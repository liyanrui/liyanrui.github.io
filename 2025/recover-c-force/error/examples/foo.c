#include <sim-err.h>
float foo(float a) {
        if (a == 0) {
                SIM_ERR("foo error!");
                return 0;
        }
        return 1 / a;
}
int main(void) {
        float a = foo(0);
        SIM_MER {
                printf("a = %f\n", a);
        }
        float b = foo(3); SIM_PUN;
        printf("b = %f\n", b);
        return 0;
}
