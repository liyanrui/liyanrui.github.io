#include <sim-err2.h>

float foo(float x) {
        if (x == 0) {
                SIM_ERR2("division by zero!");
                return 0;
        } else return 1 / x;
}

int main(void) {
        float a = foo(3); SIM_ASK2; printf("a = %f\n", a);
        float b = foo(0); SIM_ASK2; printf("b = %f\n", b);
        return 0;
}
