#include <sim-err3.h>

void foo_1(void) {
        SIM_ERR3("狼来了！");
}
void foo_2(void) {
        SIM_ERR3("狼来了！");
}
void foo_3(void) {
        SIM_ERR3("狼来了！");
}

int main(void) {
        foo_1();
        foo_2();
        foo_3();
        SIM_ASK3;
        return 0;
}
