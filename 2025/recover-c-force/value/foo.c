#include <sim-val.h>
int main(void) {
        /* 常规测试 */ {
                double pi = 3.1415926;
                SimVal *x = sim_val(pi, double);
                double a = sim_val_raw(x, double);
                printf("a = %lf\n", a);
                sim_val_free(x);
        }
        /* 边界测试 */ {
                SimVal *x = SIM_VAL(NULL, 8);
                double a = sim_val_raw(x, double);
                printf("a = %lf\n", a);
                sim_val_free(x);                
        }
        return 0;
}
