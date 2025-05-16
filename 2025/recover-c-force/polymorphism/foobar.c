#include <bar.h>
int main(void) {
        /* 构造 Bar 对象数组 */
        SimArray *bar_objects = sim_array(Bar *);
        sim_array_add(bar_objects, bar(1.0), Bar *);
        sim_array_add(bar_objects, bar(2.7), Bar *);
        sim_array_add(bar_objects, bar(3.14), Bar *);
        /* 求和 */
        Bar *sum = (Bar *)foo_sum(bar_objects);
        printf("result = %lf\n", sum->data);
        /* 资源释放 */
        bar_free(sum);
        for (size_t i = 0; i < bar_objects->n; i++) {
                bar_free(sim_array_raw(bar_objects, i, Bar *));
        }
        sim_array_free(bar_objects);
        return 0;
}
