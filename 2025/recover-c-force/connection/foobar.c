#include <sim-list.h>
/* 电话机类型 */
typedef void (*Phone)(const char *caller);
/* 通话双方形成的连接 */
typedef struct connection {
         /* 通话方 x */
         Phone x; const char *x_name;
         /* 通话方 y */
         Phone y; const char *y_name;
} Connection;

/* 构造连接表 */
void connect(SimList *connections, 
             Phone x, const char *x_name, 
             Phone y, const char *y_name) {
        Connection one = {.x = x, .x_name = x_name, 
                          .y = y, .y_name = y_name};
        sim_list_suffix(connections, one, Connection);
}
void foo(const char *caller) {
        printf("foo: hi, %s\n", caller);
}
void bar(const char *caller) {
        printf("bar: hi, %s\n", caller);
}

int main(void) {
        SimList *x = sim_list();
        /* 连接 foo 和 bar */
        connect(x, foo, "foo", bar, "bar");
        /* 通话 */
        for (SimLink *it = x->head; it; it = it->next) {
                Connection c = sim_link_raw(it, Connection);
                c.x(c.y_name); /* x 呼叫 y */
                c.y(c.x_name); /* y 回应 x */
        }
        sim_list_free(x);
        return 0;
}
