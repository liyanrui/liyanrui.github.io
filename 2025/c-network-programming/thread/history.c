#include <pthread.h>
#include <stdio.h>

void *foo(void *arg) {
        int *a = arg;
        printf("咚！\n");
        *a += 1;
        return NULL;
}

int main(void) {
        printf("叮！\n");
        pthread_t foo_t;
        int n = 42;
        if (pthread_create(&foo_t, NULL, foo, &n) == 0) {
                if (pthread_join(foo_t, NULL) == 0) {
                        printf("history: %d\n", n);
                }
        }
        return 0;
}
