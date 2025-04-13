#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int a = 0;

void *foo(void *arg) {
        for (size_t i = 0; i < 10; i++) {
                a++;
                sleep(0.01);
                a--;
        }
        return NULL;
}

void create_two_threads(void *(*f)(void *)) {
        pthread_t foo_t, bar_t;
        pthread_create(&foo_t, NULL, f, NULL);
        pthread_create(&bar_t, NULL, f, NULL);
        pthread_join(bar_t, NULL);
        pthread_join(foo_t, NULL);
}

int main(void) {
        create_two_threads(foo);
        if (a > 0) printf("a = %d\n", a);
        return 0;
}
