#include <stdio.h>
#include <stdlib.h>

typedef struct {
        size_t n;
        int *body;
} Array_type_1;
typedef struct {
        size_t n;
        double *body;
} Array_type_2;


Array_type_1 *array_new_1(size_t n) {
        Array_type_1 *a;
        a = malloc(sizeof(Array_type_1));
        a->n = n;
        a->body = malloc(n * sizeof(int));
        return a;
}
void array_free_3(Array_type_1 * a) {
        free(a->body);
        free(a);
}
void array_print_1(Array_type_1 *a) {
        printf("{");
        for (size_t i = 0; i < a->n - 1; i++) {
            printf("%s%d%s", "",a->body[i],", ");
        }
        printf("%s%d%s", "",a->body[a->n - 1],"}\n");
}

Array_type_2 *array_new_2(size_t n) {
        Array_type_2 *a;
        a = malloc(sizeof(Array_type_2));
        a->n = n;
        a->body = malloc(n * sizeof(double));
        return a;
}
void array_free_4(Array_type_2 * a) {
        free(a->body);
        free(a);
}
void array_print_2(Array_type_2 *a) {
        printf("{");
        for (size_t i = 0; i < a->n - 1; i++) {
            printf("%s%.3f%s", "",a->body[i],", ");
        }
        printf("%s%.3f%s", "",a->body[a->n - 1],"}\n");
}


int main(void) {
        size_t n = 7;
        
        Array_type_1 *a = array_new_1(n);
        for (size_t i = 0; i < n; i++) a->body[i] = i + 1;
        array_print_1(a);
        array_free_3(a);
        
        Array_type_2 *b = array_new_2(n);
        for (size_t i = 0; i < n; i++) b->body[i] = i + 1;
        array_print_2(b);
        array_free_4(b);
        return 0;
}
