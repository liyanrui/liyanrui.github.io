#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

#define MAX_PRODUCED 5
int data;
int produced = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t data_ready = PTHREAD_COND_INITIALIZER;

void *producer(void *arg) {
        sleep(10); /* 模拟一些运算过程 */
        for (int i = 0; i < MAX_PRODUCED; i++) {
                pthread_mutex_lock(&mutex);
                /* 在区间 [0, 99] 中随机生成 1 个数字 */
                data = rand() % 100;
                produced++; /* 记录生成数据的次数 */
                printf("生产者生产数据: %d\n", data);
                pthread_cond_signal(&data_ready);
                pthread_mutex_unlock(&mutex);
                sleep(1); /* 模拟一些运算过程 */
        }
        return NULL;
}

void *consumer(void *arg) {
        while (1) {
                printf("消费者等待……\n");
                pthread_mutex_lock(&mutex);
                pthread_cond_wait(&data_ready, &mutex);
                printf("消费者消费数据: %d\n", data);
                pthread_mutex_unlock(&mutex);
        }
        return NULL;
}

int main() {
        pthread_t producer_thread, consumer_thread;
        pthread_create(&producer_thread, NULL, producer, NULL);
        pthread_create(&consumer_thread, NULL, consumer, NULL);
        pthread_join(producer_thread, NULL);
        pthread_join(consumer_thread, NULL);
        return 0;
}
