/* files-monitor.c */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/select.h>
#include <unistd.h>

int main(void) {
        /* 打开 a.txt 和 b.txt */
        int fd_a = open("a.txt", O_RDONLY);
        if (fd_a == -1) {
                fprintf(stderr, "failed to open a.txt");
                exit(-1);
        }
        int fd_b = open("b.txt", O_RDONLY);
        if (fd_b == -1) {
                fprintf(stderr, "failed to open b.txt");
                exit(-1);
        }
        /* 构建文件描述符集合 */
        int fd_max = (fd_a > fd_b) ? fd_a : fd_b;
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fd_a, &fds);
        FD_SET(fd_b, &fds);
        /* 阻塞式轮询文件描述符集 */
        fd_set fds_copy = fds;
        char buffer[1024];
        while (1) {
                fds = fds_copy;
                int a = select(fd_max + 1, &fds, NULL, NULL, NULL);
                if (a == -1) {
                        fprintf(stderr, "select error!\n");
                        exit(-1);
                }
                /* 读取文件内容 */
                for (int i = 0; i < fd_max + 1; i++) {
                        if (FD_ISSET(i, &fds)) { /* 找到一个可读文件 */
                                ssize_t n;
                                while ((n = read(i, buffer, sizeof(buffer))) > 0) {
                                        write(STDOUT_FILENO, buffer, n);
                                }
                                if (n == -1) {
                                        fprintf(stderr, "read failed!\n");
                                        exit(-1);
                                }
                        }
                }
        }
        close(fd_b);
        close(fd_a);
        return 0;
}
