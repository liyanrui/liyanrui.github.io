---
title: 弦动
abstract: 奏出妙音，绝非易事，向来如此！
date: 03 月 25 日
...

# 前言

之前，我用了爱因斯坦的广义相对论类比同步 I/O 多路复用机制，用了量子力学类比多进程机制，那么现在，我该用什么类比计算机中的线程呢？不妨用超弦理论，它们至少都是「线」状的。

线程也是一种分身术，只是不同于进程穿过 `fork` 制作的双缝。每个进程都包含着一个主线程。在 C 程序中，主线程是每个进程中的 `main` 函数。在主线程中，可创建一些子线程。这个过程可类比为，孙悟空从头上拔下一些毫毛，在口中嚼碎，喷出去，变出诸多化身。这些化身的法力弱，胜在数量庞大。至于多进程机制的类比，在《西游记》里也是有的，即孙悟空和六耳猕猴的关系。

线程是程序运行时操作系统能够予以调度的最小单元。如同超弦理论中，弦以某种模式振动而表现为某种基本粒子，例如电子，光子，夸克……我们不妨认为，线程的运行决定了进程的功能。当多个线程并行运行时，它像一根弦的振动效应。这种类比自然是不严谨的，但是在单个进程中通过多个线程实现的并行运算，它象征着同步 I/O 多路复用机制与多进程机制的融合，犹如超弦理论那般自信地认为自己统一了广义相对论和量子力学，从而跻身为万物理论。

这些类比并不严谨，但它们可以作为一路走过来的我们亲手做的路标。有了它们，我们能够很快找到回去的路，这意味着能够更安心地探索未知空间，更何况这些物理学概念本身就非常有趣。当你发现，精准把握主线程和子线程的调度原理——像理解弦的振动效应，也许便获得了一种奇异的能力，通过它便能够聆听计算机中每个程序弹奏出的乐章。

# 叮咚一声

在 Unix 或 Linux 系统中，可使用 `pthread_create` 函数创建子线程，该函数的形式为

```c
#include <pthread.h>

int pthread_create(pthread_t *restrict thread,
                   const pthread_attr_t *restrict attr,
                   void *(*start_routine)(void *),
                   void *restrict arg);
```


`pthread_create` 的返回值为 0 表示成功，否则表示失败。该函数本该为新创建的子线程分配一个结构不透明的数据类型的实例表征线程对象，但返回值专用于表示执行过程是否成功，故而只能在参数列表中以结构体指针的方式传出，即第一个参数为该函数返回的子线程对象。第二个参数是线程属性，初学时无需关心，设为 `NULL` 即可。第三个参数是线程的某一例程——某个具体实现，而第四个参数是传递于该实例的参数。

以下程序创建了一个只会「咚」一声的子线程，主线程负责「叮」：

```c
/* foo.c */
#include <pthread.h>
#include <stdio.h>

void *foo(void *arg) {
        printf("咚！\n");
        return NULL;
}

int main(void) {
        printf("叮！\n");
        pthread_t thread;
        /* 为了代码简单，以下函数未处理可能出现的错误 */
        pthread_create(&thread, NULL, foo, NULL);
        pthread_join(thread, NULL);
        return 0;
}
```

与多进程机制中使用 `wait` 回收子进程相似，创建的子线程，也需要回收，所用函数为 `pthread_join`，其第一个参数为线程对象，第二个参数是线程例程的返回值，它是个双重指针——我想，你应该已经熟悉了这种在 C 语言编程中常用的手段，即以双重指针的形式传出指针。由于上述线程例程 `foo` 的返回值为 `NULL`，意味着 `phthread_join` 无需关心其返回值，故而将其第 2 个参数设为 `NULL`。`pthread_join` 返回 0 表示成功，否则表示失败。

Linux 内核层面未对线程提供直接支持，而是通过外部库 pthread（POSIX Threads）予以支持，故而编译上述代码，需要连接该库：

```console
$ gcc foo.c -o foo -lpthread
```

不过，gcc 提供了比 `-lpthread` 更全面的选项 `-pthread`，大多数情况下，我们用后者。另外，在你的 Linux 系统里，gcc 会默认连接 pthread 库，可以不必明确给出 pthread 库的连接选项，不过我是在写教程，只能不厌其烦。

运行所得程序 foo，结果可能并不会让你觉得惊奇：

```
$ ./foo
叮！
咚！
```

# 线程的历史

我将线程例程的返回值视为历史，就像弹奏的琴音会对应到乐谱上的某个符号——这个符号表征着琴音的某个频率。下面的程序构造了一个有历史的线程：

```c
/* foo.c */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

void *foo(void *arg) {
        printf("咚！\n");

        int *a = malloc(sizeof(int));
        *a = 42;
        return a;
}

int main(void) {
        printf("叮！\n");
        pthread_t foo_t;
        if (pthread_create(&foo_t, NULL, foo, NULL) == 0) {
                int *a = NULL;
                if (pthread_join(foo_t, (void **)&a) == 0) {
                        printf("history: %d\n", *a);
                        free(a);
                }
        }
        return 0;
}
```

这次写的程序，在创建和回收子线程的过程中，有了一些防御性处理。可以看到，在主线程中回收子线程时，可以获得子线程的返回值。程序的输出为：

```
叮！
咚！
history: 42
```

子线程也能修改父线程的某个变量，相当于父进程的某段历史被子进程改写。例如

```c
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
```

程序的输出为：

```
叮！
咚！
history: 43
```

# 开始混乱

你可能会觉得，上文的几个程序，它们即使直接调用 `foo` 函数，输出的结果也是一样，何必用 `pthread_create` 呢？我希望你有这个疑问，否则接下来你不会享受不到感受惊奇的一刻了。看以下程序：

```c
#include <pthread.h>
#include <stdio.h>

void *foo(void *arg) {
        printf("叮！\n");
        return NULL;
}

void *bar(void *arg) {
        printf("咚！\n");
        return NULL;
}

int main(void) {
        pthread_t foo_t, bar_t;
        pthread_create(&foo_t, NULL, foo, NULL);
        pthread_create(&bar_t, NULL, bar, NULL);
        pthread_join(bar_t, NULL);
        pthread_join(foo_t, NULL);
        return 0;
}
```

上述程序，编译后，若多次运行，你可能经常会得到

```
叮！
咚！
```

但偶尔会得到

```
咚！
叮！
```

这意味着什么？你已无法确定程序的输出是什么了！

请记住这句话，所有的不确定性，它更深刻的内涵可能是，宇宙的运行原理依赖的是某种并行机制，而不确定性不过是我们对该机制的最为直接的感受。你扔出了一枚硬币，它出现正面的概率是 1/2，此事亦可理解为，是因你存在的概率为 1/2 而导致了这一结果。

你习惯了世界是确定的，应该会觉得我所说的，接近癫狂，但是也许真的存在不确定性的转移呢？我认为，同时通过双缝的电子，它之所以会在观测中坍缩，原因是电子将自身的不确定性转移到了观测设备上了。

无论你是否同意我的比喻，至少你需要承认，现在的进程，即使它是单进程，但是在多线程的作用下，它已经呈现出不确定性了。在超弦理论中，基本粒子，是一根弦在一个蜷缩在时空中的高维空间中的振动形式。依照这个比喻，我们可以认为，上述程序是主线程在一个「三维空间」中的振动形式，程序的不确定性因此而出现。超弦理论通过在数学上构造了一个蜷缩在时空中的六维空间作为弦的振动空间，从而兼容了量子力学的本质——不确定性。

以下程序，可以让进程的不确定性更为清晰：

```c
/* foo.c */
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int a = 0;

void *foo(void *arg) {
        for (size_t i = 0; i < 10; i++) {
                a++;
                sleep(0.1);
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
        printf("a = %d\n", a);
        return 0;
}
```

上述程序中，当线程 `foo_t` 和 `bar_t` 同时执行例程时，全局变量 `a` 的值会被它们修改。这种修改，在逻辑上，似乎并没有改变 `a` 的值，因为修改过程是让 `a` 增 1，线程主动阻塞 0.1 秒以模拟线程做了一些其他工作，然后再让 `a` 减 1，`a` 的最终结果应该还是 0。但是，倘若多次执行上述程序，例如执行 100 次：

```console
$ for ((i = 1; i <= 100; i++)); do ./foo; done
```

你会发现，虽然多数情况下，程序 foo 输出的 `a` 值为 0，但是偶尔会出现 `a` 值为 1 或 -1 的情况，我不能确定是否还存在其他情况，但无论如何，倘若有其他程序依赖这个程序的输出的 `a` 值为 0，那么很有可能会导致一场灾难，例如用于发射宇宙飞船的程序。

为什么会如此呢？自然是因为线程 `foo_t` 和 `bar_t` 是同时运行的，然而 `a` 是个全局变量，二者同时对 `a` 的值进行修改，若是 `foo_t` 在某一步修改 `a` 值时，它面对的是 `bar_t` 所修改的结果，可能会出现错乱，反之亦然。

# 互斥

我们将可能会导致多线程程序出现数据不确定性的区域称为临界区，为了保证程序运行结果的确定性，需要对临界区加以保护，以避免多个线程在该区域出现冲突。
