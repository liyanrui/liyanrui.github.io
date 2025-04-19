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

# 叮咚

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

# 历史

我更喜欢将线程例程的返回值视为历史，就像弹奏的琴音会对应到乐谱上的某个符号——这个符号表征着琴音的某个频率。下面的程序构造了一个有历史的线程：

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

请记住这句话，所有的不确定性，它更深刻的内涵可能是，宇宙的运行原理依赖的是某种并发机制，而不确定性不过是我们对该机制的最为直接的感受。你扔出了一枚硬币，它出现正面的概率是 1/2，此事亦可理解为，是因你存在的概率为 1/2 而导致了这一结果。

你习惯了世界是确定的，应该会觉得我所说的，接近癫狂，但是也许真的存在不确定性的转移呢？我认为，同时通过双缝的电子，它之所以会在观测中坍缩，原因是电子将自身的不确定性转移到了观测设备上了。

无论你是否同意我的比喻，至少你需要承认，现在的进程，即使它是单进程，但是在多线程的作用下，它已经呈现出不确定性了。在超弦理论中，基本粒子，是一根弦在一个蜷缩在时空中的高维空间中的振动形式。依照这个比喻，我们可以认为，上述程序是主线程在一个「三维空间」中的振动形式，程序的不确定性因此而出现。超弦理论通过在数学上构造了一个蜷缩在时空中的六维空间作为弦的振动空间，从而兼容了量子力学的本质——不确定性。

以下程序，可以让进程的不确定性更为清晰：

```c
/* foo.c */
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>

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

你会发现，虽然多数情况下，程序 foo 输出的 `a` 值为 0，但是偶尔会出现 `a` 值为 1 或 -1 的情况，我不能确定是否还存在其他情况，但无论如何，倘若有其他程序依赖这个程序的输出的 `a` 值为 0，那么很有可能会导致一场灾难，例如用于发射宇宙飞船的程序，但是对人类产生更大影响的是，历史的混乱可能会引发人类的战争。

为什么会如此呢？自然是因为线程 `foo_t` 和 `bar_t` 是同时运行的，然而 `a` 是个全局变量，二者同时对 `a` 的值进行修改，若是 `foo_t` 在某一步修改 `a` 值时，它面对的是 `bar_t` 所修改的结果，可能会出现错乱，反之亦然。

# 服务端并发

主线程创建的多个子线程可以同时运行，这意味着我们可以像基于多进程机制实现 threebody 对多个客户端并发访问的支持。请将历史再度回退到「[封装](../wrapper/index.html)」时代。我们可以基于田园时代的的字符串类和网络类，结合多线程机制，在套接字函数皆为阻塞的情景中为 threebody 赋予与多进程并发相似的能力。

```c
/* threebody.c */
#include <unistd.h>
#include <pthread.h>
#include "sim-network.h"

void *handler(void *arg) {
        /* 类型转换 */
        SimServer *threebody_copy = arg;

        /* 从客户端接收数据 */
        SimStr *msg_from = sim_server_receive(threebody_copy);
        if (sim_str_safe(msg_from)) {
                printf("%s\n", sim_str_raw(msg_from));
        }
        sim_str_free(msg_from);
        
        /* 向客户端发送信息 */
        SimStr *msg_to = sim_str("threebody: Hi!");
        sim_server_send(threebody_copy, msg_to);
        sim_str_free(msg_to);
        
        /* 关闭客户端，释放 threebody 副本 */
        sim_server_close(threebody_copy);
        free(threebody_copy);
        return NULL;
}

int main(void) {
        SimServer *threebody = sim_server("localhost", "8080");
        while (1) {
                sim_server_run_once(threebody);
                /* 创建新的线程 */
                pthread_t t;
                SimServer *threebody_copy = sim_server_copy(threebody);
                pthread_create(&t, NULL, handler, threebody_copy);
                pthread_detach(t);
        }
        sim_server_free(threebody);
        return 0;
}
```

上述代码为了防止多个线程因共享 `SimServer` 对象 `threebody` 而带来的客户端套接字错乱，为每个新建的线程使用 `sim_server_copy` 构造了 `threebody` 的副本，该副本包含着新线程与之通信的客户端套接字。倘若不如此，那么很有可能会出现一个线程所用的客户端套接字会被主线程的 `sim_server_sun` 在接受新的客户端连接时修改而失效。

`sim_server_copy` 的声明和定义如下：

```c
/* sim-network.h ++ */
/* 构造的副本，可用 free 释放 */
SimServer *sim_server_copy(SimServer *server);
```

```c
/* sim-network.c ++ */
SimServer *sim_server_copy(SimServer *server) {
        if (!server) return NULL;
        SimServer *copy = malloc(sizeof(SimServer));
        if (!copy) {
                fprintf(stderr, "sim_server_copy error!\n");
                return NULL;
        }
        *copy = *server;
        return copy;
}
```

还需要注意的是，上述的 threebody.c 中在创建新线程 `t` 后，又将 `t` 传给  `pthread_detach` 处理，该函数的作用是，让 `t` 脱离主线程而独立运行，可避免在主线程中使用 `pthread_join` 回收子线程。若不用此法，在主线程中使用 `pthread_join` 回收子线程，会导致主线程阻塞，亦即导致服务端的并发能力丧失。

编译 threebody.c，运行所得程序 threebody：

```console
$ gcc -I. sim-str.c sim-network.c threebody.c -o threebody
$ ./threebody
```

然后打开两个终端，分别用于运行「[两朵乌云](../blocking/index.html)」中的 other-ywj 和 ywj。先在一个终端里运行 other-ywj，它与 threebody 建立连接后，会主动延迟 15 秒，然后才开始与 threebody 通信。随即在另一个终端迅速运行 ywj。

上述试验所得结果与「[世界是你的，也是我的](../process/index.html)」中的服务端多进程并发试验结果相似，ywj 无需等到 other-ywj 得到 threebody 的回复后放能与 threebody 通信。理论上，线程的切换速度远胜进程，故而服务端基于多线程的并发，性能通常优于多进程的并发。

线程与网络编程的联系，言尽于此。关于线程本身，却还有一些有趣且值得探究的方向。

# 互斥

多个线程的并行，若它们之间共享数据，便会导致混乱。我们无法消除这些混乱，但是却可以对其施加约束，这是操作系统赋予我们的能力。

我们将可能会导致多线程程序出现数据不确定性的区域称为临界区。为了保证程序运行结果的确定性，需要在临界区对线程加以约束，以避免它们在该区域出现冲突。这种约束，用计算机学科的术语表达，就是同步，即所有的线程到了临界区，都不要着急，都要乖乖等待操作系统的调度，在得到许可后方能访问临界区。站在人生的十字路口，我该何去何从？很简单，红灯停，绿灯行。

前言中希望用超弦理论比喻多线程机制，上文多少有些偏离该主旨，不过超弦理论也实在过于抽象，它也必须附着在人类对琴弦的认知上。多线程机制的临界区约束，犹如弹古筝、琵琶或吉他时，需要摁压琴弦，否则无法弹出富有变化的旋律。我不懂器乐，甚至对任何称作音乐的知识，几乎一无所知，所以我也不知道这样说是否正确。若有偏颇，望你不吝赐教。与摁压琴弦类似的现象，在超弦理论中同样存在，即所有的开弦必须附着在某种结构上，诸如 D 膜。一条开弦，可以附着在多个 D 膜上……不要问我何谓 D 膜，我也不知道，犹如我不懂音乐。

Unix 或 Linux 系统中对临界区的施加的约束称为互斥（Mutex），不过软件工程中通常称之为「锁」，用于标定某块代码，不容多线程同时访问。互斥是系统资源，以 `pthread_mutex_t` 类型表示。互斥最简单的用法如下：

```c
/* 全局互斥（或全局锁） */
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *foo(void *arg) {
        for (size_t i = 0; i < 10; i++) {
                pthread_mutex_lock(&mutex); /* 加锁 */
                a++;
                sleep(0.1);
                a--;
                pthread_mutex_unlock(&mutex); /* 解锁 */
        }
        return NULL;
}
```

上述代码片段，定义了一个以全局变量形式存在的互斥 `mutex`，在函数 `foo` 中，用 `pthread_mutex_lock/unlock` 对临界区域进行标定，从而约束了同时访问临界区的多个线程，增强了进程运行的确定性。

基于上述代码修改上一节中的示例程序，便可保证程序的输出的 `a` 值不会出现非 0 的情况。

互斥的基本原理，想必你已基本理解了，你有没有发现，它很像之前所学习的网络编程中的阻塞？阻塞虽然简单，但性能不高，会导致很多线程在等待中浪费了太多时间。

# 内耗

看下面的程序：

```c
/* foo.c */
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

#define MAX_PRODUCED 5
int data;
int produced = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *producer(void *arg) {
        sleep(10); /* 模拟一些运算过程 */
        for (int i = 0; i < MAX_PRODUCED; i++) {
                pthread_mutex_lock(&mutex);
                /* 在区间 [0, 99] 中随机生成 1 个数字 */
                data = rand() % 100;
                produced++; /* 记录生成数据的次数 */
                printf("生产者生产数据: %d\n", data);
                pthread_mutex_unlock(&mutex);
                sleep(1); /* 模拟一些运算过程 */
        }
        return NULL;
}

void *consumer(void *arg) {
        while (1) {
                sleep(1); /* 模拟一些运算过程 */
                printf("消费者等待……\n");
                pthread_mutex_lock(&mutex);
                if (produced > 0) { /* 有数据产出 */
                        printf("消费者消费数据: %d\n", data);
                        produced--; /* 消费 1 条数据 */
                } else {
                        pthread_mutex_unlock(&mutex);
                        continue;
                }
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
```

这个程序实现了一个非常简单的生产者/消费者模型。`producer` 负责生产，`consumer` 负责消费。程序中使用了全局变量 `data` 表示被生产和消费的对象。

多线程例程 `producer` 和 `consumer` 共用了同一个互斥 `mutex`，这意味着虽然二者同时运行的，但是当二者之一对互斥进行锁定时，另一个例程若需要使用该互斥，则必须等待前者解除互斥锁定。如此便形成了一种奇特的现象，这两个例程发生了纠缠，就像是一个开弦在蜷缩在时空中的高维空间中发生了量子态范畴的自我纠缠……我也不知道我在说什么。我猜，你若是想理解我这句话，必须始终记住，例程 `producer` 和 `consumer` 不过是主线程在这个高维空间中的两种不同的振动形态，它们发生的纠缠，实际上还是主线程在该空间中的自我纠缠。

如果你编译上述程序并运行，得到的结果可能是

```
消费者等待……
消费者等待……
消费者等待……
消费者等待……
消费者等待……
消费者等待……
生产者生产数据: 83
消费者等待……
消费者消费数据: 83
生产者生产数据: 86
消费者等待……
消费者消费数据: 86
生产者生产数据: 77
消费者等待……
```

也许你敏锐地注意到了，当生产者尚未产生数据时，消费者不得不在空耗中等待，每次等待都要锁定互斥，解锁互斥……这个过程是颇为浪费 CPU，即使我故意用了 `sleep(1)` 主动进行阻塞，以免你看到太多次 `消费者等待……`。

还记得上次我们是如何解决网络编程中在非阻塞情况下服务端过多消耗 CPU 这一问题的吗？我们是利用了操作系统提供的同步 I/O 多路复用机制，而且我也多次强调，这种机制堪比计算机软件世界里的爱因斯坦的广义相对论。事实上，超弦理论……不，操作系统也为多线程机制中也提供了类似的功能。

# 超距作用

在使用互斥加以约束的临界区中，可以通过条件变量实现一个例程在运行时触发其他例程运行，同时该例程的运行也可由其他例程触发。

我们通过改写上一节中的两个线程例程，来理解条件变量的基本用法。首先，定义一个全局的条件变量：

```c
pthread_cond_t data_ready = PTHREAD_COND_INITIALIZER;
```

`data_ready` 便是条件变量。作为初学者，不必纠结 `PTHREAD_COND_INITIALIZER` 是什么含义以及有无其他值可初始化条件变量。

然后，在 `producer` 中，若数据准备好，就发出一个信号：

```c
void *producer(void *arg) {
        sleep(10); /* 模拟一些运算过程 */
        for (int i = 0; i < MAX_PRODUCED; i++) {
                pthread_mutex_lock(&mutex);
                /* 在区间 [0, 99] 中随机生成 1 个数字 */
                data = rand() % 100;
                printf("生产者生产数据: %d\n", data);
                pthread_cond_signal(&data_ready); /* 发出信号 */
                pthread_mutex_unlock(&mutex);
                sleep(1); /* 模拟一些运算过程 */
        }
        return NULL;
}
```

上述代码中的 `pthread_cond_signal` 可以通知某个正在等待 `data_ready` 这个条件变量的线程例程，数据准备好了！在我们的示例中，等待 `data_ready` 的是 `consumer`：

```c
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
```

在上述示例中，`pthread_cond_wait` 解锁 `mutex`，然后会主动阻塞，以等待其他例程发送的信号。收到信号后，它会锁定 `mutex`，然后返回，于是其后的语句得以继续执行。

用上述修改替换上一节示例中的两个例程，运行结果如下：

```
消费者等待……
生产者生产数据: 83
消费者消费数据: 83
消费者等待……
生产者生产数据: 86
消费者消费数据: 86
消费者等待……
生产者生产数据: 77
消费者消费数据: 77
消费者等待……
生产者生产数据: 15
消费者消费数据: 15
消费者等待……
生产者生产数据: 93
消费者消费数据: 93
消费者等待……
```

上述结果与上一节不同之处在于，在 `producer` 耗费 10 秒钟的时间准备数据时，`consumer` 被阻塞在 `pthread_cond_wait` 语句处，且不占用 CPU，直至 `producer` 通过 `pthread_cond_signal` 唤醒它。

使用 `pthread_cond_wait` 时需要注意，处于等待的线程例程可能会被虚假唤醒，而此时可能例程所需要的数据并未备好，从而导致进程依然会出现不确定性。为了保险起见，需对例程增加人为防御。例如：

```c
int produced = 0;
void *producer(void *arg) {
        sleep(10); /* 模拟一些运算过程 */
        for (int i = 0; i < MAX_PRODUCED; i++) {
                pthread_mutex_lock(&mutex);
                data = rand() % 100;
                produced++;
                printf("生产者生产数据: %d\n", data);
                pthread_cond_signal(&data_ready); /* 发出信号 */
                pthread_mutex_unlock(&mutex);
                sleep(1); /* 模拟一些运算过程 */
        }
        return NULL;
}

void *consumer(void *arg) {
        while (1) {
                printf("消费者等待……\n");
                pthread_mutex_lock(&mutex);
                while (produced == 0) {
                        pthread_cond_wait(&data_ready, &mutex);
                }
                printf("消费者消费数据: %d\n", data);
                produced--;
                pthread_mutex_unlock(&mutex);
        }
        return NULL;
}
```

我们用一个计数器 `produced`，为 `pthread_cond_wait` 增加了一层保险机制，即使 `pthread_cond_wait` 被误触，例程依然可以根据 `produced` 的值断定是否继续等待。

如果有多个线程例程都在等待同一个条件变量，`pthread_cond_signal` 只能通知其中一个，若全部通知可使用 `pthread_cond_broadcast`，二者用法相同。一个线程能够通知所有正在等待的线程，这让你想到了什么？前者像不像一个服务端，后者像不像连接到服务端的客户端？由于互斥的存在，被唤醒的多个线程，也只能逐一访问临界区。服务端/客户端模型，在理念上，完全可以称之为生产者/消费者模型。

这很像同步 I/O 多路复用机制，对吧？所以我说，多线程机制实际上能够统一同步 I/O 多路复用机制和多进程并发机制，犹如超弦理论能够统一广义相对论和量子力学。

# 总结

我不懂超弦理论，但是如果研究超弦理论的人，能尝试从计算机多线程机制中获得一些比喻，也许有助于他们能够写出更好的超弦理论科普书。不过，曾经存在着五种超弦理论，后来爱德华·威滕提出了膜理论，将五种超弦理论统一了起来，手法是除了引入膜结构，也为五种超弦理论所认为的十维宇宙模型增加了一个新的维度，论证了五种超弦理论只是这个新维度退化后的产物。也许你会问，那么计算机软件世界里，存在与 M 理论对应的机制吗？

