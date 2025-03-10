---
title: Socket 是什么？
abstract: 一个简单的概念是如何变得难以理解的？
date: 02 月 26 日
...

我从未想到过，终有一天我会写一份文档，介绍如何用 C 编写网络程序，原因是这么多年了，我从未学会网络编程。也许国内计算机专家将 socket 这个英文单词翻译「套接字」的时候，就已经注定了我很难学会它。

网络编程，无论理论家们是在一本又一本厚厚的教材里是如何阐述它的，但在现实中，它就是面向 socket 的编程。

何为 socket？socket 的本意是插座、插口、窝、槽、臼等。国内计算机网络专家将其译为「套接字」着实是自以为是的不知所云。将电源插头插到墙上的套接字上，将灯泡拧到套接字上，他的两眼从套接字里鼓出来，他的胳膊脱套接字了……翻译学尊重的信、达、雅，这三个字，套接字竟然一个都套接不住。从信和达的角度，可将 socket 译为「网络端口」，简称网口，不过在这份文档中，我选择不译。

网络编程中的 socket，形式上由网络上进行通信的两台计算机各自的 IP 地址加上某个端口的编号构成，亦即一个网络地址序对，例如

```plain
(192.168.0.31:31225, 192.168.0.10:8080)
```

其中冒号之前的部分是 IP 地址，例如 192.168.0.10，之后的部分是端口号，例如 8080。socket 的用途是，为网络中一台计算机上的某个进程与另一台计算机上的进程之间的通信建立信息传输通道。

何谓进程？进程就是活着的程序，亦即计算机中正在运行的程序。没有任何一个进程能完成人类所需要的所有的计算，需要无数的进程的相互协作，而协作的前提是进程之间可以实现通信。一个进程需要与同一机器上的其他进程通信，也需要与另一台机器上的进程通信，这就是网络编程要解决的主要问题。

现在要建立一个直觉，所谓网络通信，指的是运行在网络上某台机器或两台不同的机器上的两个进程的通信。假设进程 A 要给进程 B 发送信息，A 需要知道 B 的网络地址以及自己的网络地址，类似于你要给一个人写信，信封上需要写上他的地址，也要写上自己的地址。此时，B 并不需要知道 A 或任何一个其他进程要给它发送信息，但 B 需要在任何进程给它发送信息之前创建自己的网络地址并守着它，等待其他进程传过来的信息。当 B 等到了 A 发来的信息，可以不予理睬，也可以予以回复。若 B 打算回复信息，它需要从 A 发来的信息中找到 A 的网络地址，然后向 A 发送信息。同理，A 在向 B 发送信息后，会一直守着自己的网络地址等待着 B 的回复。

进程 A 和 B 一直守在自己的网络地址等待回复，有些愚蠢，但人类有时会宁愿如此愚蠢，例如抱柱的尾生。更聪明一些的办法是，进程可以忙一些别的事情，只是每隔一段时间检查自己的网络地址，看有没有已经传过来的信息。最聪明的做法是，建立类似于邮局之类的事务，让进程彻底无需守候信息的到来。

上述进程 A 和 B 的通信方式即所谓的全双工通信，即通信的两端，皆可独立发送信息和接受信息。由于是进程 A 先向 B 发送信息，这意味着 A 对 B 有所求，故而称 B 是服务端（server），而 A 是客户端（client），任何一个网络程序的形式都是 c/s 架构。

进程之间基于 socket 的通信，使得进程可以是异构的。例如，上述的进程 A 可以是 C 语言写的，而 B 可以是 Java 写的，只要它们都遵守基于 socket 通信这个约定，便可以实现通信。无数个进程之间的通信，便构成了 Internet。计算机网络通信实际上并没有任何难以理解之处，它的理论和模型没有任何比传统的邮政系统更先进的地方，其先进之处仅仅是信息的传输速度更快，从而在信息传输方面几乎取代了邮政。

有时，我很怀念写信的那个年代，因为那个年代能够告诉我一个朴素的概念，socket 像一个写着通信双方地址的信封。
