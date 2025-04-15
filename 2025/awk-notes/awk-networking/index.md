---
title: Awk 网络编程指南
abstract: 用 gawk 编写网络程序。
date: 02 月 23 日
...

对于一些在使用 Awk 语言处理文本方面颇有经验的人，他们甚至未必认为 Awk 语言能够实现网络编程。的确如此，Awk 语言并不支持网络编程，但是 gawk 改变了这个事实。gawk 对 Awk 语言进行了扩展，网络编程便在其中，于 gawk 3.1 版本实现，详见「[Gawkinet: TCP/IP Internetworking with Gawk](https://www.gnu.org/software/gawk/manual/gawkinet/)」。

# 双向管道

学习 gawk 的网络编程，需要先理解 gawk 对单向管道的扩展，即双向管道。gawk 网络编程是基于双向管道实现的。

标准的 Awk 语言支持管道。例如

```console
$ awk 'BEGIN { print "123456789" | "rev" }'
987654321
```

`print` 输出的内容通过管道 `|` 传送给了 Linux 命令 `rev`，由后者反转文本，但是这个管道是单向的，Awk 程序将输出内容传给 `rev` 之后，而 `rev` 的结果无法传回 Awk 程序。若 Awk 程序想获得 `rev` 的结果，只能是基于临时文件的方式，将 `rev` 的输出保存为一份临时文件，Awk 程序读取该文件的内容。

gawk 对 Awk 的管道进行了扩展，使之支持双向通信。双向管道的符号是 `|&`。使用双向管道连接 `rev`，从 `rev` 取回结果的代码如下：

```awk
print "123456789" |& "rev" # 向 rev 发送数据
"rev" |& getline           # 从 rev 获取数据
```

通过双向管道与 Awk 程序连接的程序称为协同进程（Coprocess），上例中的 `rev` 便是协同进程。不过，上述代码实际上会导致程序死锁。因为 `rev` 在等待输入结束，但 `print` 语句无法给出输入结束标识。同时，`getline` 在等待 `rev` 的输出。在协同进程输出全部数据后，需使用 `close` 函数关闭双向管道的 `to` 端——向协同进程发送数据的通道，然后协同进程得到输入结束标识，故而上述代码需修改为

```awk
print "123456789" |& "rev"; close("rev", "to")
"rev" |& getline
```

双向管道的 `from` 端是协同进程向 Awk 程序传输数据的通道。由于协同进程的输出会带有结束标记，故而无需显式关闭 `from` 端，亦可将其显式关闭：

```awk
print "123456789" |& "rev"; close("rev", "to")
"rev" |& getline; close("rev", "from")
```

# 特殊文件

gawk 的网络编程基于双向管道实现，只是与管道连接的不再是协同进程，而是一种特殊文件，其形式如下：

```awk
# /网络类型/协议/本地端口/主机名/远程端口
/net-type/protocol/localport/hostname/remoteport
```

这种特殊文件形式，即可用于表达服务器端，也可表达客户端。例如，将本机作为服务器端，使用 IP v4 网络类型和 TCP 协议，以端口 8080 提供某种网络服务，则特殊文件的写法为

```awk
/inet4/tcp/8080/0/0
```

若某个客户端访问该服务器端，则客户端对应的特殊文件写法为

```awk
/inet4/tcp/0/服务器地址(IP 或域名)/8080
```

这种特殊文件的写法，除了网络类型和协议之外的其他部分，无论是服务器程序还是客户端程序，只要自身无需关心的部分，皆写为 0。例如，作为服务器端的程序，它不必关心 `hostname` 和 `remoteport`，因为它不需要主动访问其他机器上的进程，而作为客户端的程序则不必关心 `localport`，因为没有主机会主动访问它。

gawk 将网络编程的 socket（所谓的套接字）创建过程隐含在上述特殊文件的构建过程中了，从而显著简化了网络程序的编写，但是也牺牲了实现能够应对复杂需求的网络程序的可能性，亦即使用 gawk 通常只能编写简单的网络程序。工业级的网络程序，例如支持网络并发访问和反向代理等功能的服务器程序，需使用其他编程语言在底层的 socket 层面方能实现。

服务器端程序，向与之连接的客户端程序发送信息，只需将信息发送到特殊文件。例如

```awk
print "Hello world!" |& "/inet4/tcp/8888/0/0"
```

服务器端程序从与之连接的客户端程序读取信息，只需从特殊文件读取信息。例如

```awk
"/inet4/tcp/8888/0/0" |& getline  # 从客户端程序读取一条记录，存于 $0
```

同理，客户端程序从与之连接的服务器端程序获取信息，只需从特殊文件读取信息。例如

```awk
"/inet4/tcp/0/服务器地址/8888" |& getline  # 从服务器端程序读取一条记录，存于 $0
```

客户端程序向与之连接的服务器端程序发送信息，只需向特殊文件写入数据。例如

```awk
print "hi" |& "/inet4/tcp/0/服务器地址/8888"
```

分别用于表示服务端和客户端的特殊文件，二者存在一个重叠，即端口。在服务端，端口是本机端口；在客户端，端口是远程端口，二者所指对象都是服务器上的那个端口。可以将服务器理解为有许多房间的一栋楼，端口是房间的门牌号。

特殊文件的网络协议字段是 `inet4` 或 `inet6`，分别表示 IP v4 和 IP v6。可以简写为 `inet`，此时若系统环境使用的是 IP v4 网络，则 `inet` 表示 `inet4`，若系统环境为 IP v6 网络，则 `inet` 即 `inet6`。

# Hello world!

下面的脚本构造了一个会说「Hello world!」的服务端程序 server.awk：

```awk
BEGIN {
    print "Hello world!" |& "/inet/tcp/8888/0/0"
}
```

运行 server.awk：

```console
$ awk -f server.awk
```

现在 server.awk 程序会在本机的 `8888` 端口等待客户端的访问。用网络编程术语描述，这个过程是**阻塞**的，即上述程序通过双向管道向特殊文件写入数据后，会停止执行，直到有客户端程序发起连接。

客户端程序通过特殊文件访问运行 server.awk 程序的主机的 `8888` 端口时，可以得到 server.awk 发送过来的数据。下面是位于运行 server.awk 的机器上的一个客户端程序 client.awk：

```awk
BEGIN {
    "/inet/tcp/0/localhost/8888" |& getline
    print $0
}
```

由于 client.awk 访问的服务器就是本机，故而网络主机地址是 `localhost`，亦可写为 `127.0.0.1`。运行 client.awk，可在屏幕上打印来自 server.awk 的「Hello world!」，

```awk
$ awk -f client.awk
Hello world!
```

然后 client.awk 和 server.awk 分别自动结束运行，各自所用的特殊文件也会被自动关闭。

# 端口查看

上一节实现的 server.awk 和 client.awk，皆未对表示网络连接的特殊文件进行显式关闭。学究一些，是需要显式关闭的，即

```awk
BEGIN {
    service = "/inet/tcp/8888/0/0"
    print "Hello world!" |& service
    close(service)
}
```

```awk
BEGIN {
    server = "/inet/tcp/0/localhost/8888"
    server |& getline
    print $0
    close(server)
}
```

实际上在 Awk 程序退出时，会自动关闭这些特殊文件。可以使用 `netstat` 命令做一个试验，验证这一观点。

首先，执行上一节所写的 server.awk 程序，然后使用以下命令查看本机上哪个进程正在使用 TCP 协议并占用端口 `8888`：

```console
$ sudo netstat -tlp | grep ":8888"
tcp    0    0 0.0.0.0:8888    0.0.0.0:*    LISTEN    36099/awk
```

结果显示是一个 awk 程序。

现在若再一次执行 server.awk 程序，会出错：

```console
awk: server.awk:2: fatal: cannot open two way pipe 
  `/inet4/tcp/8888/0/0' for input/output: Address already in use
```

现在执行上一节的 client.awk 程序，访问 server.awk，然后二者自动退出。

再一次执行上述的 `netstat` 命令，则没有任何信息输出了，这意味着 `/inet4/tcp/8888/0/0` 已被关闭。

上述 `netstat` 命令所使用的各选项的含义如下：

* `-t`：查看 TCP 连接。若查看 UDP 连接，使用 `-u`。
* `-l`：显示所有处于监听（LISTEN）状态的端口（常用于检查服务是否启动）。
* `-p`：显示占用端口的进程名和 PID（需超级用户权限）。

# 简单的服务器

有的时候，必须关闭连接，例如实现一个可以持续运行的服务器。前两节所实现的 server.awk，在客户端访问一次后便终止退出了，它无法持续运行。要让一个服务端程序持续运行，只需要在一个无限循环中不断开启和关闭即可，即

```awk
BEGIN {
    while (1) {
        service = "/inet/tcp/8888/0/0"
        print "Hello world!" |& service
        close(service)
    }
}
```

对上述代码略加修改，便可构造一个简单的可持续运行的 HTTP 服务器：

```awk
BEGIN {
    RS = ORS = "\r\n"
    http_service = "/inet/tcp/8888/0/0"
    hello = "<html><head>" \
            "<meta charset=\"utf-8\" />" \
            "<title>一个著名的问候</title></head>" \
            "<body><h1>你好，世界！</h1></body></html>"
    n = length(hello) + length(ORS)
    while (1) {
        print "HTTP/1.0 200 OK"        |& http_service
        print "Content-Length: " n ORS |& http_service
        print hello                    |& http_service
        while ((http_service |& getline) > 0) {
            continue
        }
        close(http_service)
    }
}
```

假设将上述代码保存为 http-server.awk，执行该程序：

```console
$ awk -f http-server.awk
```

在运行该 HTTP 服务器程序的机器上，打开网络浏览器，在地址栏输入 `localhost:8888`，便可访问该服务器。我的 Firefox 浏览器所得结果如下图所示：

![简单的 HTTP 服务器访问结果](figures/simple-http-server.png)

若要理解上述 http-server.awk 的代码，需要懂得 HTTP 报文的基本知识，不懂也没关系，知道 `hello` 的值是 HTTP 报文并且知道 HTTP 报文是 HTTP 协议的一部分即可。

上述代码中第一层 `while` 循环可以保证服务器持续运行，而在该循环内部，除了将 HTTP 报文发送给 `http_service` 连接的代码外，最为关键的是下面这段代码：

```awk
while ((http_service |& getline) > 0) {
    continue
}
close(http_service)
```

上述这段代码，用于读取客户端（网络浏览器）向连接传送的信息，但是这些信息皆被忽略了，当客户端的信息传送完毕后，`while` 中的双向管道的结果不再是正数，故而 `while` 循环终止，继而连接被关闭。若对来自客户端的信息感兴趣，可在 `while` 循环中将信息打印出来：

```awk
while ((http_service |& getline) > 0) {
    print $0
    continue
}
close(http_service)
```

至于 http-server.awk 向连接发送的报文是如何被网络浏览器端获得并呈现，那是网络浏览器的任务，在本质上它与前文我们所写的 client.awk 并无不同，当然在实现上会复杂好多个数量级。

# 可交互的服务器

若懂得 CGI 协议，可以将 http-server.awk 修改为一个可以支持在网页上动态交互的 HTTP 服务器。对此，我现在没兴趣，暂且略过。「[Gawkinet: TCP/IP Internetworking with Gawk](https://www.gnu.org/software/gawk/manual/gawkinet/)」的 2.9 节实现了一个可交互的 HTTP 服务器，不过示例代码并不稳健——服务器的连接可能会因超时而意外断开。

# 局限

gawk 基于双向管道实现的网络连接和数据传输，服务端无法支持并发访问。例如上一节实现的 http-server.awk，运行该服务器程序后，可以使用 telnet 访问它：

```awk
$ telnet localhost 8888
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
HTTP/1.0 200 OK
Content-Length: 102

<html><head><meta charset="utf-8" /><title>一个著名的问候</title></head><body><h1>你好，世界！</h1></body></html>
```

此时保持上述 telnet 的连接未断，再次向服务端发起连接，会被拒绝：

```awk
$ telnet localhost 8888
Trying 127.0.0.1...
telnet: Unable to connect to remote host: Connection refused
```

这意味着 http-server.awk 所实现的服务器无法支持两个并发连接。原因是什么呢？相当于电话占线。gawk 将复杂的网络连接过程封装为可与双向管道配合使用的特殊文件形式，便意味着无法让网络连接支持更为复杂的需求了。

# 总结

用 Awk 语言编写的网络程序虽无大用，但是对于熟悉网络编程并建立一些工程直觉有所裨益。对于 Awk 编程本身而言，由于网络的透明性，Awk 程序可以将一些复杂的计算任务交于其他进程，这个进程可以是运行于本机的，也可以是运行于同一网络上的其他机器上的，且其对应的程序也可以是由其他语言编写，从而可以弥补 Awk 语言的不足，从这一点而言，gawk 为 Awk 语言所作的网络编程扩展，其意义深远。

...

