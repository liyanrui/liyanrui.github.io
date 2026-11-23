自从习得 go 协程，hi.go 在初冬的白杨树下苦思 1 日，似有所得。它说，若有命令通过 /tmp/hi-pipe 管道向我发来请求时，该请求会触发路由函数，若将路由函数化为 go 协程，岂不意味着我可以同时响应很多请求么？我让它先写出代码来看。它写出

```${language}
@ 触发路由函数，以协程形式运行 #
var response string
if routers[request] != nil {
        go routers[request](&response, request)
} else {
        go routers["/"](&response, request)
}
fmt.Println(response)
@
```

这想法大致可行，只是路由函数会与 `main` 函数同时运行，`respoonse` 变量的值由前者赋予，故而它在后者中得以输出时，可能尚未有值。hi.go 说，`response` 的值是路由函数赋予的，既然如此，为何不直接在路由函数直接将其输出呢？

我说，也许以后我们要对 `response` 的值再作一些包装，例如在其前后增加一些信息。若是在路由函数内便将其输出，就失去包装它的机会了。若是让路由函数代为包装，则意味着可能每个路由函数都要重复写一段包装代码。实际上，我们应该将 `response` 改为通道变量，亦即

```GO
@ response 通道 #
response := make(chan string)
@
```

路由表以及 `handle_func` 函数也要改为通道形式，

```GO
@ 路由表：通道版本 #
var routers = make(map[string]func(chan string, string))
@
```

```GO
@ handle_func 函数：通道版本 #
func handle_func(path string, handler func(chan string, string)) {
        routers[path] = handler
}
@
```

用作示例的路由函数 `foo` 需要修改为

```GO
@ handle 实例 foo：通道版本 #
func foo(w chan string, r string) {
        w <- fmt.Sprintf("对于 %s，我认为烟涛微茫信难求！", r)
}
@
```

依然基于 /tmp/hi.sig 管道实现 hi.go 的阻塞，

```GO
@ /tmp/hi.sig 开关 #
sig, _ := os.Open("/tmp/hi.sig")
io.ReadAll(sig)
sig.Close()
@
```

然后将路由函数的触发过程修改为

```GO
@ 触发路由函数，以协程 + 通道形式运行 #
if routers[request] != nil {
        go routers[request](response, request)
} else {
        go routers["/"](response, request)
}
result := <- response
fmt.Println(result)
@
```

现在，将 hi.go 升级为

```GO
@ hi.go：协程 + 通道 #
package main

import (
        "fmt"
        "os"
        "io"
        "strings"
        "bufio"
)

# 路由表：通道版本 @
# handle_func 函数：通道版本 @
# handle 实例 foo：通道版本 @
# observer 函数 @

func main() {
        # 启动 observer @
        # 建立路径 / 到 foo 的映射 @
        input, err := os.Open("/tmp/hi-pipe")
        if err != nil {
                fmt.Fprintln(os.Stderr, err)
                return
        }
        # response 通道 @
        for {
                # /tmp/hi.sig 开关 @
                scanner := bufio.NewScanner(input)
                for scanner.Scan() {
                        # 从当前一行的内容中获取请求 @
                        # 触发路由函数，以协程 + 通道形式运行 @
                }
        }
        input.Close()
}
@
```

我让 hi.go 跑了一下试试。hi.go 认为，现在觉得一举一动，轻松自如。我说，这应该只是 go 协程给了你某种心理暗示，导致你产生了错觉，实际上你跟之前几乎一样笨重。

当你用 `go` 运行函数 `foo` 时，它的确是与 `main` 函数同时运行了，可是随后从 `response` 通道取值时，`main` 函数中的内层 `for` 循环会被阻塞，亦即在该循环里没处理一次请求，都会被 `response` 通道取值语句阻塞一次。这样相当于所有的请求依然是在排队且被逐一处理，而不是你所想象的，有一组请求同时被处理了。

hi.go 思索片刻说，我明白了，应该将通道取值过程从 `main` 函数的内层 `for` 语句里移出来，单独构成一个循环，亦即

```GO
@ 并发处理请求，逐一作出响应 #
n := 0
scanner := bufio.NewScanner(input)
for scanner.Scan() {
        # 从当前一行的内容中获取请求 @
        if routers[request] != nil {
                go routers[request](response, request)
        } else {
                go routers["/"](response, request)
        }
        n++
}
for i:= 0; i < n; i++ {
        result := <- response
        fmt.Println(result)
}
@
```

我说，你能想到要统计通道写入数据的次数，并基于该次数从通道取数据，甚为明智。hi.go 说，过奖了，有没有无需统计的办法？我说，不知道，即使有，未必就比统计更好。现在，我将你更新为

```GO
@ hi.go：并发 + 通道 #
package main

import (
        "fmt"
        "os"
        "io"
        "strings"
        "bufio"
)

# 路由表：通道版本 @
# handle_func 函数：通道版本 @
# handle 实例 foo：通道版本 @
# observer 函数 @

func main() {
        # 启动 observer @
        # 建立路径 / 到 foo 的映射 @
        input, err := os.Open("/tmp/hi-pipe")
        if err != nil {
                fmt.Fprintln(os.Stderr, err)
                return
        }
        # response 通道 @
        for {
                # /tmp/hi.sig 开关 @
                # 并发处理请求，逐一作出响应 @
        }
        input.Close()
}
@
```

hi.go 又跑了几次，感慨道，现在的确身轻如燕……处理请求的过程可谓从容不迫，作出响应的过程可谓有条不紊。
