# 多闻数穷，不若守中

也许你已经发现了，上一节所写的 gnuplot.go 程序在本质上依然不支持多用户同时在线访问，它没有真正解决问题，只是将问题从固定的两个文件转移到了计数器身上了。问题集中在下面这几行代码上，

@ 更新 gp_id #
if gp_id > 99 {
        gp_id = 0
}
gp_id++
gp_name := fmt.Sprintf("drawing-%d", gp_id)
@

当多个用户同时向 gnuplot.go 发起请求时，后者便会同时执行上述代码多次，由于 `gp_id` 是全局变量，于是又出现了多次同时修改同一个对象的情况。可能用户 A 拿到的 `gp_id` 值本该是 1，但是用户 B 同时执行了这段代码，导致 A 拿到的值是 2。观音虽有百手，但其本体却只有 1 个，`gp_id` 则是其本体的一部分。

为了克服这类问题，Go 语言的 `sync` 包提供了互斥锁机制。该机制的想法非常简单，就是给一段代码加一把锁，所有的同时发生的事情，在这把锁面前，都要排队，然后再逐一穿过这段代码。锁本身也是一个全局变量，例如

@ gp_id 锁 #
var gp_id_mu sync.Mutex
@

对上述代码加锁和解锁的过程可写为

@ 更新 gp_id # ^+
gp_id_mu.Lock()
@

@ 更新 gp_id # +
gp_id_mu.Unlock()
@

上述代码我用了两个新的文学编程标记，`^+` 表示指定的代码片段之前插入代码，`+` 则表示在指定的代码片段之后插入代码。上述示例产生的效果是，在名字为「`更新 gp_id`」这段代码之前添加

```go
gp_id_mu.Lock()
```

然后在其之后添加

```go
gp_id_mu.Unlock()
```

于是，意味着名字为「`更新 gp_id`」这段代码最终变为

```
gp_id_mu.Lock()
if gp_id _ESC_GT_ 99 {
        gp_id = 0
}
gp_id++
gp_name := fmt.Sprintf("drawing-%d", gp_id)
gp_id_mu.Unlock()
```

如此一来，对 `gp_id` 的修改和取值操作便会被 `gp_id_mu` 形成的保护区锁定，所有需要访问这个区域的程序流，都要在操作系统中排队，逐一进入。排队自然是要以时间为代价，亦即 gnuplot.go 程序不如之前的版本跑得快，只是用户通常感受不到这点差异，毕竟 gnuplot.go 在调用 `gnuplot` 命令时耗费的时间远大于更新 `gp_id` 的值。

现在，正式写出一个真正可用的 gnuplot.go 程序，并且再普及如何用文学编程的方式定义一些代码片段，并将其组装为完整的程序。首先，需要写出程序几乎千篇一律的开头，它们的区别通常仅仅是所需要的包有所不同，

@ gnuplot.go 程序开头 #
package main

import (
        # gnuplot 服务所需要的包 @
)
@

gnuplot.go 需要以下包

@ gnuplot 服务所需要的包 #
"fmt"
"net/http"
"strings"
"os"
"os/exec"
"sync"
@

gnuplot.go 的网页界面保存在全局变量 `page` 中，

@ gnuplot 的网页界面 #
var page = `<!DOCTYPE html>
<html>
<head><meta charset="utf-8"></head>
<body>
  <form method="POST" style="width: 480px">
    <textarea name="gnuplot-code" rows="8"
              spellcheck="false"
              style="display: block;
                     width: 100%;
                     box-sizing: border-box;
                     resize: vertical">{{CODE}}</textarea>
    <button style="display: block;
                   margin-left: auto;
                   margin-bottom: 1em">提交</button>
  </form>
  {{IMG}}
</body>
</html>`
@

对于用户从网页文本框中提交的文本，处理过程为

@ gnuplot 函数 [v2] #
var gp_id = 0
# gp_id 锁 @
func gnuplot(w http.ResponseWriter, r *http.Request) {
        var code = "plot sin(1/x)"
        out := strings.Replace(page, "{{CODE}}", code, 1)
        input := r.FormValue("gnuplot-code")
        if input != "" {
                # 更新 gp_id @
                gp_file_name := gp_name + ".gp"
                os.WriteFile(gp_file_name, []byte(new_input), 0644)
                cmd := exec.Command("gnuplot", gp_file_name)
                cmd.Run()
                out = strings.Replace(page, "{{CODE}}", input, 1)
                img := `<img src="` + gp_name + `.png" width="480px" />`
                out = strings.Replace(out, "{{IMG}}", img, 1)
        }
        fmt.Fprint(w, out)
}
@

在 gnuplot.go 的 `main` 函数中，设定路由函数，并启动网络服务，

@ gnuplot.go 的 main 函数 #
func main() {
	http.HandleFunc("/gnuplot", gnuplot)
	http.Handle("/", http.FileServer(http.Dir(".")))
	http.ListenAndServe(":8080", nil)
}
@

将上述代码片段组装起来，便可得到 gnuplot.go 程序，

@ gnuplot.go 程序 #
# gnuplot.go 程序开头 @
# gnuplot 的网页界面 @
# gnuplot 函数 [v2] @
# gnuplot.go 的 main 函数 @
@

若已经装了 xi 程序，且已获得本文档的源文件「[go-with-me.xi](https://liyanrui.github.io/2026/go-with-me/go-with-me.xi)」，执行

```console
$ xi -t -e "gnuplot.go 程序" -o gnuplot.go go-with-me.xi
```

便可得到完整的且含有本节更新内容的 gnuplot.go 程序，它已经是一个可以真正使用的网络服务程序了，只是该程序调用的 `gnuplot` 命令不是足够安全，不过运行该程序，仅向自己的朋友提供服务基本可行。
