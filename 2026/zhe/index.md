---
title: zhe 脚本
subtitle: 
date: 2026 年 10 月 26 日
...

# m4 语言

写文档或者写程序代码时，难免会有所修改。若修改之处存在多处，所有文本编辑器都会提供基本的查找/替换功能，用于对内容批量修改，例如将所有的 `foo_1` 修改为 `foo-1`。更好的文本编辑器会提供基于正则表达式的查找/替换功能，从而批量修改一些相似的内容，例如将 `foo_1`，`foo_2`……修改为 `foo-1`，`foo-2`……可是，若想基于待修改的相似内容中的部分文本作出不同的内容替换，例如我想将所有的 `foo`，按其在内容中的出现次序，依次替换为 `foo-1`，`foo-2`……这种任务对于多数文本编辑器而言通常是超纲的，需要编写脚本程序来解决，然而还有另一种可能更为节约的方案，用宏编程语言编写代码，完成文本替换。

m4 是微小但完备的宏语言，倘若在其 `-1` 空间定义宏 `foo`，

<pre class="m4-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-1"> foo.m4 </span><span class="snippet-name-stop">#</span>
divert(-1)dnl
define(`_N_', 0)
define(foo, `define(`_N_', incr(_N_))`foo'-_N_')
divert(0)dnl
</pre>

假设有一段话，

<pre class="text-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-2"> foo.txt </span><span class="snippet-name-stop">#</span>
佛曰 foo，即非 foo，是名 foo。
</pre>

m4 解释器读入 foo.m4 和 foo.txt 并处理它们，

```console
$ m4 foo.m4 foo.txt
```

相当于将 foo.m4 作用于 foo.txt，使得 foo.txt 中的 `foo` 皆以 foo.m4 中 `foo` 宏的定义展开，从而形成

```
佛曰 foo-1，即非 foo-2，是名 foo-3。
```

m4 宏可以有参数，这意味着 m4 语言能够完成更为复杂的文本替换任务。例如，定义宏 `start` 和 `stop`，

<pre class="${language}-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-3"> start-stop.m4 </span><span class="snippet-name-stop">#</span>
divert(-1)
define(`_start_', `pushdef(`block_name', $1)\start$1')
define(`_stop_', `\stop`'block_name`'popdef(`block_name')')
divert(0)dnl
</pre>

若对

<pre class="TEX-lang"><span class="snippet-name-start">@</span><span class="snippet-name" id="xi-4"> ctx-foo.tex </span><span class="snippet-name-stop">#</span>
\start(text)
... ... ...
\start(itemize)
\item ... ... ...
\item ... ... ...
\stop
\stop
</pre>

执行

```
$ sed "s/\\\start\>/_start_/g; s/\\\stop\>/_stop_/g" ctx-foo.tex
  | m4 start-stop.m4 -
```

结果为

```TEX
\starttext
... ... ...
\startitemize
\item ... ... ...
\item ... ... ...
\stopitemize
\stoptext
```