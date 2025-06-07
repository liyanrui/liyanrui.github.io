---
title: zhe 的设计与实现
subtitle: 
abstract: 以文学程序的形式，介绍中文宏处理器 zhe 的设计与实现。
date: 01 月 29 日
...

# 前言

理解本文的一切，需要对 M4、AWK 以及 Bash 语言有所了解。若需补充这些知识，可阅读文献 [1-3]。

M4 语言是一种宏语言，其语法与英文习惯相符，在视觉上很容易分辨普通文字和宏语句，但是对于中文宏，便显得甚为违和。例如，以下 M4 代码定义了中文宏 `宏一`：

```m4
define(宏一, 你好，我是宏一)
```

在 M4 语言中，中文宏名是非法的，只能以间接的形式调用：

```m4
indir(宏一)！
```

本文基于 AWK 和 Bash 语言，为 M4 中文宏实现更为直观方便的调用形式，作为 M4 语言的非侵入式扩展，只是该扩展缺乏完备性——可能不支持 M4 语言的全部内涵。

# 定义与调用分离

理论上，M4 宏可以随时定义随时调用，甚至可以永远在定义和调用，但是从通常用途的角度，定义通常放在负的空间，在零空间调用。例如

<pre id="foo.m4" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ foo.m4 #</span>
divert(-1)
define(宏一, 你好，我是宏一)
divert(0)dnl
宏一！
</pre>

我将上述 M4 代码中的

```m4
divert(-1)
define(宏一, 你好，我是宏一)
divert(0)dnl
```

视为宏的定义空间，将剩下的部分，称为宏的应用空间。如此划分空间，便于收集已定义的宏，同时也便于为中文宏构造间接调用。另外需要注意，在 M4 语言中，中文宏名的非法性，会导致上述代码中定义的 `宏一` 并不会被 m4 处理器无限展开，若是西文环境，则不然，例如：

```m4
divert(-1)
define(foo, `i am foo')
divert(0)dnl
expanding foo!
```

结果会导致 m4 处理器对 `foo` 递归展开，得到的是一个永远都没法结束的结果：

```
expanding i am i am i am i am i am i am ...
```

# 雏形

以下 AWK 脚本可以区分宏的定义空间和应用空间：

<pre id="foo.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ foo.awk #</span>
<span class="sr">/divert\(-1\)/</span> <span class="p">{</span>
    <span class="nx">action</span> <span class="o">=</span> <span class="mi">0</span>
    <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">next</span>
<span class="p">}</span>
<span class="sr">/divert\(0\)dnl/</span> <span class="p">{</span>
    <span class="nx">action</span> <span class="o">=</span> <span class="mi">1</span>
    <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">next</span>
<span class="p">}</span>
<span class="p">{</span>
   <span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">action</span><span class="p">)</span> <span class="p">{</span>
       <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
       <a href="#收集已定义的宏" class="orez-callee-link"># 收集已定义的宏 @</a>
   <span class="p">}</span> <span class="k">else</span> <span class="p">{</span>
       <a href="#为中文宏构造间接调用" class="orez-callee-link"># 为中文宏构造间接调用 @</a>
       <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
   <span class="p">}</span>
<span class="p">}</span>
</pre>

在宏的定义空间中，可以用一个数组，收集 `define` 语句定义的宏名：

<pre id="收集已定义的宏" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 收集已定义的宏 #</span>
<span class="nx">x</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
<span class="k">while</span><span class="p">(</span><span class="kr">match</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="sr">/define\((.*),/</span><span class="p">,</span> <span class="nx">s</span><span class="p">))</span> <span class="p">{</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/^[` \t]*/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">[</span><span class="mi">1</span><span class="p">])</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[&#39; \t]*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">[</span><span class="mi">1</span><span class="p">])</span>
    <span class="nx">macros</span><span class="p">[</span><span class="nx">s</span><span class="p">[</span><span class="mi">1</span><span class="p">]]</span> <span class="o">=</span> <span class="nx">s</span><span class="p">[</span><span class="mi">1</span><span class="p">]</span>
    <span class="nx">x</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="nb">RSTART</span> <span class="o">+</span> <span class="nb">RLENGTH</span><span class="p">)</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#foo.awk" class="proc-emissions-name">foo.awk</a>
</pre>

`RSTART` 是 `match` 函数从 `x` 中获得与正则表达式 `/define\((.*),/` 匹配部分的起始位置，`RLENGTH` 是匹配长度。`s` 用于存储正则表达式中捕获的内容，即  `/define\((.*),/` 中的 `(.*)` 部分，后者是正则表达式的捕获语法。`s[1]` 表示捕获到的第一个字符串，随后两次调用 `sub` 函数，可去除 `s[1]` 两侧可能存在的 m4 引号。

不过，能够支持正则表达式捕获语法的 awk，并非标准 awk，因为在标准 AWK 语言中，没有正则表达式捕获语法。GNU awk 所支持的 AWK 语言是扩展的 AWK 语言。对于一些仅实现了标准 AWK 语言的 awk 程序，它们不兼容 GNU awk 脚本，例如某些近年来发行的 Ubuntu 系统及其衍生系统，它们默认安装的 awk 程序是 mawk。若仅使用标准 AWK 语言实现与上述代码同等功能，会略微复杂一些：

<pre id="收集已定义的宏（标准AWK代码）" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 收集已定义的宏（标准 AWK 代码） #</span>
<span class="nx">x</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
<span class="k">while</span><span class="p">(</span><span class="kr">match</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="sr">/define\(/</span><span class="p">))</span> <span class="p">{</span>
    <span class="nx">s</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="nb">RSTART</span> <span class="o">+</span> <span class="nb">RLENGTH</span><span class="p">)</span>
    <span class="nx">x</span> <span class="o">=</span> <span class="nx">s</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/,.*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/^[` \t]*/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[&#39; \t]*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="nx">macros</span><span class="p">[</span><span class="nx">s</span><span class="p">]</span> <span class="o">=</span> <span class="nx">s</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#zhe.awk" class="proc-emissions-name">zhe.awk</a>
</pre>

收集到所有已存在定义的宏后，在宏的应用空间，需要反复搜索文本里出现的宏名，包括但不限于中文宏名，然后为其构造间接调用：

<pre id="为中文宏构造间接调用" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 为中文宏构造间接调用 #</span>
<span class="k">for</span> <span class="p">(</span><span class="nx">i</span> <span class="o">in</span> <span class="nx">macros</span><span class="p">)</span> <span class="p">{</span>
    <span class="nx">x</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
    <span class="nx">before</span> <span class="o">=</span> <span class="s2">&quot;&quot;</span>
    <span class="k">while</span> <span class="p">((</span><span class="nx">j</span> <span class="o">=</span> <span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="nx">i</span><span class="p">))</span> <span class="o">&gt;</span> <span class="mi">0</span><span class="p">)</span> <span class="p">{</span>
        <span class="nx">n</span> <span class="o">=</span> <span class="kr">length</span><span class="p">(</span><span class="nx">i</span><span class="p">)</span>
        <span class="nx">before</span> <span class="o">=</span> <span class="nx">before</span> <span class="kr">substr</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="mi">1</span><span class="p">,</span> <span class="nx">j</span> <span class="o">-</span> <span class="mi">1</span><span class="p">)</span> <span class="s2">&quot;`&#39;indir(&quot;</span> <span class="nx">i</span> <span class="s2">&quot;)&quot;</span>
        <span class="nx">after</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="nx">j</span> <span class="o">+</span> <span class="nx">n</span><span class="p">)</span>
        <span class="nx">x</span> <span class="o">=</span> <span class="nx">after</span>
    <span class="p">}</span>
    <span class="o">$</span><span class="mi">0</span> <span class="o">=</span> <span class="nx">before</span> <span class="nx">x</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#foo.awk" class="proc-emissions-name">foo.awk</a>
</pre>

将 foo.awk 作用于上一节的 foo.m4 文件：

```console
$ awk -f foo.awk foo.m4
```

结果为

```m4
divert(-1)
define(宏一, 你好，我是宏一)
divert(0)dnl
`'indir(宏一)！
```

若使用 GNU m4 处理上述 M4 文件，即

```console
$ awk -f foo.awk foo.m4 | m4
```

结果为

```
你好，我是宏一！
```

# 中文宏的调用形式

现在，对中文宏的调用形式加以明确。

首先，对于无参数的宏，可以直接调用，或者用括号的形式。例如对于中文宏

```m4
define(宏一, 你好，我是宏一)
```

调用形式可以是

```
宏一
```

也可以是

```
（宏一）
```

亦即可用半角或全角的圆括号将宏名囊括起来。

对于有参数宏，例如

```m4
define(宏二, `$1，我是宏一')
```

调用形式只能是全角圆括号形式，宏名和参数都在括号内，以全角的逗号予以间隔：

```
（宏二，你好）
```

若参数为多个，参数之间也是以全角逗号予以间隔。

还需要考虑宏的逃逸问题，亦即有些文字，虽然是宏的调用形式，但希望它们被当成普通文本。M4 可通过引号对宏进行逃逸。我也选择使用中文全角单引号作为中文宏的逃逸标识。例如

```m4
define(宏二, `$1，我是宏一')
用引号，对宏进行逃逸，‘（宏二，你好）’。
```

# 一念所动

为中文宏构造间接调用形式以及宏的逃逸处理，必定是在遍历一行文本的过程中进行：

<pre id="有参数宏的处理过程" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 有参数宏的处理过程 #</span>
<span class="nx">i</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span> <span class="nx">n</span> <span class="o">=</span> <span class="kr">length</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">)</span>
<span class="nx">esc</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span> <span class="nx">zhe</span> <span class="o">=</span> <span class="mi">0</span>
<span class="k">while</span> <span class="p">(</span><span class="nx">i</span> <span class="o">&lt;=</span> <span class="nx">n</span><span class="p">)</span> <span class="p">{</span>
    <a href="#判断是否进入逃逸状态->esc" class="orez-callee-link"># 判断是否进入逃逸状态 -> esc @</a>
    <span class="k">if</span> <span class="p">(</span><span class="nx">esc</span><span class="p">)</span> <span class="p">{</span>
        <a href="#判断是否退出逃逸状态->esc" class="orez-callee-link"># 判断是否退出逃逸状态 -> esc @</a>
    <span class="p">}</span> <span class="k">else</span> <span class="p">{</span>
        <a href="#判断是否遇到中文宏->zhe" class="orez-callee-link"># 判断是否遇到中文宏 -> zhe @</a>
        <span class="k">if</span> <span class="p">(</span><span class="nx">zhe</span><span class="p">)</span> <span class="p">{</span>
            <a href="#构造中文宏的间接调用形式" class="orez-callee-link"># 构造中文宏的间接调用形式 @</a>
            <span class="nx">n</span> <span class="o">=</span> <span class="kr">length</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">)</span>
            <span class="nx">zhe</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="p">}</span>
    <span class="p">}</span>
    <span class="nx">i</span><span class="o">++</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#zhe.awk" class="proc-emissions-name">zhe.awk</a>
</pre>

上述代码之所以用 `while`，而非 `for` 循环结构，是因为上述遍历文本的过程是变动性的，即文本的长度可能会发生变化。还需要注意，上述代码的循环变量 `i`，在 gawk 中是每个字符的下标，而在先天残疾的 mawk 中，它只是每个字节的下标，亦即 gawk 支持 utf-8 编码，mawk 不支持，而且上述代码之所以是这种遍历形式，也要拜 mawk 所赐，我想让这个 AWK 脚本能与之兼容。

判断是否进入逃逸状态，只需检测当前字符（对于 mawk 而言，是当前字节）是否为中文或西文的左引号：

<pre id="判断是否进入逃逸状态->esc" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 判断是否进入逃逸状态 -> esc #</span>
<span class="nx">x</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="nx">i</span><span class="p">)</span>
<span class="k">if</span> <span class="p">(</span><span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="s2">&quot;`&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span> <span class="o">||</span> <span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="s2">&quot;‘&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span><span class="p">)</span> <span class="nx">esc</span><span class="o">++</span>
<span class="orez-symbol">=&gt;</span> <a href="#有参数宏的处理过程" class="proc-emissions-name">有参数宏的处理过程</a>
<span class="orez-symbol">=&gt;</span> <a href="#无参数宏的处理过程" class="proc-emissions-name">无参数宏的处理过程</a>
</pre>

是否退出逃逸状态，需要检测与左引号配对的右引号：

<pre id="判断是否退出逃逸状态->esc" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 判断是否退出逃逸状态 -> esc #</span>
<span class="k">if</span> <span class="p">(</span><span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="s2">&quot;&#39;&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span> <span class="o">||</span> <span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="s2">&quot;’&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span><span class="p">)</span> <span class="nx">esc</span><span class="o">--</span>
<span class="orez-symbol">=&gt;</span> <a href="#有参数宏的处理过程" class="proc-emissions-name">有参数宏的处理过程</a>
<span class="orez-symbol">=&gt;</span> <a href="#无参数宏的处理过程" class="proc-emissions-name">无参数宏的处理过程</a>
</pre>

判断是否遇到中文宏，只需检测是否遇到左括号：

<pre id="判断是否遇到中文宏->zhe" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 判断是否遇到中文宏 -> zhe #</span>
<span class="k">if</span> <span class="p">(</span><span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="s2">&quot;（&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span><span class="p">)</span> <span class="nx">zhe</span> <span class="o">=</span> <span class="mi">1</span>
<span class="orez-symbol">=&gt;</span> <a href="#有参数宏的处理过程" class="proc-emissions-name">有参数宏的处理过程</a>
</pre>

遇到中文宏，首先将其提取出来，并保存其前后文本，然后为中文宏构造间接调用形式，完成后再与宏的前后文本拼接，并更新文本的长度。一念之动，终归于寂，整个过程如下：

<pre id="构造中文宏的间接调用形式" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 构造中文宏的间接调用形式 #</span>
<a href="#将文本分为三个部分：before，macro和after" class="orez-callee-link"># 将文本分为三个部分：before，macro 和 after @</a>
<a href="#macro变换" class="orez-callee-link"># macro 变换 @</a>
<span class="o">$</span><span class="mi">0</span> <span class="o">=</span> <span class="nx">before</span> <span class="nx">macro</span> <span class="nx">after</span>
<span class="orez-symbol">=&gt;</span> <a href="#有参数宏的处理过程" class="proc-emissions-name">有参数宏的处理过程</a>
</pre>

遇到中文宏的指标是发现全角左括号，只需在其后续文本中寻找与之匹配的右括号，便可获得该中文宏的全部。以中文宏为中间点，可将文本分为三个部分：

<pre id="将文本分为三个部分：before，macro和after" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 将文本分为三个部分：before，macro 和 after #</span>
<span class="nx">depth</span> <span class="o">=</span> <span class="mi">1</span>
<span class="k">for</span> <span class="p">(</span><span class="nx">j</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span> <span class="nx">j</span> <span class="o">&lt;</span> <span class="nx">n</span><span class="p">;</span> <span class="nx">j</span><span class="o">++</span><span class="p">)</span> <span class="p">{</span>
    <span class="nx">y</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="nx">j</span> <span class="o">+</span> <span class="mi">1</span><span class="p">)</span>
    <span class="k">if</span> <span class="p">(</span><span class="kr">index</span><span class="p">(</span><span class="nx">y</span><span class="p">,</span> <span class="s2">&quot;（&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span><span class="p">)</span> <span class="p">{</span>
        <span class="nx">depth</span><span class="o">++</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="kr">index</span><span class="p">(</span><span class="nx">y</span><span class="p">,</span> <span class="s2">&quot;）&quot;</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span><span class="p">)</span> <span class="p">{</span>
        <span class="nx">depth</span><span class="o">--</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">depth</span> <span class="o">==</span> <span class="mi">0</span><span class="p">)</span> <span class="k">break</span>
<span class="p">}</span>
<span class="k">if</span> <span class="p">(</span><span class="nx">depth</span> <span class="o">!=</span> <span class="mi">0</span><span class="p">)</span> <span class="p">{</span>
    <span class="kr">print</span> <span class="s2">&quot;错误：在 &lt;&quot;</span> <span class="kr">substr</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="mi">1</span><span class="p">,</span> <span class="kr">length</span><span class="p">(</span><span class="nx">x</span><span class="p">)</span> <span class="o">/</span> <span class="mi">10</span><span class="p">)</span> <span class="s2">&quot;...&gt; 存在非封闭括号&quot;</span>
<span class="p">}</span> <span class="k">else</span> <span class="p">{</span>
    <span class="nx">m</span> <span class="o">=</span> <span class="kr">length</span><span class="p">(</span><span class="s2">&quot;）&quot;</span><span class="p">)</span>
    <span class="nx">before</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="mi">1</span><span class="p">,</span> <span class="nx">i</span> <span class="o">-</span> <span class="mi">1</span><span class="p">)</span>
    <span class="nx">macro</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="nx">i</span><span class="p">,</span> <span class="nx">j</span> <span class="o">+</span> <span class="nx">m</span><span class="p">)</span>
    <span class="nx">after</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="nx">i</span> <span class="o">+</span> <span class="nx">j</span> <span class="o">+</span> <span class="nx">m</span><span class="p">)</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#构造中文宏的间接调用形式" class="proc-emissions-name">构造中文宏的间接调用形式</a>
</pre>

对 `macro` 进行变换，是将其首尾中文全角括号转换为西文半角括号，将隔离参数的中文全角逗号换成西文半角逗号，然后在左侧增加 `indir` 前缀，还需要将指向当前字符的下标 `i` 跳过该前缀：

<pre id="macro变换" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ macro 变换 #</span>
<span class="kr">sub</span><span class="p">(</span><span class="sr">/^（/</span><span class="p">,</span> <span class="s2">&quot;(&quot;</span><span class="p">,</span> <span class="nx">macro</span><span class="p">)</span>
<span class="kr">sub</span><span class="p">(</span><span class="sr">/）$/</span><span class="p">,</span> <span class="s2">&quot;)&quot;</span><span class="p">,</span> <span class="nx">macro</span><span class="p">)</span>
<span class="kr">gsub</span><span class="p">(</span><span class="sr">/，/</span><span class="p">,</span> <span class="s2">&quot;,&quot;</span><span class="p">,</span> <span class="nx">macro</span><span class="p">)</span>
<span class="nx">prefix</span> <span class="o">=</span> <span class="s2">&quot;`&#39;indir&quot;</span>
<span class="nx">macro</span> <span class="o">=</span> <span class="nx">prefix</span> <span class="nx">macro</span>
<span class="nx">i</span> <span class="o">=</span> <span class="nx">i</span> <span class="o">+</span> <span class="kr">length</span><span class="p">(</span><span class="nx">prefix</span><span class="p">)</span>
<span class="orez-symbol">=&gt;</span> <a href="#构造中文宏的间接调用形式" class="proc-emissions-name">构造中文宏的间接调用形式</a>
</pre>

# 无参数宏

无参数中文宏的调用可以不使用括号形式，需要根据从宏的定义空间获取的宏集为其构造间接调用：

<pre id="无参数宏的处理过程" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 无参数宏的处理过程 #</span>
<span class="k">for</span> <span class="p">(</span><span class="nx">a</span> <span class="o">in</span> <span class="nx">macros</span><span class="p">)</span> <span class="p">{</span>
    <span class="nx">i</span> <span class="o">=</span> <span class="mi">1</span><span class="p">;</span> <span class="nx">n</span> <span class="o">=</span> <span class="kr">length</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">)</span>
    <span class="nx">esc</span> <span class="o">=</span> <span class="mi">0</span><span class="p">;</span> <span class="nx">zhe</span> <span class="o">=</span> <span class="mi">0</span>
    <span class="k">while</span> <span class="p">(</span><span class="nx">i</span> <span class="o">&lt;=</span> <span class="nx">n</span><span class="p">)</span> <span class="p">{</span>
        <a href="#判断是否进入逃逸状态->esc" class="orez-callee-link"># 判断是否进入逃逸状态 -> esc @</a>
        <span class="k">if</span> <span class="p">(</span><span class="nx">esc</span><span class="p">)</span> <span class="p">{</span>
            <a href="#判断是否退出逃逸状态->esc" class="orez-callee-link"># 判断是否退出逃逸状态 -> esc @</a>
        <span class="p">}</span> <span class="k">else</span> <span class="p">{</span>
            <a href="#判断是否遇到无参数宏->zhe" class="orez-callee-link"># 判断是否遇到无参数宏 -> zhe @</a>
            <span class="k">if</span> <span class="p">(</span><span class="nx">zhe</span><span class="p">)</span> <span class="p">{</span>
                <a href="#构造无参数宏的间接调用形式" class="orez-callee-link"># 构造无参数宏的间接调用形式 @</a>
                <span class="nx">n</span> <span class="o">=</span> <span class="kr">length</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">)</span>
                <span class="nx">zhe</span> <span class="o">=</span> <span class="mi">0</span>
            <span class="p">}</span>
        <span class="p">}</span>
        <span class="nx">i</span><span class="o">++</span>
    <span class="p">}</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#zhe.awk" class="proc-emissions-name">zhe.awk</a>
</pre>

判断是否遇到无参数宏，只需要比较当前下标 `i` 所指位置为起始的文本是否为宏名，且 `i` 位置之前的文本不存在宏的间接调用，即 `indir(`：

<pre id="判断是否遇到无参数宏->zhe" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 判断是否遇到无参数宏 -> zhe #</span>
<span class="k">if</span> <span class="p">(</span><span class="kr">index</span><span class="p">(</span><span class="nx">x</span><span class="p">,</span> <span class="nx">a</span><span class="p">)</span> <span class="o">==</span> <span class="mi">1</span><span class="p">)</span> <span class="p">{</span>
    <span class="nx">y</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="mi">1</span><span class="p">,</span> <span class="nx">i</span> <span class="o">-</span> <span class="mi">1</span><span class="p">)</span>
    <span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="kr">match</span><span class="p">(</span><span class="nx">y</span><span class="p">,</span> <span class="sr">/indir\($/</span><span class="p">))</span> <span class="p">{</span>
        <span class="nx">zhe</span> <span class="o">=</span> <span class="mi">1</span>
    <span class="p">}</span>
<span class="p">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#无参数宏的处理过程" class="proc-emissions-name">无参数宏的处理过程</a>
</pre>

构造无参数宏的间接调用形式，也是将文本分为三个部分，before，macro 和 after，对 macro 部分进行变换，之后再将三个部分拼接为 `$0`：

<pre id="构造无参数宏的间接调用形式" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 构造无参数宏的间接调用形式 #</span>
<span class="nx">prefix</span> <span class="o">=</span> <span class="s2">&quot;`&#39;indir(&quot;</span>
<span class="nx">before</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="mi">1</span><span class="p">,</span> <span class="nx">i</span> <span class="o">-</span> <span class="mi">1</span><span class="p">)</span>
<span class="nx">macro</span> <span class="o">=</span> <span class="nx">prefix</span> <span class="nx">a</span> <span class="s2">&quot;)&quot;</span>
<span class="nx">after</span> <span class="o">=</span> <span class="kr">substr</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="nx">i</span> <span class="o">+</span> <span class="kr">length</span><span class="p">(</span><span class="nx">a</span><span class="p">))</span>
<span class="o">$</span><span class="mi">0</span> <span class="o">=</span> <span class="nx">before</span> <span class="nx">macro</span> <span class="nx">after</span>
<span class="nx">i</span> <span class="o">=</span> <span class="nx">i</span> <span class="o">+</span> <span class="kr">length</span><span class="p">(</span><span class="nx">prefix</span><span class="p">)</span>
<span class="orez-symbol">=&gt;</span> <a href="#无参数宏的处理过程" class="proc-emissions-name">无参数宏的处理过程</a>
</pre>

# 完整的 AWK 脚本

<pre id="zhe.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ zhe.awk #</span>
<span class="sr">/divert\(-1\)/</span> <span class="p">{</span>
    <span class="nx">action</span> <span class="o">=</span> <span class="mi">0</span>
    <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">next</span>
<span class="p">}</span>
<span class="sr">/divert\(0\)dnl/</span> <span class="p">{</span>
    <span class="nx">action</span> <span class="o">=</span> <span class="mi">1</span>
    <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">next</span>
<span class="p">}</span>
<span class="p">{</span>
   <span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">action</span><span class="p">)</span> <span class="p">{</span>
       <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
       <a href="#收集已定义的宏（标准AWK代码）" class="orez-callee-link"># 收集已定义的宏（标准 AWK 代码） @</a>
   <span class="p">}</span> <span class="k">else</span> <span class="p">{</span>
       <a href="#有参数宏的处理过程" class="orez-callee-link"># 有参数宏的处理过程 @</a>
       <a href="#无参数宏的处理过程" class="orez-callee-link"># 无参数宏的处理过程 @</a>
       <span class="kr">gsub</span><span class="p">(</span><span class="sr">/‘/</span><span class="p">,</span> <span class="s2">&quot;`&quot;</span><span class="p">,</span> <span class="o">$</span><span class="mi">0</span><span class="p">)</span>
       <span class="kr">gsub</span><span class="p">(</span><span class="sr">/’/</span><span class="p">,</span> <span class="s2">&quot;&#39;&quot;</span><span class="p">,</span> <span class="o">$</span><span class="mi">0</span><span class="p">)</span>
       <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
   <span class="p">}</span>
<span class="p">}</span>
</pre>

注意，上述代码的最后部分是引号替换，将用于宏名逃逸的中文全角单引号替换为 M4 的英文半角引号。

还需要注意的是，上述 AWK 脚本，性能很低，因为它没有词法分析和语法分析，亦即没有将文本组织成一种高效的数据结构，从而可以进行局部修改。上述脚本的做法是野蛮的，粗暴的，文本局部的每一次变动，都会波及整体。

# zhe 命令

宏的定义与调用是分离的。可将宏的定义放在单独的 M4 文件，但是开头的 `divert(-1)` 和末尾的 `divert(0)dnl` 可以省略，在 bash 脚本中可以悄悄加上去。该 bash 脚本的第一个参数（即 `$1`）便是宏定义文件，以下代码可为该文件增加首部和尾部，并将文件内容保存为临时文件：

<pre id="加载宏的定义文件" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 加载宏的定义文件 #</span>
<span class="nv">ZHE_TMP</span><span class="o">=</span><span class="k">$(</span>mktemp<span class="k">)</span>
awk<span class="w"> </span><span class="s1">&#39;BEGIN{print &quot;divert(-1)&quot;} {print $0} END{print &quot;divert(0)dnl&quot;}&#39;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span>&gt;<span class="w"> </span><span class="nv">$ZHE_TMP</span>
<span class="orez-symbol">=&gt;</span> <a href="#zhe" class="proc-emissions-name">zhe</a>
</pre>

以下代码，将第 2 个参数（即 `$2`）——宏调用文件合并到 `$ZHE_TMP` 文件：

<pre id="合并宏调用文件" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 合并宏调用文件 #</span>
awk<span class="w"> </span><span class="s1">&#39;{print $0}&#39;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="nv">$ZHE_TMP</span>
<span class="orez-symbol">=&gt;</span> <a href="#zhe" class="proc-emissions-name">zhe</a>
</pre>

用于构造中文宏间接调用的 AWK 脚本 zhe.awk，假设它位于上述 bash 脚本所在目录的 helper 目录，则 bash 脚本通过自身所在路径获取该脚本，用它处理 `$ZHE_TMP` 文件，并将结果转交于 m4，并将结果输出到 `$3`，假如它存在：

<pre id="让zhe.awk发挥作用" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 让 zhe.awk 发挥作用 #</span>
<span class="nv">ZHE_SELF_PATH</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="k">$(</span>dirname<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">BASH_SOURCE</span><span class="p">[0]</span><span class="si">}</span><span class="s2">&quot;</span><span class="k">)</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">&amp;&amp;</span><span class="w"> </span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span>
<span class="k">if</span><span class="w"> </span><span class="o">[[</span><span class="w"> </span><span class="nv">$3</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;&quot;</span><span class="w"> </span><span class="o">]]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
<span class="w">    </span>awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$ZHE_SELF_PATH</span><span class="s2">/helper/zhe.awk&quot;</span><span class="w"> </span><span class="nv">$ZHE_TMP</span><span class="w"> </span><span class="p">|</span><span class="w"> </span>m4
<span class="k">else</span>
<span class="w">    </span>awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$ZHE_SELF_PATH</span><span class="s2">/helper/zhe.awk&quot;</span><span class="w"> </span><span class="nv">$ZHE_TMP</span><span class="w"> </span><span class="p">|</span><span class="w"> </span>m4<span class="w"> </span>&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$3</span><span class="s2">&quot;</span>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#zhe" class="proc-emissions-name">zhe</a>
</pre>

完整的 bash 脚本如下：

<pre id="zhe" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ zhe #</span>
<span class="ch">#!/bin/bash</span>
<a href="#加载宏的定义文件" class="orez-callee-link"># 加载宏的定义文件 @</a>
<a href="#合并宏调用文件" class="orez-callee-link"># 合并宏调用文件 @</a>
<a href="#让zhe.awk发挥作用" class="orez-callee-link"># 让 zhe.awk 发挥作用 @</a>
</pre>

# 参考

1. [让这世界再多一份 GNU m4 教程](https://segmentfault.com/a/1190000004104696)
2. [AWK 小传](https://segmentfault.com/a/1190000016745490)
3. [写给高年级小学生的《Bash 指南》](https://segmentfault.com/a/1190000017229619)
