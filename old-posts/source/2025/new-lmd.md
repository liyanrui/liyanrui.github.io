---
title: lmd 的设计与实现
lang: zh-CN
date: 2025 年 01 月 24 日
abstract: 
category: ../../index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在
        [讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

# 前言

我曾经用过两三个静态网站构建工具，用的最久的，是 [nikola](https://www.getnikola.com/getting-started.html)，它很优秀，无论是安装，还是使用，都比较简单。不过，我难以回答之前为什么要弃用它。大概是有一次对它进行升级，下载它依赖的一些 Python 包时出现了我难以解决的问题，导致升级失败，这件事让我不太舒服，便弃用了。刚才我又试了一次，即使按照官方文档提醒的，在 Ubuntu 或其衍生版本中，应使用 Python 的包管理器 pip 进行安装，然而在我的 Linux Mint 22 环境中依然在安装过程中出错。

大概是 Bash 语言太过于古老，以至于我什么时候认真学过它都有些记忆模糊了。大概是在 2020 年，也可能更早一些，我在学习了几天 Bash 语言之后，想写个脚本练手，恰好我又略懂号称[文档格式转换界的瑞士军刀——pandoc](https://pandoc.org/) 的用法，于是基于 Bash 和 pandoc 写了个脚本，取名为 gar，它能够借助 pandoc 将 Markdown 文档转化为网页，并且提供了文章基本管理功能。gar 脚本虽然简陋，但是它让我意识到，实现一个静态网站构建工具，并不需要流行的静态网站构建工具所表现出的那般复杂。当然，它们的复杂有它们的理由，只是那些理由不是我的。

2023 年，我在又一次温习 Bash 和 Awk 时，对 gar 脚本进行了改进，基于 Awk 脚本为其实现了文档目录管理的功能，并将 gar 更名为 lmd，并写了一份文档，介绍了[它的用法](https://liyanrui.github.io/output/bash/lmd.html)。

今年，我在为 [orez 的新版本](https://liyanrui.github.io/output/2025/orez-v1.html)构造 Markdown 后端时，尝试将它与 lmd 脚本配合起来，以静态网站的形式发布文学程序的文档，详见「[orez + lmd = ?](https://liyanrui.github.io/output/2025/orez-lmd.html)」一文。在此过程中，我发现 lmd 脚本外围的部分 Awk 代码与 Ubuntu 系的发行版默认使用的 mawk 不兼容，在修改这部分 Awk 代码时，发现我对 lmd 脚本里的代码也感觉非常陌生。于是，决定重新实现 lmd，以文学编程的方式，除了作为文学编程以及 orez 的 Markdown 后端是否可用的一次实践，也用于防备以后再次阅读 lmd 脚本里的代码觉得陌生。

# pandoc

构造一个静态网站，用于发布日志、想法、感悟以及类似讨武曌檄或共产主义宣言之类的内容，首先需要放弃直接使用 HTML 语言撰写文档这种想法，因为现在已经不是 2000 年代了。Markdown 语言可以实现杠杆作用，让你用更小的力气撬动 HTML 的繁冗笨重，而具体的杠杆就是类像 pandoc 这样的工具。

lmd 脚本所构建的静态网站，网页只有两种形式：网站的首页和文章。在 lmd 脚本的视域，静态网站相当于文集，网站的首页相当于文集的目录。这意味着，从 Markdown 到 HTML，需要为 pandoc 事先定义两份文档模板。对于 pandoc 而言，用户定义的文档模板并非必须，因为它有一些默认模板可用。但是，倘若需要对页面部分内容是否需要 pandoc 呈现出来有搜要求，则必须为 pandoc 提供自定义的模板。

我为网站首页和文章分别定义了模板 homepage.template 和 post.template，其内容见附录 [pandoc 模板](#pandoc-模板)。下面以一份简单但完备的 Markdown 文档为例，演示如何使用 pandoc 按照模板 post.template 生成 HTML 文档。假设这份 Markdown 文档为 foo.md，其内容为

<pre id="foo.md" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ foo.md #</span>
---
title: 标题
lang: zh-CN
date: 2025 年 01 月 14 日
abstract: 这是一份示例。
category: 
footer: 页脚
...

<span class="gh"># 第一节</span>

... 内容 ...

![<span class="nt">插图</span>](<span class="na">foo.png</span>)

<span class="gh"># 第二节</span>

... 行内公式：$a^2 + b^2 = c^2$

行间公式：

$$
E = mc^2
$$
</pre>

将模板文件 post.template 放到一个名为 templates 的目录（目录名只能是 templates）里，假设 /tmp/templates，则以下命令可基于 post.template 模板和 MathJax 脚本，将 foo.md 转化为支持 TeX 数学公式的 HTML 文件 foo.html：

```console
$ pandoc foo.md --standalone --table-of-contents \
         --data-dir=/tmp --template=post.template \
         --mathjax=https://cdn.bootcss.com/mathjax/3.2.2/es5/tex-mml-chtml.js \
         --output foo.html
```

下面对上述命令中的参数予以说明：

* `--standalone`：可让 pandoc 生成完整的 HTML 文件，即可直接在网络浏览器中呈现的网页文件。
* `--table-of-contents`：生成目录。
* `--data-dir`：设定模板文件目录所在路径。
* `--mathjax`：设定 MathJax 脚本 CDN 地址，用于渲染文档中的数学公式。
* `--output`：设定输出文件名。

以上命令生成的 HTML 页面并不美观，存在标题和正文字体皆过大，页脚文字未居中，目录占据空间过多等问题。这些问题皆属于排版问题，可通过 CSS 予以解决。pandoc 的 `--css` 选项支持加载用户定义的样式文件。附录的[样式表](#样式表)提供了 lmd.css 文件，假设它与 pandoc 生成的 foo.html 位于同一目录，以下命令可解决上述排版问题：

```console
$ pandoc foo.md --standalone --table-of-contents \
         --data-dir=/tmp --template=post.template \
         --mathjax=https://cdn.bootcss.com/mathjax/3.2.2/es5/tex-mml-chtml.js \
         --css=lmd.css \
         --output foo.html
```

至于网站首页的构建，对应的 pandoc 命令要简单一些：

```console
$ pandoc foo.md --standalone \
         --data-dir=/tmp --template=homepage.template \
         --css=lmd.css \
         --output foo.html
```

上述的 pandoc 命令是下文要实现的 lmd 脚本的核心。即使不借助 lmd 脚本，单纯使用 pandoc 命令也能构建静态网站，只是过程会较为繁琐。

# 网站初始化

lmd 要构建的网站，其目录结构和必需文件如下：

```
根目录
├── appearance
│   ├── lmd.css
│   └── pandoc
│       └── data
│           └── templates
│               ├── homepage.template
│               └── post.template
└── lmd.conf
```

上述结构中，appearence 用于定制网站页面的外观，所包含的文件皆在上一节出现过，它们的用途在此不再赘述。index.md 文件用于构造网站首页内容。lmd.conf 中存储的是用户的一些信息。

lmd 脚本从如何初始化上述的网站结构开始。首先需要考虑一个问题，诸如 lmd.css，homepage.template 以及 post.template 这些文件，它们应该事先存在于某处，在 lmd 脚本初始化网站结构时，将它们复制过去，那么它们应该存放在何处呢？我的选择是，让它们与 lmd 脚本位于同一目录，但是 lmd 脚本如何知道自己的位置呢？

根据文档 [1] 的探讨，确定 lmd 脚本所在路径的 Bash 代码如下：

<pre id="确定脚本自身所在->LMD_SELF_PATH" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 确定脚本自身所在 -> LMD_SELF_PATH #</span>
<span class="nv">LMD_SELF_PATH</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="k">$(</span>dirname<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">BASH_SOURCE</span><span class="p">[0]</span><span class="si">}</span><span class="s2">&quot;</span><span class="k">)</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">&amp;&amp;</span><span class="w"> </span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-test-01.sh" class="proc-emissions-name">lmd-test-01.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-init-test.sh" class="proc-emissions-name">lmd-init-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-init-index-test.sh" class="proc-emissions-name">lmd-init-index-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-new-category-test.sh" class="proc-emissions-name">lmd-new-category-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-convert-test.sh" class="proc-emissions-name">lmd-convert-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-delete-post-test.sh" class="proc-emissions-name">lmd-delete-post-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

以下脚本可用于验证上述代码是否可用：

<pre id="lmd-test-01.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-test-01.sh #</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在-> LMD_SELF_PATH @</a>
<span class="nb">echo</span><span class="w"> </span><span class="nv">$LMD_SELF_PATH</span>
</pre>

假设上述脚本位于 /tmp 目录，执行它，会输出 `/tmp`：

```console
$ cd /tmp
$ bash lmd-test-01.sh
/tmp
```

注意，上述命令之所以强调用 `bash` 执行脚本，而不是 `sh`，是因为 `sh` 指向系统默认的 Shell，而奇葩的 Ubuntu 及其衍生版本，系统默认 Shell 可能不是 bash，而是 dash。Bash 语言的一些特性，dash 不支持，故而它通常无法执行 Bash 脚本。我很是讨厌 Ubtuntu 这种经常出现的有小礼而无大义的举措。

假设 lmd 所在目录的结构为

```
lmd 所在目录
├── data
│   └── appearance
│       ├── lmd.css
│       └── pandoc
│           └── data
│               └── templates
│                   ├── homepage.template
│                   └── post.template
│── lmd.conf
└── lmd 脚本
```

其中，lmd.conf 为网站一些基本信息的配置文件，它本质上也是一份 Bash 脚本，定义了一些变量：

<pre id="lmd.conf" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd.conf #</span>
<span class="nv">MARKDOWN_EXT</span><span class="o">=</span><span class="s1">&#39;.md&#39;</span><span class="w"> </span><span class="c1"># markdown 文件扩展名</span>
<span class="nv">LANGUAGE</span><span class="o">=</span><span class="s2">&quot;zh-CN&quot;</span><span class="w">   </span><span class="c1"># 网站语言为简体中文</span>
<span class="nv">DATE</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span>date<span class="w"> </span>+<span class="s1">&#39;%Y 年 %m 月 %d 日&#39;</span><span class="k">)</span><span class="s2">&quot;</span><span class="w">  </span><span class="c1"># 文章时间戳</span>
<span class="nv">EMAIL</span><span class="o">=</span><span class="s1">&#39;lyr.m2@live.cn&#39;</span><span class="w">  </span><span class="c1"># 站长邮箱</span>
<span class="nv">ISSUES</span><span class="o">=</span><span class="s1">&#39;... ... ...&#39;</span><span class="w"> </span><span class="c1"># 讨论区</span>
<span class="c1"># 页脚内容</span>
<span class="nv">FOOTER</span><span class="o">=</span><span class="s2">&quot;我的联系方式：&lt;</span><span class="nv">$EMAIL</span><span class="s2">&gt; 或在[讨论区](</span><span class="nv">$ISSUES</span><span class="s2">)提问。&quot;</span>
</pre>

这些变量在后文网站首页和分类首页创建过程中会被用到。

基于获取脚本自身目录的代码以及上述 lmd 脚本所在目录的结构，便可写出网站初始化过程：

<pre id="网站初始化" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 网站初始化 #</span>
<span class="k">function</span><span class="w"> </span>lmd_init<span class="w"> </span><span class="o">{</span>
    <a href="#参数检验" class="orez-callee-link"># 参数检验 @</a>
    <a href="#创建网站目录初始结构" class="orez-callee-link"># 创建网站目录初始结构 @</a>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-init-test.sh" class="proc-emissions-name">lmd-init-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

`lmd_init` 函数可以作为 Bash 函数定义的一个简单示例，它接受一个参数，即 `$1`，是网站目录名。以下代码片段用于检测该参数是否为空目录是否已经存在：

<pre id="参数检验" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 参数检验 #</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd_init failed.&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;You should provide me the site-directory name.&quot;</span>
<span class="w">    </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="k">fi</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-d<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd_init failed.&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;The directory as site-directory already exists.&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Please use another different site-directory name.&quot;</span>
<span class="w">    </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#网站初始化" class="proc-emissions-name">网站初始化</a>
</pre>

网站目录结构的初始化过程只需将 lmd 脚本所在目录里的 appearance 目录复制到网站根目录下即可：

<pre id="创建网站目录初始结构" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 创建网站目录初始结构 #</span>
mkdir<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">&amp;&amp;</span><span class="w"> </span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
cp<span class="w"> </span>-r<span class="w"> </span><span class="nv">$LMD_SELF_PATH</span>/data/appearance<span class="w"> </span>./
cp<span class="w"> </span><span class="nv">$LMD_SELF_PATH</span>/lmd.conf<span class="w"> </span>./
<span class="orez-symbol">=&gt;</span> <a href="#网站初始化" class="proc-emissions-name">网站初始化</a>
</pre>

以下代码可用于验证或演示 `lmd_init` 函数的用法：

<pre id="lmd-init-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-init-test.sh #</span>
<span class="ch">#!/usr/bin/env bash</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在 -> LMD_SELF_PATH @</a>
<a href="#网站初始化" class="orez-callee-link"># 网站初始化 @</a>
lmd_init<span class="w"> </span><span class="nv">$1</span>
</pre>

假设 lmd-init-test.sh 位于 /tmp/lmd 目录，且伴有上文所述 data/appearance 目录，以下命令可在 /tmp 目录构建 demo 目录：

```console
$ cd /tmp
$ bash lmd/lmd-init-test.sh demo
```

网站初始化过程完成后，需要根据自身的情况修改网站根目录下的 lmd.conf 里的一些变量的值。

# 首页

网站首页是访问网站中其它页面的入口。网站可以没有首页，如同一本书没有目录。

首页的初始化过程所需信息已尽在 lmd.conf 文件中。用户可根据自己的情况修改 lmd.conf 文件。lmd 脚本可通过 `source` 命令「吸入」lmd.conf 中定义的变量，然后依据它们值构造网站首页，该过程如下：

<pre id="网站首页初始化" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 网站首页初始化 #</span>
<span class="k">function</span><span class="w"> </span>lmd_init_index<span class="w"> </span><span class="o">{</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>!<span class="w"> </span>-e<span class="w"> </span><span class="s2">&quot;lmd.conf&quot;</span><span class="w"> </span><span class="o">]</span><span class="w"> </span><span class="c1"># 当前目录并非网站根目录，报错退出。</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Error: lmd.conf not exist.&quot;</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;The current directory maybe not the root directory of this site.&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">fi</span>
<span class="w">    </span><span class="nb">source</span><span class="w"> </span>lmd.conf
    <a href="#创建首页" class="orez-callee-link"># 创建首页 @</a>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-init-index-test.sh" class="proc-emissions-name">lmd-init-index-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

根据 lmd.conf 中定义的 Markdown 文档扩展名，创建首页文件，并写入文档首部数据：

<pre id="创建首页" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 创建首页 #</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">index</span><span class="o">=</span><span class="s2">&quot;index</span><span class="nv">$MARKDOWN_EXT</span><span class="s2">&quot;</span>
touch<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$index</span><span class="s2">&quot;</span>
<span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;---&quot;</span><span class="w"> </span>&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$index</span><span class="s2">&quot;</span>
<span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;title: </span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$index</span><span class="s2">&quot;</span>
<span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;subtitle: &quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$index</span><span class="s2">&quot;</span>
<span class="nb">echo</span><span class="w"> </span>-e<span class="w"> </span><span class="s2">&quot;...\n&quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$index</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#网站首页初始化" class="proc-emissions-name">网站首页初始化</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd_new_category待续" class="proc-emissions-name">lmd_new_category 待续</a>
</pre>

pandoc 所支持的 Markdown 文档首部数据格式本质上是 YAML 格式，这是 pandoc 对 Markdown 文本标记语言的扩展，并非 Markdown 语言标准。

函数 `lmd_init_index` 必须在网站根目录下执行，否则它会因找不到 lmd.conf 文件而报错退出。该函数接受 1 个参数，即网站的标题。以下脚本可用于测试 `lmd_init_index` 是否可用：

<pre id="lmd-init-index-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-init-index-test.sh #</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在 -> LMD_SELF_PATH @</a>
<a href="#网站首页初始化" class="orez-callee-link"># 网站首页初始化 @</a>
lmd_init_index<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
</pre>

注意，上述代码中 `lmd_init_index` 接受的参数，为其添加引号是必须的，因为该参数中可能存在空格，若对其不加引号，会导致 bash 错以为参数是多个。关于这一点，详解见 [2]。

将上一节构建的网站根目录 /tmp/demo 作为工作目录，将上述脚本放在 /tmp/lmd 目录下，然后执行以下命令：

```console
$ cd /tmp/demo
$ bash ../lmd/lmd-init-index-test.sh "Demo site"
```

上述命令可在 /tmp/foo 目录下建立 index.md（假设 lmd.conf 中定义的 Markdown 扩展名是 .md），其内容如下：

```markdown
---
title: Demo
subtitle: 
...

```

需要注意的是，在函数 `lmd_init_index` 的定义中，`index` 是使用 `local` 修饰的，表示该变量为函数内的局部变量，函数外部不可访问。在 Bash 语言中，变量默认是全局的，`local` 修饰的变量只能在函数中出现。另外，Bash 语言的变量，无论全局还是局部，都是可以重定义，后文会用到这个特性。

# 分类

每篇文章，都应该对应一个分类（Category）。若某篇文章没有分类，那么它对应的分类便是网站根目录。对文章进行分类，本质上是为其创建目录。例如，如果我想用 lmd 脚本创建的网站发布日志，那么可创建 blog 目录，所有的日志类型的文章放在该目录即可。与网站根目录相似，分类目录里也需要首页，作为该分类中所有文章的入口。还需要注意，分类可以有子分类。

## 创建分类

函数 lmd_new_category 用于创建文章分类，其实现如下：

<pre id="创建文章分类" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 创建文章分类 #</span>
<span class="k">function</span><span class="w"> </span>lmd_new_category<span class="w"> </span><span class="o">{</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span><span class="w"> </span><span class="o">||</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd_new_category failed.&quot;</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;You should provide \&quot;Category name\&quot; and directory.&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">fi</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-d<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd_new_category failed.&quot;</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;The directory as category already exists.&quot;</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Please use a different directory name.&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">fi</span>
<span class="w">    </span>mkdir<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="p">;</span><span class="w"> </span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
    <a href="#lmd_new_category待续" class="orez-callee-link"># lmd_new_category 待续 @</a>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-new-category-test.sh" class="proc-emissions-name">lmd-new-category-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

函数 `lmd_new_category` 写到创建了分类目录（即 `$2`）后，便难以再写下去，因为接下来要在分类目录创建首页，但是该过程依赖位于网站根目录中的配置文件 lmd.conf 中的一些信息，可是如何找到 lmd.conf 呢？从当前目录递归上溯搜索上层目录即可，函数 `lmd_load_conf` 实现了这一过程：

<pre id="搜索并加载lmd.conf" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 搜索并加载 lmd.conf #</span>
<span class="k">function</span><span class="w"> </span>lmd_load_conf<span class="w"> </span><span class="o">{</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;/&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Error: I can not find lmd.conf!&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">elif</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-e<span class="w"> </span>lmd.conf<span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">source</span><span class="w"> </span>lmd.conf
<span class="w">    </span><span class="k">else</span>
<span class="w">        </span><span class="nb">cd</span><span class="w"> </span>../
<span class="w">        </span>lmd_load_conf
<span class="w">    </span><span class="k">fi</span>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-new-category-test.sh" class="proc-emissions-name">lmd-new-category-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-new-post-test.sh" class="proc-emissions-name">lmd-new-post-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-convert-test.sh" class="proc-emissions-name">lmd-convert-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-delete-post-test.sh" class="proc-emissions-name">lmd-delete-post-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

注意，`lmd_load_conf` 过程会将工作目录（当前目录）修改为网站根目录，故而在调用 `lmd_load_conf` 之前，需要保存工作目录，调用 `lmd_load_conf` 后再予以恢复：

<pre id="安全载入配置文件lmd.conf" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 安全载入配置文件 lmd.conf #</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">current_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span>
lmd_load_conf
<span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$current_path</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd_new_category待续" class="proc-emissions-name">lmd_new_category 待续</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除分类" class="proc-emissions-name">删除分类</a>
<span class="orez-symbol">=&gt;</span> <a href="#创建新文章" class="proc-emissions-name">创建新文章</a>
<span class="orez-symbol">=&gt;</span> <a href="#文章->HTML文件" class="proc-emissions-name">文章 -> HTML 文件</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除文章" class="proc-emissions-name">删除文章</a>
</pre>

基于 `lmd_load_conf`，便可实现 `lmd_new_category` 剩余部分：

<pre id="lmd_new_category待续" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd_new_category 待续 #</span>
<a href="#安全载入配置文件lmd.conf" class="orez-callee-link"># 安全载入配置文件 lmd.conf @</a>
<a href="#创建首页" class="orez-callee-link"># 创建首页 @</a>
<a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="orez-callee-link"># 构建分类首页与上级分类首页的关联并将其转换为 HTML 文件 @</a>
<span class="orez-symbol">=&gt;</span> <a href="#创建文章分类" class="proc-emissions-name">创建文章分类</a>
</pre>

上述代码中，构建分类首页与上级分类首页的关联，过程有些复杂，涉及一些文本匹配处理任务，Bash 语言难以胜任，为此我选择更擅长文本处理的 Awk 语言做此事。倘若对 Awk 语言一无所知，可阅读在下多年前写的拙文「Awk 小传」[3]。

构建分类首页与上级分类首页的关联并将其转换为 HTML 文件，要解决以下问题：

* 在分类首页的文档首部区域增加 lang，category 以及 footer 字段。
* 将分类首页转化的 HTML 文件路径写入上级分类首页。
* 借助 pandoc 分别生成分类首页和上级分类首页的 HTML 文件。

先看上述第一个问题，要在分类首页追加字段原本可在创建该文档时添加，但是为了文档更为简约，可在幕后添加。以下 Awk 脚本可向 Markdown 文件首部追加字段：

<pre id="append-meta-data.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ append-meta-data.awk #</span>
<span class="nb">BEGIN</span> <span class="p">{</span>
    <span class="nx">beginning</span> <span class="o">=</span> <span class="mi">1</span>
    <span class="nx">meta_data</span> <span class="o">=</span> <span class="mi">0</span>
<span class="p">}</span>
<span class="p">{</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">beginning</span> <span class="o">&amp;&amp;</span> <span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^--- *$/</span><span class="p">)</span> <span class="p">{</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="nx">meta_data</span> <span class="o">=</span> <span class="mi">1</span>
        <span class="kr">next</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">meta_data</span> <span class="o">&amp;&amp;</span> <span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^\.\.\. *$/</span><span class="p">)</span> <span class="p">{</span>
        <span class="k">if</span> <span class="p">(</span><span class="nx">category</span><span class="p">)</span> <span class="kr">print</span> <span class="s2">&quot;category: &quot;</span> <span class="nx">category</span>
        <span class="k">if</span> <span class="p">(</span><span class="nx">lang</span><span class="p">)</span> <span class="kr">print</span> <span class="s2">&quot;lang: &quot;</span> <span class="nx">lang</span>
        <span class="k">if</span> <span class="p">(</span><span class="nx">footer</span><span class="p">)</span> <span class="kr">print</span> <span class="s2">&quot;footer: &quot;</span> <span class="nx">footer</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="nx">beginning</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="nx">meta_data</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="kr">next</span>
    <span class="p">}</span>
    <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
<span class="p">}</span>
</pre>

上述 Awk 脚本可以检测 Markdown 文档首部，在首部终结符 `...` 之前添加三个字段，字段的值是三个 Awk 变量，`category`，`lang` 和 `footer`，它们在 Awk 脚本里未定义，需要在使用 awk 命令时，以参数的形式定义并传入。例如，以下 Markdown 文档 foo.md：

```markdown
---
title: 日志
subtitle:
...

... 文章列表，略 ...
```

使用上述 Awk 脚本，为其首部追加字段的命令如下：

```console
$ awk -v category="../index.html" \
      -v footer="我的联系方式……" \
      -v lang="zh-CN" -f append-meta-data.awk index.md
```

结果可得：

```markdown
---
title: 日志
subtitle:
category: ../index.html
lang: zh-CN
footer: 我的联系方式……
...

... 文章列表，略 ...
```

根据上述 awk 命令的用法，命令中定义的变量，`footer` 和 `lang` 的值可从 lmd.conf 获取。`category` 的值是指向分类目录的父目录下 index.html（该目录下的 index.md 会被 pandoc 转化为 index.html）的路径，根据该路径可为各级分类的首页建立关联链接。除了网站首页，分类首页和文章，都应该含有 `category` 字段。

分类首页或文章的 `category` 字段是如何变成指向上级分类首页的链接呢？秘密在 post.template 文件里，该文件有以下代码：

```html
$if(category)$
<div class="category">
<a href="$category$">回上级页面</a>
</div>
$endif$
```

其中 `$category$` 便是在对分类首页首部 `category` 字段进行取值。由此不难想到，分类首页首部其他字段的值，也会以类似的形式被取出，由 pandoc 填充到文章模板里。

至于上级分类首页里出现的下级分类首页的链接，需由 Awk 脚本予以构造。在上级分类首页里出现的链接形如

```markdown
* [标题](链接)
```

这意味着，需要从下级分类首页首部抓取 `title` 字段的值，而链接部分可基于下级分类目录名进行构造。

从一份含有首部数据的 Markdown 文档抓取 `title` 信息的 Awk 脚本如下：

<pre id="get-title.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ get-title.awk #</span>
<span class="sr">/^ *title:.*$/</span> <span class="p">{</span>
    <span class="nx">s</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[ \t]*title:[ \t]*/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[ \t]*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">print</span> <span class="nx">s</span>
    <span class="k">exit</span> <span class="mi">0</span>
<span class="p">}</span>
</pre>

即 Markdown 文档中，凡能与正则表达式「`^ *title:.*$`」匹配的行（即 `$0`），将其前缀 `title:` 去除，并去除两端空白字符，所得结果即为字段 `title` 的值，得到该值后，将其打印出来，然后终止 Awk 程序。同理，可写出抓取 `date` 的 Awk 脚本：

<pre id="get-date.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ get-date.awk #</span>
<span class="sr">/^ *date:.*$/</span> <span class="p">{</span>
    <span class="nx">s</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[ \t]*date:[ \t]*/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[ \t]*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">print</span> <span class="nx">s</span>
    <span class="k">exit</span> <span class="mi">0</span>
<span class="p">}</span>
</pre>

Awk 脚本抓取的内容可通过子 Shell 命令即 `$(...)` 的形式传递于 lmd 脚本。lmd 脚本获得这些信息后，便可将其传递于另一个 Awk 脚本，后者用于判定文章是否已在所属分类的首页里出现，其实现如下：

<pre id="find-post.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ find-post.awk #</span>
<span class="sr">/^\*[ \t]*\[[^\]]*\]\(.*\)/</span> <span class="p">{</span>
    <span class="nx">s</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/^\*[ \t]*\[[ \t]*/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="kr">sub</span><span class="p">(</span><span class="sr">/[ \t]*\].*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">s</span> <span class="o">==</span> <span class="nx">title</span><span class="p">)</span> <span class="p">{</span>
        <span class="kr">print</span> <span class="s2">&quot;exist&quot;</span>
        <span class="k">exit</span> <span class="mi">0</span>
    <span class="p">}</span>
<span class="p">}</span>
</pre>

若当前分类首页在上级分类首页里未出现，则将其添加到分类首页里的分类列表，相应的 Awk 脚本如下：

<pre id="add-category-or-post.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ add-category-or-post.awk #</span>
<span class="nb">BEGIN</span> <span class="p">{</span>
    <span class="nx">item_date</span> <span class="o">=</span> <span class="s2">&quot;&quot;</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">date</span><span class="p">)</span> <span class="nx">item_date</span> <span class="o">=</span> <span class="s2">&quot;&lt;span class=\&quot;index-date\&quot;&gt;&quot;</span> <span class="nx">date</span> <span class="s2">&quot;&lt;/span&gt;&quot;</span>
    <span class="nx">item</span> <span class="o">=</span> <span class="s2">&quot;* [&quot;</span> <span class="nx">title</span> <span class="s2">&quot;](&quot;</span> <span class="nx">post_path</span> <span class="s2">&quot;)&quot;</span> <span class="nx">item_date</span>
    <span class="nx">metadata_beginning</span> <span class="o">=</span> <span class="mi">1</span>
    <span class="nx">in_metadata</span> <span class="o">=</span> <span class="mi">0</span>
    <span class="nx">metadata_end</span> <span class="o">=</span> <span class="mi">0</span>
    <span class="nx">finished</span> <span class="o">=</span> <span class="mi">0</span>
<span class="p">}</span>
<span class="p">{</span>
    <span class="c1"># 跳过文档首部 metadata</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">metadata_beginning</span> <span class="o">&amp;&amp;</span> <span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^--- *$/</span><span class="p">)</span> <span class="p">{</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="nx">in_metadata</span> <span class="o">=</span> <span class="mi">1</span>
        <span class="nx">metadata_beginning</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="kr">next</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">in_metadata</span> <span class="o">&amp;&amp;</span> <span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^\.\.\. *$/</span><span class="p">)</span> <span class="p">{</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="nx">in_metadata</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="nx">metadata_end</span> <span class="o">=</span> <span class="mi">1</span>
        <span class="kr">next</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">in_metadata</span><span class="p">)</span> <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span><span class="p">;</span> <span class="kr">next</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">metadata_end</span><span class="p">)</span> <span class="p">{</span>
        <span class="c1"># 在正文区域遇到非空行，添加 post 链接</span>
        <span class="k">if</span> <span class="p">(</span><span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^[ \t]*$/</span><span class="p">)</span> <span class="p">{</span>
            <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="p">}</span> <span class="k">else</span> <span class="p">{</span>
            <span class="kr">print</span> <span class="nx">item</span>
            <span class="nx">finished</span> <span class="o">=</span> <span class="mi">1</span>
        <span class="p">}</span>
    <span class="p">}</span>
<span class="p">}</span>
<span class="nb">END</span> <span class="p">{</span>
    <span class="c1"># 以防页面内容为空</span>
    <span class="k">if</span> <span class="p">(</span><span class="o">!</span><span class="nx">finished</span><span class="p">)</span> <span class="kr">print</span> <span class="nx">item</span>
<span class="p">}</span>
</pre>

上述脚本中的变量 `title`，`date` 以及 `post_path` 及它们的值皆由 lmd 脚本在调用 awk 时以参数的形式定义并传入。

向网络首页或分类首页追加字段的代码如下：

<pre id="向网站首页或分类首页追加字段->.tmp_index.md" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 向网站首页或分类首页追加字段 -> .tmp_index.md #</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-e<span class="w"> </span><span class="s2">&quot;lmd.conf&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span><span class="w"> </span><span class="c1"># 若当前分类未在根目录，需要为其上级分类首页构造返回上级的上级的链接</span>
<span class="w">    </span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">lang</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$LANG</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-v<span class="w"> </span><span class="nv">footer</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$FOOTER</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/append-meta-data.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span><span class="s2">&quot;index</span><span class="nv">$MARKDOWN_EXT</span><span class="s2">&quot;</span><span class="w"> </span>&gt;<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="k">else</span>
<span class="w">    </span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">category</span><span class="o">=</span><span class="s2">&quot;../index.html&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-v<span class="w"> </span><span class="nv">lang</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$LANG</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-v<span class="w"> </span><span class="nv">footer</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$FOOTER</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/append-meta-data.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span><span class="s2">&quot;index</span><span class="nv">$MARKDOWN_EXT</span><span class="s2">&quot;</span><span class="w"> </span>&gt;<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#向分类首页和上级分类首页的首部追加字段" class="proc-emissions-name">向分类首页和上级分类首页的首部追加字段</a>
<span class="orez-symbol">=&gt;</span> <a href="#向分类首页和上级分类首页的首部追加字段" class="proc-emissions-name">向分类首页和上级分类首页的首部追加字段</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除分类" class="proc-emissions-name">删除分类</a>
<span class="orez-symbol">=&gt;</span> <a href="#向文章和分类首页的首部追加字段" class="proc-emissions-name">向文章和分类首页的首部追加字段</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除文章" class="proc-emissions-name">删除文章</a>
</pre>

以下代码片段可向分类首页追加字段，生成临时分类首页文档：

<pre id="向分类首页和上级分类首页的首部追加字段" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 向分类首页和上级分类首页的首部追加字段 #</span>
<a href="#向网站首页或分类首页追加字段->.tmp_index.md" class="orez-callee-link"># 向网站首页或分类首页追加字段 -> .tmp_index.md @</a>
<span class="nb">cd</span><span class="w"> </span>..
<a href="#向网站首页或分类首页追加字段->.tmp_index.md" class="orez-callee-link"># 向网站首页或分类首页追加字段 -> .tmp_index.md @</a>
<span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="proc-emissions-name">构建分类首页与上级分类首页的关联并将其转换为 HTML 文件</a>
</pre>

以下代码片段将当前分类首页添加到上级分类首页，生成临时的上级分类首页文档：

<pre id="将分类首页添加至上级分类首页中的分类列表" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 将分类首页添加至上级分类首页中的分类列表 #</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="k">$(</span>awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/get-title.awk&quot;</span><span class="w"> </span>index<span class="nv">$MARKDOWN_EXT</span><span class="k">)</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">exist</span><span class="o">=</span><span class="k">$(</span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$title</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">                  </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/find-post.awk&quot;</span><span class="w"> </span>../index<span class="nv">$MARKDOWN_EXT</span><span class="k">)</span>
<a href="#向分类首页添加链接" class="orez-callee-link"># 向分类首页添加链接 @</a>
<span class="orez-symbol">=&gt;</span> <a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="proc-emissions-name">构建分类首页与上级分类首页的关联并将其转换为 HTML 文件</a>
</pre>

上述代码中，向分类首页添加链接的过程如下：

<pre id="向分类首页添加链接" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 向分类首页添加链接 #</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$exist</span><span class="s2">&quot;</span><span class="w"> </span>!<span class="o">=</span><span class="w"> </span><span class="s2">&quot;true&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nb">local</span><span class="w"> </span><span class="nv">post_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">/index.html&quot;</span>
<span class="w">    </span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$title</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-v<span class="w"> </span><span class="nv">post_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$post_path</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/add-category-or-post.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>../.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>&gt;<span class="w"> </span>../.tmp_0_index<span class="nv">$MARKDOWN_EXT</span>
<span class="w">    </span>mv<span class="w"> </span>../.tmp_0_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>../.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#将分类首页添加至上级分类首页中的分类列表" class="proc-emissions-name">将分类首页添加至上级分类首页中的分类列表</a>
</pre>

最后，使用 pandoc，将上述代码生成的临时 Markdown 文档转换为 HTML 文件，但是 pandoc 需要知道 lmd.css 以及文章模板的位置，而此时工作目录是在文章目录，需要确定工作目录到网站根目录的相对路径，基于该路径构造 lmd.css 和文章模板的路径。函数 `lmd_path_to_start` 可构造这样的路径，其定义如下：

<pre id="构造从当前目录到网站根目录的路径" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 构造从当前目录到网站根目录的路径 #</span>
<span class="k">function</span><span class="w"> </span>lmd_path_to_start<span class="w"> </span><span class="o">{</span>
<span class="w">    </span><span class="nb">local</span><span class="w"> </span><span class="nv">relative_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;/&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Error: I can not find right path!&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">elif</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-e<span class="w"> </span>lmd.conf<span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">relative_path</span><span class="p">%/</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="k">else</span>
<span class="w">        </span><span class="nv">relative_path</span><span class="o">=</span><span class="s2">&quot;../</span><span class="nv">$relative_path</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="nb">cd</span><span class="w"> </span>../
<span class="w">        </span>lmd_path_to_start<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$relative_path</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="k">fi</span>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-new-category-test.sh" class="proc-emissions-name">lmd-new-category-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-convert-test.sh" class="proc-emissions-name">lmd-convert-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-delete-post-test.sh" class="proc-emissions-name">lmd-delete-post-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

`lmd_path_to_start` 的实现与 `lmd_load_conf` 相似，前者只是在递归上溯搜寻 lmd.conf 的过程中记录了路径，但是同样会破坏工作目录，因此需要以下代码令其安全：

<pre id="安全构造从当前目录到网站根目录的路径->path_to_root" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 安全构造从当前目录到网站根目录的路径 -> path_to_root #</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">current_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">path_to_root</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span>lmd_path_to_start<span class="w"> </span><span class="s2">&quot;&quot;</span><span class="k">)</span><span class="s2">&quot;</span>
<span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$current_path</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#将网站首页或分类首页转化为网页" class="proc-emissions-name">将网站首页或分类首页转化为网页</a>
<span class="orez-symbol">=&gt;</span> <a href="#$tmp_file->网页" class="proc-emissions-name">$tmp_file -> 网页</a>
</pre>

将网站首页或分类首页转化为 HTML 文件的代码如下：

<pre id="将网站首页或分类首页转化为网页" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 将网站首页或分类首页转化为网页 #</span>
<a href="#安全构造从当前目录到网站根目录的路径->path_to_root" class="orez-callee-link"># 安全构造从当前目录到网站根目录的路径 -> path_to_root @</a>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nv">path_to_root</span><span class="o">=</span><span class="s2">&quot;.&quot;</span>
<span class="w">    </span>pandoc<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--css<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">/appearance/lmd.css&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--data-dir<span class="o">=</span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">/appearance/pandoc/data&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--template<span class="o">=</span>homepage.template<span class="w"> </span><span class="se">\</span>
<span class="w">       </span>-o<span class="w"> </span><span class="s2">&quot;index.html&quot;</span>
<span class="k">else</span>
<span class="w">    </span>pandoc<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--css<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">/appearance/lmd.css&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--data-dir<span class="o">=</span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">/appearance/pandoc/data&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--template<span class="o">=</span>post.template<span class="w"> </span><span class="se">\</span>
<span class="w">       </span>-o<span class="w"> </span><span class="s2">&quot;index.html&quot;</span>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#分类首页->网页1" class="proc-emissions-name">分类首页 -> 网页</a>
<span class="orez-symbol">=&gt;</span> <a href="#上级分类首页->网页1" class="proc-emissions-name">上级分类首页 -> 网页</a>
</pre>

以下代码片段，将当前分类首页转化为 HTML 文件：

<pre id="分类首页->网页1" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 分类首页 -> 网页 #</span>
<a href="#将网站首页或分类首页转化为网页" class="orez-callee-link"># 将网站首页或分类首页转化为网页 @</a>
<span class="orez-symbol">=&gt;</span> <a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="proc-emissions-name">构建分类首页与上级分类首页的关联并将其转换为 HTML 文件</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除分类" class="proc-emissions-name">删除分类</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除文章" class="proc-emissions-name">删除文章</a>
</pre>

以下代码片段，将上级分类首页转化为 HTML 文件：

<pre id="上级分类首页->网页1" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 上级分类首页 -> 网页 #</span>
<span class="nb">cd</span><span class="w"> </span>../
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-e<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span><span class="o">]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
    <a href="#将网站首页或分类首页转化为网页" class="orez-callee-link"># 将网站首页或分类首页转化为网页 @</a>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="proc-emissions-name">构建分类首页与上级分类首页的关联并将其转换为 HTML 文件</a>
<span class="orez-symbol">=&gt;</span> <a href="#文章->HTML文件" class="proc-emissions-name">文章 -> HTML 文件</a>
</pre>

还需要考虑一个问题，临时的分类首页文档及其上级分类首页文档，在 pandoc 用过后该如何处理？为了保持首页源文档的清洁，使用 append-meta-data.awk 脚本添加的信息需要予以消除，然后再用它们代替原首页源文档。用于消除 append-meta-data.awk 脚本所添加信息的 Awk 脚本如下：

<pre id="delete-meta-data.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ delete-meta-data.awk #</span>
<span class="nb">BEGIN</span> <span class="p">{</span>
    <span class="nx">beginning</span> <span class="o">=</span> <span class="mi">1</span>
    <span class="nx">meta_data</span> <span class="o">=</span> <span class="mi">0</span>
<span class="p">}</span>
<span class="p">{</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">beginning</span> <span class="o">&amp;&amp;</span> <span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^--- *$/</span><span class="p">)</span> <span class="p">{</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="nx">meta_data</span> <span class="o">=</span> <span class="mi">1</span>
        <span class="kr">next</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">meta_data</span> <span class="o">&amp;&amp;</span> <span class="o">$</span><span class="mi">0</span> <span class="o">~</span> <span class="sr">/^\.\.\. *$/</span><span class="p">)</span> <span class="p">{</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
        <span class="nx">beginning</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="nx">meta_data</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="kr">next</span>
    <span class="p">}</span>
    <span class="k">if</span> <span class="p">(</span><span class="nx">meta_data</span><span class="p">)</span> <span class="p">{</span>
        <span class="k">if</span> <span class="p">(</span><span class="kr">match</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="sr">/^category:/</span><span class="p">))</span> <span class="kr">next</span>
        <span class="k">if</span> <span class="p">(</span><span class="kr">match</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="sr">/^lang:/</span><span class="p">))</span> <span class="kr">next</span>
        <span class="k">if</span> <span class="p">(</span><span class="kr">match</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="sr">/^footer:/</span><span class="p">))</span> <span class="kr">next</span>
        <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
    <span class="p">}</span> <span class="k">else</span> <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
<span class="p">}</span>
</pre>

从临时的网站首页或分类首页删除 `category`，`lang` 和 `footer` 字段的代码如下：

<pre id="保持网站首页或分类首页的简洁" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 保持网站首页或分类首页的简洁 #</span>
awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/delete-meta-data.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">    </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>&gt;<span class="w"> </span>index<span class="nv">$MARKDOWN_EXT</span>
rm<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="orez-symbol">=&gt;</span> <a href="#分类首页->网页2" class="proc-emissions-name">分类首页 -> 网页</a>
<span class="orez-symbol">=&gt;</span> <a href="#上级分类首页->网页2" class="proc-emissions-name">上级分类首页 -> 网页</a>
</pre>

基于上述代码，删除临时分类首页里的字段：

<pre id="分类首页->网页2" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 分类首页 -> 网页 #</span>  <span class="orez-symbol">+</span>
<a href="#保持网站首页或分类首页的简洁" class="orez-callee-link"># 保持网站首页或分类首页的简洁 @</a>
<span class="orez-symbol">=&gt;</span> <a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="proc-emissions-name">构建分类首页与上级分类首页的关联并将其转换为 HTML 文件</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除分类" class="proc-emissions-name">删除分类</a>
<span class="orez-symbol">=&gt;</span> <a href="#删除文章" class="proc-emissions-name">删除文章</a>
</pre>

删除临时的上级分类首页里的字段：

<pre id="上级分类首页->网页2" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 上级分类首页 -> 网页 #</span>  <span class="orez-symbol">+</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-e<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span><span class="o">]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
    <a href="#保持网站首页或分类首页的简洁" class="orez-callee-link"># 保持网站首页或分类首页的简洁 @</a>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="proc-emissions-name">构建分类首页与上级分类首页的关联并将其转换为 HTML 文件</a>
<span class="orez-symbol">=&gt;</span> <a href="#文章->HTML文件" class="proc-emissions-name">文章 -> HTML 文件</a>
</pre>

将上述代码片段组装起来，便可实现 `lmd_new_category` 未实现的部分：

<pre id="构建分类首页与上级分类首页的关联并将其转换为HTML文件" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 构建分类首页与上级分类首页的关联并将其转换为 HTML 文件 #</span>
<a href="#向分类首页和上级分类首页的首部追加字段" class="orez-callee-link"># 向分类首页和上级分类首页的首部追加字段 @</a>
<a href="#将分类首页添加至上级分类首页中的分类列表" class="orez-callee-link"># 将分类首页添加至上级分类首页中的分类列表 @</a>
<span class="orez-callee-text"># 分类首页 -> 网页 @</span> <a href="#分类首页->网页1">[1]</a> <a href="#分类首页->网页2">[2]</a>
<span class="orez-callee-text"># 上级分类首页 -> 网页 @</span> <a href="#上级分类首页->网页1">[1]</a> <a href="#上级分类首页->网页2">[2]</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd_new_category待续" class="proc-emissions-name">lmd_new_category 待续</a>
</pre>

以下为 `lmd_new_category` 的测试代码：

<pre id="lmd-new-category-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-new-category-test.sh #</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在 -> LMD_SELF_PATH @</a>
<a href="#搜索并加载lmd.conf" class="orez-callee-link"># 搜索并加载 lmd.conf @</a>
<a href="#构造从当前目录到网站根目录的路径" class="orez-callee-link"># 构造从当前目录到网站根目录的路径 @</a>
<a href="#创建文章分类" class="orez-callee-link"># 创建文章分类 @</a>
lmd_new_category<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
</pre>

假设上述脚本存于 /tmp/lmd 目录，在 /tmp/demo 目录下，执行上述脚本：

```console
$ cd /tmp/demo
$ bash ../lmd/lmd-new-category-test.sh "日志" blog
```

结果可在 /tmp/demo 中创建目录 blog，该目录里有 index.md 和 index.html 文件，其上级目录原本便有 index.md，但是会新增 index.html。

## 删除分类

删除分类，不仅要删除分类目录，还要删除它在上级分类首页中的链接：

<pre id="删除分类" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 删除分类 #</span>
<span class="k">function</span><span class="w"> </span>lmd_delete_category<span class="w"> </span><span class="o">{</span>
    <a href="#安全载入配置文件lmd.conf" class="orez-callee-link"># 安全载入配置文件 lmd.conf @</a>
    <a href="#在上级分类首页中删除链接->.tmp_index$MARKDOWN_EXT" class="orez-callee-link"># 在上级分类首页中删除链接 -> .tmp_index$MARKDOWN_EXT @</a>
    mv<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>index<span class="nv">$MARKDOWN_EXT</span>
    <a href="#向网站首页或分类首页追加字段->.tmp_index.md" class="orez-callee-link"># 向网站首页或分类首页追加字段 -> .tmp_index.md @</a>
    <span class="orez-callee-text"># 分类首页 -> 网页 @</span> <a href="#分类首页->网页1">[1]</a> <a href="#分类首页->网页2">[2]</a>
    rm<span class="w"> </span>-rf<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

为了在上级分类首页中删除特定的下级分类首页链接，需要借助以下 Awk 脚本：

<pre id="delete-item.awk" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ delete-item.awk #</span>
<span class="p">{</span>
    <span class="k">if</span> <span class="p">(</span><span class="kr">match</span><span class="p">(</span><span class="o">$</span><span class="mi">0</span><span class="p">,</span> <span class="sr">/^\*[ \t]*\[[^\]]*\]\(.*\)/</span><span class="p">))</span> <span class="p">{</span>
        <span class="nx">s</span> <span class="o">=</span> <span class="o">$</span><span class="mi">0</span>
        <span class="kr">sub</span><span class="p">(</span><span class="sr">/^\*[ \t]*\[[ \t]*/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
        <span class="kr">sub</span><span class="p">(</span><span class="sr">/[ \t]*\].*$/</span><span class="p">,</span> <span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="nx">s</span><span class="p">)</span>
        <span class="k">if</span> <span class="p">(</span><span class="nx">s</span> <span class="o">!=</span> <span class="nx">title</span><span class="p">)</span> <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
    <span class="p">}</span> <span class="k">else</span> <span class="kr">print</span> <span class="o">$</span><span class="mi">0</span>
<span class="p">}</span>
</pre>

基于上述 Awk 脚本，从上级分类首页中删除链接：

<pre id="在上级分类首页中删除链接->.tmp_index$MARKDOWN_EXT" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 在上级分类首页中删除链接-> .tmp_index$MARKDOWN_EXT #</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="k">$(</span>awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/get-title.awk&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">/index</span><span class="nv">$MARKDOWN_EXT</span><span class="s2">&quot;</span><span class="k">)</span>
awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$title</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">    </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/delete-item.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">    </span>index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>&gt;<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="orez-symbol">=&gt;</span> <a href="#删除分类" class="proc-emissions-name">删除分类</a>
</pre>

# 文章

在任何一个分类里创建新的文章——Markdown 文件，皆需要为其创建一个目录，目录的名字与 Markdown 文件名相同，且目录中除包含与之同名的 Markdown 文件之外，还包含一个子目录 figures，用于存放文章插图。

## 创建文章

函数 `lmd_new_post` 可创建文章目录和文章本身：

<pre id="创建新文章" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 创建新文章 #</span>
<span class="k">function</span><span class="w"> </span>lmd_new_post<span class="w"> </span><span class="o">{</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span><span class="w"> </span><span class="o">||</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Error: You should provide the name of post and its file.&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">fi</span>
<span class="w">    </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-e<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="w">    </span><span class="k">then</span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Error: The post already exists.&quot;</span>
<span class="w">        </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="w">    </span><span class="k">fi</span>
    <a href="#安全载入配置文件lmd.conf" class="orez-callee-link"># 安全载入配置文件 lmd.conf @</a>
    <span class="nb">local</span><span class="w"> </span><span class="nv">post</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="nv">$MARKDOWN_EXT</span>
<span class="w">    </span>mkdir<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="p">;</span><span class="w"> </span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="p">;</span><span class="w"> </span>mkdir<span class="w"> </span>figures
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;---&quot;</span><span class="w"> </span>&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$post</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;title: </span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$post</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;date: </span><span class="nv">$DATE</span><span class="s2">&quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$post</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;abstract: &quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$post</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span>-e<span class="w"> </span><span class="s2">&quot;...\n&quot;</span><span class="w"> </span>&gt;&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$post</span><span class="s2">&quot;</span>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-new-post-test.sh" class="proc-emissions-name">lmd-new-post-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

以下是 `lmd_new_post` 的测试代码：

<pre id="lmd-new-post-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-new-post-test.sh #</span>
<a href="#搜索并加载lmd.conf" class="orez-callee-link"># 搜索并加载 lmd.conf @</a>
<a href="#创建新文章" class="orez-callee-link"># 创建新文章 @</a>
lmd_new_post<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
</pre>

假设上述脚本位于 /tmp/lmd 目录，在 /tmp/demo/blog 目录下执行它：

```console
$ cd /tmp/demo/blog
$ bash ../../lmd/lmd-new-post-test.sh "第一篇文章" first-post
```

结果可在 /tmp/demo/blog 目录下创建 first-post 目录，后者包含 first-post.md 文件和 figures 目录。first-post.md 内容如下：

```markdown
---
title: 第一篇文章
date: 2025 年 01 月 20 日
abstract: 
...

```

## 生成网页

从文章的源文档（Markdown 文档）生成 HTML 文件的过程与前文中构建分类首页的过程相似，由函数 `lmd_convert` 实现，如下：

<pre id="文章->HTML文件" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 文章 -> HTML 文件 #</span>
<span class="k">function</span><span class="w"> </span>lmd_convert<span class="w"> </span><span class="o">{</span>
    <a href="#安全载入配置文件lmd.conf" class="orez-callee-link"># 安全载入配置文件 lmd.conf @</a>
    <span class="nb">local</span><span class="w"> </span><span class="nv">tmp_file</span><span class="o">=</span>.tmp_<span class="nv">$1</span>
    <a href="#向文章和分类首页的首部追加字段" class="orez-callee-link"># 向文章和分类首页的首部追加字段 @</a>
    <a href="#将文章添加到分类首页或网站首页" class="orez-callee-link"># 将文章添加到分类首页或网站首页 @</a>
    <a href="#$tmp_file->网页" class="orez-callee-link"># $tmp_file -> 网页 @</a>
    <span class="orez-callee-text"># 上级分类首页 -> 网页 @</span> <a href="#上级分类首页->网页1">[1]</a> <a href="#上级分类首页->网页2">[2]</a>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-convert-test.sh" class="proc-emissions-name">lmd-convert-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

下面逐一实现上述尚未实现的代码片段。首先，向 Markdown 文档首部追加字段：

<pre id="向文章和分类首页的首部追加字段" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 向文章和分类首页的首部追加字段 #</span>
awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">category</span><span class="o">=</span><span class="s2">&quot;../index.html&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">    </span>-v<span class="w"> </span><span class="nv">lang</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$LANG</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">    </span>-v<span class="w"> </span><span class="nv">footer</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$FOOTER</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">    </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/append-meta-data.awk&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span>&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$tmp_file</span><span class="s2">&quot;</span>
<span class="nb">cd</span><span class="w"> </span>..<span class="w">  </span><span class="c1"># 处理分类首页</span>
<a href="#向网站首页或分类首页追加字段->.tmp_index.md" class="orez-callee-link"># 向网站首页或分类首页追加字段 -> .tmp_index.md @</a>
<span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">1</span><span class="p">%</span><span class="nv">$MARKDOWN_EXT</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#文章->HTML文件" class="proc-emissions-name">文章 -> HTML 文件</a>
</pre>

然后将文章添加到分类首页：

<pre id="将文章添加到分类首页或网站首页" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 将文章添加到分类首页或网站首页 #</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="k">$(</span>awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/get-title.awk&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="k">)</span>
<span class="nb">local</span><span class="w"> </span><span class="nv">exist</span><span class="o">=</span><span class="k">$(</span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$title</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">                  </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/find-post.awk&quot;</span><span class="w"> </span><span class="s2">&quot;../index.md&quot;</span><span class="k">)</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$exist</span><span class="s2">&quot;</span><span class="w"> </span>!<span class="o">=</span><span class="w"> </span><span class="s2">&quot;true&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nb">local</span><span class="w"> </span><span class="nv">post</span><span class="o">=</span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">1</span><span class="p">%</span><span class="nv">$MARKDOWN_EXT</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="w">    </span><span class="nb">local</span><span class="w"> </span><span class="nv">post_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$post</span><span class="s2">/</span><span class="si">${</span><span class="nv">post</span><span class="si">}</span><span class="s2">.html&quot;</span>
<span class="w">    </span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$title</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-v<span class="w"> </span><span class="nv">post_path</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$post_path</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/add-category-or-post.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>../.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>&gt;<span class="w"> </span>../.tmp_0_index<span class="nv">$MARKDOWN_EXT</span>
<span class="w">    </span>mv<span class="w"> </span>../.tmp_0_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>../.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#文章->HTML文件" class="proc-emissions-name">文章 -> HTML 文件</a>
</pre>

上述代码中，应用了 Bash 字符串处理的小技巧，即 `${1%$MARKDOWN_EXT}`，表示将参数 `$1` 中包含的 Markdown 扩展名消除。`%` 是消除字符串指定后缀的运算符。例如，假设 Bash 变量 `FOO=foobar`，`${FOO%bar}` 结果为 `foo`。消除指定的前缀，运算符是 `#`。

将临时的文章转换为网页：

<pre id="$tmp_file->网页" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ $tmp_file -> 网页 #</span>
<a href="#安全构造从当前目录到网站根目录的路径->path_to_root" class="orez-callee-link"># 安全构造从当前目录到网站根目录的路径 -> path_to_root @</a>
pandoc<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$tmp_file</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--css<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">/appearance/lmd.css&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--data-dir<span class="o">=</span><span class="s2">&quot;</span><span class="nv">$path_to_root</span><span class="s2">/appearance/pandoc/data&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--template<span class="o">=</span>post.template<span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--mathjax<span class="o">=</span>https://cdnbootcss.com/mathjax/3.2.2/es5/tex-mml-chtml.js<span class="w"> </span><span class="se">\</span>
<span class="w">       </span>--highlight-style<span class="o">=</span>pygments<span class="w"> </span><span class="se">\</span>
<span class="w">       </span>-o<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">1</span><span class="p">%</span><span class="nv">$MARKDOWN_EXT</span><span class="si">}</span><span class="s2">.html&quot;</span>
awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/delete-meta-data.awk&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$tmp_file</span><span class="s2">&quot;</span><span class="w"> </span>&gt;<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
rm<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$tmp_file</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#文章->HTML文件" class="proc-emissions-name">文章 -> HTML 文件</a>
</pre>

将分类首页转化为网页的代码已在「[创建分类](#创建分类)」一节中实现。

以下代码用于测试 `lmd_convert`：

<pre id="lmd-convert-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-convert-test.sh #</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在-> LMD_SELF_PATH @</a>
<a href="#搜索并加载lmd.conf" class="orez-callee-link"># 搜索并加载 lmd.conf @</a>
<a href="#构造从当前目录到网站根目录的路径" class="orez-callee-link"># 构造从当前目录到网站根目录的路径 @</a>
<a href="#文章->HTML文件" class="orez-callee-link"># 文章 -> HTML 文件 @</a>
lmd_convert<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
</pre>

假设上述所有 Awk 脚本皆存放于 /tmp/lmd/helper 目录，将 lmd-convert-test.sh 置于 /tmp/lmd 目录，将 /tmp/demo/blog/first-post 作为工作目录，执行以下命令：

```console
$ cd /tmp/demo/blog/first-post
$ bash /tmp/lmd/lmd-convert-test.sh first-post.md
```

上述命令可在 first-post 目录下生成 first-post.html 文件，且在 blog 下生成 index.html 文件。

## 删除文章

与删除分类相似，删除文章，除删除文章目录之外，还要从所述分类首页上删除该文章的链接：

<pre id="删除文章" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 删除文章 #</span>
<span class="k">function</span><span class="w"> </span>lmd_delete_post<span class="w"> </span><span class="o">{</span>
    <a href="#安全载入配置文件lmd.conf" class="orez-callee-link"># 安全载入配置文件 lmd.conf @</a>
    <span class="nb">local</span><span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="k">$(</span>awk<span class="w"> </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/get-title.awk&quot;</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">/</span><span class="nv">$1$MARKDOWN_EXT</span><span class="s2">&quot;</span><span class="k">)</span>
<span class="w">    </span>awk<span class="w"> </span>-v<span class="w"> </span><span class="nv">title</span><span class="o">=</span><span class="s2">&quot;</span><span class="nv">$title</span><span class="s2">&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>-f<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$LMD_SELF_PATH</span><span class="s2">/helper/delete-item.awk&quot;</span><span class="w"> </span><span class="se">\</span>
<span class="w">        </span>index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>&gt;<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span>
<span class="w">    </span>mv<span class="w"> </span>.tmp_index<span class="nv">$MARKDOWN_EXT</span><span class="w"> </span>index<span class="nv">$MARKDOWN_EXT</span>
    <a href="#向网站首页或分类首页追加字段->.tmp_index.md" class="orez-callee-link"># 向网站首页或分类首页追加字段 -> .tmp_index.md @</a>
    <span class="orez-callee-text"># 分类首页 -> 网页 @</span> <a href="#分类首页->网页1">[1]</a> <a href="#分类首页->网页2">[2]</a>
    rm<span class="w"> </span>-rf<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-delete-post-test.sh" class="proc-emissions-name">lmd-delete-post-test.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

以下是 `lmd_delete_post` 的测试代码：

<pre id="lmd-delete-post-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-delete-post-test.sh #</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在-> LMD_SELF_PATH @</a>
<a href="#搜索并加载lmd.conf" class="orez-callee-link"># 搜索并加载 lmd.conf @</a>
<a href="#构造从当前目录到网站根目录的路径" class="orez-callee-link"># 构造从当前目录到网站根目录的路径 @</a>
<a href="#删除文章" class="orez-callee-link"># 删除文章 @</a>
lmd_delete_post<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span>
</pre>

# 界面

lmd 脚本的主要功能皆已实现，现在为这些功能构造简单的命令行界面：

<pre id="lmd界面" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd 界面 #</span>
<span class="k">case</span><span class="w"> </span><span class="nv">$1</span><span class="w"> </span><span class="k">in</span><span class="w"> </span>
<span class="w">    </span>init<span class="o">)</span>
<span class="w">        </span>lmd_init<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$3</span><span class="s2">&quot;</span>
<span class="w">        </span>lmd_init_index<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="p">;;</span>
<span class="w">    </span>new<span class="o">)</span>
<span class="w">        </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="nv">$2</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;category&quot;</span><span class="w"> </span><span class="o">]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
<span class="w">            </span>lmd_new_category<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="p">@:</span><span class="nv">3</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="k">elif</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="nv">$2</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;post&quot;</span><span class="w"> </span><span class="o">]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
<span class="w">            </span>lmd_new_post<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="p">@:</span><span class="nv">3</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="k">else</span>
<span class="w">            </span><span class="nb">echo</span><span class="w"> </span><span class="s1">&#39;Error: unknown option!&#39;</span>
<span class="w">        </span><span class="k">fi</span>
<span class="w">        </span><span class="p">;;</span>
<span class="w">    </span>delete<span class="o">)</span>
<span class="w">        </span><span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="nv">$2</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;category&quot;</span><span class="w"> </span><span class="o">]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
<span class="w">            </span>lmd_delete_category<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="p">@:</span><span class="nv">3</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="k">elif</span><span class="w"> </span><span class="o">[</span><span class="w"> </span><span class="nv">$2</span><span class="w"> </span><span class="o">=</span><span class="w"> </span><span class="s2">&quot;post&quot;</span><span class="w"> </span><span class="o">]</span><span class="p">;</span><span class="w"> </span><span class="k">then</span>
<span class="w">            </span>lmd_delete_post<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="p">@:</span><span class="nv">3</span><span class="si">}</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="k">else</span>
<span class="w">            </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Error: unknown option!&quot;</span>
<span class="w">        </span><span class="k">fi</span>
<span class="w">        </span><span class="p">;;</span>
<span class="w">    </span>convert<span class="o">)</span><span class="w"> </span>lmd_convert<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="p">@:</span><span class="nv">2</span><span class="si">}</span><span class="s2">&quot;</span><span class="w"> </span><span class="p">;;</span><span class="w">    </span>
<span class="w">    </span>root<span class="o">)</span><span class="w"> </span><span class="nb">echo</span><span class="w"> </span><span class="k">$(</span>lmd_path_to_start<span class="k">)</span><span class="w"> </span><span class="p">;;</span>
<span class="w">    </span>tree<span class="o">)</span>
<span class="w">        </span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="k">$(</span>lmd_path_to_start<span class="k">)</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="nv">WORKS</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span>basename<span class="w"> </span><span class="k">$(</span><span class="nb">pwd</span><span class="k">))</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="nb">cd</span><span class="w"> </span>../
<span class="w">        </span>tree<span class="w"> </span><span class="si">${</span><span class="p">@:</span><span class="nv">2</span><span class="si">}</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$WORKS</span><span class="s2">&quot;</span>
<span class="w">        </span><span class="p">;;</span>
<span class="w">    </span>*<span class="o">)</span><span class="w"> </span>
<span class="w">        </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd: I do not understand you!&quot;</span>
<span class="w">        </span><span class="c1">#exit -1</span>
<span class="w">        </span><span class="p">;;</span>
<span class="k">esac</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd脚本" class="proc-emissions-name">lmd 脚本</a>
</pre>

上述代码中，使用了 Bash 数组切片的语法，例如 `${@:2}`，表示从命令行参数列表里截取第 2 个及其之后的参数。

# lmd 脚本

将前文定义的代码片段组装起来，便可得到 lmd 脚本的全部内容：

<pre id="lmd脚本" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd 脚本 #</span>
<span class="ch">#!/usr/bin/env bash</span>
<a href="#确定脚本自身所在->LMD_SELF_PATH" class="orez-callee-link"># 确定脚本自身所在 -> LMD_SELF_PATH @</a>
<a href="#搜索并加载lmd.conf" class="orez-callee-link"># 搜索并加载 lmd.conf @</a>
<a href="#构造从当前目录到网站根目录的路径" class="orez-callee-link"># 构造从当前目录到网站根目录的路径 @</a>
<a href="#网站初始化" class="orez-callee-link"># 网站初始化 @</a>
<a href="#网站首页初始化" class="orez-callee-link"># 网站首页初始化 @</a>
<a href="#创建文章分类" class="orez-callee-link"># 创建文章分类 @</a>
<a href="#删除分类" class="orez-callee-link"># 删除分类 @</a>
<a href="#创建新文章" class="orez-callee-link"># 创建新文章 @</a>
<a href="#文章->HTML文件" class="orez-callee-link"># 文章 -> HTML 文件 @</a>
<a href="#删除文章" class="orez-callee-link"># 删除文章 @</a>
<a href="#lmd界面" class="orez-callee-link"># lmd 界面 @</a>
</pre>

# 项目构建

本文档的源文档名为 new-lmd.orz，下载地址：<http://liyanrui.github.io/source/2025/new-lmd.orz>

下载 new-lmd.orz 文件后，使用 orez 提取 lmd 脚本、网页样式表和 pandoc 模板，便可构建 lmd 项目的目录。不过，需要注意，由于 orez 的文学编程语法未提供符号转义功能，网页样式表和 pandoc 模板的开头出现的 `@` 和尖括号与 orez 文学编程语法有冲突，故而在 new-lmd.orz 中，我使用 `ESC_AT`，`ESC_LEFT_ANGLE` 和 `ESC_RIGHT_ANGLE` 代替了这些符号，使用 orez 提取这部分内容后，需要用 sed 或 awk 之类的文本处理工具将这些代符号替换为相应符号。

首先，假设在 $HOME/opt 目录（若无该目录，不妨自行建立）下建立 lmd 目录，并进入：

```console
$ cd $HOME/opt
$ mkdir lmd
$ cd lmd
```

将 new-lmd.orz 文件复制到 lmd 目录，然后执行以下命令便可从 new-lmd.orz 中获得 lmd 脚本和配置文件 lmd.conf，并赋予 lmd 脚本可执行权限：

```console
$ orez -t new-lmd.orz -e "lmd 脚本" -o lmd
$ orez -t new-lmd.orz -e "lmd.conf"
$ chmod +x lmd
```

抽取 lmd 脚本所需的 Awk 脚本：

```console
$ mkdir helper
$ cd helper
$ for i in {append-meta-data,get-title,\
get-date,find-post,\
add-category-or-post,delete-meta-data,delete-item}.awk; \
do orez -t ../new-lmd.orz -e "$i"; done
$ cd ..
```

抽取样式表，并替换 `ESC_AT`：

```console
$ mkdir -p data/appearance
$ orez -t new-lmd.orz -e "lmd.css" -o data/appearance/lmd.css
$ sed -i "s/ESC_AT /@/g" data/appearance/lmd.css
```

抽取 pandoc 模板，并替换 `ESC_LEFT_ANGLE` 和 `ESC_RIGHT_ANGLE`：

```console
$ mkdir -p data/appearance/pandoc/data/templates
$ cd data/appearance/pandoc/data/templates
$ orez -t $HOME/opt/lmd/new-lmd.orz -e "homepage.template"
$ sed -i "s/ESC_LEFT_ANGLE /</g; s/ ESC_RIGHT_ANGLE/>/g" homepage.template
$ orez -t $HOME/opt/lmd/new-lmd.orz -e "post.template"
$ sed -i "s/ESC_LEFT_ANGLE /</g; s/ ESC_RIGHT_ANGLE/>/g" post.template
```

至此，完整的 lmd 脚本项目构建完毕。要使用 lmd 脚本，请将其路径填写到 Bash Shell 的 `PATH` 变量里，例如在 $HOME/.bashrc 文件中添加以下内容：

```bash
export PATH=$HOME/opt/lmd:$PATH
```

生效后，便可在 Bash Shell 中直接使用 `lmd` 命令了。例如，在 $HOME/documents 目录初始化网站 foo：

```console
$ cd $HOME/documents
$ lmd init "Foo" foo
$ cd foo
$ lmd tree
foo
├── appearance
│   ├── lmd.css
│   └── pandoc
│       └── data
│           └── templates
│               ├── homepage.template
│               └── post.template
├── index.md
└── lmd.conf

5 directories, 5 files
```

# 附录

## pandoc 模板

网站首页模板：

<pre id="homepage.template" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ homepage.template #</span>
<span class="cp">&lt;!DOCTYPE html&gt;</span>
<a href="#pandoc模板首部" class="orez-callee-link"># pandoc 模板首部 @</a>
$for(author-meta)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;author&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$author-meta$&quot;</span> <span class="p">/&gt;</span>
$endfor$
$if(date-meta)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;dcterms.date&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$date-meta$&quot;</span> <span class="p">/&gt;</span>
$endif$
$if(keywords)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;keywords&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$for(keywords)$$keywords$$sep$, $endfor$&quot;</span> <span class="p">/&gt;</span>
$endif$
$if(description-meta)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;description&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$description-meta$&quot;</span> <span class="p">/&gt;</span>
$endif$
  <span class="p">&lt;</span><span class="nt">title</span><span class="p">&gt;</span>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$<span class="p">&lt;/</span><span class="nt">title</span><span class="p">&gt;</span>
  <span class="p">&lt;</span><span class="nt">style</span><span class="p">&gt;</span>
<span class="w">    </span><span class="o">$</span><span class="nt">styles</span><span class="p">.</span><span class="nc">html</span><span class="o">()$</span>
<span class="w">  </span><span class="p">&lt;/</span><span class="nt">style</span><span class="p">&gt;</span>
$for(css)$
  <span class="p">&lt;</span><span class="nt">link</span> <span class="na">rel</span><span class="o">=</span><span class="s">&quot;stylesheet&quot;</span> <span class="na">href</span><span class="o">=</span><span class="s">&quot;$css$&quot;</span> <span class="p">/&gt;</span>
$endfor$
$for(header-includes)$
  $header-includes$
$endfor$
$if(math)$
  $math$
$endif$
<span class="p">&lt;/</span><span class="nt">head</span><span class="p">&gt;</span>
<span class="p">&lt;</span><span class="nt">body</span><span class="p">&gt;</span>
$for(include-before)$
$include-before$
$endfor$
$if(title)$
<span class="p">&lt;</span><span class="nt">header</span> <span class="na">id</span><span class="o">=</span><span class="s">&quot;title-block-header&quot;</span><span class="p">&gt;</span>
<span class="p">&lt;</span><span class="nt">h1</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;home-title&quot;</span><span class="p">&gt;</span>$title$<span class="p">&lt;/</span><span class="nt">h1</span><span class="p">&gt;</span>
<span class="p">&lt;/</span><span class="nt">header</span><span class="p">&gt;</span>
$endif$
$if(subtitle)$
<span class="p">&lt;</span><span class="nt">p</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;subtitle&quot;</span><span class="p">&gt;</span>$subtitle$<span class="p">&lt;/</span><span class="nt">p</span><span class="p">&gt;</span>
$endif$
<span class="p">&lt;</span><span class="nt">hr</span> <span class="p">/&gt;</span>
$body$
$if(footer)$
<span class="p">&lt;</span><span class="nt">hr</span> <span class="p">/&gt;</span>
<span class="p">&lt;</span><span class="nt">div</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;footer&quot;</span><span class="p">&gt;</span>$footer$<span class="p">&lt;/</span><span class="nt">div</span><span class="p">&gt;</span>
$endif$
<span class="p">&lt;/</span><span class="nt">body</span><span class="p">&gt;</span>
<span class="p">&lt;/</span><span class="nt">html</span><span class="p">&gt;</span>
</pre>

网站文章模板：

<pre id="post.template" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ post.template #</span>
<span class="cp">&lt;!DOCTYPE html&gt;</span>
<a href="#pandoc模板首部" class="orez-callee-link"># pandoc 模板首部 @</a>
$for(author-meta)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;author&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$author-meta$&quot;</span> <span class="p">/&gt;</span>
$endfor$
$if(date-meta)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;dcterms.date&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$date-meta$&quot;</span> <span class="p">/&gt;</span>
$endif$
$if(keywords)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;keywords&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$for(keywords)$$keywords$$sep$, $endfor$&quot;</span> <span class="p">/&gt;</span>
$endif$
$if(description-meta)$
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;description&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;$description-meta$&quot;</span> <span class="p">/&gt;</span>
$endif$
  <span class="p">&lt;</span><span class="nt">title</span><span class="p">&gt;</span>$if(title-prefix)$$title-prefix$ – $endif$$pagetitle$<span class="p">&lt;/</span><span class="nt">title</span><span class="p">&gt;</span>
  <span class="p">&lt;</span><span class="nt">style</span><span class="p">&gt;</span>
<span class="w">    </span><span class="o">$</span><span class="nt">styles</span><span class="p">.</span><span class="nc">html</span><span class="o">()$</span>
<span class="w">  </span><span class="p">&lt;/</span><span class="nt">style</span><span class="p">&gt;</span>
$for(css)$
  <span class="p">&lt;</span><span class="nt">link</span> <span class="na">rel</span><span class="o">=</span><span class="s">&quot;stylesheet&quot;</span> <span class="na">href</span><span class="o">=</span><span class="s">&quot;$css$&quot;</span> <span class="p">/&gt;</span>
$endfor$
$for(header-includes)$
  $header-includes$
$endfor$
$if(math)$
  $math$
$endif$
<span class="p">&lt;/</span><span class="nt">head</span><span class="p">&gt;</span>
<span class="p">&lt;</span><span class="nt">body</span><span class="p">&gt;</span>
$for(include-before)$
$include-before$
$endfor$
$if(category)$
<span class="p">&lt;</span><span class="nt">div</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;category&quot;</span><span class="p">&gt;</span>
<span class="p">&lt;</span><span class="nt">a</span> <span class="na">href</span><span class="o">=</span><span class="s">&quot;$category$&quot;</span><span class="p">&gt;</span>回上级页面<span class="p">&lt;/</span><span class="nt">a</span><span class="p">&gt;</span>
<span class="p">&lt;/</span><span class="nt">div</span><span class="p">&gt;</span>
$endif$
$if(title)$
<span class="p">&lt;</span><span class="nt">header</span> <span class="na">id</span><span class="o">=</span><span class="s">&quot;title-block-header&quot;</span><span class="p">&gt;</span>
<span class="p">&lt;</span><span class="nt">h1</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;title&quot;</span><span class="p">&gt;</span>$title$<span class="p">&lt;/</span><span class="nt">h1</span><span class="p">&gt;</span>
$if(subtitle)$
<span class="p">&lt;</span><span class="nt">p</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;subtitle&quot;</span><span class="p">&gt;</span>$subtitle$<span class="p">&lt;/</span><span class="nt">p</span><span class="p">&gt;</span>
$endif$
$if(date)$
<span class="p">&lt;</span><span class="nt">p</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;date&quot;</span><span class="p">&gt;</span>$date$<span class="p">&lt;/</span><span class="nt">p</span><span class="p">&gt;</span>
$endif$
<span class="p">&lt;/</span><span class="nt">header</span><span class="p">&gt;</span>
$endif$
<span class="p">&lt;</span><span class="nt">hr</span> <span class="p">/&gt;</span>
$if(toc)$
<span class="p">&lt;</span><span class="nt">nav</span> <span class="na">id</span><span class="o">=</span><span class="s">&quot;$idprefix$TOC&quot;</span> <span class="na">role</span><span class="o">=</span><span class="s">&quot;doc-toc&quot;</span><span class="p">&gt;</span>
$table-of-contents$
<span class="p">&lt;/</span><span class="nt">nav</span><span class="p">&gt;</span>
$endif$
$body$
$if(footer)$
<span class="p">&lt;</span><span class="nt">hr</span> <span class="p">/&gt;</span>
<span class="p">&lt;</span><span class="nt">div</span> <span class="na">class</span><span class="o">=</span><span class="s">&quot;footer&quot;</span><span class="p">&gt;</span>$footer$<span class="p">&lt;/</span><span class="nt">div</span><span class="p">&gt;</span>
$endif$
<span class="p">&lt;/</span><span class="nt">body</span><span class="p">&gt;</span>
<span class="p">&lt;/</span><span class="nt">html</span><span class="p">&gt;</span>
</pre>

上述两个模板引用了以下片段：

<pre id="pandoc模板首部" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ pandoc 模板首部 #</span>
<span class="p">&lt;</span><span class="nt">html</span> <span class="na">xmlns</span><span class="o">=</span><span class="s">&quot;http://www.w3.org/1999/xhtml&quot;</span> <span class="na">lang</span><span class="o">=</span><span class="s">&quot;$lang$&quot;</span> 
      <span class="na">xml:lang</span><span class="o">=</span><span class="s">&quot;$lang$&quot;</span><span class="err">$</span><span class="na">if</span><span class="err">(</span><span class="na">dir</span><span class="err">)$</span> <span class="na">dir</span><span class="o">=</span><span class="s">&quot;$dir$&quot;</span><span class="err">$</span><span class="na">endif</span><span class="err">$</span><span class="p">&gt;</span>
<span class="p">&lt;</span><span class="nt">head</span><span class="p">&gt;</span>
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">charset</span><span class="o">=</span><span class="s">&quot;utf-8&quot;</span> <span class="p">/&gt;</span>
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;generator&quot;</span> <span class="na">content</span><span class="o">=</span><span class="s">&quot;pandoc&quot;</span> <span class="p">/&gt;</span>
  <span class="p">&lt;</span><span class="nt">meta</span> <span class="na">name</span><span class="o">=</span><span class="s">&quot;viewport&quot;</span> 
        <span class="na">content</span><span class="o">=</span><span class="s">&quot;width=device-width, initial-scale=1.0, user-scalable=yes&quot;</span> <span class="p">/&gt;</span>
<span class="orez-symbol">=&gt;</span> <a href="#homepage.template" class="proc-emissions-name">homepage.template</a>
<span class="orez-symbol">=&gt;</span> <a href="#post.template" class="proc-emissions-name">post.template</a>
</pre>

pandoc 的文档模板，除了 HTML 标记之外，还有一些变量，例如 `$title$`——表示文档标题，以及一些条件语句。若对 HTML 语言有所了解，上述文档模板的内容不难理解。pandoc 可将 Markdown 里的一些信息提取出来并转化为 HTML 文本，嵌入文档模板中由变量和条件语句构造的相应位置，从而形成具体的 HTML 文档。

## 样式表

<pre id="lmd.css" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd.css #</span>
<span class="p">@</span><span class="k">media</span><span class="w"> </span><span class="nt">screen</span><span class="w"> </span><span class="nt">and</span><span class="w"> </span><span class="o">(</span><span class="nt">max-width</span><span class="p">:</span><span class="nd">1024px</span><span class="o">)</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="p">#</span><span class="nn">TOC</span><span class="w"> </span><span class="p">{</span><span class="k">display</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;}</span>
<span class="w">    </span><span class="nt">body</span><span class="w"> </span><span class="p">{</span><span class="k">max-width</span><span class="p">:</span><span class="w"> </span><span class="mi">65</span><span class="kt">%</span><span class="w"> </span><span class="cp">!important</span><span class="p">;}</span>
<span class="p">}</span>
<span class="p">@</span><span class="k">media</span><span class="w"> </span><span class="nt">screen</span><span class="w"> </span><span class="nt">and</span><span class="w"> </span><span class="o">(</span><span class="nt">max-width</span><span class="p">:</span><span class="nd">720px</span><span class="o">)</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="nt">body</span><span class="w"> </span><span class="p">{</span><span class="k">max-width</span><span class="p">:</span><span class="w"> </span><span class="mi">80</span><span class="kt">%</span><span class="w"> </span><span class="cp">!important</span><span class="p">;}</span>
<span class="w">    </span><span class="nt">div</span><span class="p">.</span><span class="nc">category</span><span class="w"> </span><span class="p">{</span><span class="k">right</span><span class="p">:</span><span class="w"> </span><span class="mf">.5</span><span class="kt">em</span><span class="w"> </span><span class="cp">!important</span><span class="p">;}</span>
<span class="p">}</span>
<span class="nt">html</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mi">14</span><span class="kt">px</span><span class="p">;</span>
<span class="w">    </span><span class="k">line-height</span><span class="p">:</span><span class="w"> </span><span class="mi">24</span><span class="kt">px</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">body</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">max-width</span><span class="p">:</span><span class="w"> </span><span class="mi">50</span><span class="kt">%</span><span class="p">;</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="w"> </span><span class="kc">auto</span><span class="p">;</span>
<span class="w">    </span><span class="k">padding</span><span class="p">:</span><span class="w"> </span><span class="mi">20</span><span class="kt">px</span><span class="p">;</span>
<span class="w">    </span><span class="k">hyphens</span><span class="p">:</span><span class="w"> </span><span class="kc">auto</span><span class="p">;</span>
<span class="w">    </span><span class="k">word-wrap</span><span class="p">:</span><span class="w"> </span><span class="kc">break-word</span><span class="p">;</span>
<span class="w">    </span><span class="c">/* 对于 firefox，font-kerning 需要设为 auto，中文标点才会正常。*/</span>
<span class="w">    </span><span class="c">/* font-kerning: normal; */</span>
<span class="w">    </span><span class="k">font-family</span><span class="p">:</span><span class="w"> </span><span class="s2">&quot;Arial&quot;</span><span class="p">,</span><span class="w"> </span><span class="s2">&quot;Noto Sans CJK SC&quot;</span><span class="p">,</span><span class="w"> </span><span class="kc">sans-serif</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">header</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">center</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">h1</span><span class="o">,</span><span class="w"> </span><span class="nt">h2</span><span class="o">,</span><span class="w"> </span><span class="nt">h3</span><span class="o">,</span><span class="w"> </span><span class="nt">h4</span><span class="o">,</span><span class="w"> </span><span class="nt">h5</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mf">.5</span><span class="kt">em</span><span class="w"> </span><span class="kc">auto</span><span class="p">;</span>
<span class="w">    </span><span class="k">line-height</span><span class="p">:</span><span class="w"> </span><span class="mf">1.5</span><span class="kt">em</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">h1</span><span class="p">.</span><span class="nc">home-title</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">2.65</span><span class="kt">em</span><span class="p">;</span><span class="w"> </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mf">.5</span><span class="kt">em</span><span class="w"> </span><span class="kc">auto</span><span class="p">;}</span>
<span class="nt">h1</span><span class="p">.</span><span class="nc">title</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mi">2</span><span class="kt">em</span><span class="p">;</span><span class="w"> </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mf">.5</span><span class="kt">em</span><span class="w"> </span><span class="kc">auto</span><span class="p">;}</span>
<span class="nt">h1</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">1.6</span><span class="kt">em</span><span class="p">;}</span>
<span class="nt">h2</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">1.4</span><span class="kt">em</span><span class="p">;}</span>
<span class="nt">h3</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">1.2</span><span class="kt">em</span><span class="p">;}</span>
<span class="nt">p</span><span class="p">.</span><span class="nc">subtitle</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="mf">1.2</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">center</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">span</span><span class="p">.</span><span class="nc">index-date</span><span class="w"> </span><span class="p">{</span><span class="k">margin-left</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;</span><span class="w"> </span><span class="k">margin-right</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;}</span>
<span class="p">#</span><span class="nn">TOC</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">padding</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="kt">em</span><span class="w"> </span><span class="mf">.5</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">border</span><span class="p">:</span><span class="w"> </span><span class="mi">2</span><span class="kt">pt</span><span class="w"> </span><span class="kc">solid</span><span class="w"> </span><span class="kc">darkgray</span><span class="p">;</span>
<span class="w">    </span><span class="k">line-height</span><span class="p">:</span><span class="w"> </span><span class="mf">1.5</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">position</span><span class="p">:</span><span class="w"> </span><span class="kc">fixed</span><span class="p">;</span>
<span class="w">    </span><span class="k">right</span><span class="p">:</span><span class="w"> </span><span class="mf">1.75</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">top</span><span class="p">:</span><span class="w"> </span><span class="mi">6</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">overflow</span><span class="p">:</span><span class="w"> </span><span class="kc">hidden</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">div</span><span class="p">.</span><span class="nc">category</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">width</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="n">text-color</span><span class="p">:</span><span class="kc">white</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">center</span><span class="p">;</span>
<span class="w">    </span><span class="k">position</span><span class="p">:</span><span class="w"> </span><span class="kc">fixed</span><span class="p">;</span>
<span class="w">    </span><span class="k">right</span><span class="p">:</span><span class="w"> </span><span class="mf">1.75</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">bottom</span><span class="p">:</span><span class="w"> </span><span class="mi">6</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">line-height</span><span class="p">:</span><span class="w"> </span><span class="mf">1.2</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">padding</span><span class="p">:</span><span class="w"> </span><span class="mf">.5</span><span class="kt">em</span><span class="w"> </span><span class="mf">.3</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">background</span><span class="p">:</span><span class="w"> </span><span class="mh">#666666</span><span class="p">;</span>
<span class="p">}</span>
<span class="p">#</span><span class="nn">TOC</span><span class="w"> </span><span class="nt">a</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">0.9</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">font-weight</span><span class="p">:</span><span class="w"> </span><span class="kc">normal</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">hr</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="mh">#eeeeee</span><span class="p">;</span>
<span class="w">    </span><span class="n">size</span><span class="p">:</span><span class="w"> </span><span class="mi">10</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">div</span><span class="p">.</span><span class="nc">category</span><span class="w"> </span><span class="nt">a</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">text-decoration</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">div</span><span class="p">.</span><span class="nc">category</span><span class="w"> </span><span class="nt">a</span><span class="p">:</span><span class="nd">link</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="kc">white</span><span class="p">;</span>
<span class="w">    </span><span class="k">font-weight</span><span class="p">:</span><span class="w"> </span><span class="kc">bold</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">div</span><span class="p">.</span><span class="nc">category</span><span class="w"> </span><span class="nt">a</span><span class="p">:</span><span class="nd">hover</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="kc">white</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-decoration</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">div</span><span class="p">.</span><span class="nc">category</span><span class="w"> </span><span class="nt">a</span><span class="p">:</span><span class="nd">visited</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="kc">white</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-decoration</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">p</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mf">1.3</span><span class="kt">em</span><span class="w"> </span><span class="mi">0</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">justify</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">figure</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="kc">auto</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">center</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">img</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="w"> </span><span class="mi">0</span><span class="p">;</span>
<span class="w">    </span><span class="k">max-width</span><span class="p">:</span><span class="w"> </span><span class="mi">100</span><span class="kt">%</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">figcaption</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">0.9</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">center</span><span class="p">;</span>
<span class="w">    </span><span class="k">display</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">pre</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mf">0.9</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">padding</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">line-height</span><span class="p">:</span><span class="w"> </span><span class="mf">1.5</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">overflow</span><span class="p">:</span><span class="kc">auto</span><span class="w"> </span><span class="cp">!important</span><span class="p">;</span>
<span class="w">    </span><span class="k">background</span><span class="p">:</span><span class="w"> </span><span class="mh">#f8f8f8</span><span class="p">;</span>
<span class="w">    </span><span class="k">border</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">px</span><span class="w"> </span><span class="kc">solid</span><span class="w"> </span><span class="mh">#ccc</span><span class="p">;</span>
<span class="w">    </span><span class="k">border-radius</span><span class="p">:</span><span class="w"> </span><span class="mf">0.25</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">max-height</span><span class="p">:</span><span class="w"> </span><span class="mi">400</span><span class="kt">px</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">code</span><span class="o">,</span><span class="w"> </span><span class="nt">code</span><span class="w"> </span><span class="nt">span</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="mh">#e83e8c</span><span class="p">;</span>
<span class="w">    </span><span class="k">font-family</span><span class="p">:</span><span class="w"> </span><span class="n">Monaco</span><span class="p">,</span><span class="w"> </span><span class="s1">&#39;Lucida Console&#39;</span><span class="p">,</span><span class="w"> </span><span class="kc">monospace</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">pre</span><span class="w"> </span><span class="nt">code</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">overflow</span><span class="p">:</span><span class="kc">auto</span><span class="w"> </span><span class="cp">!important</span><span class="p">;</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="mh">#000000</span><span class="p">;</span>
<span class="w">    </span><span class="k">font-family</span><span class="p">:</span><span class="w"> </span><span class="n">Monaco</span><span class="p">,</span><span class="w"> </span><span class="s1">&#39;Lucida Console&#39;</span><span class="p">,</span><span class="w"> </span><span class="kc">monospace</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">a</span><span class="p">:</span><span class="nd">link</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="mh">#993333</span><span class="p">;</span>
<span class="w">    </span><span class="k">font-weight</span><span class="p">:</span><span class="w"> </span><span class="kc">bold</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-decoration</span><span class="p">:</span><span class="w"> </span><span class="kc">underline</span><span class="p">;</span><span class="w"> </span>
<span class="p">}</span>
<span class="nt">a</span><span class="p">:</span><span class="nd">hover</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="mh">#FF6666</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-decoration</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">a</span><span class="p">:</span><span class="nd">visited</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">color</span><span class="p">:</span><span class="w"> </span><span class="mh">#0066CC</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-decoration</span><span class="p">:</span><span class="w"> </span><span class="kc">none</span><span class="p">;</span>
<span class="p">}</span>
<span class="c">/* metadata */</span>
<span class="nt">p</span><span class="p">.</span><span class="nc">author</span><span class="o">,</span><span class="w"> </span><span class="nt">p</span><span class="p">.</span><span class="nc">date</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">center</span><span class="p">;</span><span class="w"> </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="w"> </span><span class="kc">auto</span><span class="p">;}</span>
<span class="nt">blockquote</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="kt">px</span><span class="w"> </span><span class="cp">!important</span><span class="p">;</span>
<span class="w">    </span><span class="k">border-left</span><span class="p">:</span><span class="w"> </span><span class="mi">4</span><span class="kt">px</span><span class="w"> </span><span class="kc">solid</span><span class="w"> </span><span class="mh">#009A61</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">blockquote</span><span class="w"> </span><span class="nt">p</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">font-size</span><span class="p">:</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;</span>
<span class="w">    </span><span class="k">margin</span><span class="p">:</span><span class="w"> </span><span class="mi">0</span><span class="kt">px</span><span class="w"> </span><span class="cp">!important</span><span class="p">;</span>
<span class="w">    </span><span class="k">text-align</span><span class="p">:</span><span class="w"> </span><span class="kc">justify</span><span class="p">;</span>
<span class="w">    </span><span class="k">padding</span><span class="p">:</span><span class="mf">0.5</span><span class="kt">em</span><span class="p">;</span>
<span class="p">}</span>
<span class="c">/* 列表 */</span>
<span class="p">#</span><span class="nn">TOC</span><span class="w"> </span><span class="nt">ul</span><span class="o">,</span><span class="w">  </span><span class="p">#</span><span class="nn">TOC</span><span class="w"> </span><span class="nt">ol</span><span class="w"> </span><span class="p">{</span>
<span class="w">    </span><span class="k">padding</span><span class="p">:</span><span class="mi">0</span><span class="kt">em</span><span class="w"> </span><span class="mi">1</span><span class="kt">em</span><span class="p">;</span>
<span class="p">}</span>
<span class="nt">li</span><span class="w"> </span><span class="p">{</span><span class="k">padding-bottom</span><span class="p">:</span><span class="mf">0.25</span><span class="kt">em</span><span class="p">}</span>
<span class="c">/* 文章里小节标题的序号与标题名称之间的间距, 针对我写的生成文章目录的 m4 宏 */</span>
<span class="nt">span</span><span class="p">.</span><span class="nc">section-sep</span><span class="w"> </span><span class="p">{</span><span class="w"> </span><span class="k">margin-left</span><span class="p">:</span><span class="w"> </span><span class="mf">0.5</span><span class="kt">em</span><span class="p">;</span><span class="w"> </span><span class="k">margin-right</span><span class="p">:</span><span class="w"> </span><span class="mf">0.5</span><span class="kt">em</span><span class="p">;</span><span class="w"> </span><span class="p">}</span>
<span class="nt">div</span><span class="p">.</span><span class="nc">footer</span><span class="w"> </span><span class="p">{</span><span class="k">text-align</span><span class="p">:</span><span class="kc">center</span><span class="p">;}</span>
</pre>

# 参考

1. [获取脚本自身路径](https://segmentfault.com/a/1190000039423978)
2. [双引号的重要性](https://segmentfault.com/a/1190000039424194)
3. [Awk 小传](https://segmentfault.com/a/1190000016745490)
