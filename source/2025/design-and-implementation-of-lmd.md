---
title: lmd 的设计与实现
lang: zh-CN
date: 2025 年 01 月 13 日
abstract: 
category: 
footer: 我的联系方式：<lyr.m2@live.cn> 或在
        [讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

# 前言

我曾经用过两三个静态网站构建工具，用的最久的，是 [nikola](https://www.getnikola.com/getting-started.html)，它很优秀，无论是安装，还是使用，都比较简单。不过，我难以回答之前为什么要弃用它。大概是有一次对它进行升级，下载它依赖的一些 Python 包时出现了我难以解决的问题，导致升级失败，这件事让我不太舒服，便弃用了。刚才我又试了一次，即使按照官方文档提醒的，在 Ubuntu 或其衍生版本中，应使用 Python 的包管理器 pip 进行安装，然而在我的 Linux Mint 22 环境中依然在安装过程中出错。

大概是在 2020 年，我在学习 Bash 语言，想写个脚本练手，恰好我又略懂号称[文档格式转换界的瑞士军刀——pandoc](https://pandoc.org/) 的用法，于是基于 Bash 和 pandoc 写了个脚本，取名为 gar，它能够借助 pandoc 将 Markdown 文档转化为网页，并且提供了文章基本管理功能。gar 脚本虽然简陋，但是它让我意识到，实现一个静态网站构建工具，并不需要流行的静态网站构建工具所表现出的那般复杂。当然，它们的复杂有它们的理由，只是那些理由不是我的。

2023 年，我在又一次温习 Bash 和 Awk 时，对 gar 脚本进行了改进，基于 Awk 脚本为其实现了a文档目录管理的功能，并将 gar 更名为 lmd，并写了一份文档，介绍了[它的用法](https://liyanrui.github.io/output/bash/lmd.html)。

今年，我在为 [orez 的新版本](https://liyanrui.github.io/output/2025/orez-v1.html)构造 Markdown 后端时，尝试将它与 lmd 脚本配合起来，以静态网站的形式发布文学程序的文档，详见「[orez + lmd = ?](https://liyanrui.github.io/output/2025/orez-lmd.html)」一文。在此过程中，我发现 lmd 脚本外围的部分 Awk 代码与 Ubuntu 系的发行版默认使用的 mawk 不兼容，在修改这部分 Awk 代码时，发现我对 lmd 脚本里的代码也感觉非常陌生。于是，我决定重新实现 lmd，以文学编程的方式，除了作为文学编程以及 orez 的 Markdown 后端是否可用的一次实践，也用于防备很久以后再次阅读 lmd 脚本里的代码再度觉得陌生。

# 从 Markdown 到 HTML

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

上述的 pandoc 命令是下文要实现的 lmd 脚本的核心，亦即即使不借助 lmd 脚本，单纯使用 pandoc 命令也能构建静态网站，只是过程会较为繁琐。

# 网站初始化

lmd 要构建的网站，其目录结构和必须文件如下：

```
根目录
├── appearance
│   ├── lmd.css
│   └── pandoc
│       └── data
│           └── templates
│               ├── homepage.template
│               └── post.template
├── index.md
├── lmd.conf
└── posts
```

上述结构中，appearence 用于定制网站页面的外观，所包含的文件皆在上一节出现过，它们的用途在此不再赘述。index.md 文件用于构造网站首页内容。lmd.conf 中存储的是用户的一些信息。posts 目录是文章目录。

lmd 脚本从如何初始化上述的网站结构开始。首先需要考虑一个问题，诸如 lmd.css，homepage.template 以及 post.template 这些文件，它们应该事先存在于某处，在 lmd 脚本初始化网站结构时，将它们复制过去，那么它们应该存放在何处呢？我的选择是，让它们与 lmd 脚本位于同一目录，但是 lmd 脚本如何知道自己的位置呢？

根据文档 [1] 的探讨，确定 lmd 脚本所在路径的 Bash 代码如下：

<pre id="确定脚本自身所在" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 确定脚本自身所在 #</span>
<span class="nv">LMD_SELF_PATH</span><span class="o">=</span><span class="s2">&quot;</span><span class="k">$(</span><span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="k">$(</span>dirname<span class="w"> </span><span class="s2">&quot;</span><span class="si">${</span><span class="nv">BASH_SOURCE</span><span class="p">[0]</span><span class="si">}</span><span class="s2">&quot;</span><span class="k">)</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">&amp;&amp;</span><span class="w"> </span><span class="nb">pwd</span><span class="k">)</span><span class="s2">&quot;</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-test-01.sh" class="proc-emissions-name">lmd-test-01.sh</a>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-init-test.sh" class="proc-emissions-name">lmd-init-test.sh</a>
</pre>

以下脚本可用于验证上述代码是否可用：

<pre id="lmd-test-01.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-test-01.sh #</span>
<span class="ch">#!/usr/bin/env bash</span>
<a href="#确定脚本自身所在" class="orez-callee-link"># 确定脚本自身所在 @</a>
<span class="nb">echo</span><span class="w"> </span><span class="nv">$LMD_SELF_PATH</span>
</pre>

假设上述脚本位于 /tmp 目录，执行它，会输出 `/tmp`：

```console
$ cd /tmp
$ bash lmd-test-01.sh
/tmp
```

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
└── lmd 脚本
```

基于获取脚本自身目录的代码以及上述 lmd 脚本所在目录的结构，便可写出网站初始化过程：

<pre id="网站初始化过程雏形" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 网站初始化过程雏形 #</span>
<span class="k">function</span><span class="w"> </span>lmd_init<span class="w"> </span><span class="o">{</span>
    <a href="#参数检验" class="orez-callee-link"># 参数检验 @</a>
    <a href="#创建网站目录初始结构" class="orez-callee-link"># 创建网站目录初始结构 @</a>
<span class="o">}</span>
<span class="orez-symbol">=&gt;</span> <a href="#lmd-init-test.sh" class="proc-emissions-name">lmd-init-test.sh</a>
</pre>

`lmd_init` 函数接受 2 个参数，第一个参数，即 `$1` 为网站的名字；第二个参数，即 `$2` 为网站目录。以下代码片段用于检测 `$1` 和 `$2` 是否为空，以及 `$2` 是否已经存在：

<pre id="参数检验" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 参数检验 #</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$1</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span><span class="w"> </span><span class="o">||</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-z<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd_init failed.&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;You should provide me \&quot;site name\&quot; and site-directory.&quot;</span>
<span class="w">    </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="k">fi</span>
<span class="k">if</span><span class="w"> </span><span class="o">[</span><span class="w"> </span>-d<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span><span class="w"> </span><span class="o">]</span>
<span class="k">then</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;lmd_init failed.&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;The directory as work-directory already exists.&quot;</span>
<span class="w">    </span><span class="nb">echo</span><span class="w"> </span><span class="s2">&quot;Please use a different work-directory name.&quot;</span>
<span class="w">    </span><span class="nb">exit</span><span class="w"> </span>-1
<span class="k">fi</span>
<span class="orez-symbol">=&gt;</span> <a href="#网站初始化过程雏形" class="proc-emissions-name">网站初始化过程雏形</a>
</pre>

网站目录结构的初始化过程只需将 lmd 脚本所在目录里的 appearance 目录复制到网站根目录下即可：

<pre id="创建网站目录初始结构" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ 创建网站目录初始结构 #</span>
mkdir<span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
<span class="nb">cd</span><span class="w"> </span><span class="s2">&quot;</span><span class="nv">$2</span><span class="s2">&quot;</span>
mkdir<span class="w"> </span>appearance
cp<span class="w"> </span>-r<span class="w"> </span><span class="nv">$LMD_SELF_PATH</span>/data/appearance<span class="w"> </span>./
<span class="orez-symbol">=&gt;</span> <a href="#网站初始化过程雏形" class="proc-emissions-name">网站初始化过程雏形</a>
</pre>

以下代码可用于验证或演示 `lmd_init` 函数的用法：

<pre id="lmd-init-test.sh" class="orez-snippet-with-name">
<span class="orez-snippet-name">@ lmd-init-test.sh #</span>
<span class="ch">#!/usr/bin/env bash</span>
<a href="#确定脚本自身所在" class="orez-callee-link"># 确定脚本自身所在 @</a>
<a href="#网站初始化过程雏形" class="orez-callee-link"># 网站初始化过程雏形 @</a>
lmd_init<span class="w"> </span><span class="nv">$1</span><span class="w"> </span><span class="nv">$2</span>
</pre>

假设 lmd-init-test.sh 位于 /tmp/lmd 目录，且伴有上文所述 data/appearance 目录，以下命令可在 /tmp 目录构建 demo 目录：

```console
$ cd /tmp
$ bash lmd/lmd-init-test.sh "Site demo" demo
```

上述命令生成的 demo 目录里，仅存在 appearance 目录，亦即命令的 "Site demo" 参数并未用到，原因是网站的名字是为网站首页准备的，下一节探讨如何初始化网站首页内容。

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

1. [Bash Tips 01：获取脚本自身路径](https://segmentfault.com/a/1190000039423978)
