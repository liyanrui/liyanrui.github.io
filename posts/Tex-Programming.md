<!--
.. title: TeX 编程
.. slug: Tex-Programming
.. date: 2018-11-08 10:40:06 UTC+08:00
.. tags: 
.. category: 
.. link: 
.. description: 
.. type: text
-->

TeX 是一种面向文档排版的编程语言，适用于处理科技文献的排版任务。本文几乎不关心 TeX 的排版功能，只是从一门完备的编程语言应当具备的要素的角度去认识它。

# 常量

最基本的常量是单个字符，TeX 解释器会照实对其予以解释。例如

```TeX
Hello world!
```

TeX 解释器逐字照实输出。输出到何处？现代的 TeX 解释器，诸如 pdftex、xetex、luatex 等，会将其输出至 PDF 格式的文档。

常量之间只有一种运算，即连接。我们在输入文本时，便已经实施了该运算——将一组字符连成文本。若将一组字符合成为一个常量，可以用 `{...}`，例如

```TeX
{Hello world!}
```

# 变量

变量可通过无参数的宏予以构造，例如通过 `\def` 构造一个变量并赋值：

```TeX
\def\myvar{Hello world!}
```

`\myvar` 便是一个变量，它的值为 `Hello world!`。在 TeX 中，变量只有一种类型，即文本类型。

# 函数

函数即有参数的宏。函数与变量并没有本质上的区别。所以，在数学中，变量也会被称为常函数。

函数可以吸收常量或变量，将它们与其他常量或变量进行组合。例如，

```TeX
\def\myfunc#1{#1 world!}
```

若 `\myfunc` 吸收了常量 `Hello`，

```TeX
\myfunc{Hello}
```

之后，就会将 `Hello` 与 `world!` 连接为 `Hello world!`。`{...}` 可将一组字符常量合并为一个常量。在上例中，若不用 `{...}`，而是直接

```TeX
\myfunc Hello
```

结果得到的是「`H world!ello`」，因此 `\myfunc` 此时只吸收常量 `H`，剩下的 `ello` 只能等待与 `\myfunc` 的结果连接。

若将 `Hello` 作为值赋予一个变量，`\myfunc` 也能吸收这个变量：

```TeX
\def\hello{Hello}
\myfunc\hello
```

由于函数与变量并没有本质区别，所以函数也能吸收函数，例如：

```TeX
\def\foobar#1{#1 {Hello}}
\foobar\myfunc
```

结果为「`Hello world!`」。

若一个函数将吸收到的量与这个函数自身进行组合，结果会导致 TeX 解释器陷入到不停地解释这个函数的过程，直至崩溃。例如

```TeX
\def\foobar#1{#1\myfunc{#1}}
\foobar{Hello world!}
```

在现实世界，类似这种形式的机器叫永动机。与 TeX 世界一样，现实世界也造不出永动机。换言之，若现实世界能造出永动机，那么在 TeX 世界一定也能。

若 `\foobar` 不吸收任何量，也不与任何量组合，即

```TeX
\def\foobar{\foobar}
\foobar
```

在 TeX 的世界里，它可以永动，然而它却什么都不能做了。像 `\foobar` 这样的宏，在 TeX 中称为递归宏。

# 寄存器和条件

永动机虽然造不出来，让一个函数自身与其所吸收的量进行组合，这种形式可以产生循环形式的动力。在现实世界，利用这种动力所取得的上天入地效果，我们都有所见识。在 TeX 世界里也能如此，否则就不会有 LaTeX 和 ConTeX 的出现。但是，要利用这种动力，就需要通过一些开关对其进行控制，否则这种动力便会摧毁整个 TeX 世界。

最简单的开关是控制循环的次数，即控制一个函数自身与其所吸收的量进行组合的次数。这需要使用 TeX 的计数器。使用 `\newcount` 可以向 TeX 申请一个计数寄存器作为计数器，例如

```TeX
\newcount\mycount
```

若让这个计数器从 0 开始，只需

```TeX
\mycount=0
```

若要控制函数自身与其所吸收的量进行组合的次数不大于 10 次，只需在该过程中增加控制语句

```TeX
\ifnum\mycount=10
\else 函数自身与其所吸收的量的组合\advance\mycount by 1
\fi
```

例如

```TeX
\def\foobar#1{
  \ifnum\number\mycount=10
  \else #1\advance\mycount by 1\foobar{#1}
  \fi
}

\newcount\mycount
\mycount=0
\foobar{Hello world!}
```

可将 `Hello world!` 分段输出十次。

`\newcount` 的作用是分配一个未使用的计数寄存器，并赋予它一个名字。通过这个名字便可以使用这个计数寄存器中存储的数值。Knuth 的 TeX 最多支持 256 个计数寄存器，现代的 TeX 对此进行了扩展，例如 LuaTeX 可支持 65536 个。可直接以数字为后缀的 `\count` 使用计数寄存器，例如

```TeX
\def\foobar#1{
  \ifnum\number\count65534=10
  \else #1\par\advance\count65535 by 1\foobar{#1}
  \fi
}

\count65535=0
\foobar{Hello world!}
```

但是这样做，很容易引起混乱。例如，倘若某种 TeX 格式将 `\count65535` 用于存储某个重要的排版数据，这里使用了这个寄存器，那么这个寄存器中原有的值就会被覆盖，可能会导致排版结果出现难以预测的结果。因此，通常推荐使用 `\newcount` 申请一个尚未被使用的寄存器。这里需要纠正一下前文中的一个说法，TeX 的变量的类型只有，即文本类型。通过 `\newcount` 构造的计数寄存器本质上是整数类型的变量。

`\advance` 用于整型变量的加减运算，例如对一个整型变量加 10,再减 30，再增加 1 倍：

```TeX
\newcount\abc
\abc=0
\advance\abc by 10
\advance\abc by -30
\advance\abc by\abc
\the\abc
```

结果为 -40。`\the` 用于攫取整型变量的值。

事实上，TeX 变量的类型还有更多。除了计数寄存器，还有盒子（box）寄存器、维度（dimen）寄存器、skip 寄存器、musikip 寄存器以及 toks 寄存器，这些变量的值皆能用 `\the` 获取。

`\ifnum` 用于比较两个数值的关系，即大于、小于和等于。类似的条件语句还有

* `\iftrue` 永远为真，`\iffalse` 永远为假；
* `\if`：测试两个字符是否相同；
* `\ifx`：测试两个记号（Token）是否相同；
* `\ifcat`：测试两个记号的类别码是否相同；
* `\ifdim`：比较两个尺寸的关系；
* `\ifodd`：测试一个数值是否为奇数；
* ……更多的，见《The TeXbook》第 20 章 ……

这些条件语句，待需要使用它们之时再作细究。

# 尾递归

利用递归函数可以制作通用的循环语句，例如若制作类似于 TeX 的 `\loop ...\repeat` 的结构，只需

```TeX
\def\myloop#1\repeat{\def\body{#1}\myiterate}
\def\myiterate{\body\myiterate\else\relax\fi}
```

`\relax` 是个什么都不做的控制序列，将其删除，对 `\myloop` 毫无影响，但是使用它可以让 `\myiterate` 的定义更清晰。

现在，用 `\myloop ...\repeat` 结构将 `Hello world!` 输出 10 次：

```TeX
\newcount\mycount
\mycount=0
\myloop Hello world!\advance\mycount by 1\ifnum\mycount<9\repeat
```

现在来看 `\myiterate` 的定义……


