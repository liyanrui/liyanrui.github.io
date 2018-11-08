<!--
.. title: TeX 编程
.. slug: Tex-Programming
.. date: 2018-11-08 10:40:06 UTC+08:00
.. tags: TeX
.. category: 排版
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

变量即无参数的宏。可通过 `\def` 构造一个变量并赋值，例如

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

在 TeX 的世界里，它可以永动，然而它却什么都不能做了。

# 条件

永动机虽然造不出来，让一个函数自身与其所吸收的量进行组合，这种形式可以产生循环形式的动力。在现实世界，利用这种动力所取得的上天入地效果，我们都有所见识。在 TeX 世界里也能如此，否则就不会有 LaTeX 和 ConTeX 的出现。但是，要利用这种动力，就需要通过一些开关对其进行控制，否则这种动力便会摧毁整个 TeX 世界。

最简单的开关是控制循环的次数，即控制一个函数自身与其所吸收的量进行组合的次数。这需要使用 TeX 的计数器。使用 `\newcount` 可以获得一个计数器，例如

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
\newcount\mycount
\def\foobar#1{#1
  \ifnum\number\mycount=10
  \else\advance\mycount by 1\foobar{#1}
  \fi
}

\mycount=0
\foobar{Hello world!}
```

可将「`Hello world!`」连续输出 10 次。

在 TeX 中，`\newcount` 构造的计数器称为计数寄存器。但是从编程的角度来看，所谓技术寄存器，不过是整型变量罢了。不过，TeX 最多只能同时支持 256 个这样的整型变量，`\count0`，`\count1`，……，`\count255`。通过 `\count` + 数字可以直接使用这些变量，但是很容易引起混乱。`\newcount` 可从这些变量中选出当前尚未启用的一个，并将我们指定的名字赋予它，例如上述的 `\mycount`。显然，使用 `\newcount` 这种迂回的方式会更舒适一些。
