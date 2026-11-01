有时，会遇到一些结构化文本。例如以下 ConTeXt 排版代码

```
\starttext
... ... ...
\startitemize
\item ... ... ...
\item ... ... ...
\stopitemize
... ... ...
\stoptext
```

如果你懒得在 `\stop` 语句再输入一次 `text` 和 `itemize`，也许会期望将上述内容简写为

```
_start(text)
... ... ...
_start(itemize)
\item ... ... ...
\item ... ... ...
_stop_
... ... ...
_stop_
```

若要达成该期望，只需将 `_start` 和 `_stop_` 定义为 m4 宏，由 m4 将上述文本展开为 ConTeXt 排版代码。

> 注意，`_start` 宏需要有参数，为了与无参数的宏有所区分，我故意令其右侧不出现下划线。

`_start` 宏很好定义，

```m4
define(`_start', `\start$1')
```

可是 `_stop_` 就很难了，因为它没有参数，`\stop` 该与什么衔接呢？

若在 `_start` 的定义中再定义一个宏，将 `_start` 的参数保存下来，然后在 `_stop_` 的定义中使用，

```m4
divert(-1)
define(`_start', `\start$1`'define(`_cmd_', `$1')')
define(`_stop_', `\stop`'_cmd_')
divert(0)dnl
```

的确能将

```
_start(text)
... ... ...
_stop_
```

展开为

```
\starttext
... ... ...
\stoptext
```

可是，若遇到以下文本，

```
_start(text)
_start(itemize)
_stop_
_stop_
```

结果则是

```
\starttext
\startitemize
\stopitemize
\stopitemize
```

原因是，`_start(text)` 定义的 `_cmd_`，在调用 `_start(itemize)` 时被覆盖了，导致 `_cmd_` 的展开结果是 `itemize`，而不再是 `text`。

由于你已经掌握了计数器宏的写法，也许你会想出下面这个巧妙的方案，

```m4
divert(-1)
define(`_N_', 0)
define(`_N_incr_', `define(`_N_', incr(_N_))')
define(`_N_decr_', `define(`_N_', decr(_N_))')
define(`_start', `\start$1`'_N_incr_`'define(`_cmd'_N_, `$1')')
define(`_stop_', `\stop`'indir(_cmd`'_N_)`'_N_decr_')
divert(0)dnl
```

这个巧妙的方案是可用的，不过也许你需要经过多次失败后，方能将其写得正确，而且一旦写出来，你就会发现它已经像天书了，过些时日也许你自己都看不懂了。

前文曾说过，宏不是设计出来的，而是基于结果倒推出来的。以上述最为复杂的 `_start` 宏为例，先不要去想如何去定义它，而是先写出自己想要什么。例如，我想要的是

```
\starttext`'计数器递增`'define(`_cmd'计数器当前的值, `text')
```

或

```
\startitemize`'计数器递增`'define(`_cmd'计数器当前的值, `itemize')
```

将可变部分表示为宏的参数，将计数器递增表示为 `_N_incr_`，将计数器的值表示为 `_N_`，于是可得

```
\start$1`'_N_incr_`'define(`_cmd'_N_, `$1')
```

将上述内容添加一层引号，便可作为 `_start` 宏体，

```
define(`_start', `\start$1`'_N_incr_`'define(`_cmd'_N_, `$1')')
```

这种倒推过程很容易理解，只是倒推的结果容易令人晕眩，而且甚至会产生误解，认为需要有一些超乎寻常的能力方能写出如此晦涩的代码。实际上，你需要掌握的只是这种倒推过程，无论何时都没有必要去理解倒推结果！即使以后你需要修改宏的定义，实际上你要修改的是上述倒推过程。

理解不了上述过程，也无妨，它不过是以一种简单粗暴的方式实现了一种脆弱的栈结构罢了。m4 为宏定义提供了栈空间，可以临时用 `pushdef` 将宏定义压入栈中以备调用，用毕之后，可使用 `popdef` 将其从栈中弹出。例如

```m4
\starttext`'pushdef(`_block_name_', `text')
\startitemize`'pushdef(`_block_name_', `itemize')
\stop`'_block_name_`'popdef(`_block_name_')
\stop`'_block_name_`'popdef(`_block_name_')
```

展开结果为

```
\starttext
\startitemize
\stopitemize
\stoptext
```

用上述倒推的方式很容易写出更好的 `_start` 和 `_stop_` 宏，

```m4
divert(-1)
define(`_start', `\start$1`'pushdef(`_block_name_', `$1')')
define(`_stop_', `\stop`'_block_name_`'popdef(`_block_name_')')
divert(0)dnl
```

若你能正确写出 `_start` 和 `_stop_` 这样的宏，意味着在 m4 语言的世界里，你已经可与写出本节内容时的我比肩而立了。

