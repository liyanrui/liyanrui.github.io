上一节定义的 `_N_incr_` 和 `_N_decr_` 宏虽然被 m4 内建宏 `pushdef` 和 `popdef` 降维打击了，但它们是由启发性的，我们有能力定义更为通用的操作计数器的宏。m4 的 `incr` 和 `decr` 只能操作数字，它无法将运算结果保存起来备用。如果，定义 `_incr_` 宏，其调用语句形如

```m4
_incr(`n')
```

其展开结果形如

```
如果 _counter_n_ 未定义，则 define(`_counter_n_', 1)，
否则 define(`_counter_n_', incr(_counter_n_))
```

m4 语言提供了条件表达式。内建宏 `ifdef` 可用于检测一个宏是否有定义，例如

```m4
ifdef(`_counter_n_', `a', `b')
```

其意为，若 `_counter_n_` 宏有定义，则结果为 `a`，否则为 `b`，由于我们并未定义 `_counter_n_` 宏，故而该表达式的展开结果为 `b`。若在条件表达式之前给出定义 `_counter_n_`，

```m4
define(`_counter_n_', 0)dnl
ifdef(`_counter_n_', `a', `b')
```

则展开结果为 `a`。

基于 `ifdef` 宏，上述我们假想的 `_incr_` 宏的展开结果可表示为

```m4
ifdef(`_counter_n_', 
      `define(`_counter_n_', incr(_counter_n_))',
      `define(`_counter_n_', 1)')
```

由于 `n` 是 `_incr_` 宏的参数，故而上述代码可改写为

```m4
ifdef(`_counter_$1_', 
      `define(`_counter_$1_', incr(_counter_$1_))',
      `define(`_counter_$1_', 1)')
```

用一层引号包含上述代码，便可作为 `_incr` 的宏体了，

```m4
define(`_incr',
       `ifdef(`_counter_$1_', 
             `define(`_counter_$1_', incr(_counter_$1_))',
             `define(`_counter_$1_', 1)')')
```

以下代码

```
_incr(`n') _counter_n_
_incr(`n') _counter_n_
_incr(`n') _counter_n_
```

展开结果为

```
 1
 2
 3
```

方便起见，再定义一个专用于取计数器值的宏，

```m4
define(`_get', _counter_$1_)
```

则上述示例可修改为

```
_incr(`n') _get(`n')
_incr(`n') _get(`n')
_incr(`n') _get(`n')
```

展开结果不变。同理，可以定义 `_decr` 宏，

```m4
define(`_decr',
       `ifdef(`_counter_$1_', 
             `define(`_counter_$1_', decr(_counter_$1_))',
             `define(`_counter_$1_', 0)')')
```

综合应用 `_incr`，`_get` 以及 `_decr`，

```
_incr(`n') _get(`n')
_incr(`n') _get(`n')
_decr(`n') _get(`n')
_incr(`n') _get(`n')
```

展开结果为

```
 1
 2
 1
 2
```

现在，我们可以尝试定义一个更通用的计数器操作宏 `_op`，期望 `_op(`n', `++')` 的展开结果为

```m4
若 $2 是 `++' 则 _incr(`$1'); 若 $2 是 `--' 则 _decr(`$1')
```

m4 提供了内建宏 `ifelse`，基于它可以表示上述结果，

```m4
ifelse(`$2', `++', `_incr(`$1')', `ifelse(`$2', `--', `_decr(`$1')')')
```

上述语句单从逻辑上，可略写为

```m4
ifelse(`$2', `++`, `a', `b')
```

意思是，若 `$2` 是 `++`，则结果为 `a`，亦即 `$1` 增 1，否则结果为 `b`，亦即再度判断 `$2` 是否为 `--` 并做相应处理。

若将 `ifelse` 语句写成

```m4
ifelse(a, b, c)
```

意思是，若 `a` 和 `b` 相同，则结果为 `c`，否则结果为空。

若用引号包含上述最为复杂的那条 `ifelse` 语句，便可将其作为 `_op` 的宏体，

```m4
define(`_op', `ifelse(`$2', `++', 
                      `_incr(`$1')', 
                      `ifelse(`$2', `--', 
                              `_decr(`$1')')')')
```

验证 `_op` 是否可用，

```
_op(`n', `++') _get(`n')
_op(`n', `++') _get(`n')
_op(`n', `--') _get(`n')
_op(`n', `--') _get(`n')
```

结果应当为

```
 1
 2
 1
 0
```

将上述宏定义汇总起来，

```m4
divert(-1)
define(`_incr',
       `ifdef(`_counter_$1_', 
             `define(`_counter_$1_', incr(_counter_$1_))',
             `define(`_counter_$1_', 1)')')
define(`_decr',
       `ifdef(`_counter_$1_', 
             `define(`_counter_$1_', decr(_counter_$1_))',
             `define(`_counter_$1_', 0)')')
define(`_op', `ifelse(`$2', `++', 
                      `_incr(`$1')', 
                      `ifelse(`$2', `--', 
                              `_decr(`$1')')')')
define(`_get', _counter_$1_)
divert(0)dnl
```

你对 m4 语言的理解和应用，已是小有气象了。
