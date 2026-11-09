---
title: 基于 m4 管理 C 对象的生命域
subtitle: 
date: 2026 年 11 月 07 日
...

# 前言

在 C++ 和 Rust 这样的语言里，一个对象在退出其所属作用域时，其占用的资源通常可被自动释放。C 语言没有这样的机制，对象的资源无论是分配还是释放，皆需手工完成。不过，基于 m4 语言，可以在 C 语言的外围，为对象构造生命域，当对象退出其所属生命域时，便自动释放其资源。

> 不知 m4 者，可阅读​[《m4 的咒语》](https://zhuanlan.zhihu.com/p/2099184764834025671)；
> 对 m4 有所了解但缺乏实践经验者，可阅读​[《基于 m4 实现 C 泛型层》](https://zhuanlan.zhihu.com/p/2100949381667205608)；
> 有 m4 实践经验者，若想有所精进，可阅读​[《GNU m4 技法集录》](https://zhuanlan.zhihu.com/p/2102122424175271972)以及本文。

# 生命域标记

C 语言的每个作用域可以包含 1 个或多个生命域。每个生命域都要维护一个表达生命域深度的宏变量，该变量为

```m4
define(`_life_n_', 0)
```

生命域的开始标记是 `_life_start_`，其定义为，

```m4
define(`_life_start_', `define(`_life_n_', incr(_life_n_))')
```

生命域的结束标记是 `_life_stop_`，其定义为

```m4
define(`_life_stop_', `define(`_life_n_', decr(_life_n_))')
```

以下文本可用于验证生命域开始和结束标记能否维护生命域深度，

```c
if (1) {
        // _life_start_
        _life_n_
        if (1) {
                // _life_start_
                _life_n_
                // _life_stop_
        }
        _life_n_
        // _life_stop_
}
```

展开结果为

```
if (1) {
        // 
        1
        if (1) {
                // 
                2
                // 
        }
        1
        // 
}
```

# 内存分配与释放

需要进入生命域的对象，其内存必须由 `_malloc` 宏分配。`_malloc` 的定义为

```m4
define(`_malloc', `$2 $1 = malloc($3)pushdef(`_free_'_life_n_, `free($1);')')
```

以下代码

```c
// _life_start_
_malloc(a, int *, sizeof(int));
_malloc(b, double *, sizeof(double));
indir(`_free_'_life_n_)popdef(`_free_'_life_n_)
indir(`_free_'_life_n_)popdef(`_free_'_life_n_)
// _life_stop_
```

上述代码的展开结果为

```c
// 
int * a = malloc(sizeof(int));
double * b = malloc(sizeof(double));
free(b);
free(a);
// 
```

由于生命域中每个对象的 `free` 语句已经存入了宏定义栈，释放生命域中每个对象的方法，调出位于栈顶的 `free` 语句，然后将其宿主宏从栈中弹出，该过程由 `_exec_free_` 宏完成，

```m4
define(`_exec_free_', `indir(`_free_'_life_n_)popdef(`_free_'_life_n_)')
```

`_free` 宏递归运行，每次递归皆会调用 `_exec_free_` 输出一条 `free` 语句，直至宏定义栈为空，

```m4
define(`_free_', `ifdef(`_free_'_life_n_, `_exec_free_$0')')
```

以下代码构造了两层生命域，用于验证 `_free_` 宏，

```c
// _life_start_
_malloc(a, int *, sizeof(int));
_malloc(b, double *, sizeof(double));
if (1) {
        // _life_start_
        _malloc(c, int *, sizeof(int));
        _malloc(d, double *, sizeof(double));
        _free_
        // _life_stop_
}
_free_
// _life_stop_
```

上述代码的展开结果为

```c
// 
int * a = malloc(sizeof(int));
double * b = malloc(sizeof(double));
if (1) {
        // 
        int * c = malloc(sizeof(int));
        double * d = malloc(sizeof(double));
        free(d);free(c);
        // 
}
free(b);free(a);
// 
```

你会发现除了 `free` 语句不能换行，并无其他问题。让 `free` 换行并不困难，只需将 `_free_` 定义为

```m4
define(`_free_', `ifdef(`_free_'_life_n_, `_exec_free_
$0')')
```

只是增加换行后，第一条 `free` 语句之前的缩进在后续 `free` 语句里不会出现，因为 `_free_` 宏并不知道它前面的缩进多长。不过，基于生命域的深度值能够构造缩进，但前提是需要固定每一层生命域的缩进长度。这只是一个想法，我无意实现，因为这些 `free` 语句位于同一行，对于 C 语言而言是合法的。

`_free_` 不需要我们手工调用，可以将其植入 `_life_stop_` 宏，由后者调用，

```m4
define(`_life_stop_', `_free_`'define(`_life_n_', decr(_life_n_))')
```

只是 `_life_stop_` 不能再像前面的示例里那样放在注释里了，例如

```c
// _life_start_
_malloc(a, int *, sizeof(int));
_malloc(b, double *, sizeof(double));
if (1) {
        // _life_start_
        _malloc(c, int *, sizeof(int));
        _malloc(d, double *, sizeof(double));
        _life_stop_ //
}
_life_stop_ //
```

展开结果为

```c
int * a = malloc(sizeof(int));
double * b = malloc(sizeof(double));
if (1) {
        // 
        int * c = malloc(sizeof(int));
        double * d = malloc(sizeof(double));
        free(d);free(c); //
}
free(b);free(a); //
```

不过，从形式上看，`// _life_start_` 和 `_life_stop_ //` 也能构成一种对称结构。

# 自动构造生命域

若是写一个程序，将 C 代码里表达作用域的花括号全部替换为 `_life_start_` 和 `_life_stop_`，这意味着我们拥有了一个隐形的 C 内存管理层。这样的脚本，之前我已经让 DeepSeek 写出了，见[《基于 m4 实现 C 泛型层》](https://zhuanlan.zhihu.com/p/2100949381667205608)的「跟踪作用域」一节，对该节 trace-scopes.awk 脚本只需稍加改动，

```awk
# trace-scopes.awk
# 用法：awk -f trace-scopes.awk foo-meta.c > foo-meta-scoped.c
#
# 功能：
#   1. 将 C 代码中的作用域花括号 { } 替换为 _life_start_ / _life_stop_
#   2. 跳过字符串、字符字面量、行注释、块注释中的花括号
#   3. 跳过预处理指令（# 开头，含 \ 续行）中的花括号
#   4. 处理行尾反斜杠续行（行注释、字符串、字符字面量）
#   5. 对 _malloc 的托管变量名做跨作用域重名检查，警告输出到 stderr
#
# 约定：
#   - _malloc 的调用不跨行
#   - _obj_life 的变量名重名不做检查，靠约定
#   - 字符串/字符字面量跨行仅通过行尾 \ 续行实现

BEGIN {
    state       = "normal"   # normal | string | char | line_comment | block_comment
    in_pp       = 0          # 是否在预处理指令中
    pp_cont     = 0          # 预处理指令是否续行
    scope_depth = 0          # 与 m4 的 _life_n_ 同步
}

{
    line = $0
    n    = length(line)
    cont = (n > 0 && substr(line, n, 1) == "\\")

    # 预处理行检测：仅在 normal 状态、非续行中、行首（去空白）为 # 时进入
    if (state == "normal" && !pp_cont) {
        tmp = line
        sub(/^[ \t]+/, "", tmp)
        if (substr(tmp, 1, 1) == "#") in_pp = 1
    }

    if (in_pp) {
        print line
        if (cont) {
            pp_cont = 1
        } else {
            in_pp   = 0
            pp_cont = 0
        }
        next
    }

    out = ""
    for (i = 1; i <= n; i++) {
        c  = substr(line, i, 1)
        c2 = substr(line, i + 1, 1)
        if (state == "normal") {
            if (c == "/" && c2 == "/") {
                out = out c c2; i++; state = "line_comment"
            } else if (c == "/" && c2 == "*") {
                out = out c c2; i++; state = "block_comment"
            } else if (c == "\"") {
                out = out c; state = "string"
            } else if (c == "'") {
                out = out c; state = "char"
            } else if (c == "{") {
                out = out "_life_start_"
                scope_depth++
            } else if (c == "}") {
                out = out "_life_stop_"
                # 清理当前作用域的托管变量记录
                for (k in managed) {
                    split(k, parts, SUBSEP)
                    if (parts[1] + 0 == scope_depth) delete managed[k]
                }
                scope_depth--
            } else {
                # 检测 _malloc 变量名，用于重名检查
                prev = (i > 1) ? substr(line, i - 1, 1) : " "
                if (prev !~ /[A-Za-z0-9_]/ && c == "_" && substr(line, i, 7) == "_malloc") {
                    j = i + 8
                    while (j <= n && (substr(line, j, 1) == " " || substr(line, j, 1) == "\t")) j++
                    k = j
                    while (k <= n && substr(line, k, 1) ~ /[A-Za-z0-9_]/) k++
                    name = substr(line, j, k - j)
                    if (name != "") {
                        for (d = 1; d <= scope_depth; d++) {
                            if ((d, name) in managed) {
                                printf "warning: managed variable \"%s\" at depth %d shadows depth %d\n", \
                                       name, scope_depth, d > "/dev/stderr"
                                break
                            }
                        }
                        managed[scope_depth, name] = 1
                    }
                }
                out = out c
            }
        } else if (state == "string" || state == "char") {
            out = out c
            if (c == "\\") {
                i++; out = out substr(line, i, 1)
            } else if (state == "string" && c == "\"") {
                state = "normal"
            } else if (state == "char" && c == "'") {
                state = "normal"
            }
        } else if (state == "line_comment") {
            out = out c
        } else if (state == "block_comment") {
            out = out c
            if (c == "*" && c2 == "/") {
                out = out c2; i++; state = "normal"
            }
        }
    }
    print out

    # 行注释在行尾结束，除非有 \ 续行
    if (state == "line_comment" && !cont) state = "normal"
}
```

便可用于将 C 的作用域替换为生命域。例如，假设 foo.c 的内容为

```c
int main(void) {
        _malloc(a, int *, sizeof(int));
        _malloc(b, double *, sizeof(double));
        if (1) {
                _malloc(c, int *, sizeof(int));
                _malloc(d, double *, sizeof(double));
        }
        return 0;
}
```

可见 foo.c 中并无生命域标记。使用 trace-scopes.awk 处理 foo.c，

```console
$ awk -f trace-scopes.awk foo.c
```

结果为

```c
int main(void) _life_start_
        _malloc(a, int *, sizeof(int));
        _malloc(b, double *, sizeof(double));
        if (1) _life_start_
                _malloc(c, int *, sizeof(int));
                _malloc(d, double *, sizeof(double));
        _life_stop_
        return 0;
_life_stop_

```

只需重新定义 `_life_start_` 和 `_life_stop_`，

```m4
define(`_life_start_', `define(`_life_n_', incr(_life_n_)){')
define(`_life_stop_', `_free_}define(`_life_n_', decr(_life_n_))')
```

亦即令生命域标记的展开结果含有花括号，用于还原 C 的作用域。于是，上述 awk 脚本的处理结果最终会被展开为

```c
int main(void) {
        int * a = malloc(sizeof(int));
        double * b = malloc(sizeof(double));
        if (1) {
                int * c = malloc(sizeof(int));
                double * d = malloc(sizeof(double));
        free(d);free(c);}
        return 0;
free(b);free(a);}
```

虽然代码并不好看，但是这些代码是让 C 编译器看的，你无需在意它是否美好。

# 一些特殊情况

前文已经实现了一个基于 m4 的隐形的 C 内存管理层。不过，需要考虑 `return`，`break`，`continue`，`goto` 等跳转语句，它们可以让程序流从 C 的当前作用域中跳出，导致当前生命域无法终止，内存出现泄漏。

`return` 语句的问题可以解决，只需在一个递归过程中，调用各层生命域所保存的 `free` 语句即可。只是在此过程中，生命域栈会被逐层清空，需要将每个被调用的 `free` 语句存入一个临时栈，最后基于该临时栈恢复生命域栈，该过程由 `_exec_return` 宏实现，

```m4
define(`_exec_return', 
`ifdef(`_free_'$1, 
`pushdef(`_free_tmp_'$1, defn(`_free_'$1))dnl 保存 free 语句
indir(`_free_'$1)popdef(`_free_'$1)$0($1)', dnl 执行 free 语句
`ifelse(eval(`$1 > 0 '), 1, `$0(decr($1))')'dnl 递归遍历
)')
```

从各层临时栈中恢复生命域栈的过程由 `_recover_life_stack` 完成，

```m4
define(`_recover_life_stack', 
`ifdef(`_free_tmp_'$1, 
`pushdef(`_free_'$1, defn(`_free_tmp_'$1))popdef(`_free_tmp_'$1)$0($1)',
`ifelse(eval($1 < _life_n_), 1, `$0(incr($1))')')')
```

`_return` 宏在完成调用各层 `free` 语句后，恢复生命栈，最后展开为 `return` 语句，

```m4
define(`_return', 
       `do {_exec_return(_life_n_)_recover_life_stack(1)dnl
return $1;} while (0)')
```

`break` 和 `continue` 语句的生命域问题不好解决，因为与它们配合使用的 `for`，`while` 以及 `switch` 等语句的作用域实际上是同一个作用域在时间作用下的叠加结构，而生命域没有时间。此外，在循环语句中，`break` 和 `continue` 可能会在嵌套的条件语句里出现，需要为其寻找最近的有效生命域层，但最近的有效生命域层可能位于循环语句之外。最难以解决的是 `goto` 语句，在该语句面前，函数内部的作用域相当于不存在，故而生命域也不存在。对于这些语句的生命域维护，最好的方案是不解决，而是给出一个普适的约定，**对于任一对象，若确定其所有权需要转移到其他作用域，则其内存资源只能用 C 原生的机制管理。**本质上，`break`，`continue` 以及 `goto` 会对一些对象产生了所有权转移效用。

一些对象的内存是由特定函数分配的，例如动态数组、链表、树等容器对象，通常有专用的分配和释放函数，需要定义一个更具一般性的内存分配宏 `_obj_life`，

```m4
define(`_obj_life', `$1`'pushdef(`_free_'_life_n_, `$2;')')
```

例如，假设 `List` 是链表类型，构造链表对象的函数是 `list_new`，释放链表的函数是 `lise_free`，则 `_obj_life` 的用法为

```c
{
        List *a = _obj_life(list_new(), list_free(a));
}
```

上述代码的展开结果为

```c
{
        List *a = list_new();
list_free(a);}
```

还有一种情况，在嵌套的 C 作用域里，内层作用域的对象可以与外层作用域的对象重名，而 `_malloc` 宏所保存的 `free` 语句基于对象名释放内存，对于正常的生命域而言，不会出现问题，但是若是在内层作用域里调用了 `_return` 宏，便会出现对象被重复释放内存的情况。为此，还需作一个约定，**在嵌套的作用域中，不要创建同名对象。**

# 总结

除了上述的 trace-scopes.awk 脚本，我将实现 C 对象生命域的 m4 代码汇总为 c-life.m4 文件，

```m4
divert(-1)
define(`_life_n_', 0)
define(`_life_start_', `define(`_life_n_', incr(_life_n_)){')
define(`_malloc', `$2 $1 = malloc($3)pushdef(`_free_'_life_n_, `free($1);')')
define(`_exec_free_', `indir(`_free_'_life_n_)popdef(`_free_'_life_n_)')
define(`_free_', `ifdef(`_free_'_life_n_, `_exec_free_`'$0')')
define(`_life_stop_', `_free_}define(`_life_n_', decr(_life_n_))')
define(`_exec_return', 
       `ifdef(`_free_'$1, 
              `indir(`_free_'$1)pushdef(`_free_tmp_'$1, defn(`_free_'$1))popdef(`_free_'$1)$0($1)',
              `ifelse(eval(`$1 > 0 '), 1, `$0(decr($1))')')')
define(`_recover_life_stack', 
       `ifdef(`_free_tmp_'$1, 
              `pushdef(`_free_'$1, defn(`_free_tmp_'$1))popdef(`_free_tmp_'$1)$0($1)',
              `ifelse(eval($1 < _life_n_), 1, `$0(incr($1))')')')
define(`_return', 
       `_exec_return(_life_n_)_recover_life_stack(1)`'return $1')
define(`_obj_life', `$1`'pushdef(`_free_'_life_n_, `$2;')')
divert(0)dnl
```

为演示 C 生命域的用法，构造 foo-meta.c 文件，其内容为

```c
#include <stdio.h>
int main(void) {
        _malloc(a, int *, sizeof(int));
        _malloc(b, double *, sizeof(double));
        List *foo = _obj_life(list_new(), list_free(foo));
        for (size_t i = 0; i < 10; i++) {
                _malloc(c, float *, i * sizeof(float));
        }
        _return(0);
}
```

执行以下命令，可将 foo-meta.c 转化为 foo.c 文件，

```console
$ awk -f trace-scopes.awk foo-meta.c | m4 c-life.m4 - > foo.c
```

foo.c 的内容为

```c
#include <stdio.h>
int main(void) {
        int * a = malloc(sizeof(int));
        double * b = malloc(sizeof(double));
        List *foo = list_new();
        for (size_t i = 0; i < 10; i++) {
                float * c = malloc(i * sizeof(float));
        free(c);}
        do {list_free(foo);free(b);free(a);;return 0;} while(0);
list_free(foo);free(b);free(a);}
```

