---
title: 用 m4 为 C 语言构造递归匿名函数 · 补遗
lang: zh-CN
date: 2023 年 05 月 26 日
abstract: 
category: ./index.html
footer: 我的联系方式：<lyr.m2@live.cn> 或在[讨论区](https://github.com/liyanrui/liyanrui.github.io/issues)提问。
...

数年前，曾用 m4 写了一个宏（玩具），

```m4
divert(-1)
changeword(`[_a-zA-Z@&][_a-zA-Z0-9]*')
define(`_C_CLOSURE', `divert(1)')
define(`_C_CORE',    `divert(2)')
define(`_LAMBDA_SET_VAR', `define(`$1', `$2')')
_LAMBDA_SET_VAR(`?N', 0)
 
define(`_LAMBDA',
`_C_CLOSURE`'static $2 _LAMBDA_`'defn(`?N')`('$1`)'{$3;}
_C_CORE`'_LAMBDA_`'defn(`?N')`'dnl
_LAMBDA_SET_VAR(`?N', incr(defn(`?N')))`'dnl
')
 
define(`_VAR_IN_L_N', `var_$1_just_in_LAMBDA_`'defn(`?N')')
define(`@', `_C_CLOSURE`'static $1 _VAR_IN_L_N($2); _C_CORE`'$1 $2 = $3; _VAR_IN_L_N($2) = $2`'')
define(`&', `_VAR_IN_L_N($1)')
divert(0)dnl
```

用于为 C 语言构造 Lambda 函数，例如

```C
#include <stdio.h>
_C_CORE
int main(void)
{
        @(`int', `x', `1');
        if(_LAMBDA(`int y', `int', `return &(`x') > y')(2)) {
                printf("False!\n");
        } else {
                printf("True!\n");
        }
}
```

上述 C 代码会被 m4 展开为

```C
#include <stdio.h>
static int var_x_just_in_LAMBDA_0;
static int _LAMBDA_0(int y)
{
    return var_x_just_in_LAMBDA_0 > y;
}
int main(void)
{
    int x = 1;
    var_x_just_in_LAMBDA_0 = x;
    if (_LAMBDA_0(2)) {
        printf("False!\n");
    } else {
        printf("True!\n");
    }
}
```

宏 `_LAMBDA` 的缺陷是只能用于单重匿名函数，不支持匿名函数的嵌套，因为该宏只用了两层 m4 空间，一层用于构造匿名函数的定义，一层用于调用匿名函数，而要表达嵌套的匿名函数结构，需要更多层 m4 空间，并且保证外层匿名函数的定义总是位于内层匿名函数的定义所在空间的下一级空间。传统的 m4，只支持 11 个空间，除去 -1 和 0 号空间以及最高层用于存放普通 C 代码的 9 号空间，其他空间都用上，只能支持 8 层嵌套匿名函数，且每次使用匿名函数后，需要手动重置层号。
