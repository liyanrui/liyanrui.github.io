divert(-1)dnl
define(`_REPEAT', `ifelse($1, 1, ``foo'', ``foo''-`_REPEAT(decr($1))')')dnl
define(`foo', `ifelse($1, 1, ``foo''(1), `_REPEAT($1)')')dnl
divert(0)dnl
佛说 foo(1)，即非 foo(2)，是名 foo(3)
