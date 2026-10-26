define(`zhdefine', `divert(0)@zh-macro: $1
divert(1)`define(`$1', `$2')'')dnl
zhdefine(`中文宏 1', `无参数的中文宏')
zhdefine(`中文宏 2', `参数为「$1」和「$2」的中文宏')
