define(`zhdefine', `divert(0)@zh-macro: $1
divert(1)`define(`$1', `$2')'')dnl
zhdefine(`正文开始', `\starttext')
zhdefine(`正文结束', `\stoptext')