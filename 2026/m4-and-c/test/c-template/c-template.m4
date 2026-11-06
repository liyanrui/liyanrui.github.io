divert(-1)
# 作用域
define(`_scope_', 0)
define(`_begin_scope_', `define(`_scope_', incr(_scope_)){')
define(`_end_scope_', `define(`_scope_', decr(_scope_))}')
# 变量和表达式
define(`_type_', 0)
define(`_def_type', `ifdef(`_$1_type_',
                           `', `define(`_$1_type_', 
                                       define(`_type_', incr(_type_))_type_)')')
define(`_delsep', `ifelse(regexp(`$1', `\*$'), -1, ` ', `')')
define(`_var',
       `$2`'_delsep(`$2')$1`'ifelse(`$3', `',
                                    `',
                                    ` = $3')define(`_var_$1_t_'_scope_,
                                                   `$2')_def_type(`$2')')
define(`_expr', `$1`'define(`_expr_$1_t_'_scope_, `$2')_def_type(`$2')')
define(`_type_id', ``_'_def_type(`$1')indir(`_$1_type_')')
define(`_type_name', `ifdef(`_var_$1_t_'_scope_, 
                            `indir(`_var_$1_t_'_scope_)',
                            `ifdef(`_expr_$1_t_'_scope_,
                                  `indir(`_expr_$1_t_'_scope_)', `')')')
# 定义 _RSQ_，其展开结果是右单引号
changequote([[,]])define([[_RSQ_]], [[']])changequote
# 函数
define(`_func_name', ``$1'`_'esyscmd(`echo '_RSQ_`$2'_RSQ_` | awk -f ui/snap-types.awk')')
define(`_unmark',`esyscmd(`echo '_RSQ_`$1'_RSQ_` | awk -f ui/unmarks.awk')')
define(`_func', `$3 _func_name(`$1', `$2')_unmark(`$2')')
define(`_arg_t', `_type_id(_type_name(`$1'))')
define(`_args_t', `ifelse(`$2', `', 
                          `_arg_t(`$1')', 
                          `_arg_t(`$1')$0(shift($@))')')
define(`_args_t_name', `ifelse(`$2', `',
                               `_type_name(`$1')',
                               `_type_name(`$1') and $0(shift($@))')')
define(`_with', `ifelse(`$2', `', 
                        `_type_id(`$1')', 
                        `_type_id(`$1')$0(shift($@))')')
divert(0)dnl
