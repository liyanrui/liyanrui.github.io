m4_divert(-1)
m4_changecom(`@@@@: ')

m4_dnl 第一级标题的序号及宏定义
m4_define(`|||SEC_N|||', 0)
m4_define(`_SEC_', `m4_divert(1)* <a href="#$1" target="_parent">$1</a>
m4_define(@!**<|||SEC_N|||>**!@, m4_incr(m4_defn(@!**<|||SEC_N|||>**!@)))m4_dnl
m4_define(@!**<|||SUBSEC_N|||>**!@, 0)m4_dnl
m4_divert(2)<span id="$1"></span>
# <a href="#m4-TOC">m4_defn(@!**<|||SEC_N|||>**!@) $1</a>')

m4_dnl 第二级标题的序号及宏定义
m4_define(`_SUBSEC_', `m4_divert(1)m4_dnl
    * <a href="#$1" target="_parent">$1</a>
m4_define(@!**<|||SUBSEC_N|||>**!@, m4_incr(m4_defn(@!**<|||SUBSEC_N|||>**!@)))m4_dnl
m4_define(@!**<|||SUBSUBSEC_N|||>**!@, 1)m4_dnl
m4_divert(2)<span id="$1"></span>
## <a href="#m4-TOC">m4_defn(@!**<|||SEC_N|||>**!@).m4_defn(@!**<|||SUBSEC_N|||>**!@) $1</a>')

m4_dnl 第三级标题的序号及宏定义
m4_define(`_SUBSUBSEC_', `m4_divert(1)m4_dnl
        * <a href="#$1" target="_parent">$1</a>
m4_divert(2)<span id="$1"></span>
### <a href="#m4-TOC">m4_dnl
m4_defn(@!**<|||SEC_N|||>**!@).m4_dnl
m4_defn(@!**<|||SUBSEC_N|||>**!@).m4_dnl
m4_defn(@!**<|||SUBSUBSEC_N|||>**!@) $1@!**<>**!@m4_dnl
</a>m4_dnl
m4_define(@!**<|||SUBSUBSEC_N|||>**!@, m4_incr(m4_defn(@!**<|||SUBSUBSEC_N|||>**!@)))')

m4_dnl 用于设定目录出现位置的宏的定义
m4_define(`_TOC_',
`m4_divert(1)**目录**<span id="m4-TOC"></span>

m4_divert(2)m4_dnl')

m4_dnl 没啥大用处的宏……好久没用它了
m4_define(`_PAR_', `<p class="par-step">*</p>')

m4_dnl 引号改了。上面的宏的定义中那些尚未被展开的宏，它们所用的引号必须是修改后的！！！
m4_changequote(`@!**<', `>**!@') 
m4_divert(0)m4_dnl
