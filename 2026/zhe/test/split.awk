BEGIN { names = "zhm-names.txt"; defs = "zhm-defines.m4" }
/^@zh-macro: / {
    if (!($2 in seen)) { seen[$2] = 1; print $2 > names }
    next
}
{ print > defs }
