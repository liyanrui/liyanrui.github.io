BEGIN { names = "zhm-names.txt"; defs = "zhm-defines.m4" }
/^@zh-macro:/ {
    name = $0
    sub(/^@zh-macro:[[:space:]]*/, "", name)
    sub(/[[:space:]]+$/, "", name)
    if (name != "" && !(name in seen)) { seen[name] = 1; print name > names }
    next
}
{ print > defs }
