BEGIN {
    names = "zhm-names.txt"
    defs = "zhm-defs.m4"
    printf "" > names
    print "divert(-1)" > defs
}
/^@zh-macro:/ {
    name = $0
    sub(/^@zh-macro:[[:space:]]*/, "", name)
    sub(/[[:space:]]+$/, "", name)
    if (name != "" && !(name in seen)) {
        seen[name] = 1
        print name > names
    }
    next
}
{ print > defs }
END { print "divert(0)dnl" > defs }