/\\starttyping/ { typing = 1; print; next}
typing {
    $0 = gensub(/\\m{([^}]+)}/, "\\1", "g")
    $0 = gensub(/\\fn{([^}]+)}/, "\\1", "g")
    $0 = gensub(/\\t{([^}]+)}/, "\\1", "g")
    $0 = gensub(/\\p{([^}]+)}/, "\\1", "g")
    $0 = gensub(/\\c{([^}]+)}/, "\\1", "g")
    if (/"[^"]*"/) {
        gsub(/\\backslash[ \t]*/, "\\")
    }
    print; next
}
/\\stoptyping/ { typing = 0; print; next}
{ print }
