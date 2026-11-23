#!/usr/bin/awk -f
/^@ .* [#][ \t]*$/        { print; in_snip = 1; next }
in_snip && /^@[ \t]*$/    { print; in_snip = 0; next }
in_snip {
	gsub(/&/, "\\&amp;")
	gsub(/</, "\\&lt;")
	gsub(/>/, "\\&gt;")
}
{ print }
