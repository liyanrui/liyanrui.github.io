divert(-1)
define(`_start_', `pushdef(`block_name', $1)\start$1')
define(`_stop_', `\stop`'block_name`'popdef(`block_name')')
divert(0)dnl