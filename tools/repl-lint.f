\ repl-lint.f -- CLI wrapper for REPL exit lint.
\ Load after tools/repl-lint-core.f and tools/argv.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/repl-lint-core.f
require tools/argv.f

: REPL-CONFIG ( -- )
   s" tools/repl-lint.f [ROOT]" ARGV-USAGE!
   ARGV-PARSE
   0 1 ARGV-EXPECT-POS
   ARGV-POS# 0= if s" ." REPL-ROOT! exit then
   0 ARGV-POS$ REPL-ROOT! ;

: REPL-MAIN ( -- )
   REPL-CONFIG
   REPL-LINT ;

REPL-MAIN
