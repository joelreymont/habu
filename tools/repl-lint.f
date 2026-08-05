\ repl-lint.f -- CLI wrapper for REPL exit lint.
\ Load after tools/repl-lint-core.f and lib/argv.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/repl-lint-core.f
require lib/argv.f

package LINT-REPL
private

: CONFIG ( -- )
   s" tools/repl-lint.f [ROOT]" ARGV:USAGE!
   ARGV:PARSE
   0 1 ARGV:EXPECT-POS
   ARGV:POS# 0= if s" ." ROOT! exit then
   0 ARGV:POS$ ROOT! ;

: MAIN ( -- )
   CONFIG
   LINT ;

MAIN

;package
