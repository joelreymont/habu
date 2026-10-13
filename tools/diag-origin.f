\ diag-origin.f - CLI wrapper for diagnostic origin markers.
require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/diag-origin-core.f

: DIAG-ORIGIN-MAIN ( -- )
   SCRIPT-ARGC 1 <> if DO-USAGE then
   0 SCRIPT-ARGV$ DIAG-ORIGIN ;

DIAG-ORIGIN-MAIN
