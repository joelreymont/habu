\ diag-origin.f - CLI wrapper for diagnostic origin markers.
require lib/errors.f
require lib/string.f
require lib/memory.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/diag-origin-core.f

: DIAG-ORIGIN-MAIN ( -- )
   SCRIPT-ARGC 1 <> if DO-USAGE then
   0 SCRIPT-ARGV$ DIAG-ORIGIN ;

DIAG-ORIGIN-MAIN
