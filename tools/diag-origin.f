\ diag-origin.f - CLI wrapper for diagnostic origin markers.
\ Load after lib/errors.f, lib/string.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, and tools/diag-origin-core.f.

package DIAG-ORIGIN-CLI
private

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if DIAG-ORIGIN:USAGE then
   0 SCRIPT-ARGV$ DIAG-ORIGIN:RUN ;

MAIN

;package
