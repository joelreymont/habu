\ diag-origin.f - CLI wrapper for diagnostic origin markers.
\ Load after lib/errors.f, lib/string.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, and tools/diag-origin-core.f.

: DIAG-ORIGIN-MAIN ( -- )
   SCRIPT-ARGC 1 <> if DO-USAGE then
   0 SCRIPT-ARGV$ DIAG-ORIGIN ;

DIAG-ORIGIN-MAIN
