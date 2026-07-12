\ shadow-lint-test.f - focused definer-classification fixtures.

require lib/test.f
require tools/lint/shadow-lint.f

package SHADOW-LINT-TOOL

: SLT-LAYOUT-BUFFER ( -- )
   s" 1 LAYOUT-BUFFER DUP family" TOKENIZE
   1 DEF-NAME-OFFSET 1 T=
   2 TOK s" DUP" LINT-STR= TTRUE
   2 TOK PRIM? TTRUE ;

: SLT-MAIN ( -- )
   T-RESET
   SLT-LAYOUT-BUFFER
   T-REPORT
   s" shadow-lint-test: ok" type cr ;

SLT-MAIN

;package
