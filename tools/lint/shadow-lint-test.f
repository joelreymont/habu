\ shadow-lint-test.f - focused definer-classification fixtures.

require lib/test.f
require tools/lint/shadow-lint.f

package SHADOW-LINT-TOOL

: SLT-LAYOUT-BUFFER ( -- )
   s" LAYOUT-BUFFER DUP family 1" TOKENIZE
   0 DEF-NAME-OFFSET 1 T=
   1 TOK s" DUP" LINT-STR= TTRUE
   1 TOK PRIM? TTRUE ;

: SLT-MAIN ( -- )
   T-RESET
   SLT-LAYOUT-BUFFER
   T-REPORT
   s" shadow-lint-test: ok" type cr ;

SLT-MAIN

end-package
