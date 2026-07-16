\ shadow-lint-test.f - focused definer-classification fixtures.

require lib/test.f
require tools/lint/shadow-lint.f

package SHADOW-LINT-TOOL

: SLT-LAYOUT-BUFFER ( -- )
   s" 1 LAYOUT-BUFFER DUP family" TOKENIZE
   1 DEF-NAME-OFFSET 1 T=
   2 TOK s" DUP" LINT-STR= TTRUE
   2 TOK PRIM? TTRUE ;

: SLT-LITERALS ( -- )
   0 BAD !
   S\" s\" : fork cannot clone\" drop" s" literal" LINT-SOURCE
   S\" S\\\" : fork cannot clone\\\" 2drop" s" escaped-literal" LINT-SOURCE
   S\" \\ : fork\n( : fork )\n: SAFE ( -- ) ;" s" comments" LINT-SOURCE
   BAD @ 0 T= ;

: SLT-DEFINITION ( -- )
   0 BAD !
   s" : fork ( -- ) ;" s" definition" LINT-SOURCE
   BAD @ 1 T= ;

: SLT-MAIN ( -- )
   T-RESET
   SLT-LAYOUT-BUFFER
   SLT-LITERALS
   SLT-DEFINITION
   T-REPORT
   s" shadow-lint-test: ok" type cr ;

SLT-MAIN

;package
