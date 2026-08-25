\ native-rename-rows.f - whole-value stack renames through production code.

require lib/test.f
require lib/adt/option.f
require src/compiler/native/compiler.f

package NRR

private

: OPTION>N ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: SIDE ( n -- n )
   dup 0 > if 1 + else 2 + then ;

: SWAP-BUNDLE ( option<n> n -- n option<n> )
   swap ;

: ROT-BUNDLE ( option<n> n n -- n n option<n> )
   rot ;

: DUP-BUNDLE ( option<n> -- option<n> option<n> )
   dup ;

: JOIN-BUNDLE ( option<n> n -- n option<n> )
   dup 0 > if 1 + else 2 + then swap ;

: LOOP-BUNDLE ( option<n> n -- n option<n> )
   3 0 ?do over drop loop swap ;

: CALL-BUNDLE ( option<n> n -- n option<n> )
   SIDE swap ;

public

: CASES ( -- )
   s" swap moves a two-cell value whole" T-LABEL
   43 OPTION:SOME 7 SWAP-BUNDLE OPTION>N 43 T= 7 T=

   s" rot moves a two-cell value whole" T-LABEL
   43 OPTION:SOME 7 9 ROT-BUNDLE OPTION>N 43 T= 9 T= 7 T=

   s" dup copies a two-cell value whole" T-LABEL
   43 OPTION:SOME DUP-BUNDLE OPTION>N 43 T= OPTION>N 43 T=

   s" a bundle crosses a join, loop edge and call" T-LABEL
   43 OPTION:SOME 7 JOIN-BUNDLE OPTION>N 43 T= 8 T=
   43 OPTION:SOME 7 LOOP-BUNDLE OPTION>N 43 T= 7 T=
   43 OPTION:SOME 7 CALL-BUNDLE OPTION>N 43 T= 8 T= ;

;package

T-RESET
NRR:CASES
T-REPORT
