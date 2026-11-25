\ closed-source-suite.f - generated source runs as a closed program.
require src/lib/codegen.f
require src/lib/error.f
require src/lib/test/assert.f

package CLOSED-SOURCE-TEST

private

CODEGEN:BUFFER GEN

: GEN+ ( str -- ) GEN CODEGEN:APPEND-STRING ;

\ The declaration every definer here means: `: <name> ( -- n ) <v> ;`.
: RENDER ( n str -- ) {: v:n s:str :}
   GEN CODEGEN:RESET
   s" : " GEN+  s GEN+  s"  ( -- n ) " GEN+
   v GEN CODEGEN:APPEND-DECIMAL  s"  ;" GEN+ ;

: CROSS ( -- ) GEN CODEGEN:CONTENTS evaluate-closed ;

\ A source-generating definer hands its rendered text to evaluate-closed: the
\ floor at the definer's depth, leaving nothing. The three definers below differ
\ only in what they append: a sound one's product certifies, one leaving a cell
\ is rejected with a record and 70, and one taking a cell it never pushed
\ throws 70 with the caller's cells intact.
: SOUND ( n str -- ) RENDER CROSS ;

: LEAKY ( n str -- ) {: v:n s:str :}
   v s RENDER  s"  " GEN+  v GEN CODEGEN:APPEND-DECIMAL  CROSS ;

: GREEDY ( n str -- ) RENDER  s"  drop" GEN+  CROSS ;

: CERTIFIES ( -- )
   s" a sound definer's text loads" T-LABEL
   7 s" CSRC-SEVEN" SOUND
   s" CSRC-SEVEN 7 T=" evaluate-closed
   s" its product certifies in a checked caller" T-LABEL
   s" CSRC-SEVEN-USE ( -- n ) CSRC-SEVEN" CHECK! -1 T=
   s" against the effect the definer declared" T-LABEL
   s" CSRC-SEVEN-LOSE ( -- ) CSRC-SEVEN" CHECK! 0 T= ;

: RESIDUE ( -- )
   s" a definer whose text leaves a cell throws 70" T-LABEL
   [: 7 s" CSRC-LEAK" LEAKY ;] 70 TTHROWSQ ;

: UNDER ( -- n )
   7 [: 8 s" CSRC-TAKE" GREEDY ;] catch 70 T= ;

: FLOOR ( -- )
   s" a definer whose text takes a cell it never pushed throws 70" T-LABEL
   UNDER
   s" and the definer's caller keeps its cell" T-LABEL
   7 T= ;

: RUN ( -- )
   T-RESET
   CERTIFIES RESIDUE FLOOR
   T-REPORT ;

' RUN
;package
execute
