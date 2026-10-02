\ closed-source-suite.f - generated source and loaded files run as closed
\ programs.
\
\ A source-generating definer renders its declaration into a lib/codegen.f
\ buffer and hands the text to the loader's boundary, INCLUDE-EVALUATE
\ (src/core/include.f), the word TYPE-DECL:TDECL-EVAL-XT is bound to for every
\ generated core declaration. That boundary is evaluate-closed: the text runs
\ with its floor at the definer's depth and must leave nothing. The three
\ definers below render the same declaration and differ only in what their
\ generator appends after it: a sound one's product certifies like any checked
\ word, one that leaves a cell is refused E-EVAL-RESIDUE, and one that takes a
\ cell it never pushed throws 70 with the caller's cells intact. A file loaded
\ by require, include or --load crosses the same boundary, so
\ test/closed-source-residue-bad.f, which ends `1 2`, is refused E-EVAL-RESIDUE
\ through `required`. lib/type/deftype.f calls evaluate-closed itself and the
\ FFI declarer in lib/ffi-abi.f crosses INCLUDE-EVALUATE; test/deftype-suite.f
\ and lib/ffi-test.f certify their products.
require lib/codegen.f
require lib/errors.f
require lib/test.f

package CLOSED-SOURCE-TEST

private

$100 CODEGEN:BUFFER GEN

: GEN+ ( ptr u8 n -- ) GEN CODEGEN:APPEND-STRING ;

\ The declaration every definer here means: `: <name> ( -- n ) <v> ;`.
: RENDER ( n ptr u8 n -- ) {: v:n a:ptr u:n :}
   GEN CODEGEN:RESET
   s" : " GEN+  a u GEN+  s"  ( -- n ) " GEN+
   v GEN CODEGEN:APPEND-DECIMAL  s"  ;" GEN+ ;

: CROSS ( -- ) GEN CODEGEN:CONTENTS INCLUDE-EVALUATE ;

: SOUND ( n ptr u8 n -- ) RENDER CROSS ;

\ Pushes its value again after the declaration.
: LEAKY ( n ptr u8 n -- ) {: v:n a:ptr u:n :}
   v a u RENDER  s"  " GEN+  v GEN CODEGEN:APPEND-DECIMAL  CROSS ;

\ Drops a cell the text never pushed.
: GREEDY ( n ptr u8 n -- ) RENDER  s"  drop" GEN+  CROSS ;

: CERTIFIES ( -- )
   s" a sound definer's text loads" T-LABEL
   7 s" CSRC-SEVEN" SOUND
   s" CSRC-SEVEN 7 T=" INCLUDE-EVALUATE
   s" its product certifies in a checked caller" T-LABEL
   s" CSRC-SEVEN-USE ( -- n ) CSRC-SEVEN" CHECK! -1 T=
   s" against the effect the definer declared" T-LABEL
   s" CSRC-SEVEN-LOSE ( -- ) CSRC-SEVEN" CHECK! 0 T= ;

: RESIDUE ( -- )
   s" a definer whose text leaves a cell is refused by name" T-LABEL
   [: 7 s" CSRC-LEAK" LEAKY ;] E-EVAL-RESIDUE TTHROWSQ ;

: UNDER ( -- n )
   7 [: 8 s" CSRC-TAKE" GREEDY ;] catch 70 T= ;

: FLOOR ( -- )
   s" a definer whose text takes a cell it never pushed throws 70" T-LABEL
   UNDER
   s" and the definer's caller keeps its cell" T-LABEL
   7 T= ;

: LOADED ( -- )
   s" a required file that ends 1 2 is refused by name" T-LABEL
   [: s" test/closed-source-residue-bad.f" required ;] E-EVAL-RESIDUE TTHROWSQ ;

: RUN ( -- )
   T-RESET
   CERTIFIES RESIDUE FLOOR LOADED
   T-REPORT ;

' RUN
;package
execute
