\ Integer admission is shared by interpret, evaluate, and native compilation.
require lib/test.f
require test/gate-common.f
require test/checker-assert.f

package INTEGER-LITERALS-TEST

: MINIMUM ( -- n ) 1 63 lshift ;
: MAXIMUM ( -- n ) MINIMUM 1 - ;
: DEC-MAX ( -- n ) 9223372036854775807 ;
: DEC-MIN ( -- n ) -9223372036854775808 ;
: HEX-MAX ( -- n ) $FFFFFFFFFFFFFFFF ;
: HEX-MIN ( -- n ) $8000000000000000 ;
: HEX-NEG-MAX ( -- n ) -$FFFFFFFFFFFFFFFF ;
: HEX-NEG-MIN ( -- n ) -$8000000000000000 ;
: 18446744073709551616X ( -- n ) 41 ;
: $10000000000000000G ( -- n ) 42 ;


: PARSED= ( ptr u8 n n -- ) {: src:ptr len:n want:n :}
   src len num-parse {: got:n flt:bool ok:bool :}
   ok TTRUE flt TFALSE got want T= ;


: REFUSED ( ptr u8 n -- )
   num-parse {: got:n flt:bool ok:bool :}
   ok TFALSE flt TFALSE got 0 T= ;


: VALUES ( -- )
   DEC-MAX MAXIMUM T=
   DEC-MIN MINIMUM T=
   HEX-MAX -1 T=
   HEX-MIN MINIMUM T=
   HEX-NEG-MAX 1 T=
   HEX-NEG-MIN MINIMUM T=
   18446744073709551616X 41 T=
   $10000000000000000G 42 T=
   s" 0" 0 PARSED=
   s" -0" 0 PARSED=
   s" 9223372036854775807" MAXIMUM PARSED=
   s" -9223372036854775808" MINIMUM PARSED=
   s" 0009223372036854775807" MAXIMUM PARSED=
   s" -0009223372036854775808" MINIMUM PARSED=
   s" $7FFFFFFFFFFFFFFF" MAXIMUM PARSED=
   s" $8000000000000000" MINIMUM PARSED=
   s" $FFFFFFFFFFFFFFFF" -1 PARSED=
   s" $ffffffffffffffff" -1 PARSED=
   s" -$FFFFFFFFFFFFFFFF" 1 PARSED=
   s" -$8000000000000000" MINIMUM PARSED=
   s" $000FFFFFFFFFFFFFFFF" -1 PARSED=
   s" 18446744073709551616X" REFUSED
   s" $10000000000000000G" REFUSED ;


: BAD-INTERPRETED ( ptr u8 n -- ) {: lit:ptr len:n :}
   GE-SRC-RESET
   lit len GE-SRC+
   s"  drop" GE-SRC-LINE
   70 lit len s" overflowing interpreted integer" GE-EVAL-FORK-BAD ;


: BAD-COMPILED ( ptr u8 n -- ) {: lit:ptr len:n :}
   GE-SRC-RESET
   s" : INTEGER-OVERFLOW-BAD ( -- n ) " GE-SRC+
   lit len GE-SRC+
   s"  ;" GE-SRC-LINE
   70 lit len s" overflowing compiled integer" GE-EVAL-FORK-BAD ;


: HOSTILE-NAME ( ptr u8 n -- ) {: lit:ptr len:n :}
   GE-SRC-RESET
   s" : " GE-SRC+ lit len GE-SRC+
   s"  ( -- n ) 42 ;" GE-SRC-LINE ;


: BAD-NAME-INTERPRETED ( ptr u8 n -- ) {: lit:ptr len:n :}
   lit len HOSTILE-NAME
   lit len GE-SRC+
   s"  drop" GE-SRC-LINE
   70 lit len s" overflowing integer cannot call its name" GE-EVAL-FORK-BAD ;


: BAD-NAME-COMPILED ( ptr u8 n -- ) {: lit:ptr len:n :}
   lit len HOSTILE-NAME
   s" : INTEGER-OVERFLOW-CALL ( -- n ) " GE-SRC+
   lit len GE-SRC+
   s"  ;" GE-SRC-LINE
   70 lit len s" overflowing integer cannot compile a name call" GE-EVAL-FORK-BAD ;


: BAD-LITERAL ( ptr u8 n -- ) {: lit:ptr len:n :}
   lit len REFUSED
   lit len BAD-INTERPRETED
   lit len BAD-COMPILED
   lit len BAD-NAME-INTERPRETED
   lit len BAD-NAME-COMPILED ;


: REFUSALS ( -- )
   s" 9223372036854775808" BAD-LITERAL
   s" -9223372036854775809" BAD-LITERAL
   s" 18446744073709551615" BAD-LITERAL
   s" -18446744073709551615" BAD-LITERAL
   s" 18446744073709551616" BAD-LITERAL
   s" -18446744073709551616" BAD-LITERAL
   s" 36893488147419103232" BAD-LITERAL
   s" -36893488147419103232" BAD-LITERAL
   s" $10000000000000000" BAD-LITERAL
   s" -$10000000000000000" BAD-LITERAL
   s" $1FFFFFFFFFFFFFFFF" BAD-LITERAL
   s" -$1FFFFFFFFFFFFFFFF" BAD-LITERAL ;

\ Checker-only admission must refuse the same spellings before compilation.
\ Numeric-shaped names stay claimed by the literal grammar even on overflow.
: CHECKER-ADMISSION ( -- )
   s" IL-MAX ( -- n ) 9223372036854775807" CHECK-QUIET-CANDIDATE! -1 T=
   s" IL-MIN ( -- n ) -9223372036854775808" CHECK-QUIET-CANDIDATE! -1 T=
   s" IL-HEX ( -- n ) $FFFFFFFFFFFFFFFF" CHECK-QUIET-CANDIDATE! -1 T=
   s" IL-NEG-HEX ( -- n ) -$FFFFFFFFFFFFFFFF" CHECK-QUIET-CANDIDATE! -1 T=
   s" IL-OVER ( -- n ) 9223372036854775808" CHECK-QUIET-CANDIDATE! 0 T=
   s" IL-UNDER ( -- n ) -9223372036854775809" CHECK-QUIET-CANDIDATE! 0 T=
   s" IL-WRAP ( -- n ) 18446744073709551616" CHECK-QUIET-CANDIDATE! 0 T=
   s" IL-HEX-WRAP ( -- n ) $10000000000000000" CHECK-QUIET-CANDIDATE! 0 T=
   s" IL-NAME ( -- n ) 18446744073709551616X" CHECK-QUIET-CANDIDATE! -1 T= ;

T-RESET
9223372036854775807 MAXIMUM T=
-9223372036854775808 MINIMUM T=
$FFFFFFFFFFFFFFFF -1 T=
$8000000000000000 MINIMUM T=
-$FFFFFFFFFFFFFFFF 1 T=
-$8000000000000000 MINIMUM T=
18446744073709551616X 41 T=
$10000000000000000G 42 T=
VALUES
REFUSALS
: 9223372036854775808 ( -- n ) 42 ;
: $10000000000000000 ( -- n ) 42 ;
CHECKER-ADMISSION
T-REPORT

;package
