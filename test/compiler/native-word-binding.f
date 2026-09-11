\ Intrinsic spellings keep the dictionary binding chosen by their package.
require lib/test.f

package NATIVE-BOUND-PUBLIC
public

: CELLS ( -- n ) 4 ;
: + ( n n -- n ) drop ;
: DUP ( n -- n n ) 42 ;

: COUNT-OK? ( n -- bool ) CELLS = ;
: FIRST ( n n -- n ) + ;
: PAIR ( n -- n n ) DUP ;
: MIXED-PAIR ( n -- n n ) dUp ;

;package

package NATIVE-BOUND-PRIVATE
private

9 constant CELLS

public

: COUNT ( -- n ) CELLS ;

;package

package NATIVE-WORD-BINDING-TEST
private

: MIXED-COUNTDOWN ( n -- n )
   DuP 0= IF DrOp 7 EXIT THEN 1- ReCuRsE ;

: MIXED-SUM ( n -- n )
   0 SwAp 0 ?Do i + LoOp ;

: MIXED-QUOT ( n -- n )
   [: DuP + ;] ExEcUtE ;

: RUN ( -- )
   T-RESET
   s" package words retain their own effect and behavior" T-LABEL
   4 NATIVE-BOUND-PUBLIC:COUNT-OK? TTRUE
   5 NATIVE-BOUND-PUBLIC:COUNT-OK? TFALSE
   17 29 NATIVE-BOUND-PUBLIC:FIRST 17 T=
   7 NATIVE-BOUND-PUBLIC:PAIR 42 T= 7 T=
   8 NATIVE-BOUND-PUBLIC:MIXED-PAIR 42 T= 8 T=
   NATIVE-BOUND-PRIVATE:COUNT 9 T=
   NATIVE-BOUND-PUBLIC:CELLS 4 T=
   s" global intrinsic bindings remain available outside those packages" T-LABEL
   3 cells 24 T=
   17 29 + 46 T=
   7 dup 7 T= 7 T=
   s" case-folded tape names keep control, rename, and quotation behavior" T-LABEL
   0 MIXED-COUNTDOWN 7 T=
   4 MIXED-COUNTDOWN 7 T=
   0 MIXED-SUM 0 T=
   5 MIXED-SUM 10 T=
   9 MIXED-QUOT 18 T=
   T-REPORT ;

RUN
;package
