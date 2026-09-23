\ Intrinsic spellings keep the dictionary binding chosen by their package.
require lib/test.f

package NATIVE-BOUND-PUBLIC
public

: CELLS ( -- n ) 4 ;
: + ( n n -- n ) drop ;
: DUP ( n -- n n ) 42 ;
: throw ( n -- n ) 1+ ;
: die ( n -- n ) 1+ 1+ ;

: COUNT-OK? ( n -- bool ) CELLS = ;
: FIRST ( n n -- n ) + ;
: PAIR ( n -- n n ) DUP ;
: MIXED-PAIR ( n -- n n ) dUp ;
: THROW-RETURNS ( n -- n ) throw ;
: DIE-RETURNS ( n -- n ) die ;

;package

package NATIVE-BOUND-PRIVATE
private

9 constant CELLS

public

: COUNT ( -- n ) CELLS ;

;package

\ ---- one spelling, three definitions, one session ----------------------------
\ The dialect's vocabulary is registered ONCE per load, so whether a spelling is
\ still the engine's own is asked per DEFINITION, at the moment a token reads the
\ row. These three pin both directions inside one session: `xor` is the intrinsic
\ operation, then a package owns it, then it is the intrinsic again. A model that
\ cached membership for the load would answer one of the three wrongly.
: XOR-BEFORE ( n n -- n ) xor ;

package NATIVE-BOUND-XOR
public

: xor ( n n -- n ) drop drop 99 ;

: XOR-INSIDE ( n n -- n ) xor ;

;package

: XOR-AFTER ( n n -- n ) xor ;

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
   41 NATIVE-BOUND-PUBLIC:THROW-RETURNS 42 T=
   40 NATIVE-BOUND-PUBLIC:DIE-RETURNS 42 T=
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
   s" one spelling is intrinsic, then a package's, then intrinsic again" T-LABEL
   6 3 XOR-BEFORE 5 T=
   6 3 NATIVE-BOUND-XOR:XOR-INSIDE 99 T=
   6 3 XOR-AFTER 5 T=
   T-REPORT ;

RUN
;package
