\ Production vocabulary behavior and the instructions published for it.
require lib/test.f
require lib/ieee754.f
require src/arch/arm64/asm.f
require tools/codegen-tail-probe.f

\ Tier 1 below: those published instructions are the optimizing compiler's
\ (7 rows fail at the default tier).
1 set-tier

package NVOCAB-FIXTURE
public

: GT ( n n -- bool ) > ;
: GE ( n n -- bool ) >= ;
: NE ( n n -- bool ) <> ;
: LT ( n n -- bool ) < ;
: LE ( n n -- bool ) <= ;
: EQ ( n n -- bool ) = ;
: ZERO? ( n -- bool ) 0= ;
: NEGATIVE? ( n -- bool ) 0< ;
: LESS-THREE? ( n -- bool ) 3 < ;
: F-LT ( r r -- bool ) f< ;
: F-GT ( r r -- bool ) f> ;
: F-EQ ( r r -- bool ) f= ;
: F-ZERO? ( r -- bool ) f0= ;
: F-NEGATIVE? ( r -- bool ) f0< ;
: SELECT-GT ( n n -- n ) 2dup > if 2drop 1 exit then 2drop 0 ;
: SELECT-GE ( n n -- n ) 2dup >= if 2drop 1 exit then 2drop 0 ;
: SELECT-NE ( n n -- n ) 2dup <> if 2drop 1 exit then 2drop 0 ;
: UNTIL-GT ( n -- n ) begin 1+ dup 3 > until ;
: BITS-AND ( n n -- n ) and ;
: BITS-OR ( n n -- n ) or ;
: BITS-XOR ( n n -- n ) xor ;
: SHIFT-LEFT ( n n -- n ) lshift ;
: SHIFT-RIGHT ( n n -- n ) rshift ;
: COMPLEMENT ( n -- n ) invert ;
: CELL-SIZE ( n -- n ) cells ;
: DROP-PAIR ( n n n -- n ) 2drop ;
: TAG-BITS ( n -- n ) 7 and ;

32 constant SPACE
9 constant TAB
10 constant LF
13 constant CR
: WHITE? ( n -- bool )
   dup SPACE = over TAB = or over LF = or swap CR = or ;

;package

package NVOCAB-TEST
using NVOCAB-FIXTURE
using A64ASM

\ Preserve the full-cell flag representation asserted by the original cases.
CAST: FLAG-CELL ( bool -- n )

\ Read published code; the compiler releases its temporary emission after use.
$FF000010 constant BCOND-MASK
$54000000 constant BCOND-FORM
$FFE00C00 constant CSEL-MASK
$9A800000 constant CSEL-FORM
$FFE0FFE0 constant MVN-MASK

: FORM-COUNT ( ptr u8 n n n -- n )
   {: name:ptr bytes:n mask:n form:n :}
   0
   name bytes NTAILPROBE:INSNS 0 ?do
      name bytes i NTAILPROBE:INSN@ mask and form = if 1+ then
   loop ;

: SELECT-COND ( ptr u8 n -- n ) {: name:ptr bytes:n :}
   -1
   name bytes NTAILPROBE:INSNS 0 ?do
      name bytes i NTAILPROBE:INSN@ {: word:n :}
      word CSEL-MASK and CSEL-FORM = if
         drop word 12 rshift $F and
      then
   loop ;

: SELECT-FORM ( ptr u8 n n -- ) {: name:ptr bytes:n cond:n :}
   name bytes BCOND-MASK BCOND-FORM FORM-COUNT 0 T=
   name bytes CSEL-MASK CSEL-FORM FORM-COUNT 1 T=
   name bytes SELECT-COND cond T= ;

: COMPARES ( -- )
   3 4 GT TFALSE 4 3 GT FLAG-CELL -1 T= -1 -2 GT FLAG-CELL -1 T=
   3 4 GE TFALSE 4 3 GE FLAG-CELL -1 T= 3 3 GE FLAG-CELL -1 T=
   3 4 NE FLAG-CELL -1 T= 3 3 NE TFALSE
   0 ZERO? FLAG-CELL -1 T= -1 ZERO? TFALSE 5 ZERO? TFALSE ;

$8000000000000000 constant MIN-N
$7FFFFFFFFFFFFFFF constant MAX-N

: FLAG-VALUES ( -- )
   s" integer comparisons preserve full-cell flags at signed extremes" T-LABEL
   MIN-N MAX-N LT FLAG-CELL -1 T= MAX-N MIN-N LT FLAG-CELL 0 T=
   MIN-N MAX-N LE FLAG-CELL -1 T= MAX-N MIN-N LE FLAG-CELL 0 T=
   MAX-N MIN-N GT FLAG-CELL -1 T= MIN-N MAX-N GT FLAG-CELL 0 T=
   MAX-N MIN-N GE FLAG-CELL -1 T= MIN-N MAX-N GE FLAG-CELL 0 T=
   MIN-N MIN-N EQ FLAG-CELL -1 T= MIN-N MAX-N EQ FLAG-CELL 0 T=
   MIN-N MAX-N NE FLAG-CELL -1 T= MIN-N MIN-N NE FLAG-CELL 0 T=
   MIN-N MIN-N LE FLAG-CELL -1 T= MAX-N MAX-N GE FLAG-CELL -1 T=
   s" immediate comparisons preserve full-cell true and false" T-LABEL
   0 ZERO? FLAG-CELL -1 T= MAX-N ZERO? FLAG-CELL 0 T=
   MIN-N NEGATIVE? FLAG-CELL -1 T= MAX-N NEGATIVE? FLAG-CELL 0 T=
   MIN-N LESS-THREE? FLAG-CELL -1 T= MAX-N LESS-THREE? FLAG-CELL 0 T= ;

: F-UNORDERED ( r -- ) {: nan:r :}
   nan 1.0 F-LT FLAG-CELL 0 T= 1.0 nan F-LT FLAG-CELL 0 T=
   nan 1.0 F-GT FLAG-CELL 0 T= 1.0 nan F-GT FLAG-CELL 0 T=
   nan nan F-EQ FLAG-CELL 0 T= nan 1.0 F-EQ FLAG-CELL 0 T=
   nan F-ZERO? FLAG-CELL 0 T= nan F-NEGATIVE? FLAG-CELL 0 T= ;

: FLOAT-FLAGS ( -- )
   s" floating comparisons preserve full-cell flags" T-LABEL
   1.5 2.5 F-LT FLAG-CELL -1 T= 2.5 1.5 F-LT FLAG-CELL 0 T=
   2.5 1.5 F-GT FLAG-CELL -1 T= 1.5 2.5 F-GT FLAG-CELL 0 T=
   1.5 1.5 F-EQ FLAG-CELL -1 T= 1.5 2.5 F-EQ FLAG-CELL 0 T=
   -1.5 F-NEGATIVE? FLAG-CELL -1 T= 1.5 F-NEGATIVE? FLAG-CELL 0 T=
   s" quiet and signaling NaNs keep every floating flag false" T-LABEL
   $7FF8000000000000 IEEE754:BITS>F64 F-UNORDERED
   $7FF0000000000001 IEEE754:BITS>F64 F-UNORDERED
   $FFF8000000000000 IEEE754:BITS>F64 F-UNORDERED
   $8000000000000000 IEEE754:BITS>F64 {: negzero:r :}
   s" both signed zeros are equal and neither is negative" T-LABEL
   0.0 negzero F-EQ FLAG-CELL -1 T= negzero 0.0 F-EQ FLAG-CELL -1 T=
   0.0 negzero F-LT FLAG-CELL 0 T= negzero 0.0 F-LT FLAG-CELL 0 T=
   0.0 negzero F-GT FLAG-CELL 0 T= negzero 0.0 F-GT FLAG-CELL 0 T=
   0.0 F-ZERO? FLAG-CELL -1 T= negzero F-ZERO? FLAG-CELL -1 T=
   0.0 F-NEGATIVE? FLAG-CELL 0 T= negzero F-NEGATIVE? FLAG-CELL 0 T= ;

: SELECTS ( -- )
   4 3 SELECT-GT 1 T= 3 3 SELECT-GT 0 T= 3 4 SELECT-GT 0 T=
   4 3 SELECT-GE 1 T= 3 3 SELECT-GE 1 T= 3 4 SELECT-GE 0 T=
   4 3 SELECT-NE 1 T= 3 3 SELECT-NE 0 T= 3 4 SELECT-NE 1 T=
   s" NVOCAB-FIXTURE:SELECT-GT" C-GT SELECT-FORM
   s" NVOCAB-FIXTURE:SELECT-GE" C-GE SELECT-FORM
   s" NVOCAB-FIXTURE:SELECT-NE" C-NE SELECT-FORM
   0 UNTIL-GT 4 T= 10 UNTIL-GT 11 T= ;

: BITWISE ( -- )
   12 10 BITS-AND 8 T= -1 0 BITS-AND 0 T= -1 -1 BITS-AND -1 T=
   12 10 BITS-OR 14 T= -1 0 BITS-OR -1 T=
   12 10 BITS-XOR 6 T= -1 -1 BITS-XOR 0 T=
   0 COMPLEMENT -1 T= -1 COMPLEMENT 0 T= 5 COMPLEMENT -6 T=
   s" NVOCAB-FIXTURE:COMPLEMENT" MVN-MASK 0 0 ENC-MVN MVN-MASK and
   FORM-COUNT 1 T= ;

: SHIFTS ( -- )
   1 0 SHIFT-LEFT 1 T= 1 63 SHIFT-LEFT $8000000000000000 T=
   1 64 SHIFT-LEFT 1 T= 1 65 SHIFT-LEFT 2 T=
   -1 0 SHIFT-RIGHT -1 T= -1 63 SHIFT-RIGHT 1 T=
   -1 64 SHIFT-RIGHT -1 T= -1 1 SHIFT-RIGHT $7FFFFFFFFFFFFFFF T= ;

: VALUES ( -- )
   0 CELL-SIZE 0 T= 3 CELL-SIZE 24 T= -3 CELL-SIZE -24 T=
   1 2 3 DROP-PAIR 1 T=
   255 TAG-BITS 7 T= 8 TAG-BITS 0 T= 13 TAG-BITS 5 T=
   32 WHITE? FLAG-CELL -1 T= 9 WHITE? FLAG-CELL -1 T= 10 WHITE? FLAG-CELL -1 T= 13 WHITE? FLAG-CELL -1 T=
   65 WHITE? TFALSE 0 WHITE? TFALSE 33 WHITE? TFALSE ;

: RUN ( -- )
   T-RESET COMPARES FLAG-VALUES FLOAT-FLAGS SELECTS BITWISE SHIFTS VALUES T-REPORT ;

RUN
;using
;using
;package
