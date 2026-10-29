\ fmath-basis.f - exact binary64 basis scaling in `package FBASIS`: SCALE3 maps p
\ to o + A diag(s) A^-1 (p-o) and rounds each coordinate once, to nearest even.
\ The frame is twelve bit patterns by rows [a00 a01 a02 o0, ...], axes in the
\ first three columns; scratch holds WORK-CELLS cells and must not overlap it.
\ 408 base-2^26 limbs cover the 42-term sums (53-bit significands, exp >= -1074).
\ STORAGE CLASS. CALLER-OWNED: limb arithmetic lives in the caller's scratch.

require src/lib/span.f
require src/lib/ieee754.f
require src/lib/fmath.f

package FBASIS

408 constant LIMBS
26 constant WIDTH
$3FFFFFF constant MASK
$4000000 constant BASE
-5370 constant LOW-EXP

0 constant POS-OFF
408 constant NEG-OFF
816 constant PROD-OFF
832 constant NEXT-OFF
848 constant NUM-OFF
1256 constant DEN-OFF
1664 constant SHIFT-OFF
2072 constant PLEN-OFF
2073 constant EXP-OFF
2074 constant SIGN-OFF
2075 constant CARRY-OFF
2076 constant LEN-OFF
2077 constant INDEX-OFF
2078 constant BORROW-OFF
2079 constant NSIGN-OFF
2080 constant DSIGN-OFF
2081 constant QUOT-OFF
2082 constant REXP-OFF
2083 constant VALUE-OFF
2084 constant BIT-OFF

: BUF ( SPAN:span<cell> n n -- SPAN:span<cell> )
   {: work off:n len:n :}
   work off cells len cells SPAN:SUB ;
: POS ( SPAN:span<cell> -- SPAN:span<cell> ) POS-OFF LIMBS BUF ;
: NEG ( SPAN:span<cell> -- SPAN:span<cell> ) NEG-OFF LIMBS BUF ;
: PROD ( SPAN:span<cell> -- SPAN:span<cell> ) PROD-OFF 16 BUF ;
: NEXT ( SPAN:span<cell> -- SPAN:span<cell> ) NEXT-OFF 16 BUF ;
: NUM ( SPAN:span<cell> -- SPAN:span<cell> ) NUM-OFF LIMBS BUF ;
: DEN ( SPAN:span<cell> -- SPAN:span<cell> ) DEN-OFF LIMBS BUF ;
: SHIFTED ( SPAN:span<cell> -- SPAN:span<cell> ) SHIFT-OFF LIMBS BUF ;
: S@ ( SPAN:span<cell> n -- n ) SPAN:CELL@ ;
: S! ( n SPAN:span<cell> n -- ) SPAN:CELL! ;

: CLEAR ( SPAN:span<cell> n -- )
   {: buf len:n :}
   len 0 ?do 0 buf i SPAN:CELL! loop ;
: START ( SPAN:span<cell> -- )
   {: work :}
   work POS LIMBS CLEAR work NEG LIMBS CLEAR ;

: MUL-FACTOR ( SPAN:span<cell> r -- )
   {: work x:r :}
   x IEEE754:F64>BITS {: bits:n :}
   bits 63 rshift work SIGN-OFF S@ xor work SIGN-OFF S!
   bits 52 rshift $7FF and {: exponent:n :}
   exponent $7FF = if FMATH:E-DOMAIN throw then
   exponent 0<> if
      bits $FFFFFFFFFFFFF and $10000000000000 or
      exponent 1075 -
   else bits $FFFFFFFFFFFFF and -1074 then
   work EXP-OFF S@ + work EXP-OFF S! {: sig:n :}
   work NEXT 16 CLEAR
   3 0 ?do
      sig i WIDTH * rshift MASK and {: digit:n :}
      0 work CARRY-OFF S!
      work PLEN-OFF S@ 0 ?do
         work PROD i SPAN:CELL@ digit *
         work NEXT i j + SPAN:CELL@ + work CARRY-OFF S@ + {: value:n :}
         value MASK and work NEXT i j + SPAN:CELL!
         value WIDTH rshift work CARRY-OFF S!
      loop
      work CARRY-OFF S@ work NEXT work PLEN-OFF S@ i + SPAN:CELL!
   loop
   work PLEN-OFF S@ 3 + work LEN-OFF S!
   begin work LEN-OFF S@ 1 >
      work NEXT work LEN-OFF S@ 1- SPAN:CELL@ 0= and while
      work LEN-OFF S@ 1- work LEN-OFF S!
   repeat
   work LEN-OFF S@ work PLEN-OFF S!
   work LEN-OFF S@ 0 ?do
      work NEXT i SPAN:CELL@ work PROD i SPAN:CELL!
   loop ;

: ADD-PROD ( SPAN:span<cell> SPAN:span<cell> -- )
   {: work buf :}
   work EXP-OFF S@ LOW-EXP - WIDTH /mod {: shift:n offset:n :}
   0 work CARRY-OFF S!
   work PLEN-OFF S@ 0 ?do
      work PROD i SPAN:CELL@ shift lshift work CARRY-OFF S@ +
      buf offset i + SPAN:CELL@ + {: value:n :}
      value MASK and buf offset i + SPAN:CELL!
      value WIDTH rshift work CARRY-OFF S!
   loop
   offset work PLEN-OFF S@ + work INDEX-OFF S!
   begin work CARRY-OFF S@ 0<> while
      work CARRY-OFF S@ buf work INDEX-OFF S@ SPAN:CELL@ + {: value:n :}
      value MASK and buf work INDEX-OFF S@ SPAN:CELL!
      value WIDTH rshift work CARRY-OFF S!
      work INDEX-OFF S@ 1+ work INDEX-OFF S!
   repeat ;

: TERM ( SPAN:span<cell> r r r r r -- )
   {: work a:r b:r c:r d:r e:r :}
   work PROD 16 CLEAR
   1 work PROD 0 SPAN:CELL!
   1 work PLEN-OFF S! 0 work EXP-OFF S! 0 work SIGN-OFF S!
   work a MUL-FACTOR work b MUL-FACTOR work c MUL-FACTOR
   work d MUL-FACTOR work e MUL-FACTOR
   work SIGN-OFF S@ 0= if work work POS else work work NEG then ADD-PROD ;

: COMPARE-SUM ( SPAN:span<cell> -- n )
   {: work :}
   LIMBS work INDEX-OFF S!
   begin work INDEX-OFF S@ 0 > while
      work INDEX-OFF S@ 1- work INDEX-OFF S!
      work POS work INDEX-OFF S@ SPAN:CELL@
      work NEG work INDEX-OFF S@ SPAN:CELL@ - {: diff:n :}
      diff 0<> if diff exit then
   repeat 0 ;

: DIFFERENCE ( SPAN:span<cell> SPAN:span<cell> SPAN:span<cell> -- )
   {: work bigger smaller :}
   0 work BORROW-OFF S!
   LIMBS 0 ?do
      bigger i SPAN:CELL@ smaller i SPAN:CELL@ -
      work BORROW-OFF S@ - {: diff:n :}
      diff 0< if diff BASE + 1 else diff 0 then
      work BORROW-OFF S! work POS i SPAN:CELL!
   loop ;

: COPY ( SPAN:span<cell> SPAN:span<cell> -- )
   {: src dst :}
   LIMBS 0 ?do src i SPAN:CELL@ dst i SPAN:CELL! loop ;

: FINISH ( SPAN:span<cell> SPAN:span<cell> -- n )
   {: work dst :}
   work COMPARE-SUM 0< if
      work work NEG work POS DIFFERENCE $8000000000000000
   else
      work work POS work NEG DIFFERENCE 0
   then {: sign:n :}
   work POS dst COPY sign ;

: HIGH-BIT ( SPAN:span<cell> SPAN:span<cell> -- n )
   {: work buf :}
   LIMBS work INDEX-OFF S!
   begin work INDEX-OFF S@ 0 > while
      work INDEX-OFF S@ 1- work INDEX-OFF S!
      buf work INDEX-OFF S@ SPAN:CELL@ work VALUE-OFF S!
      work VALUE-OFF S@ 0<> if
         0 work BIT-OFF S!
         begin work VALUE-OFF S@ 1 > while
            work VALUE-OFF S@ 1 rshift work VALUE-OFF S!
            work BIT-OFF S@ 1+ work BIT-OFF S!
         repeat
         work INDEX-OFF S@ WIDTH * work BIT-OFF S@ + exit
      then
   repeat -1 ;

: BUF-CMP ( SPAN:span<cell> SPAN:span<cell> SPAN:span<cell> -- n )
   {: work a b :}
   LIMBS work INDEX-OFF S!
   begin work INDEX-OFF S@ 0 > while
      work INDEX-OFF S@ 1- work INDEX-OFF S!
      a work INDEX-OFF S@ SPAN:CELL@
      b work INDEX-OFF S@ SPAN:CELL@ - {: diff:n :}
      diff 0<> if diff exit then
   repeat 0 ;

: SHIFT ( SPAN:span<cell> SPAN:span<cell> n -- )
   {: work src count:n :}
   work SHIFTED LIMBS CLEAR
   count WIDTH /mod {: shift:n offset:n :}
   0 work CARRY-OFF S!
   LIMBS offset - 0 ?do
      src i SPAN:CELL@ shift lshift work CARRY-OFF S@ + {: value:n :}
      value MASK and work SHIFTED offset i + SPAN:CELL!
      value WIDTH rshift work CARRY-OFF S!
   loop ;

: SUBTRACT ( SPAN:span<cell> SPAN:span<cell> SPAN:span<cell> -- )
   {: work a b :}
   0 work BORROW-OFF S!
   LIMBS 0 ?do
      a i SPAN:CELL@ b i SPAN:CELL@ - work BORROW-OFF S@ - {: diff:n :}
      diff 0< if diff BASE + 1 else diff 0 then
      work BORROW-OFF S! a i SPAN:CELL!
   loop ;

: RATIO-BITS ( SPAN:span<cell> -- n )
   {: work :}
   work DEN work POS COPY
   work work POS HIGH-BIT {: dtop:n :}
   dtop 0< if FMATH:E-DOMAIN throw then
   work NUM work POS COPY
   work work POS HIGH-BIT {: ntop:n :}
   work NSIGN-OFF S@ work DSIGN-OFF S@ xor {: sign:n :}
   ntop 0< if 0 exit then
   ntop dtop - work REXP-OFF S!
   work REXP-OFF S@ 0< if
      work work NUM work REXP-OFF S@ negate SHIFT
      work work SHIFTED work DEN BUF-CMP
   else
      work work DEN work REXP-OFF S@ SHIFT
      work work NUM work SHIFTED BUF-CMP
   then 0< if work REXP-OFF S@ 1- work REXP-OFF S! then
   work REXP-OFF S@ 1024 > if FMATH:E-OUTPUT throw then
   work REXP-OFF S@ -1075 < if sign exit then
   work REXP-OFF S@ 52 - -1074 max {: shift:n :}
   shift 0< if
      work work NUM shift negate SHIFT
      work SHIFTED work NUM COPY
   else
      work work DEN shift SHIFT
      work SHIFTED work DEN COPY
   then
   0 work QUOT-OFF S!
   53 0 ?do
      52 i - {: bit:n :}
      work work DEN bit SHIFT
      work work NUM work SHIFTED BUF-CMP 0 >= if
         work work NUM work SHIFTED SUBTRACT
         work QUOT-OFF S@ 1 bit lshift or work QUOT-OFF S!
      then
   loop
   work work NUM 1 SHIFT
   work work SHIFTED work DEN BUF-CMP {: cmp:n :}
   cmp 0 > cmp 0= work QUOT-OFF S@ 1 and 0<> and or if
      work QUOT-OFF S@ 1+ work QUOT-OFF S!
   then
   work REXP-OFF S@ -1022 < if work QUOT-OFF S@ sign or exit then
   work QUOT-OFF S@ $20000000000000 = if
      work QUOT-OFF S@ 1 rshift work QUOT-OFF S!
      work REXP-OFF S@ 1+ work REXP-OFF S!
   then
   work REXP-OFF S@ 1023 + {: exponent:n :}
   exponent $7FF >= if FMATH:E-OUTPUT throw then
   exponent 52 lshift work QUOT-OFF S@ $FFFFFFFFFFFFF and or sign or ;

: AXIS ( SPAN:span<cell> n n -- r )
   {: frame row:n col:n :}
   frame row 4 * col + SPAN:CELL@ IEEE754:BITS>F64 ;
: ORIGIN ( SPAN:span<cell> n -- r )
   {: frame row:n :}
   frame row 4 * 3 + SPAN:CELL@ IEEE754:BITS>F64 ;
: CHOOSE3 ( r r r n -- r )
   {: x:r y:r z:r idx:n :}
   idx 0= if x exit then idx 1 = if y exit then z ;
: COF-A ( SPAN:span<cell> n n n -- r )
   {: frame k:n i:n t:n :}
   frame k 1+ 3 mod i 1+ t + 3 mod AXIS ;
: COF-B ( SPAN:span<cell> n n n -- r )
   {: frame k:n i:n t:n :}
   frame k 2 + 3 mod i 2 + t - 3 mod AXIS ;
: D-TERMS ( SPAN:span<cell> SPAN:span<cell> r -- )
   {: work frame factor:r :}
   3 0 ?do 2 0 ?do
      work factor frame 0 j AXIS
      frame 0 j i COF-A
      frame 0 j i COF-B
      i 0<> if fnegate then 1.0 TERM
   loop loop ;
: DIFF-FACTOR ( r r n -- r )
   {: point:r origin:r term:n :}
   term 0= if point else origin fnegate then ;

: N-COF ( SPAN:span<cell> SPAN:span<cell> r r r n n n -- )
   {: work frame point:r origin:r scale:r row:n col:n k:n :}
   2 0 ?do 2 0 ?do
      work frame row col AXIS scale
      frame k col j COF-A frame k col j COF-B
      j 0<> if fnegate then
      point origin i DIFF-FACTOR TERM
   loop loop ;

: N-TERMS ( SPAN:span<cell> SPAN:span<cell> r r r r r r n -- )
   {: work frame px:r py:r pz:r sx:r sy:r sz:r row:n :}
   work frame frame row ORIGIN D-TERMS
   3 0 ?do 3 0 ?do
      work frame px py pz i CHOOSE3 frame i ORIGIN
      sx sy sz j CHOOSE3 row j i N-COF
   loop loop ;

: COORD ( SPAN:span<cell> SPAN:span<cell> r r r r r r n -- r )
   {: work frame px:r py:r pz:r sx:r sy:r sz:r row:n :}
   work START
   work frame 1.0 D-TERMS
   work work DEN FINISH work DSIGN-OFF S!
   work work DEN HIGH-BIT 0< if FMATH:E-DOMAIN throw then
   work START
   work frame px py pz sx sy sz row N-TERMS
   work work NUM FINISH work NSIGN-OFF S!
   work RATIO-BITS IEEE754:BITS>F64 ;

: FINITE? ( r -- bool )
   IEEE754:F64>BITS 52 rshift $7FF and $7FF <> ;
: NO-OVERLAP? ( SPAN:span<cell> SPAN:span<cell> -- bool )
   {: frame work :}
   frame SPAN:BYTES SPAN:$ {: fp:ptr flen:n :}
   work SPAN:BYTES SPAN:$ {: wp:ptr wlen:n :}
   fp NULL-PTR - wp NULL-PTR - {: fa:n wa:n :}
   fa wa <= if wa fa - flen >= else fa wa - wlen >= then ;

public

2085 constant WORK-CELLS

: SCALE3 ( r r r SPAN:span<cell> r r r SPAN:span<cell> -- r r r )
   {: px:r py:r pz:r frame sx:r sy:r sz:r work :}
   frame SPAN:CELL-LEN 12 < work SPAN:CELL-LEN WORK-CELLS < or if
      FMATH:E-DOMAIN throw
   then
   frame 0 12 cells SPAN:SUB work 0 WORK-CELLS cells SPAN:SUB
   NO-OVERLAP? 0= if FMATH:E-DOMAIN throw then
   px FINITE? py FINITE? and pz FINITE? and
   sx FINITE? and sy FINITE? and sz FINITE? and 0= if
      FMATH:E-DOMAIN throw
   then
   12 0 ?do
      frame i SPAN:CELL@ IEEE754:BITS>F64 FINITE? 0= if
         FMATH:E-DOMAIN throw
      then
   loop
   work frame px py pz sx sy sz 0 COORD
   work frame px py pz sx sy sz 1 COORD
   work frame px py pz sx sy sz 2 COORD ;

;package
