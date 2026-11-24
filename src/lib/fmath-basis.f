split-str
\ fmath-basis.f - exact binary64 basis scaling in `package FBASIS`: SCALE3 maps p
\ to o + A diag(s) A^-1 (p-o) and rounds each coordinate once, to nearest even.
\ The frame is twelve bit patterns by rows [a00 a01 a02 o0, ...], axes in the
\ first three columns; scratch holds WORK-CELLS cells and must not overlap it.
\ 408 base-2^26 limbs cover the 42-term sums (53-bit significands, exp >= -1074).
\ Each limb buffer keeps a live range [lo, hi) holding all its nonzero limbs.
\ The quotient's 53 bits come from long division by the divisor shifted left
\ 52 bits, halved after each bit.
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

\ Limb buffers by id; buffer id starts at cell id * LIMBS.
0 constant POS
1 constant NEG
2 constant NUM
3 constant DEN
4 constant SHIFTED
2040 constant PROD-OFF
2056 constant NEXT-OFF
\ Buffer id's range [lo, hi) is at cells RANGE-OFF + 2id and the next.
2072 constant RANGE-OFF
2082 constant PLEN-OFF
2083 constant EXP-OFF
2084 constant SIGN-OFF

: BUF ( SPAN:span<cell> n n -- SPAN:span<cell> )
   {: work off:n len:n :}
   work off cells len cells SPAN:SUB ;
: LIMB-BUF ( SPAN:span<cell> n -- SPAN:span<cell> )
   LIMBS * LIMBS BUF ;
: PROD ( SPAN:span<cell> -- SPAN:span<cell> ) PROD-OFF 16 BUF ;
: NEXT ( SPAN:span<cell> -- SPAN:span<cell> ) NEXT-OFF 16 BUF ;
: S@ ( SPAN:span<cell> n -- n ) SPAN:CELL@ ;
: S! ( n SPAN:span<cell> n -- ) SPAN:CELL! ;
: PART ( SPAN:span<cell> n n -- SPAN:span<u8> )
   {: buf lo:n hi:n :}
   buf lo cells hi lo - cells SPAN:SUB SPAN:BYTES ;
: ZERO ( SPAN:span<cell> -- )
   {: buf :}
   0 buf SPAN:BYTES SPAN:FILL ;

: LO-OFF ( n -- n ) 2 * RANGE-OFF + ;
: RANGE@ ( SPAN:span<cell> n -- n n )
   {: work id:n :}
   work id LO-OFF S@ work id LO-OFF 1+ S@ ;
: RANGE! ( n n SPAN:span<cell> n -- )
   {: lo:n hi:n work id:n :}
   lo work id LO-OFF S! hi work id LO-OFF 1+ S! ;
\ The smallest range holding both; an empty range adds nothing.
: JOIN ( n n n n -- n n )
   {: alo:n ahi:n blo:n bhi:n :}
   alo ahi = if blo bhi exit then
   blo bhi = if alo ahi exit then
   alo blo min ahi bhi max ;
: BOTH ( SPAN:span<cell> n n -- n n )
   {: work a:n b:n :}
   work a RANGE@ work b RANGE@ JOIN ;

\ The end of [lo, hi) past its last nonzero limb, lo when none is.
: TOP ( SPAN:span<cell> n n -- n )
   {: buf lo:n hi:n :}
   hi lo ?do
      lo hi + 1- i - {: at:n :}
      buf at SPAN:CELL@ 0<> if at 1+ unloop exit then
   loop lo ;
: BOTTOM ( SPAN:span<cell> n n -- n )
   {: buf lo:n hi:n :}
   hi lo ?do buf i SPAN:CELL@ 0<> if i unloop exit then loop hi ;
: TRIM ( SPAN:span<cell> n -- )
   {: work id:n :}
   work id LIMB-BUF {: buf :}
   work id RANGE@ {: lo:n hi:n :}
   buf lo hi TOP {: top:n :}
   buf lo top BOTTOM top work id RANGE! ;

: CLEAR ( SPAN:span<cell> n -- )
   {: work id:n :}
   0 work id LIMB-BUF work id RANGE@ PART SPAN:FILL
   0 0 work id RANGE! ;
\ Zeroes the limb buffers and their ranges from any scratch.
: RESET ( SPAN:span<cell> -- )
   {: work :}
   work 0 SHIFTED 1+ LIMBS * BUF ZERO
   SHIFTED 1+ 0 ?do 0 0 work i RANGE! loop ;
: START ( SPAN:span<cell> -- )
   {: work :}
   work POS CLEAR work NEG CLEAR ;

\ NEXT[at..] += PROD * digit, its carry stored past PROD's length.
: MUL-DIGIT ( SPAN:span<cell> n n -- )
   {: work digit:n at:n :}
   work PROD {: prod :} work NEXT {: next :}
   work PLEN-OFF S@ {: plen:n :}
   0 plen 0 ?do
      prod i SPAN:CELL@ digit * next i at + SPAN:CELL@ + +
      {: value:n :}
      value MASK and next i at + SPAN:CELL!
      value WIDTH rshift
   loop
   next plen at + SPAN:CELL! ;

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
   work NEXT ZERO
   3 0 ?do work sig i WIDTH * rshift MASK and i MUL-DIGIT loop
   work NEXT 1 work PLEN-OFF S@ 3 + TOP {: len:n :}
   len work PLEN-OFF S!
   work NEXT 0 len PART SPAN:$ work PROD 0 len PART SPAN:COPY ;

\ Adds the carry into buf from limb at up; answers the end it reached.
: CARRY ( n n SPAN:span<cell> -- n )
   {: buf :}
   begin over 0<> while
      {: carry:n at:n :}
      carry buf at SPAN:CELL@ + {: value:n :}
      value MASK and buf at SPAN:CELL!
      value WIDTH rshift at 1+
   repeat nip ;

: ADD-PROD ( SPAN:span<cell> n -- )
   {: work id:n :}
   work id LIMB-BUF {: buf :} work PROD {: prod :}
   work PLEN-OFF S@ {: plen:n :}
   work EXP-OFF S@ LOW-EXP - WIDTH /mod {: shift:n offset:n :}
   0 plen 0 ?do
      prod i SPAN:CELL@ shift lshift + buf offset i + SPAN:CELL@ +
      {: value:n :}
      value MASK and buf offset i + SPAN:CELL!
      value WIDTH rshift
   loop
   offset plen + buf CARRY {: top:n :}
   work id RANGE@ offset top JOIN work id RANGE! ;

: TERM ( SPAN:span<cell> r r r r r -- )
   {: work a:r b:r c:r d:r e:r :}
   1 work PROD 0 SPAN:CELL!
   1 work PLEN-OFF S! 0 work EXP-OFF S! 0 work SIGN-OFF S!
   work a MUL-FACTOR work b MUL-FACTOR work c MUL-FACTOR
   work d MUL-FACTOR work e MUL-FACTOR
   work SIGN-OFF S@ 0= if work POS else work NEG then ADD-PROD ;

\ The sign of a - b.
: BUF-CMP ( SPAN:span<cell> n n -- n )
   {: work a:n b:n :}
   work a LIMB-BUF {: abuf :} work b LIMB-BUF {: bbuf :}
   work a b BOTH {: lo:n hi:n :}
   hi lo ?do
      lo hi + 1- i - {: at:n :}
      abuf at SPAN:CELL@ bbuf at SPAN:CELL@ - {: diff:n :}
      diff 0<> if diff unloop exit then
   loop 0 ;

\ dst = a - b, for a >= b and dst one of a and b.
: SUBTRACT ( SPAN:span<cell> n n n -- )
   {: work a:n b:n dst:n :}
   work a LIMB-BUF {: abuf :} work b LIMB-BUF {: bbuf :}
   work dst LIMB-BUF {: dbuf :}
   work a b BOTH {: lo:n hi:n :}
   0 hi lo ?do
      abuf i SPAN:CELL@ bbuf i SPAN:CELL@ - swap - {: diff:n :}
      diff 0< if diff BASE + 1 else diff 0 then
      swap dbuf i SPAN:CELL!
   loop drop
   lo hi work dst RANGE! work dst TRIM ;

: COPY ( SPAN:span<cell> n n -- )
   {: work src:n dst:n :}
   work dst CLEAR
   work src RANGE@ {: lo:n hi:n :}
   work src LIMB-BUF lo hi PART SPAN:$
   work dst LIMB-BUF lo hi PART SPAN:COPY
   lo hi work dst RANGE! ;

: FINISH ( SPAN:span<cell> n -- n )
   {: work dst:n :}
   work POS NEG BUF-CMP 0< if
      work NEG POS POS SUBTRACT $8000000000000000
   else
      work POS NEG POS SUBTRACT 0
   then {: sign:n :}
   work POS dst COPY sign ;

\ floor(log2 value) of a positive limb.
: LOG2 ( n -- n )
   0 swap begin dup 1 > while 1 rshift swap 1+ swap repeat drop ;
: HIGH-BIT ( SPAN:span<cell> n -- n )
   {: work id:n :}
   work id LIMB-BUF {: buf :}
   work id RANGE@ {: lo:n hi:n :}
   buf lo hi TOP {: top:n :}
   top lo = if -1 exit then
   top 1- WIDTH * buf top 1- SPAN:CELL@ LOG2 + ;

\ SHIFTED = src * 2^count; limbs past LIMBS are dropped.
: SHIFT ( SPAN:span<cell> n n -- )
   {: work src:n count:n :}
   work SHIFTED CLEAR
   work src LIMB-BUF {: sbuf :} work SHIFTED LIMB-BUF {: dbuf :}
   work src RANGE@ {: lo:n hi:n :}
   count WIDTH /mod {: shift:n offset:n :}
   hi 1+ LIMBS offset - min {: end:n :}
   lo end >= if exit then
   0 end lo ?do
      sbuf i SPAN:CELL@ shift lshift + {: value:n :}
      value MASK and dbuf offset i + SPAN:CELL!
      value WIDTH rshift
   loop drop
   lo offset + end offset + work SHIFTED RANGE! work SHIFTED TRIM ;

\ buf id = floor(buf id / 2).
: HALVE ( SPAN:span<cell> n -- )
   {: work id:n :}
   work id LIMB-BUF {: buf :}
   work id RANGE@ {: lo:n hi:n :}
   lo 1- 0 max {: from:n :}
   hi from ?do
      buf i SPAN:CELL@ 1 rshift
      i 1+ hi < if
         buf i 1+ SPAN:CELL@ 1 and WIDTH 1- lshift or
      then buf i SPAN:CELL!
   loop
   from hi work id RANGE! work id TRIM ;

\ floor(log2(NUM / DEN)) for nonzero NUM and DEN.
: RATIO-EXP ( SPAN:span<cell> -- n )
   {: work :}
   work NUM HIGH-BIT work DEN HIGH-BIT - {: guess:n :}
   guess 0< if
      work NUM guess negate SHIFT work SHIFTED DEN BUF-CMP
   else
      work DEN guess SHIFT work NUM SHIFTED BUF-CMP
   then 0< if guess 1- else guess then ;

\ Scales NUM or DEN so that NUM / DEN is the significand, unrounded.
: NORMALISE ( SPAN:span<cell> n -- )
   {: work rexp:n :}
   rexp 52 - -1074 max {: shift:n :}
   shift 0< if
      work NUM shift negate SHIFT work SHIFTED NUM COPY
   else
      work DEN shift SHIFT work SHIFTED DEN COPY
   then ;

\ floor(NUM / DEN) below 2^53, leaving the remainder in NUM.
: QUOTIENT ( SPAN:span<cell> -- n )
   {: work :}
   work DEN 52 SHIFT
   0 53 0 ?do
      i 0<> if work SHIFTED HALVE then
      work NUM SHIFTED BUF-CMP 0 >= if
         work NUM SHIFTED NUM SUBTRACT
         1 52 i - lshift or
      then
   loop ;

\ The quotient rounded to nearest even by its remainder.
: ROUND ( SPAN:span<cell> n -- n )
   {: work quot:n :}
   work NUM 1 SHIFT
   work SHIFTED DEN BUF-CMP {: cmp:n :}
   cmp 0 > cmp 0= quot 1 and 0<> and or if quot 1+ else quot then ;

: PACK ( n n n -- n )
   {: quot:n rexp:n sign:n :}
   rexp -1022 < if quot sign or exit then
   quot $20000000000000 = if quot 1 rshift rexp 1+
   else quot rexp then {: sig:n e:n :}
   e 1023 + {: exponent:n :}
   exponent $7FF >= if FMATH:E-OUTPUT throw then
   exponent 52 lshift sig $FFFFFFFFFFFFF and or sign or ;

: RATIO-BITS ( SPAN:span<cell> n -- n )
   {: work sign:n :}
   work NUM HIGH-BIT 0< if 0 exit then
   work RATIO-EXP {: rexp:n :}
   rexp 1024 > if FMATH:E-OUTPUT throw then
   rexp -1075 < if sign exit then
   work rexp NORMALISE
   work work QUOTIENT ROUND rexp sign PACK ;

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
   work DEN FINISH {: dsign:n :}
   work DEN HIGH-BIT 0< if FMATH:E-DOMAIN throw then
   work START
   work frame px py pz sx sy sz row N-TERMS
   work NUM FINISH dsign xor {: sign:n :}
   work sign RATIO-BITS IEEE754:BITS>F64 ;

: NO-OVERLAP? ( SPAN:span<cell> SPAN:span<cell> -- bool )
   {: frame work :}
   frame SPAN:BYTES SPAN:$ {: fp:^ flen:n :}
   work SPAN:BYTES SPAN:$ {: wp:^ wlen:n :}
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
   px FMATH:FINITE? py FMATH:FINITE? and pz FMATH:FINITE? and
   sx FMATH:FINITE? and sy FMATH:FINITE? and sz FMATH:FINITE? and 0= if
      FMATH:E-DOMAIN throw
   then
   12 0 ?do
      frame i SPAN:CELL@ IEEE754:BITS>F64 FMATH:FINITE? 0= if
         FMATH:E-DOMAIN throw
      then
   loop
   work RESET
   work frame px py pz sx sy sz 0 COORD
   work frame px py pz sx sy sz 1 COORD
   work frame px py pz sx sy sz 2 COORD ;

;package
