split-str
\ fmath-basis.f - exact binary64 basis scaling in `package FBASIS`: SCALE3 maps p
\ to o + A diag(s) A^-1 (p-o) and rounds each coordinate once, to nearest even.
\ The frame is twelve bit patterns by rows [a00 a01 a02 o0, ...], axes in the
\ first three columns; scratch holds WORK-CELLS cells and must not overlap it.
\ 408 base-2^26 limbs hold every value: bit 0 is 2^LOW-EXP, five times -1074;
\ a sum of 42 products of five finite doubles is below 2^(10496 + LOW-EXP),
\ limb 404, and a denominator shifted up 52 bits is below limb 406.
\ The scratch holds the limb buffers and the state words pass between them. A
\ limb buffer follows its live range [lo, hi), outside which its limbs are zero:
\ SCALE3 clears every buffer once, and each limb loop runs over its ranges only.
\ The quotient is a restoring division: the denominator is shifted up 52 bits
\ once and halved in place before each later quotient bit.
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

\ A limb buffer's lo and hi cells are the two cells before it.
2 constant POS-OFF
412 constant NEG-OFF
822 constant NUM-OFF
1232 constant DEN-OFF
1642 constant SHIFT-OFF
\ PROD and NEXT hold 16 limbs: five 53-bit significands make at most 11, and
\ MUL-FACTOR writes NEXT to 3 past PROD's length.
2050 constant PROD-OFF
2066 constant NEXT-OFF
2082 constant PLEN-OFF
2083 constant EXP-OFF
2084 constant SIGN-OFF
2085 constant NSIGN-OFF
2086 constant DSIGN-OFF

: POS ( ^cell -- ^cell ) POS-OFF cells + ;
: NEG ( ^cell -- ^cell ) NEG-OFF cells + ;
: PROD ( ^cell -- ^cell ) PROD-OFF cells + ;
: NEXT ( ^cell -- ^cell ) NEXT-OFF cells + ;
: NUM ( ^cell -- ^cell ) NUM-OFF cells + ;
: DEN ( ^cell -- ^cell ) DEN-OFF cells + ;
: SHIFTED ( ^cell -- ^cell ) SHIFT-OFF cells + ;

: LO ( ^cell -- ^cell ) -2 cells + ;
: HI ( ^cell -- ^cell ) -1 cells + ;
: RANGE ( ^cell -- n n ) dup LO @ swap HI @ ;
: UNION ( ^cell ^cell -- n n )
   RANGE rot RANGE {: lo:n hi:n lo2:n hi2:n :}
   lo lo2 min hi hi2 max ;
\ An empty range is [LIMBS, 0), so a union is a min and a max.
: EMPTY ( ^cell -- )
   {: b:^ :}
   LIMBS b LO ! 0 b HI ! ;
: EXTEND ( n n ^cell -- )
   {: lo:n hi:n b:^ :}
   b LO @ lo min b LO !
   b HI @ hi max b HI ! ;
\ Loop limits of a range; an empty one gives no passes.
: LIMITS ( n n -- n n ) over max swap ;

: ZERO ( ^cell n -- )
   {: b:^ len:n :}
   len 0 ?do 0 b i cells + ! loop ;
: CLEAR ( ^cell -- ) dup LIMBS ZERO EMPTY ;
: RESET ( ^cell -- )
   {: b:^ :}
   b RANGE LIMITS ?do 0 b i cells + ! loop
   b EMPTY ;
: START ( ^cell -- )
   {: w:^ :}
   w POS RESET w NEG RESET ;
: PREPARE ( ^cell -- )
   {: w:^ :}
   w POS CLEAR w NEG CLEAR w NUM CLEAR
   w DEN CLEAR w SHIFTED CLEAR ;

\ dst[0, len) += src[0, len) * digit; the carry out is stored in dst[len].
: MAC ( ^cell n ^cell n -- )
   {: src:^ len:n dst:^ digit:n :}
   digit 0= if 0 dst len cells + ! exit then
   0 len 0 ?do
      src i cells + @ digit * dst i cells + @ + + {: value:n :}
      value MASK and dst i cells + !
      value WIDTH rshift
   loop dst len cells + ! ;
: TRIM ( n ^cell -- n )
   {: b:^ :}
   begin dup 1 > over 1- b swap cells + @ 0= and while 1- repeat ;

\ A zero product stays zero and adds nothing, whatever its sign and exponent.
: ZERO? ( ^cell -- bool )
   {: w:^ :}
   w PLEN-OFF cells + @ 1 = w PROD @ 0= and ;
: MUL-FACTOR ( ^cell r -- )
   {: w:^ x:r :}
   w ZERO? if exit then
   x IEEE754:F64>BITS {: bits:n :}
   bits 63 rshift w SIGN-OFF cells + @ xor w SIGN-OFF cells + !
   bits 52 rshift $7FF and {: exponent:n :}
   exponent $7FF = if FMATH:E-DOMAIN throw then
   exponent 0<> if
      bits $FFFFFFFFFFFFF and $10000000000000 or
      exponent 1075 -
   else bits $FFFFFFFFFFFFF and -1074 then
   w EXP-OFF cells + @ + w EXP-OFF cells + ! {: sig:n :}
   w PLEN-OFF cells + @ {: plen:n :}
   \ The first pass reads NEXT[0, plen); later ones read only what earlier
   \ passes wrote.
   w NEXT plen ZERO
   3 0 ?do
      w PROD plen w NEXT i cells + sig i WIDTH * rshift MASK and MAC
   loop
   plen 3 + w NEXT TRIM {: len:n :}
   len w PLEN-OFF cells + !
   len 0 ?do w NEXT i cells + @ w PROD i cells + ! loop ;

\ dst[0, len) += src[0, len) << shift; leaves the carry out.
: ADD-SHIFTED ( ^cell n n ^cell -- n )
   {: src:^ len:n shift:n dst:^ :}
   0 len 0 ?do
      src i cells + @ shift lshift + dst i cells + @ + {: value:n :}
      value MASK and dst i cells + !
      value WIDTH rshift
   loop ;
\ Adds the carry into b from limb k up; leaves the end of what it wrote.
: CARRY ( n n ^cell -- n )
   {: b:^ :}
   begin dup 0<> while
      {: k:n carry:n :}
      carry b k cells + @ + {: value:n :}
      value MASK and b k cells + !
      k 1+ value WIDTH rshift
   repeat drop ;
\ Skipping a zero product keeps its offset out of b's range.
: ADD-PROD ( ^cell ^cell -- )
   {: w:^ b:^ :}
   w ZERO? if exit then
   w PLEN-OFF cells + @ {: plen:n :}
   w EXP-OFF cells + @ LOW-EXP - WIDTH /mod {: shift:n offset:n :}
   w PROD plen shift b offset cells + ADD-SHIFTED
   offset plen + swap b CARRY {: end:n :}
   offset end b EXTEND ;

: TERM ( ^cell r r r r r -- )
   {: w:^ a:r b:r c:r d:r e:r :}
   1 w PROD !
   1 w PLEN-OFF cells + ! 0 w EXP-OFF cells + ! 0 w SIGN-OFF cells + !
   w a MUL-FACTOR w b MUL-FACTOR w c MUL-FACTOR
   w d MUL-FACTOR w e MUL-FACTOR
   w SIGN-OFF cells + @ 0= if w w POS else w w NEG then ADD-PROD ;

\ The first non-zero a[k] - b[k] from the top, or 0.
: BUF-CMP ( ^cell ^cell -- n )
   {: a:^ b:^ :}
   a b UNION {: lo:n hi:n :}
   hi begin dup lo > while
      1- {: k:n :}
      a k cells + @ b k cells + @ - {: diff:n :}
      diff 0<> if diff exit then
      k
   repeat drop 0 ;

\ dst = a - b for a >= b; dst is a or b.
: SUBTRACT ( ^cell ^cell ^cell -- )
   {: a:^ b:^ dst:^ :}
   a b UNION {: lo:n hi:n :}
   0 lo hi LIMITS ?do
      a i cells + @ b i cells + @ - swap - {: diff:n :}
      diff 0< if diff BASE + 1 else diff 0 then
      swap dst i cells + !
   loop drop
   lo hi dst EXTEND ;

: COPY ( ^cell ^cell -- )
   {: src:^ dst:^ :}
   dst RESET
   src RANGE LIMITS ?do src i cells + @ dst i cells + ! loop
   src RANGE dst EXTEND ;

: FINISH ( ^cell ^cell -- n )
   {: w:^ dst:^ :}
   w POS w NEG BUF-CMP 0< if
      w NEG w POS w POS SUBTRACT $8000000000000000
   else
      w POS w NEG w POS SUBTRACT 0
   then {: sign:n :}
   w POS dst COPY sign ;

: LOG2 ( n -- n )
   0 swap begin dup 1 > while 1 rshift swap 1+ swap repeat drop ;
: HIGH-BIT ( ^cell -- n )
   {: b:^ :}
   b RANGE {: lo:n hi:n :}
   hi begin dup lo > while
      1- {: k:n :}
      b k cells + @ {: value:n :}
      value 0<> if k WIDTH * value LOG2 + exit then
      k
   repeat drop -1 ;

\ dst = src << count, dropping bits past LIMBS.
: SHIFT ( ^cell n ^cell -- )
   {: src:^ count:n dst:^ :}
   dst RESET
   count WIDTH /mod {: shift:n offset:n :}
   src RANGE {: lo:n hi:n :}
   hi LIMBS offset - min {: top:n :}
   lo top < if
      src lo cells + top lo - shift dst offset lo + cells + ADD-SHIFTED
      {: carry:n :}
      top offset + {: end:n :}
      end LIMBS < if carry dst end cells + ! end 1+ else end then
      lo offset + swap dst EXTEND
   then ;

\ b >>= 1 in place; the bit shifted out of b is zero. The range grows down
\ only when b[lo]'s low bit moves into limb lo - 1.
: HALVE ( ^cell -- )
   {: b:^ :}
   b RANGE {: lo:n hi:n :}
   lo hi < if
      lo 0 > b lo cells + @ 1 and 0<> and if lo 1- else lo then
      {: start:n :}
      hi 1- start ?do
         b i cells + @ 1 rshift
         b i 1+ cells + @ 1 and WIDTH 1- lshift or b i cells + !
      loop
      b hi 1- cells + @ 1 rshift b hi 1- cells + !
      start hi b EXTEND
   then ;

\ Whether NUM / DEN < 2^rough.
: BELOW? ( ^cell n -- bool )
   {: w:^ rough:n :}
   rough 0< if
      w NUM rough negate w SHIFTED SHIFT
      w SHIFTED w DEN BUF-CMP
   else
      w DEN rough w SHIFTED SHIFT
      w NUM w SHIFTED BUF-CMP
   then 0< ;
: ALIGN ( ^cell n -- )
   {: w:^ shift:n :}
   shift 0< if
      w NUM shift negate w SHIFTED SHIFT
      w SHIFTED w NUM COPY
   else
      w DEN shift w SHIFTED SHIFT
      w SHIFTED w DEN COPY
   then ;
: STEP ( ^cell n n -- n )
   {: w:^ quot:n bit:n :}
   w NUM w SHIFTED BUF-CMP 0< if quot exit then
   w NUM w SHIFTED w NUM SUBTRACT
   quot 1 bit lshift or ;
: DIVIDE ( ^cell -- n )
   {: w:^ :}
   w DEN 52 w SHIFTED SHIFT
   0 53 0 ?do
      i 0<> if w SHIFTED HALVE then
      w swap 52 i - STEP
   loop ;
: ROUND ( ^cell n -- n )
   {: w:^ quot:n :}
   w NUM 1 w SHIFTED SHIFT
   w SHIFTED w DEN BUF-CMP {: cmp:n :}
   cmp 0 > cmp 0= quot 1 and 0<> and or if quot 1+ else quot then ;
: PACK ( n n n -- n )
   {: quot:n rexp:n sign:n :}
   rexp -1022 < if quot sign or exit then
   quot $20000000000000 = if quot 1 rshift rexp 1+ else quot rexp then
   {: q:n e:n :}
   e 1023 + {: exponent:n :}
   exponent $7FF >= if FMATH:E-OUTPUT throw then
   exponent 52 lshift q $FFFFFFFFFFFFF and or sign or ;

: RATIO-BITS ( ^cell n -- n )
   {: w:^ dtop:n :}
   w NUM HIGH-BIT {: ntop:n :}
   w NSIGN-OFF cells + @ w DSIGN-OFF cells + @ xor {: sign:n :}
   ntop 0< if 0 exit then
   ntop dtop - {: rough:n :}
   w rough BELOW? if rough 1- else rough then {: rexp:n :}
   rexp 1024 > if FMATH:E-OUTPUT throw then
   rexp -1075 < if sign exit then
   w rexp 52 - -1074 max ALIGN
   w w DIVIDE ROUND rexp sign PACK ;

: AXIS ( ^cell n n -- r )
   {: frame:^ row:n col:n :}
   frame row 4 * col + cells + @ IEEE754:BITS>F64 ;
: ORIGIN ( ^cell n -- r )
   {: frame:^ row:n :}
   frame row 4 * 3 + cells + @ IEEE754:BITS>F64 ;
: CHOOSE3 ( r r r n -- r )
   {: x:r y:r z:r idx:n :}
   idx 0= if x exit then idx 1 = if y exit then z ;
: COF-A ( ^cell n n n -- r )
   {: frame:^ k:n i:n t:n :}
   frame k 1+ 3 mod i 1+ t + 3 mod AXIS ;
: COF-B ( ^cell n n n -- r )
   {: frame:^ k:n i:n t:n :}
   frame k 2 + 3 mod i 2 + t - 3 mod AXIS ;
: D-TERMS ( ^cell ^cell r -- )
   {: w:^ frame:^ factor:r :}
   3 0 ?do 2 0 ?do
      w factor frame 0 j AXIS
      frame 0 j i COF-A
      frame 0 j i COF-B
      i 0<> if fnegate then 1.0 TERM
   loop loop ;
: DIFF-FACTOR ( r r n -- r )
   {: point:r origin:r term:n :}
   term 0= if point else origin fnegate then ;

: N-COF ( ^cell ^cell r r r n n n -- )
   {: w:^ frame:^ point:r origin:r scale:r row:n col:n k:n :}
   2 0 ?do 2 0 ?do
      w frame row col AXIS scale
      frame k col j COF-A frame k col j COF-B
      j 0<> if fnegate then
      point origin i DIFF-FACTOR TERM
   loop loop ;

: N-TERMS ( ^cell ^cell r r r r r r n -- )
   {: w:^ frame:^ px:r py:r pz:r sx:r sy:r sz:r row:n :}
   w frame frame row ORIGIN D-TERMS
   3 0 ?do 3 0 ?do
      w frame px py pz i CHOOSE3 frame i ORIGIN
      sx sy sz j CHOOSE3 row j i N-COF
   loop loop ;

: COORD ( ^cell ^cell r r r r r r n -- r )
   {: w:^ frame:^ px:r py:r pz:r sx:r sy:r sz:r row:n :}
   w START
   w frame 1.0 D-TERMS
   w w DEN FINISH w DSIGN-OFF cells + !
   w DEN HIGH-BIT {: dtop:n :}
   dtop 0< if FMATH:E-DOMAIN throw then
   w START
   w frame px py pz sx sy sz row N-TERMS
   w w NUM FINISH w NSIGN-OFF cells + !
   w dtop RATIO-BITS IEEE754:BITS>F64 ;

: NO-OVERLAP? ( SPAN:span<cell> SPAN:span<cell> -- bool )
   {: frame work :}
   frame SPAN:BYTES SPAN:$ {: fp:^ flen:n :}
   work SPAN:BYTES SPAN:$ {: wp:^ wlen:n :}
   fp NULL-PTR - wp NULL-PTR - {: fa:n wa:n :}
   fa wa <= if wa fa - flen >= else fa wa - wlen >= then ;
: FINITE-FRAME? ( ^cell -- bool )
   {: frame:^ :}
   true 12 0 ?do
      frame i cells + @ IEEE754:BITS>F64 FMATH:FINITE? and
   loop ;

public

2087 constant WORK-CELLS

\ The length check and the header's limb bounds keep every offset the private
\ words form inside the work buffer, so they address its cells directly.
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
   frame 0 SPAN:CELL-AT work 0 SPAN:CELL-AT {: f:^ w:^ :}
   f FINITE-FRAME? 0= if FMATH:E-DOMAIN throw then
   w PREPARE
   w f px py pz sx sy sz 0 COORD
   w f px py pz sx sy sz 1 COORD
   w f px py pz sx sy sz 2 COORD ;

;package
