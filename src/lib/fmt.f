\ fmt.f - checked number formatting, package FMT: appenders into src/lib/string.f's
\ SB and direct printers. Integers render through a private buffer, so FMT:.U and
\ FMT:.INT leave SB alone; FMT:F.N has no bounded width and resets SB. Every
\ appender is fail-closed on its domain (E-FMT-DOMAIN, E-FMT-OVERFLOW).
\ STORAGE CLASS. TASK-LOCAL: the render buffer and scratch cells live in the
\ FMT-ABI band of the per-task DATA region; SB is task-local too.

require src/lib/float.f
require src/lib/string.f
require src/lib/ieee754.f

package FMT

48 constant FMT-ZERO
65 constant FMT-A
46 constant FMT-DOT

\ The FMT-ABI band is declared in src/habu/layout.f beside STRING-ABI and
\ asserted there against every other DATA claim. It is a declared band and not
\ TASK:+USER rows because src/habu/habu2.f requires this module for number text:
\ a `require src/lib/task.f` here would pull pthread, mmap and the FFI staging
\ tables into the base image for 52 bytes of scratch. A region is a fresh zeroed
\ mapping, so a new task's cells start at zero.
: FMT-IX ( -- ptr n )
   data-base FMT-ABI:IX-OFF + ;

: FMT-FR ( -- ptr n )
   data-base FMT-ABI:FR-OFF + ;

: FMT-DV ( -- ptr n )
   data-base FMT-ABI:DV-OFF + ;

\ An i64 never renders wider than one sign byte plus the STR-I64-DIGITS digits
\ of its magnitude, so the buffer is exact by construction and the renderer
\ needs no capacity check.

STR-I64-DIGITS 1+ constant FMT-NUM-CAP
\ src/habu/layout.f is loaded before src/lib/string.f and cannot see
\ STR-I64-DIGITS, so FMT-ABI:NUM-BUF-BYTES states the width there and this
\ executes the agreement once at load, the way src/habu/rt.f executes
\ RT:DSTACK-AGREE. A narrower band would let the renderer overrun the band's
\ top into STRING-ABI; a wider one would reserve bytes no renderer uses.
: BAND-AGREE ( -- )
   FMT-NUM-CAP FMT-ABI:NUM-BUF-BYTES <> if E-FMT-BAND throw then ;
BAND-AGREE

: FMT-NUM-BUF ( -- ptr u8 )
   data-base FMT-ABI:NUM-BUF-OFF + BYTE-VIEW ;

: FMT-NUM-U ( -- ptr n )
   data-base FMT-ABI:NUM-U-OFF + ;

: NUM-C+ ( n -- )
   FMT-NUM-BUF FMT-NUM-U @ + c!
   FMT-NUM-U @ 1+ FMT-NUM-U ! ;

: NUM$ ( -- ptr u8 n )
   FMT-NUM-BUF FMT-NUM-U @ ;

: DIGITS>NUM ( n -- )                      \ nonnegative magnitude, most significant digit first
   dup 10 < if FMT-ZERO + NUM-C+ exit then
   dup 10 / RECURSE  10 mod FMT-ZERO + NUM-C+ ;

: U>NUM ( n -- )                           \ unsigned; negative -> E-FMT-DOMAIN before any byte lands
   dup 0 < if E-FMT-DOMAIN throw then
   0 FMT-NUM-U !  DIGITS>NUM ;

: HEX-C+ ( n -- )
   dup 10 < if FMT-ZERO + else 10 - FMT-A + then NUM-C+ ;

: HEX-DIGITS>NUM ( n -- )
   dup 16 < if HEX-C+ exit then
   dup 16 / RECURSE  16 mod HEX-C+ ;

: HEX>NUM ( n -- )
   dup 0 < if E-FMT-DOMAIN throw then
   0 FMT-NUM-U !  HEX-DIGITS>NUM ;

\ Exactly `width` digits of the value's bit pattern, most significant first;
\ a 64-bit cell holds 16 digits of 4 bits. Below full width the value must
\ fit: the bits above the digits must be zero, which also refuses every
\ negative value.
: HEX-WIDTH>NUM ( n n -- )
   {: value:n width:n :}
   width 1 < width 16 > or if E-FMT-DOMAIN throw then
   width 16 < if
      value width 4 * rshift 0<> if E-FMT-DOMAIN throw then
   then
   0 FMT-NUM-U !
   width begin dup 0 > while
      1- value over 4 * rshift 15 and HEX-C+
   repeat drop ;

: INT>NUM ( n -- )                         \ signed (STR-MIN-I64 has no positive magnitude)
   0 FMT-NUM-U !
   dup STR-MIN-I64 = if
      drop STR-MINUS NUM-C+
      STR-MIN-I64$ FMT-NUM-BUF FMT-NUM-U @ + STR-I64-DIGITS BYTE-COPY
      FMT-NUM-U @ STR-I64-DIGITS + FMT-NUM-U ! exit
   then
   dup 0 < if STR-MINUS NUM-C+ negate then DIGITS>NUM ;

public

: SB-U ( n -- )                            \ unsigned int, no separators; negative -> E-FMT-DOMAIN
   U>NUM NUM$ SB-APPEND ;
: SB-HEX ( n -- )                          \ uppercase, no prefix; negative -> E-FMT-DOMAIN
   HEX>NUM NUM$ SB-APPEND ;
: SB-HEX-WIDTH ( n n -- )                  \ value width: exactly width uppercase digits, zero-padded
   HEX-WIDTH>NUM NUM$ SB-APPEND ;
: SB-INT ( n -- )
   INT>NUM NUM$ SB-APPEND ;

\ Interpret value as value / 10^places, places 0 through 18. Trailing
\ fractional zeros and a bare point are trimmed, and the zero before a fraction
\ below one is kept: 5500 3 is 5.5, -1 3 is -0.001. The integer renderer
\ supplies all digits, including STR-MIN-I64, without taking a magnitude that
\ could overflow; the point and fraction append separately so FMT-NUM-CAP
\ stays unchanged.
: SB-SCALED ( n n -- )
   {: value:n places:n :}
   places 0 < places 18 > or if E-FMT-DOMAIN throw then
   value INT>NUM
   places 0= if NUM$ SB-APPEND exit then
   value 0= if FMT-ZERO SB-APPEND-C exit then

   NUM$ {: num-p:ptr width:n :}
   value 0 < if 1 else 0 then {: sign:n :}
   num-p sign + {: p:ptr :}
   width sign - {: digits:n :}
   0 FMT-IX !
   begin
      FMT-IX @ places < FMT-IX @ digits < and
      if p digits 1- + FMT-IX @ - c@ FMT-ZERO = else false then
   while
      1 FMT-IX +!
   repeat

   sign 0<> if STR-MINUS SB-APPEND-C then
   digits places > if
      p digits places - SB-APPEND
   else
      FMT-ZERO SB-APPEND-C
   then
   places FMT-IX @ - {: frac:n :}
   frac 0= if exit then
   FMT-DOT SB-APPEND-C
   digits places < if
      places digits - 0 ?do FMT-ZERO SB-APPEND-C loop
      p digits FMT-IX @ - SB-APPEND
   else
      p digits places - + frac SB-APPEND
   then ;

private

: POW10I ( n -- n ) {: k :}
   1  0 FMT-IX !
   begin FMT-IX @ k < while
      10 *  FMT-IX @ 1+ FMT-IX !
   repeat ;

\ append frac (0 .. 10^k-1) as exactly k zero-padded digits
: SB-FRAC ( n n -- ) {: frac k :}
   k 0= if exit then
   frac FMT-FR !  k 1- POW10I FMT-DV !
   begin FMT-DV @ 0 > while
      FMT-FR @ FMT-DV @ / FMT-ZERO + SB-APPEND-C
      FMT-FR @ FMT-DV @ mod FMT-FR !
      FMT-DV @ 10 / FMT-DV !
   repeat ;

\ Fits-i64 guard: a nonnegative rounded magnitude is representable iff it stays
\ below 2^63 (STR-MAX-I64 s>f rounds to 2^63, the first double f>s cannot hold).
\ Beyond that, f>s would saturate to STR-MAX-I64 and print wrong digits.
: FIT-I64 ( r -- )
   STR-MAX-I64 s>f f< 0= if E-FMT-OVERFLOW throw then ;

\ SB-FIX appends a float with exactly k decimal places, rounded half-up (k=0
\ omits the point).
public

: SB-FIX ( r n -- ) {: k :}
   dup f0< if STR-MINUS SB-APPEND-C fnegate then
   k POW10 f*  0.5 f+                       \ rounded scaled magnitude ( r >= 0 )
   dup FIT-I64
   f>s {: scaled:n :}
   k POW10I {: ps :}
   scaled ps / SB-U
   k 0 > if FMT-DOT SB-APPEND-C  scaled ps mod k SB-FRAC then ;

\ Convert the exact finite binary64 value m*2^e to a decimal coefficient.
\ For e>=0 the coefficient is m*2^e; for e<0 it is m*5^-e with -e decimal
\ places. Base 10^9 limbs stay below 5*10^9 during multiplication, so signed
\ cell arithmetic is exact. The greatest coefficient is (2^53-1)*5^1074:
\ 767 digits, fitting FMT-ABI's 86 task-local limbs. Only the leading k+2
\ digits are needed to round to k+1 significant digits, with k at most 17.
private
$FFFFFFFFFFFFF constant SCI-FRAC-MASK
$10000000000000 constant SCI-HIDDEN-BIT
$7FF0000000000000 constant SCI-EXP-MASK
$7FFFFFFFFFFFFFFF constant SCI-MAG-MASK
1000000000 constant SCI-BASE

: SCI-LIMB ( n -- ptr n )
   data-base FMT-ABI:SCI-LIMBS-OFF + swap cells + ;

: SCI-PARTS ( n -- n n )
   {: bits:n :}
   bits 52 rshift 2047 and {: field:n :}
   bits SCI-FRAC-MASK and {: fraction:n :}
   field 0= if fraction -1074 exit then
   fraction SCI-HIDDEN-BIT or  field 1075 - ;

: SCI-INIT ( n -- )
   {: mantissa:n :}
   mantissa SCI-BASE mod 0 SCI-LIMB !
   mantissa SCI-BASE / 1 SCI-LIMB !
   1 SCI-LIMB @ 0= if 1 else 2 then FMT-IX ! ;

: SCI-MUL ( n -- )
   {: factor:n :}
   0 FMT-FR !
   FMT-IX @ 0 ?do
      i SCI-LIMB @ factor * FMT-FR @ + {: product:n :}
      product SCI-BASE mod i SCI-LIMB !
      product SCI-BASE / FMT-FR !
   loop
   FMT-FR @ 0 > if
      FMT-FR @ FMT-IX @ SCI-LIMB !
      FMT-IX @ 1+ FMT-IX !
   then ;

: SCI-DIGITS ( n -- n )
   0 swap
   begin dup 0 > while
      10 / swap 1+ swap
   repeat drop ;

\ Extend the render buffer with the next limb's nine digits, including zeros.
: SCI-ADD-LIMB ( n n -- )
   {: limb:n target:n :}
   100000000 FMT-DV !
   9 0 ?do
      FMT-NUM-U @ target < if
         limb FMT-DV @ / 10 mod FMT-ZERO + NUM-C+
      then
      FMT-DV @ 10 / FMT-DV !
   loop ;

: SCI-LEADING ( n -- )
   {: target:n :}
   FMT-IX @ 1- SCI-LIMB @ U>NUM
   FMT-NUM-U @ target >= if target FMT-NUM-U ! exit then
   FMT-IX @ 1- 0 ?do
      FMT-IX @ 2 - i - SCI-LIMB @ target SCI-ADD-LIMB
   loop
   begin FMT-NUM-U @ target < while FMT-ZERO NUM-C+ repeat ;

\ One guard digit rounds half-up. The coefficient is at most 10^18, fitting
\ signed i64 even when all nines carry. A carry advances the decimal exponent.
: SCI-COEFF ( n -- n bool )
   {: places:n :}
   0 places 1+ 0 ?do
      10 * FMT-NUM-BUF i + c@ FMT-ZERO - +
   loop
   FMT-NUM-BUF places 1+ + c@ [char] 5 >= if 1+ then
   dup places 1+ POW10I = if 10 / true else false then ;

: SCI-DECIMAL ( n n -- n n )
   {: bits:n places:n :}
   bits SCI-PARTS {: mantissa:n exponent:n :}
   mantissa SCI-INIT
   exponent 0 >= if
      exponent 0 ?do 2 SCI-MUL loop
      0
   else
      exponent negate 0 ?do 5 SCI-MUL loop
      exponent negate
   then {: shift:n :}
   FMT-IX @ 1- SCI-LIMB @ SCI-DIGITS
   FMT-IX @ 1- 9 * + shift - 1- {: decimal-exponent:n :}
   places 2 + SCI-LEADING
   places SCI-COEFF {: coefficient:n carry:bool :}
   coefficient
   carry if decimal-exponent 1+ else decimal-exponent then ;

public
: SB-SCI ( r n -- )
   {: k:n :}
   k 0 < k 17 > or if E-FMT-DOMAIN throw then
   dup IEEE754:F64>BITS {: bits:n :}
   bits SCI-EXP-MASK and SCI-EXP-MASK = if
      E-FMT-DOMAIN throw
   then
   drop
   bits SCI-MAG-MASK and 0= if
      bits 0 < if STR-MINUS SB-APPEND-C then
      0.0 k SB-FIX
      s" e0" SB-APPEND exit
   then
   bits SCI-MAG-MASK and k SCI-DECIMAL
      {: coefficient:n exponent:n :}
   bits 0 < if STR-MINUS SB-APPEND-C then
   k POW10I {: scale:n :}
   coefficient scale / SB-U
   k 0 > if FMT-DOT SB-APPEND-C  coefficient scale mod k SB-FRAC then
   [char] e SB-APPEND-C
   exponent SB-INT ;

: .U ( n -- )     U>NUM   NUM$ type ;          \ unsigned, no trailing space, SB untouched
: .INT ( n -- )   INT>NUM NUM$ type ;          \ signed, no trailing space, SB untouched
: F.N ( r n -- )  SB-RESET SB-FIX SB$ type ;   \ unbounded width: assembled in the shared builder

;package
