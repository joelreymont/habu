\ f64-text.f - exact finite binary64 text computed here in integers: PARSE
\ rounds a decimal to the nearest binary64 and FORMAT writes the shortest
\ decimal that reads back. Neither reads the locale or the rounding mode, and
\ neither calls anything foreign. Output is copied only after success.
\ STORAGE CLASS. TASK-LOCAL: the work buffers. No conversion yields.

require src/lib/error.f
require src/lib/fmath.f
require src/lib/ieee754.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/adt/result.f

package F64-TEXT
using MEM

public
\ Always enough for FORMAT: the length of the negative least subnormal.
327 constant MAX-BYTES
\ `trailing`: bytes after a decimal; `nonfinite`: nan, inf, infinity, overflow;
\ `capacity`: FORMAT room below 0 or short; `runtime`: never, callers match it.
ENUM fault malformed trailing nonfinite capacity runtime ;ENUM

private
-9860 constant E-FIRST
-9869 constant E-LAST
-9860 s" a number needs more limbs than its capacity" ERROR:CODE E-LIMBS
$7FFFFFFFFFFFFFFF constant MAGNITUDE-MASK
\ FORMAT's work: its length cell at 112, its text at 128, its number at BIG-AT.
\ PARSE's work is its number alone, of the limbs LIMBS gives.
512 constant BIG-AT
39 constant FORMAT-LIMBS
BIG-AT FORMAT-LIMBS 2 + 8 * + constant FORMAT-BYTES

: DIGIT? ( n -- bool ) dup $30 >= swap $39 <= and ;
: DIGITS ( ptr u8 n n -- n ) {: text:ptr len:n offset:n :}
   offset begin dup len < if text over + c@ DIGIT? else false then while 1+ repeat ;
: SIGN-SKIP ( ptr u8 n n -- n ) {: text:ptr len:n offset:n :}
   offset len < if text offset + c@ dup $2B = swap $2D = or if offset 1+ exit then then offset ;
: EXPONENT? ( n -- bool ) dup $65 = swap $45 = or ;

\ Zero means complete decimal; one means malformed; two means trailing bytes.
: FRACTION-END ( ptr u8 n n -- n ) {: text:ptr len:n offset:n :}
   offset len < if text offset + c@ $2E = if
      text len offset 1+ DIGITS exit
   then then offset ;

: GRAMMAR ( ptr u8 n -- n ) {: text:ptr len:n :}
   len 0 <= if 1 exit then
   text len 0 SIGN-SKIP {: first:n :}
   text len first DIGITS {: integer-end:n :}
   text len integer-end FRACTION-END {: end:n :}
   integer-end first = if end integer-end - 1 <= if 1 exit then then
   end len < if text end + c@ EXPONENT? if
      text len end 1+ SIGN-SKIP {: exponent-start:n :}
      text len exponent-start DIGITS dup exponent-start = if drop 1 exit then
   else end then else end then
   len = if 0 else 2 then ;

: NONFINITE-TEXT? ( ptr u8 n -- bool )
   2dup 0 SIGN-SKIP {: offset:n :} offset - swap offset + swap
   2dup s" nan" STR=CI if 2drop true exit then
   2dup s" inf" STR=CI if 2drop true exit then s" infinity" STR=CI ;

: POW10 ( n -- n ) 1 swap 0 ?do 10 * loop ;
: PUT-DIGITS ( n ptr u8 n -- ) {: value:n out:ptr width:n :}
   value width 0 ?do 10 /mod swap $30 + out width i - 1- + c! loop drop ;

\ A natural number: the count of limbs in use, its capacity, then that many
\ limbs of 32 bits, low first. A limb times 10^9 plus a carry fits a cell.
$FFFFFFFF constant LIMB-MASK
1000000000 constant BILLION

: LIMB@ ( ptr n n -- n ) {: big:ptr at:n :}
   at 0 >= at big @ < and if big at 2 + cells + @ else 0 then ;
: LIMB! ( n ptr n n -- ) 2 + cells + ! ;
: LIMB-CAP ( ptr n -- n ) 1 cells + @ ;
: BIG-INIT ( n ptr n -- ) {: limbs:n big:ptr :} 0 big ! limbs big 1 cells + ! ;
: NEED-LIMBS ( n ptr n -- ) LIMB-CAP > if E-LIMBS throw then ;

\ Appends a top limb.
: BIG-PUSH ( n ptr n -- ) {: limb:n big:ptr :}
   big @ 1+ big NEED-LIMBS limb big big @ LIMB! big @ 1+ big ! ;

: TRIM ( ptr n -- ) {: big:ptr :}
   big @ begin dup 0 > if big over 1- LIMB@ 0= else false then while
      1-
   repeat big ! ;

: BIG-SET ( n ptr n -- ) {: value:n big:ptr :}
   0 big !
   value 0<> if value LIMB-MASK and big BIG-PUSH then
   value 32 rshift dup 0<> if big BIG-PUSH else drop then ;

: BIG-CELL ( ptr n -- n ) {: big:ptr :}
   big 0 LIMB@ big 1 LIMB@ 32 lshift or ;

: BIT-LENGTH ( n -- n )
   0 swap begin dup 0<> while 1 rshift swap 1+ swap repeat drop ;
: BIG-BITS ( ptr n -- n )
   {: big:ptr :}
   big @ 0= if 0 exit then
   big @ 1- 32 * big big @ 1- LIMB@ BIT-LENGTH + ;


: BIG-MUL ( n ptr n -- ) {: factor:n big:ptr :}
   0 big @ 0 ?do
      big i LIMB@ factor * + dup LIMB-MASK and big i LIMB! 32 rshift
   loop
   dup 0<> if big BIG-PUSH else drop then ;

\ Adds a number below 2^32.
: BIG-ADD ( n ptr n -- )
   {: addend:n big:ptr :}
   addend big @ 0 ?do
      big i LIMB@ + dup LIMB-MASK and big i LIMB! 32 rshift
   loop
   dup 0<> if big BIG-PUSH else drop then ;


\ Answers whether the remainder was nonzero.
: BIG-DIV ( n ptr n -- bool ) {: divisor:n big:ptr :}
   0 big @ 0 ?do
      32 lshift big big @ 1- i - LIMB@ + divisor /mod
      big big @ 1- i - LIMB!
   loop 0<> big TRIM ;


\ The top limb first, so each limb is read before it is written; the result
\ has exactly the limbs its bits need.
: BIG-SHL ( n ptr n -- ) {: count:n big:ptr :}
   big BIG-BITS dup 0= if drop exit then
   count + 31 + 32 / {: top:n :}
   top big NEED-LIMBS
   count 32 / {: whole:n :} count 32 mod {: part:n :}
   top 0 ?do
      big top 1- i - whole - LIMB@ part lshift
      big top 2 - i - whole - LIMB@ 32 part - rshift or LIMB-MASK and
      big top 1- i - LIMB!
   loop top big ! ;


\ Answers whether a one bit was shifted out.
: BIG-SHR ( n ptr n -- bool ) {: count:n big:ptr :}
   count 32 / {: whole:n :} count 32 mod {: part:n :}
   big @ {: len:n :}
   false whole len min 0 ?do big i LIMB@ 0<> or loop
   big whole LIMB@ 1 part lshift 1- and 0<> or {: lost:bool :}
   len whole - 0 max {: kept:n :}
   kept 0 ?do
      big i whole + LIMB@ part rshift
      big i whole + 1+ LIMB@ 32 part - lshift or LIMB-MASK and
      big i LIMB!
   loop kept big ! big TRIM lost ;


: BIG-MUL-TENS ( n ptr n -- ) {: power:n big:ptr :}
   power 9 / 0 ?do BILLION big BIG-MUL loop
   power 9 mod POW10 big BIG-MUL ;
: BIG-DIV-TENS ( n ptr n -- bool ) {: power:n big:ptr :}
   false power 9 / 0 ?do BILLION big BIG-DIV or loop
   power 9 mod POW10 big BIG-DIV or ;


\ floor(value x 2^shift x 10^power) and whether that dropped a fraction.
: SCALED ( n n n ptr n -- n bool ) {: value:n shift:n power:n big:ptr :}
   value big BIG-SET
   shift 0 > if shift big BIG-SHL then
   power 0 > if power big BIG-MUL-TENS then
   false
   power 0 < if power negate big BIG-DIV-TENS or then
   shift 0 < if shift negate big BIG-SHR or then
   {: lost:bool :} big BIG-CELL lost ;


\ At precision p, of D = floor(value / 10^(18-p)) and D + 1, those inside
\ [least, most], the nearer, ties upward; and whether either was inside.
: PICK ( n n n n -- n bool ) {: value:n least:n most:n p:n :}
   18 p - POW10 {: unit:n :}
   value unit / {: d:n :}
   d unit * least >= {: down:bool :}
   d 1+ unit * most <= {: up:bool :}
   down up and if
      d value d unit * unit 2 / + >= if 1+ then true exit
   then
   down if d true exit then
   d 1+ up ;

\ A candidate of p + 1 digits is 10^p: one digit at the next exponent.
: ANSWER ( n n n -- n n n ) {: d:n k:n p:n :}
   d p POW10 = if d 10 / k 1+ p else d k p then ;


\ The shortest decimal in v's rounding interval, by PICK over v and its ends
\ at 18 digits; floor(L x 1292913986 / 2^32) is k or k + 1, L the bit length.
: FIND-DECIMAL ( n ptr u8 -- n n n ) {: bits:n work:ptr :}
   bits 52 rshift {: biased:n :}
   bits $FFFFFFFFFFFFF and {: mantissa:n :}
   biased 0= if mantissa -1074 else
      mantissa $10000000000000 or biased 1075 -
   then {: f:n e:n :}
   mantissa 0= biased 1 > and if 1 else 2 then {: below:n :}
   f 1 and 0= {: even:bool :}
   work BIG-AT + CELL-VIEW {: big :}
   FORMAT-LIMBS big BIG-INIT
   f BIT-LENGTH e + 1292913986 * 2000 32 lshift + 32 rshift 2000 - {: guess:n :}
   f 2 lshift {: v:n :}
   v e 2 - 17 guess - big SCALED drop {: first:n :}
   first 17 POW10 < if guess 1- else guess then {: k:n :}
   first 17 POW10 < if v e 2 - 17 k - big SCALED drop else first then {: s:n :}
   v below - e 2 - 17 k - big SCALED {: low:n low-cut:bool :}
   v 2 + e 2 - 17 k - big SCALED {: high:n high-cut:bool :}
   low-cut even 0= or if low 1+ else low then {: least:n :}
   high-cut even or if high else high 1- then {: most:n :}
   17 1 do
      s least most i PICK if k i ANSWER unloop exit then drop
   loop
   \ The interval is wider than the 17-digit step, so one candidate is inside.
   s least most 17 PICK drop k 17 ANSWER ;

: PLAIN! ( ptr u8 n n n bool -- n )
   {: out:ptr digits:n exponent:n precision:n negative:bool :}
   negative if $2D out c! 1 else 0 then {: prefix:n :}
   out prefix + {: target:ptr :}
   exponent 0 < if
      $30 target c! $2E target 1+ c!
      exponent negate 1- 0 ?do $30 target i 2 + + c! loop
      digits target exponent negate 1+ + precision PUT-DIGITS
      prefix exponent negate 1+ + precision + exit
   then
   exponent 1+ precision >= if
      digits target precision PUT-DIGITS
      exponent 1+ precision - 0 ?do $30 target precision i + + c! loop
      prefix exponent 1+ + exit
   then
   exponent 1+ {: before:n :}
   precision before - POW10 {: divisor:n :}
   digits divisor / target before PUT-DIGITS $2E target before + c!
   digits divisor mod target before 1+ + precision before - PUT-DIGITS
   prefix precision 1+ + ;

: FORMAT-LENGTH ( n ptr u8 -- n ) {: bits:n work:ptr :}
   bits MAGNITUDE-MASK and dup 0= if
      drop work 128 + 0 0 1 bits 0 < PLAIN! exit
   then work FIND-DECIMAL {: digits:n exponent:n precision:n :}
   work 128 + digits exponent precision bits 0 < PLAIN! ;

: FORMAT-WORK ( n ptr u8 -- n ptr u8 ) {: bits:n work:ptr :}
   bits work FORMAT-LENGTH work 112 + CELL-VIEW ! bits work ;

\ The output is copied out before the scope releases the work buffer.
: FORMAT-COPY ( n ptr u8 n ptr u8 NUM:alloc-byte-len -- result<n,fault> )
   drop {: bits:n out:ptr capacity:n work:ptr :}
   bits work FORMAT-WORK 2drop
   work 112 + CELL-VIEW @ {: len:n :}
   len capacity > if F64--TEXT-FAULT:CAPACITY RESULT:ERR exit then
   work 128 + out len BYTE-COPY len RESULT:OK ;


public
\ A shortest round-trip plain decimal, no terminator; equally close shortest
\ decimals resolve away from zero, as Rust Display does.
: FORMAT ( r ptr u8 n -- result<n,fault> ) {: value:r out:ptr capacity:n :}
   value FMATH:FINITE? 0= if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   capacity 0 < if F64--TEXT-FAULT:CAPACITY RESULT:ERR exit then
   value IEEE754:F64>BITS out capacity
   FORMAT-BYTES BYTES-ALLOC-LEN [: FORMAT-COPY ;] WITH-BYTES ;

private
\ A point halfway between two binary64 values has fewer significant digits.
800 constant MAX-DIGITS
100000000000000000 constant EXPONENT-CAP

: FOLD ( n n ptr n -- ) {: pending:n width:n big:ptr :}
   width POW10 big BIG-MUL pending big BIG-ADD ;


\ The state: digits kept, their power of ten and whether a dropped digit was
\ not 0. Past MAX-DIGITS a digit is dropped; an integer one raises the power.
: COUNT-DIGIT ( n n bool n bool -- n n bool )
   {: kept:n power:n lost:bool digit:n frac:bool :}
   frac if power 1- else power then {: moved:n :}
   kept 0= digit 0= and if kept moved lost exit then
   kept MAX-DIGITS >= if
      kept frac if power else power 1+ then lost digit 0<> or exit
   then
   kept 1+ moved lost ;


: COUNT-DIGITS ( n n bool ptr u8 n n bool -- n n bool )
   {: text:ptr from:n to:n frac:bool :}
   to from ?do text i + c@ $30 - frac COUNT-DIGIT loop ;


\ The state: digits taken, digits pending and their count. Only the first
\ `kept` significant digits are taken.
: FILL-DIGIT ( n n n n n ptr n -- n n n )
   {: taken:n pending:n width:n digit:n kept:n big:ptr :}
   taken kept >= taken 0= digit 0= and or if taken pending width exit then
   pending 10 * digit + {: next:n :}
   width 8 = if next 9 big FOLD taken 1+ 0 0 exit then
   taken 1+ next width 1+ ;


: FILL-DIGITS ( n n n ptr u8 n n n ptr n -- n n n )
   {: text:ptr from:n to:n kept:n big:ptr :}
   to from ?do text i + c@ $30 - kept big FILL-DIGIT loop ;


\ The exponent after the marker at offset - 1; once its magnitude reaches
\ EXPONENT-CAP it grows no more.
: EXPONENT-VALUE ( ptr u8 n n -- n )
   {: text:ptr len:n offset:n :}
   text len offset SIGN-SKIP {: start:n :}
   0 len start ?do
      dup EXPONENT-CAP < if 10 * text i + c@ $30 - + then
   loop
   start offset > if text offset + c@ $2D = if negate then then ;


\ Of a text GRAMMAR admits: where its digits start, its integer digits end and
\ its fraction ends.
: PARTS ( ptr u8 n -- n n n ) {: text:ptr len:n :}
   text len 0 SIGN-SKIP {: first:n :}
   text len first DIGITS {: integer-end:n :}
   first integer-end text len integer-end FRACTION-END ;


\ The count of significant digits kept, q with the value D x 10^q, D those
\ digits, and whether a dropped digit was not 0.
: SCAN-DECIMAL ( ptr u8 n -- n n bool )
   {: text:ptr len:n :}
   text len PARTS {: first:n integer-end:n end:n :}
   0 0 false text first integer-end false COUNT-DIGITS
   end integer-end > if text integer-end 1+ end true COUNT-DIGITS then
   {: kept:n power:n lost:bool :}
   end len < if text len end 1+ EXPONENT-VALUE else 0 then {: exponent:n :}
   kept exponent power + lost ;


\ Leaves D, the first `kept` significant digits, in big.
: FILL-DECIMAL ( ptr u8 n n ptr n -- )
   {: text:ptr len:n kept:n big:ptr :}
   text len PARTS {: first:n integer-end:n end:n :}
   0 0 0 text first integer-end kept big FILL-DIGITS
   end integer-end > if text integer-end 1+ end kept big FILL-DIGITS then
   big FOLD drop ;


\ B and s with D x 10^q = B x 2^-s, B replacing D in big, and whether B dropped
\ a fraction. As 3402 / 1024 exceeds log2 10, B has at least 64 bits for q < 0.
: BINARY-SCALE ( n ptr n -- n bool )
   {: q:n big:ptr :}
   q 0 >= if q big BIG-MUL-TENS 0 false exit then
   q negate 3402 * 1024 / 65 + big BIG-BITS - 0 max {: s:n :}
   s big BIG-SHL q negate big BIG-DIV-TENS s swap ;


\ The pattern of the binary64 nearest B x 2^-s, ties to even, lost a fraction
\ below B; the round's carry runs into the exponent, up to the infinite pattern.
: ROUNDED ( n bool ptr n -- n )
   {: s:n lost:bool big:ptr :}
   big BIG-BITS 1- s - {: top:n :}
   top 1023 > if $7FF0000000000000 exit then
   top -1022 < if 0 s 1074 - else top 1022 + top 52 - s + then
   {: field:n shift:n :}
   shift 1 < if big BIG-CELL shift negate lshift field 52 lshift + exit then
   shift 1- big BIG-SHR lost or {: sticky:bool :}
   big BIG-CELL 1 and 1 = {: half:bool :}
   1 big BIG-SHR drop
   big BIG-CELL field 52 lshift + {: pattern:n :}
   half sticky pattern 1 and 1 = or and if pattern 1+ else pattern then ;


\ Every number for `count` digits and q has at most n x 3402 / 1024 + 65 bits,
\ n = max(count + max(q, 0), -q), as 3402 / 1024 exceeds log2 10.
: LIMBS ( n n -- n ) {: count:n q:n :}
   count q 0 max + q negate max 3402 * 1024 / 65 + 31 + 32 / ;


: CONVERT ( ptr u8 n n n bool n n ptr u8 NUM:alloc-byte-len -- result<r,fault> )
   drop {: text:ptr len:n count:n q:n dropped:bool limbs:n sign:n work:ptr :}
   work CELL-VIEW {: big :}
   limbs big BIG-INIT
   text len count big FILL-DECIMAL
   q big BINARY-SCALE dropped or big ROUNDED {: bits:n :}
   bits $7FF0000000000000 >= if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   bits sign or IEEE754:BITS>F64 RESULT:OK ;


\ Decides the far ranges before it allocates; the sign joins last.
: READ-DECIMAL ( ptr u8 n -- result<r,fault> )
   {: text:ptr len:n :}
   text c@ $2D = if $8000000000000000 else 0 then {: sign:n :}
   text len SCAN-DECIMAL {: count:n q:n dropped:bool :}
   count 0= count q + -330 < or if sign IEEE754:BITS>F64 RESULT:OK exit then
   count q + 310 > if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   count q LIMBS {: limbs:n :}
   text len count q dropped limbs sign
   limbs 2 + cells BYTES-ALLOC-LEN [: CONVERT ;] WITH-BYTES ;


public
\ The nearest binary64 to a signed decimal/exponent text, ties to even, zero's
\ sign kept; padding, hex, nonfinite values and trailing bytes are refused.
: PARSE ( ptr u8 n -- result<r,fault> )
   {: text:ptr len:n :}
   len 0 < if F64--TEXT-FAULT:MALFORMED RESULT:ERR exit then
   text len NONFINITE-TEXT? if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   text len GRAMMAR {: code:n :}
   code 1 = if F64--TEXT-FAULT:MALFORMED RESULT:ERR exit then
   code 2 = if F64--TEXT-FAULT:TRAILING RESULT:ERR exit then
   text len READ-DECIMAL ;

;using
;package
