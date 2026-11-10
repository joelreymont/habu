\ f64-text.f - exact finite binary64 text: PARSE through glibc or Darwin libc,
\ and a shortest round-trip FORMAT computed here in integers, which borrows no
\ locale or rounding mode. No task yields while PARSE borrows the thread's
\ rounding mode, and caller output is copied only after success.
\ STORAGE CLASS. PROCESS-WIDE: the library and the immutable C numeric locale,
\ made by PARSE's first call, then only read; the locale is scratch, so a built
\ program makes its own. No conversion yields. TASK-LOCAL: the work buffers.

require src/lib/ffi-abi.f
require src/lib/fmath.f
require src/lib/ieee754.f
require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/adt/result.f

package F64-TEXT
using MEM

public
\ Always enough for FORMAT: the length of the negative least subnormal.
327 constant MAX-BYTES
\ Bytes left after a complete decimal are `trailing`; nan, inf, infinity and an
\ overflowing decimal `nonfinite`; a negative capacity or a text longer than it
\ `capacity`; a failed library load or native call `runtime`; else `malformed`.
ENUM fault malformed trailing nonfinite capacity runtime ;ENUM

private
-9890 constant E-NATIVE
-9891 constant E-FINITE
$7FFFFFFFFFFFFFFF constant MAGNITUDE-MASK
\ FORMAT's work: its length cell at 112, its text at 128, its number at BIG-AT.
512 constant BIG-AT
40 constant LIMBS
BIG-AT LIMBS 1+ 8 * + constant WORK-BYTES
SCRATCH-VARIABLE C-LOCALE

\ The locale calls and strtod_l are libc's, which every process holds.
PROCESS-SYMBOLS
FUNCTION: NEWLOCALE-CALL newlocale ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: FREELOCALE-CALL freelocale ( n -- ) ;FUNCTION
FUNCTION: STRTOD-CALL strtod_l ( ptr u8 n n -- r ) ;FUNCTION

\ The rounding mode is libm's on glibc; Darwin keeps it in libSystem.
: SELECT-MATH ( -- )
   HB-TARGET-MACOS? if FFI:SELECT-PROCESS exit then
   s" libm.so.6" FFI:SELECT-LIBRARY ;

SELECT-MATH
FUNCTION: GET-ROUND fegetround ( -- i32 ) ;FUNCTION
FUNCTION: SET-ROUND fesetround ( n -- i32 ) ;FUNCTION


: NEW-LOCALE ( -- n )
   HB-TARGET-MACOS? if 16 else 2 then s\" C\z" drop 0 NEWLOCALE-CALL ;


: INITIALIZE ( -- )
   C-LOCALE @ 0<> if exit then
   NEW-LOCALE dup 0= if drop E-NATIVE throw then C-LOCALE ! ;


: NATIVE-READY? ( -- bool ) [: INITIALIZE ;] catch 0= ;

: C-PARSE ( ptr u8 -- r ) 0 C-LOCALE @ STRTOD-CALL ;

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

: PARSE-ROUND ( ptr u8 -- ptr u8 ) {: work:ptr :}
   work C-PARSE dup FMATH:FINITE? 0= if drop E-FINITE throw then
   IEEE754:F64>BITS work CELL-VIEW ! work ;

\ Round to nearest around strtod_l and restore the caller's mode. PARSE-COPY
\ catches whatever this throws, inside the scope that holds the work buffer.
: PARSE-SCOPED ( ptr u8 -- ptr u8 ) {: work:ptr :}
   GET-ROUND {: prior:n :}
   prior 0 < if E-NATIVE throw then
   0 SET-ROUND 0 <> if E-NATIVE throw then
   work [: PARSE-ROUND ;] catch nip {: code:n :}
   prior SET-ROUND 0 <> if E-NATIVE throw then
   code 0 <> if code throw then work ;


\ The bits and the status, both read before the scope releases the copy.
: PARSE-COPY ( ptr u8 n ptr u8 NUM:alloc-byte-len -- n n )
   ALLOCATION>SPAN {: text:ptr len:n copy :}
   text len copy FFI:CSTR {: work:ptr :}
   work [: PARSE-SCOPED ;] catch nip {: status:n :}
   work CELL-VIEW @ status ;


public
\ Complete signed decimal/exponent syntax; padding, hex, nonfinite values and
\ trailing bytes are refused; subnormals and signed zero are kept. The locale
\ goes to strtod_l; the rounding mode is restored before answering, failure
\ included.
: PARSE ( ptr u8 n -- result<r,fault> )
   {: text:ptr len:n :}
   len 0 < if F64--TEXT-FAULT:MALFORMED RESULT:ERR exit then
   text len NONFINITE-TEXT? if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   text len GRAMMAR {: code:n :}
   code 1 = if F64--TEXT-FAULT:MALFORMED RESULT:ERR exit then
   code 2 = if F64--TEXT-FAULT:TRAILING RESULT:ERR exit then
   len MAGNITUDE-MASK 1- > if F64--TEXT-FAULT:RUNTIME RESULT:ERR exit then
   NATIVE-READY? 0= if F64--TEXT-FAULT:RUNTIME RESULT:ERR exit then
   text len len FFI:CSTR-ROOM 8 max BYTES-ALLOC-LEN [: PARSE-COPY ;] WITH-BYTES
   {: bits:n status:n :}
   status 0= if bits IEEE754:BITS>F64 RESULT:OK exit then
   status E-FINITE = if F64--TEXT-FAULT:NONFINITE RESULT:ERR else F64--TEXT-FAULT:RUNTIME RESULT:ERR then ;

private
: POW10 ( n -- n ) 1 swap 0 ?do 10 * loop ;
: PUT-DIGITS ( n ptr u8 n -- ) {: value:n out:ptr width:n :}
   value width 0 ?do 10 /mod swap $30 + out width i - 1- + c! loop drop ;

\ A natural number at BIG-AT: the count of limbs in use, then LIMBS limbs of
\ 32 bits, low first. A limb times 10^9 plus a carry fits a cell.
$FFFFFFFF constant LIMB-MASK
1000000000 constant BILLION

: LIMB@ ( ptr n n -- n ) {: big:ptr at:n :}
   at 0 >= at big @ < and if big at 1+ cells + @ else 0 then ;
: LIMB! ( n ptr n n -- ) 1+ cells + ! ;

: TRIM ( ptr n -- ) {: big:ptr :}
   big @ begin dup 0 > if big over 1- LIMB@ 0= else false then while
      1-
   repeat big ! ;

: BIG-SET ( n ptr n -- ) {: value:n big:ptr :}
   value LIMB-MASK and big 0 LIMB! value 32 rshift big 1 LIMB!
   2 big ! big TRIM ;

: BIG-CELL ( ptr n -- n ) {: big:ptr :}
   big 0 LIMB@ big 1 LIMB@ 32 lshift or ;


: BIG-MUL ( n ptr n -- ) {: factor:n big:ptr :}
   0 big @ 0 ?do
      big i LIMB@ factor * + dup LIMB-MASK and big i LIMB! 32 rshift
   loop
   dup 0<> if big big @ LIMB! big @ 1+ big ! else drop then ;


\ Answers whether the remainder was nonzero.
: BIG-DIV ( n ptr n -- bool ) {: divisor:n big:ptr :}
   0 big @ 0 ?do
      32 lshift big big @ 1- i - LIMB@ + divisor /mod
      big big @ 1- i - LIMB!
   loop 0<> big TRIM ;


\ The top limb first, so each limb is read before it is written.
: BIG-SHL ( n ptr n -- ) {: count:n big:ptr :}
   count 32 / {: whole:n :} count 32 mod {: part:n :}
   big @ whole + 1+ {: top:n :}
   top 0 ?do
      big top 1- i - whole - LIMB@ part lshift
      big top 2 - i - whole - LIMB@ 32 part - rshift or LIMB-MASK and
      big top 1- i - LIMB!
   loop top big ! big TRIM ;


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

: BIT-LENGTH ( n -- n )
   0 swap begin dup 0<> while 1 rshift swap 1+ swap repeat drop ;


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


\ Exact, in units of 10^(k-17) with 10^k <= value < 10^(k+1): the value and the
\ least and most integers its rounding interval holds. floor(L x log10 2), L
\ the bit length, is k or k + 1; 1292913986 / 2^32 gives that floor exactly.
: FIND-DECIMAL ( n ptr u8 -- n n n ) {: bits:n work:ptr :}
   bits 52 rshift {: biased:n :}
   bits $FFFFFFFFFFFFF and {: mantissa:n :}
   biased 0= if mantissa -1074 else
      mantissa $10000000000000 or biased 1075 -
   then {: f:n e:n :}
   mantissa 0= biased 1 > and if 1 else 2 then {: below:n :}
   f 1 and 0= {: even:bool :}
   work BIG-AT + CELL-VIEW {: big :}
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
\ decimals resolve away from zero, as Rust Display does; no locale or rounding.
: FORMAT ( r ptr u8 n -- result<n,fault> ) {: value:r out:ptr capacity:n :}
   value FMATH:FINITE? 0= if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   capacity 0 < if F64--TEXT-FAULT:CAPACITY RESULT:ERR exit then
   value IEEE754:F64>BITS out capacity
   WORK-BYTES BYTES-ALLOC-LEN [: FORMAT-COPY ;] WITH-BYTES ;

;using
;package
