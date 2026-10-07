\ Exact finite binary64 text on glibc and Darwin libc. No task yields while thread-local locale
\ or rounding state is borrowed. Caller output is copied only after success.
\ PARSE accepts complete signed decimal/exponent syntax, rejects padding, hex,
\ nonfinite values and trailing bytes, and preserves subnormals and signed zero.
\ FORMAT writes a shortest round-trip plain decimal without a terminator;
\ equally close shortest decimals resolve away from zero, as Rust Display does.
\ The immutable C numeric locale lives until the process image is prepared.
require lib/ffi-abi.f
require lib/image-lifecycle.f
require lib/ieee754.f
require lib/memory.f
require lib/string.f
require lib/adt/result.f

package F64-TEXT
using MEM

public
327 constant MAX-BYTES
ENUM fault malformed trailing nonfinite capacity runtime ;ENUM

private
-9890 constant E-NATIVE
-9891 constant E-FINITE
$7FFFFFFFFFFFFFFF constant MAGNITUDE-MASK
$7FF0000000000000 constant EXPONENT-MASK
512 constant WORK-BYTES
64 constant NATIVE-BYTES
variable REGISTERED
variable C-LOCALE

\ Exact native C ABI effects; no application-specific foreign policy. The locale
\ calls and the text conversions are libc's, which every process holds. The
\ targets format through different calls: glibc's strfromd, and Darwin's
\ snprintf_l, whose double is variadic and so rides the stack.
PROCESS-SYMBOLS
FUNCTION: NEWLOCALE-CALL newlocale ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: FREELOCALE-CALL freelocale ( n -- ) ;FUNCTION
FUNCTION: USE-LOCALE uselocale ( n -- n ) ;FUNCTION
FUNCTION: STRTOD-CALL strtod_l ( ptr u8 n n -- r ) ;FUNCTION
FUNCTION: STRFROMD-CALL strfromd ( ptr u8 n ptr u8 r -- i32 )
   0 1 WRITES-ARG                        \ the output and its capacity
;FUNCTION
FUNCTION: SNPRINTF-CALL snprintf_l ( ptr u8 n n ptr u8 r -- i32 )
   4 VARIADIC
   0 1 WRITES-ARG                        \ the output and its capacity
;FUNCTION

\ The rounding mode is libm's on glibc; Darwin keeps it in libSystem.
: SELECT-MATH ( -- )
   HB-TARGET-MACOS? if FFI-DECL:SELECT-PROCESS exit then
   s" libm.so.6" FFI-DECL:SELECT-LIBRARY ;

SELECT-MATH
FUNCTION: GET-ROUND fegetround ( -- i32 ) ;FUNCTION
FUNCTION: SET-ROUND fesetround ( n -- i32 ) ;FUNCTION


: FREE-C-LOCALE ( -- )
   C-LOCALE @ 0= if exit then
   C-LOCALE @ FREELOCALE-CALL
   0 C-LOCALE ! ;


\ The locale is a process-local address: preparing an image frees it, and the
\ first call after that makes a new one.
: CLEANUP-NATIVE ( -- )
   FREE-C-LOCALE
   0 REGISTERED ! ;


: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: CLEANUP-NATIVE ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;


: NEW-LOCALE ( -- n )
   HB-TARGET-MACOS? if 16 else 2 then s\" C\z" drop 0 NEWLOCALE-CALL ;


: INITIALIZE ( -- )
   C-LOCALE @ 0<> if exit then
   REGISTER-CLEANUP
   NEW-LOCALE dup 0= if drop E-NATIVE throw then C-LOCALE ! ;


: NATIVE-READY? ( -- bool ) [: INITIALIZE ;] catch 0= ;

: C-PARSE ( ptr u8 -- r ) 0 C-LOCALE @ STRTOD-CALL ;

: C-FORMAT ( r ptr u8 ptr u8 -- n ) {: value:r out:ptr format:ptr :}
   HB-TARGET-MACOS? if
      out NATIVE-BYTES C-LOCALE @ format value SNPRINTF-CALL exit
   then out NATIVE-BYTES format value STRFROMD-CALL ;

: FINITE? ( r -- bool ) IEEE754:F64>BITS EXPONENT-MASK and EXPONENT-MASK <> ;
: FREE-WORK ( ptr u8 n -- ) BYTES-ALLOC-LEN RELEASE-BYTES ;
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
   work C-PARSE dup FINITE? 0= if drop E-FINITE throw then
   IEEE754:F64>BITS work CELL-VIEW ! work ;

\ Round to nearest around strtod_l and restore the caller's mode. PARSE owns
\ the work buffer and frees it whatever this throws.
: PARSE-SCOPED ( ptr u8 -- ptr u8 ) {: work:ptr :}
   GET-ROUND {: prior:n :}
   prior 0 < if E-NATIVE throw then
   0 SET-ROUND 0 <> if E-NATIVE throw then
   work [: PARSE-ROUND ;] catch nip {: code:n :}
   prior SET-ROUND 0 <> if E-NATIVE throw then
   code 0 <> if code throw then work ;

public
: PARSE ( ptr u8 n -- result<r,fault> ) {: text:ptr len:n :}
   len 0 < if F64--TEXT-FAULT:MALFORMED RESULT:ERR exit then
   text len NONFINITE-TEXT? if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   text len GRAMMAR {: code:n :}
   code 1 = if F64--TEXT-FAULT:MALFORMED RESULT:ERR exit then
   code 2 = if F64--TEXT-FAULT:TRAILING RESULT:ERR exit then
   len MAGNITUDE-MASK 1- > if F64--TEXT-FAULT:RUNTIME RESULT:ERR exit then
   NATIVE-READY? 0= if F64--TEXT-FAULT:RUNTIME RESULT:ERR exit then
   len 1+ 8 max MEM-ALLOC-BYTES {: work:ptr size:n :}
   text work len BYTE-COPY 0 work len + c!
   work [: PARSE-SCOPED ;] catch nip {: status:n :}
   work CELL-VIEW @ {: bits:n :}
   work size FREE-WORK
   status 0= if bits IEEE754:BITS>F64 RESULT:OK exit then
   status E-FINITE = if F64--TEXT-FAULT:NONFINITE RESULT:ERR else F64--TEXT-FAULT:RUNTIME RESULT:ERR then ;

private
: POW10 ( n -- n ) 1 swap 0 ?do 10 * loop ;
: PUT-DIGITS ( n ptr u8 n -- ) {: value:n out:ptr width:n :}
   value width 0 ?do 10 /mod swap $30 + out width i - 1- + c! loop drop ;

: FORMAT-SPEC ( ptr u8 n -- ) {: out:ptr precision:n :}
   $25 out c! $2E out 1+ c!
   precision 1- dup 9 > if
      out 2 + 2 PUT-DIGITS $65 out 4 + c! 0 out 5 + c!
   else out 2 + 1 PUT-DIGITS $65 out 3 + c! 0 out 4 + c! then ;

: SMALL-NUMBER ( ptr u8 n -- n ) {: text:ptr len:n :}
   0 len 0 ?do 10 * text i + c@ $30 - + loop ;

\ The native scientific spelling has exactly precision digits and a signed exponent.
: SCIENTIFIC-PARTS ( ptr u8 n n -- n n ) {: text:ptr len:n precision:n :}
   text c@ $30 -
   precision 1- 0 ?do
      10 * text i 2 + + c@ $30 - +
   loop {: digits:n :}
   precision 1 = if 2 else precision 2 + then {: sign-at:n :}
   text sign-at 1+ + len sign-at 1+ - SMALL-NUMBER
   text sign-at + c@ $2D = if negate then digits swap ;

: CANDIDATE! ( ptr u8 n n n -- ) {: out:ptr digits:n exponent:n precision:n :}
   digits out precision PUT-DIGITS $65 out precision + c!
   exponent precision 1- - {: power:n :}
   power 0 < if $2D else $2B then out precision 1+ + c!
   power abs out precision 2 + + 3 PUT-DIGITS
   0 out precision 5 + + c! ;

: ROUNDTRIP? ( ptr u8 n n n n -- bool )
   {: out:ptr digits:n exponent:n precision:n bits:n :}
   out digits exponent precision CANDIDATE!
   out C-PARSE IEEE754:F64>BITS bits = ;

: PREVIOUS-DECIMAL ( n n n -- n n ) {: digits:n exponent:n precision:n :}
   digits precision 1- POW10 = if precision POW10 1- exponent 1- else digits 1- exponent then ;
: NEXT-DECIMAL ( n n n -- n n ) {: digits:n exponent:n precision:n :}
   digits precision POW10 1- = if precision 1- POW10 exponent 1+ else digits 1+ exponent then ;


: HIGH-BIT ( n -- n )
   dup 1 <= if drop 0 exit then 1 rshift recurse 1+ ;

: REDUCE-FIVES ( n n -- n bool )
   dup 0= if drop true exit then
   over 5 mod 0 <> if 2drop 0 false exit then
   1- swap 5 / swap recurse ;

: MULTIPLY-FIVES ( n n -- n bool )
   dup 0= if drop true exit then
   over $20000000000000 5 / > if 2drop 0 false exit then
   1- swap 5 * swap recurse ;

\ (2*digits+1)*10^scale/2 is an exact binary value only after every denominator
\ factor five cancels. The remaining odd significand must fit 53 bits. Building
\ its IEEE bits checks equality without another rounded decimal conversion.
: MIDPOINT? ( n n n n -- bool ) {: digits:n exponent:n precision:n bits:n :}
   exponent precision 1- - {: scale:n :}
   digits 2 * 1+ scale 0 < if scale negate REDUCE-FIVES else scale MULTIPLY-FIVES then
   0= if drop false exit then {: significand:n :}
   significand $20000000000000 >= if false exit then
   significand HIGH-BIT {: high:n :}
   scale 1- high + 1023 + {: biased:n :}
   biased 2047 >= if false exit then
   biased 0 <= if
      scale 1073 + dup 0 < if drop false exit then
      significand swap lshift bits = exit
   then
   biased 52 lshift significand 52 high - lshift $FFFFFFFFFFFFF and or bits = ;

: CHOOSE-TIE ( ptr u8 n n n n -- n n n )
   {: work:ptr digits:n exponent:n precision:n bits:n :}
   digits exponent precision bits MIDPOINT? if
      digits exponent precision NEXT-DECIMAL {: next:n next-exp:n :}
      work next next-exp precision bits ROUNDTRIP? if next next-exp precision exit then
   then digits exponent precision ;

\ At each precision the correctly rounded decimal and its two neighboring grid
\ points contain every possible shortest round-trip answer. Checking neighbors
\ handles asymmetric IEEE intervals; decimal powers have a smaller lower step.
: FIND-DECIMAL ( r ptr u8 -- n n n ) {: value:r work:ptr :}
   value IEEE754:F64>BITS {: bits:n :}
   18 1 do
      work 64 + i FORMAT-SPEC
      value work work 64 + C-FORMAT {: len:n :}
      len 0 < len NATIVE-BYTES >= or if E-NATIVE throw then
      work len i SCIENTIFIC-PARTS {: digits:n exponent:n :}
      work digits exponent i bits ROUNDTRIP? if work digits exponent i bits CHOOSE-TIE unloop exit then
      digits exponent i PREVIOUS-DECIMAL {: previous:n previous-exp:n :}
      work previous previous-exp i bits ROUNDTRIP? if work previous previous-exp i bits CHOOSE-TIE unloop exit then
      digits exponent i NEXT-DECIMAL {: next:n next-exp:n :}
      work next next-exp i bits ROUNDTRIP? if work next next-exp i bits CHOOSE-TIE unloop exit then
   loop E-NATIVE throw ;

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
   then IEEE754:BITS>F64 work FIND-DECIMAL {: digits:n exponent:n precision:n :}
   work 128 + digits exponent precision bits 0 < PLAIN! ;

: FORMAT-WORK ( n ptr u8 -- n ptr u8 ) {: bits:n work:ptr :}
   bits work FORMAT-LENGTH work 112 + CELL-VIEW ! bits work ;

: FORMAT-ROUNDED ( n ptr u8 -- n ptr u8 ) {: bits:n work:ptr :}
   GET-ROUND {: prior-round:n :}
   prior-round 0 < if E-NATIVE throw then
   0 SET-ROUND 0 <> if E-NATIVE throw then
   bits work [: FORMAT-WORK ;] catch >r 2drop r> {: code:n :}
   prior-round SET-ROUND {: round-rc:n :}
   code 0 <> if code throw then
   round-rc 0 <> if E-NATIVE throw then bits work ;

\ The C locale stays installed only inside the catch, so whatever throws under
\ it, a rounding row's first-call resolution included, restores the caller's.
: FORMAT-SCOPED ( n ptr u8 -- n ptr u8 ) {: bits:n work:ptr :}
   C-LOCALE @ USE-LOCALE {: prior-locale:n :}
   prior-locale 0= if E-NATIVE throw then
   bits work [: FORMAT-ROUNDED ;] catch >r 2drop r> {: code:n :}
   prior-locale USE-LOCALE {: locale-rc:n :}
   code 0 <> if code throw then
   locale-rc 0= if E-NATIVE throw then bits work ;

public
: FORMAT ( r ptr u8 n -- result<n,fault> ) {: value:r out:ptr capacity:n :}
   value FINITE? 0= if F64--TEXT-FAULT:NONFINITE RESULT:ERR exit then
   capacity 0 < if F64--TEXT-FAULT:CAPACITY RESULT:ERR exit then
   NATIVE-READY? 0= if F64--TEXT-FAULT:RUNTIME RESULT:ERR exit then
   WORK-BYTES MEM-ALLOC-BYTES {: work:ptr size:n :}
   value IEEE754:F64>BITS work [: FORMAT-SCOPED ;] catch >r 2drop r> {: code:n :}
   code 0 <> if work size FREE-WORK F64--TEXT-FAULT:RUNTIME RESULT:ERR exit then
   work 112 + CELL-VIEW @ {: len:n :}
   len capacity > if work size FREE-WORK F64--TEXT-FAULT:CAPACITY RESULT:ERR exit then
   work 128 + out len BYTE-COPY work size FREE-WORK len RESULT:OK ;

;using
;package
