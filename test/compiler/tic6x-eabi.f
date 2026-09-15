require lib/test.f
require test/checker-assert.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/sim.f
require src/arch/tic6x/eabi.f
require lib/ieee754.f

package C6XEABI-TEST
private
using C6XSIM
using C6XEABI

$FFFFFFFF constant MASK32
19 constant VECTOR-COUNT
create VECTORS
   0 , 1 , 2 , 3 , 7 , 10 , 255 , 256 , 65535 , 65536 , $7FFFFFFF , $80000000 , $80000001 ,
   $FFFFFFFE , $FFFFFFFF , 12345678 , $40000000 , $3FFFFFFF , 1000000007 ,
variable HELPER
variable FAILURES
variable QUOTIENT
variable REMAINDER

: >U32 ( n -- n ) MASK32 and ;
: >S32 ( n -- n ) >U32 dup $80000000 and 0 <> if $100000000 - then ;
: VECTOR ( n -- n ) cells VECTORS + @ ;


\ ---- oracle: C semantics on 32-bit operands, division by zero as documented ----------

: UNSIGNED-ORACLE ( n n -- ) {: x:n y:n :}
   y 0= if MASK32 QUOTIENT ! x REMAINDER ! exit then
   x y / QUOTIENT ! x y mod REMAINDER ! ;

: MAGNITUDE ( n -- n ) >S32 dup 0 < if negate then >U32 ;

: SIGNED-ORACLE ( n n -- ) {: x:n y:n :}
   x MAGNITUDE y MAGNITUDE UNSIGNED-ORACLE
   x >S32 0 < y >S32 0 < xor if QUOTIENT @ negate >U32 QUOTIENT ! then
   x >S32 0 < if REMAINDER @ negate >U32 REMAINDER ! then ;


\ ---- running one helper -----------------------------------------------------------

\ Every register holds a recognisable pattern before the call.
: FILL-REGISTERS ( -- )
   32 0 ?do $A5000000 i + i A! $B5000000 i + i B! loop ;

: UNTOUCHED? ( -- bool ) {: :}
   TRUE
   32 0 ?do
      HELPER @ i PERMITTED? 0= i 4 <> and i 5 <> and if i A@ $A5000000 i + <> if drop FALSE then then
      HELPER @ i 32 + PERMITTED? 0= i 3 <> and if i B@ $B5000000 i + <> if drop FALSE then then
   loop ;

: CALL-HELPER ( n n -- ) {: x:n y:n :}
   HELPER @ EMIT-HELPER
   RESET FILL-REGISTERS x 4 A! y 4 B!
   PROGRAM$ CALL drop ;

: CHECK ( bool ptr u8 n -- ) {: ok:bool text size:n :}
   ok 0= if 1 FAILURES +! s" eabi: " type text size type s"  x=" type 4 A@ . s"  y=" type 4 B@ . cr then ;


: DIVISION-CASES ( -- )
   VECTOR-COUNT 0 ?do VECTOR-COUNT 0 ?do
      i VECTOR {: x:n :} j VECTOR {: y:n :}
      0 HELPER ! x y CALL-HELPER x y SIGNED-ORACLE
      4 A@ QUOTIENT @ = UNTOUCHED? and s" divi" CHECK
      1 HELPER ! x y CALL-HELPER x y UNSIGNED-ORACLE
      4 A@ QUOTIENT @ = UNTOUCHED? and s" divu" CHECK
      2 HELPER ! x y CALL-HELPER x y SIGNED-ORACLE
      4 A@ REMAINDER @ = UNTOUCHED? and s" remi" CHECK
      3 HELPER ! x y CALL-HELPER x y UNSIGNED-ORACLE
      4 A@ REMAINDER @ = UNTOUCHED? and s" remu" CHECK
      4 HELPER ! x y CALL-HELPER x y SIGNED-ORACLE
      4 A@ QUOTIENT @ = 5 A@ REMAINDER @ = and UNTOUCHED? and s" divremi" CHECK
      5 HELPER ! x y CALL-HELPER x y UNSIGNED-ORACLE
      4 A@ QUOTIENT @ = 5 A@ REMAINDER @ = and UNTOUCHED? and s" divremu" CHECK
   loop loop ;


: PREPARE ( -- )
   RESET FILL-REGISTERS
   256 0 ?do i 7 * 255 and MEMORY-BASE i + MEMORY! $EE MEMORY-BASE 256 + i + MEMORY! loop ;

: MEMORY-CASES ( -- )
   MEMORY-BASE {: src:n :} MEMORY-BASE 256 + {: dst:n :}
   6 HELPER ! 6 EMIT-HELPER PREPARE dst 4 A! src 4 B! 100 6 A! PROGRAM$ CALL drop
   4 A@ dst = s" memcpy returns dst" CHECK
   TRUE 100 0 ?do dst i + MEMORY@ i 7 * 255 and <> if drop FALSE then loop s" memcpy copies 100 bytes" CHECK
   dst 100 + MEMORY@ $EE = s" memcpy stops at n" CHECK
   UNTOUCHED? s" memcpy preserves callee-saved registers" CHECK
   6 EMIT-HELPER PREPARE dst 4 A! src 4 B! 0 6 A! PROGRAM$ CALL drop
   dst MEMORY@ $EE = s" memcpy with n=0 writes nothing" CHECK
   7 HELPER ! 7 EMIT-HELPER PREPARE dst 4 A! $1234567A 4 B! 37 6 A! PROGRAM$ CALL drop
   4 A@ dst = s" memset returns dst" CHECK
   TRUE 37 0 ?do dst i + MEMORY@ $7A <> if drop FALSE then loop s" memset fills 37 bytes with the low byte" CHECK
   dst 37 + MEMORY@ $EE = s" memset stops at n" CHECK
   UNTOUCHED? s" memset preserves callee-saved registers" CHECK
   7 EMIT-HELPER PREPARE dst 4 A! 0 4 B! 0 6 A! PROGRAM$ CALL drop
   dst MEMORY@ $EE = s" memset with n=0 writes nothing" CHECK ;


\ ---- float32 division against the host's IEEE arithmetic --------------------------

40 constant FLOAT-COUNT
create FLOATS
   $00000000 , $80000000 , $00000001 , $80000001 , $00000002 , $007FFFFF , $00800000 , $00400000 ,
   $3F800000 , $40000000 , $40400000 , $3DCCCCCD , $3EAAAAAB , $40E00000 , $0DA24260 , $7149F2CA ,
   $7F7FFFFF , $7F800000 , $FF800000 , $7FC00000 , $7F800001 , $FFC00001 , $12345678 , $40490FDB ,
   $C2F6E979 , $5F3759DF , $33800000 , $4B800000 , $7E000000 , $01000000 , $BF800000 , $C0000000 ,
   $3F7FFFFF , $3F800001 , $34000000 , $4F000000 , $00000003 , $807FFFFF , $7F000000 , $00FFFFFF ,
variable EXPECTED-BITS

: FLOAT ( n -- n ) cells FLOATS + @ ;
: F-EXPONENT ( n -- n ) 23 rshift 255 and ;
: F-FRACTION ( n -- n ) $7FFFFF and ;
: NAN? ( n -- bool ) dup F-EXPONENT 255 = swap F-FRACTION 0 <> and ;
: INF? ( n -- bool ) dup F-EXPONENT 255 = swap F-FRACTION 0= and ;
: ZERO? ( n -- bool ) $7FFFFFFF and 0= ;

variable NORMAL-SHIFT

\ A finite float32 as a double, bit for bit; denormals are normalised first.
: F32>R ( n -- r ) {: bits:n :}
   bits F-EXPONENT {: e:n :} bits F-FRACTION {: m:n :} bits 31 rshift 63 lshift {: sign:n :}
   bits ZERO? if sign IEEE754:BITS>F64 exit then
   e 0= if
      m EXPECTED-BITS ! 0 NORMAL-SHIFT !
      begin EXPECTED-BITS @ $800000 and 0= while
         EXPECTED-BITS @ 1 lshift EXPECTED-BITS ! 1 NORMAL-SHIFT +!
      repeat
      sign EXPECTED-BITS @ $7FFFFF and 29 lshift or 1023 126 - NORMAL-SHIFT @ - 52 lshift or
      IEEE754:BITS>F64 exit
   then
   sign m 29 lshift or e 127 - 1023 + 52 lshift or IEEE754:BITS>F64 ;

\ A double as float32 bits with round to nearest even, denormals and overflow.
: R>F32 ( r -- n ) {: value:r :}
   value IEEE754:F64>BITS {: bits:n :}
   bits 63 rshift 31 lshift {: sign:n :}
   bits 52 rshift $7FF and {: e64:n :}
   bits $FFFFFFFFFFFFF and {: frac:n :}
   e64 0= frac 0= and if sign exit then
   e64 $7FF = if sign $7F800000 or exit then
   $10000000000000 frac or {: sig:n :}
   e64 1023 - 127 + {: e32:n :}
   e32 254 > if sign $7F800000 or exit then
   e32 1 < if
      29 1 e32 - + dup 63 > if drop 63 then {: shift:n :}
      sign sig shift IEEE754:ROUND-SHIFT-EVEN or exit
   then
   sig 29 IEEE754:ROUND-SHIFT-EVEN {: rounded:n :}
   rounded $1000000 = if sign e32 1+ 23 lshift or exit then
   sign e32 23 lshift or rounded $7FFFFF and or ;

: DIVF-ORACLE ( n n -- n ) {: x:n y:n :}
   x NAN? if x $400000 or exit then
   y NAN? if y $400000 or exit then
   x INF? y INF? and x ZERO? y ZERO? and or if $7FC00000 exit then
   x y xor $80000000 and {: sign:n :}
   x INF? y ZERO? or if sign $7F800000 or exit then
   y INF? x ZERO? or if sign exit then
   x F32>R y F32>R f/ R>F32 ;

: FLOAT-CASES ( -- )
   8 HELPER !
   FLOAT-COUNT 0 ?do FLOAT-COUNT 0 ?do
      i FLOAT {: x:n :} j FLOAT {: y:n :}
      x y CALL-HELPER
      4 A@ x y DIVF-ORACLE = UNTOUCHED? and s" divf" CHECK
   loop loop ;


: REFUSALS ( -- )
   [: 9 EMIT-HELPER ;] C6XEABI:E-OPERAND TTHROWSQ
   [: -1 HELPER-NAME$ 2drop ;] C6XEABI:E-OPERAND TTHROWSQ
   [: 0 64 PERMITTED? drop ;] C6XEABI:E-OPERAND TTHROWSQ ;


: RUN ( -- )
   0 FAILURES !
   DIVISION-CASES MEMORY-CASES FLOAT-CASES REFUSALS
   FAILURES @ 0 T=
   HELPER-COUNT 9 T=
   1 HELPER-NAME$ s" __c6xabi_divu" T$=
   T-REPORT ;

RUN
;package
