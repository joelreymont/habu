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


\ ---- every source and destination misalignment, lengths 0 to 40 ----------------------

variable SWEEP-OK

: COPY-SWEEP-CASE ( n n n -- ) {: soff:n doff:n n:n :}
   MEMORY-BASE soff + {: src:n :} MEMORY-BASE 256 + doff + {: dst:n :}
   6 EMIT-HELPER PREPARE dst 4 A! src 4 B! n 6 A! PROGRAM$ CALL drop
   4 A@ dst <> if 0 SWEEP-OK ! then
   n 0 ?do dst i + MEMORY@ soff i + 7 * 255 and <> if 0 SWEEP-OK ! then loop
   dst n + MEMORY@ $EE <> dst 1- MEMORY@ $EE <> or if 0 SWEEP-OK ! then
   UNTOUCHED? 0= if 0 SWEEP-OK ! then ;

: FILL-SWEEP-CASE ( n n -- ) {: doff:n n:n :}
   MEMORY-BASE 256 + doff + {: dst:n :}
   7 EMIT-HELPER PREPARE dst 4 A! $12345A 4 B! n 6 A! PROGRAM$ CALL drop
   4 A@ dst <> if 0 SWEEP-OK ! then
   n 0 ?do dst i + MEMORY@ $5A <> if 0 SWEEP-OK ! then loop
   dst n + MEMORY@ $EE <> dst 1- MEMORY@ $EE <> or if 0 SWEEP-OK ! then
   UNTOUCHED? 0= if 0 SWEEP-OK ! then ;

: COPY-LENGTHS ( n n -- ) {: soff:n doff:n :} 41 0 ?do soff doff i COPY-SWEEP-CASE loop ;

: SWEEPS ( -- )
   6 HELPER ! 1 SWEEP-OK !
   8 1 ?do 8 1 ?do i j COPY-LENGTHS loop loop
   SWEEP-OK @ 0 <> s" memcpy over every misalignment and length" CHECK
   7 HELPER ! 1 SWEEP-OK !
   8 1 ?do 41 0 ?do j i FILL-SWEEP-CASE loop loop
   SWEEP-OK @ 0 <> s" memset over every misalignment and length" CHECK ;


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


\ ---- float64 division against the host's IEEE arithmetic --------------------------

40 constant DOUBLE-COUNT
create DOUBLES
   $0000000000000000 , $8000000000000000 , $0000000000000001 , $000FFFFFFFFFFFFF ,
   $0010000000000000 , $3FF0000000000000 , $4000000000000000 , $4008000000000000 ,
   $3FB999999999999A , $3FD5555555555555 , $401C000000000000 , $01A56E1FC2F8F359 ,
   $7E37E43C8800759C , $7FEFFFFFFFFFFFFF , $7FF0000000000000 , $FFF0000000000000 ,
   $7FF8000000000000 , $7FF0000000000001 , $123456789ABCDEF0 , $400921FB54442D18 ,
   $C05EDD2F1A9FBE77 , $5FE0000000000000 , $3C00000000000000 , $4340000000000000 ,
   $7FD0000000000000 , $0020000000000000 , $BFF0000000000000 , $C000000000000000 ,
   $3FEFFFFFFFFFFFFF , $3FF0000000000001 , $3E80000000000000 , $41F0000000000000 ,
   $0000000000000003 , $800FFFFFFFFFFFFF , $7FE0000000000000 , $001FFFFFFFFFFFFF ,
   $4000000000000001 , $3FF8000000000000 , $0008000000000000 , $4059000000000000 ,

: DOUBLE ( n -- n ) cells DOUBLES + @ ;
: D-EXPONENT ( n -- n ) 52 rshift $7FF and ;
: D-FRACTION ( n -- n ) $FFFFFFFFFFFFF and ;
: D-NAN? ( n -- bool ) dup D-EXPONENT $7FF = swap D-FRACTION 0 <> and ;
: D-INF? ( n -- bool ) dup D-EXPONENT $7FF = swap D-FRACTION 0= and ;
: D-ZERO? ( n -- bool ) $7FFFFFFFFFFFFFFF and 0= ;
: HIGH ( n -- n ) 32 rshift MASK32 and ;
: LOW ( n -- n ) MASK32 and ;

: DIVD-ORACLE ( n n -- n ) {: x:n y:n :}
   x D-NAN? if x $0008000000000000 or exit then
   y D-NAN? if y $0008000000000000 or exit then
   x D-INF? y D-INF? and x D-ZERO? y D-ZERO? and or if $7FF8000000000000 exit then
   x y xor $8000000000000000 and {: sign:n :}
   x D-INF? y D-ZERO? or if sign $7FF0000000000000 or exit then
   y D-INF? x D-ZERO? or if sign exit then
   x IEEE754:BITS>F64 y IEEE754:BITS>F64 f/ IEEE754:F64>BITS ;

: CALL-DIVD ( n n -- ) {: x:n y:n :}
   9 EMIT-HELPER
   RESET FILL-REGISTERS x LOW 4 A! x HIGH 5 A! y LOW 4 B! y HIGH 5 B!
   PROGRAM$ CALL drop ;

: DOUBLE-CASES ( -- )
   9 HELPER !
   DOUBLE-COUNT 0 ?do DOUBLE-COUNT 0 ?do
      i DOUBLE {: x:n :} j DOUBLE {: y:n :}
      x y CALL-DIVD
      5 A@ 32 lshift 4 A@ or x y DIVD-ORACLE = UNTOUCHED? and s" divd" CHECK
   loop loop ;


: REFUSALS ( -- )
   [: 10 EMIT-HELPER ;] C6XEABI:E-OPERAND TTHROWSQ
   [: -1 HELPER-NAME$ 2drop ;] C6XEABI:E-OPERAND TTHROWSQ
   [: 0 64 PERMITTED? drop ;] C6XEABI:E-OPERAND TTHROWSQ ;


: RUN ( -- )
   0 FAILURES !
   DIVISION-CASES MEMORY-CASES SWEEPS FLOAT-CASES DOUBLE-CASES REFUSALS
   FAILURES @ 0 T=
   HELPER-COUNT 10 T=
   1 HELPER-NAME$ s" __c6xabi_divu" T$=
   T-REPORT ;

RUN
;package
