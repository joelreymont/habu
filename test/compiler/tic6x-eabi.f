require lib/test.f
require test/checker-assert.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/sim.f
require src/arch/tic6x/eabi.f

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


: REFUSALS ( -- )
   [: 8 EMIT-HELPER ;] C6XEABI:E-OPERAND TTHROWSQ
   [: -1 HELPER-NAME$ 2drop ;] C6XEABI:E-OPERAND TTHROWSQ
   [: 0 64 PERMITTED? drop ;] C6XEABI:E-OPERAND TTHROWSQ ;


: RUN ( -- )
   0 FAILURES !
   DIVISION-CASES MEMORY-CASES REFUSALS
   FAILURES @ 0 T=
   HELPER-COUNT 8 T=
   1 HELPER-NAME$ s" __c6xabi_divu" T$=
   T-REPORT ;

RUN
;package
