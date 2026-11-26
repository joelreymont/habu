\ x86-64 emissions use rel32 calls and address literals in `mov r32, imm32` or movabs.
require src/core/exit-status.f
require src/lib/le.f
require src/arch/x86-64/asm.f
require src/core/bytes.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package X64
private

: REL32 ( n -- n )
   dup -2147483648 < over 2147483647 > or if
      s" x64link: rel32 target is out of reach" EXIT-STATUS:ENCODE die
   then ;

public

: CALL! ( n n n ^u8 -- )
   {: to:n kind:n at:n p:^ :}
   kind NEMIT:CALL = if $E8 else $E9 then p c!
   to at - 5 - REL32 p 1+ LE:U32! ;

: CODE-ADDRESS! ( n n ^u8 -- )
   {: to:n at:n p:^ :}
   to p 2 + LE:U64! ;

\ The site loads 0 whatever v is: v is a host address, and PATCH writes values.
: ADDRESS! ( n ^u8 n n n -- )
   {: v:n b:^ size:n at:n kind:n :}
   0 b at + X64ASM:MOV-IMM! ;

: PATCH ( n ^u8 n [ n -- n ] [ n -- n ] [ n n -- bool ] -- )
   {: e:n dst:^ at:n data resolve live :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      e s live execute if
         e i NSHADOW:CALL-TARGET@ resolve execute
         e i NSHADOW:CALL-KIND@ at s + dst s + CALL!
      then
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      e s live execute if
         e i NSHADOW:ADDR-SITE-VALUE@
         e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if data else resolve then execute
      else
         0
      then
      dst s + X64ASM:MOV-IMM!
   loop ;

;package
;package
