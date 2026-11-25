split-str
\ link.f - an arm64 shadow emission patched in place for its link-time address:
\ calls become bl/b and code addresses adrp+add relative to where the code runs,
\ data address chains take the value a data resolver gives. Words visible only
\ inside LINKER.

require src/lib/le.f
require src/arch/arm64/asm.f
require src/habu/address-carrier.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package ARM64

\ The address chain at p, inside bytes ending at end, made to hold v.
: CHAIN! ( n ^u8 ^u8 -- )
   {: v:n p:^ end:^ :}
   p v  p end ADDRESS-CARRIER:CHAIN-SIZE  ADDRESS-CARRIER:SET-CHAIN-VALUE ;

public

\ The word at p, a call site at link-time address at, becomes bl (NEMIT:CALL)
\ or b (NEMIT:TAIL) to to; ENC-BL and ENC-B throw E-A64EMIT-REACH past reach.
: CALL! ( n n n ^u8 -- )
   {: to:n kind:n at:n p:^ :}
   to at - {: d:n :}
   kind NEMIT:CALL = if d A64ASM:ENC-BL else d A64ASM:ENC-B then  p LE:U32! ;


\ The 16-byte chain at p, link-time address at, becomes adrp+add of to and two
\ nops in its own register; ENC-ADRP throws E-A64EMIT-REACH past adrp's reach.
: CODE-ADDRESS! ( n n ^u8 -- )
   {: to:n at:n p:^ :}
   to 12 rshift  at 12 rshift - {: pages:n :}
   p LE:U32@ ADDRESS-CARRIER:ADDR-RD-MASK and {: rd:n :}
   rd pages A64ASM:ENC-ADRP  p LE:U32!
   rd rd to $FFF and A64ASM:ENC-ADDI  p 4 + LE:U32!
   A64ASM:ENC-NOP  p 8 + LE:U32!
   A64ASM:ENC-NOP  p 12 + LE:U32! ;


\ Emission e's calls and address chains rewritten in dst, which holds its bytes and runs from
\ link-time address at: calls and code chains to resolve of their target, data chains through data.
: PATCH ( n ^u8 n [ n -- n ] [ n -- n ] -- )
   {: e:n dst:^ at:n data resolve :}
   dst e NSHADOW:SIZE + {: end:^ :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      e i NSHADOW:CALL-TARGET@ resolve execute  e i NSHADOW:CALL-KIND@
      at s +  dst s +  CALL!
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      dst s + {: p:^ :}
      e i NSHADOW:ADDR-SITE-VALUE@ {: v:n :}
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         v data execute  p end CHAIN!
      else
         v resolve execute  at s +  p CODE-ADDRESS!
      then
   loop ;


\ v written as the address value at site of a row of kind in emission bytes b, s of them.
: ADDRESS! ( n ^u8 n n n -- )
   {: v:n b:^ s:n site:n kind:n :}
   v  b site +  b s +  CHAIN! ;

;package
;package
