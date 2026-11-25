\ link.f - an arm64 shadow emission patched in place for its link-time address:
\ calls become bl/b and code addresses adrp+add relative to where the code runs,
\ data address chains take the value a data resolver gives. A code site the
\ link does not write is `adrp rd, 0; add rd, rd, 0`. Words visible only
\ inside LINKER.

require src/lib/le.f
require src/arch/arm64/asm.f
require src/habu/address-carrier.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package ARM64

public

\ The word at p, a call site at link-time address at, becomes bl (NEMIT:CALL)
\ or b (NEMIT:TAIL) to to; ENC-BL and ENC-B throw E-A64EMIT-REACH past reach.
: CALL! ( n n n ^u8 -- )
   {: to:n kind:n at:n p:^ :}
   to at - {: d:n :}
   kind NEMIT:CALL = if d A64ASM:ENC-BL else d A64ASM:ENC-B then  p LE:U32! ;


\ The two words at p, link-time address at, become adrp+add of to in the
\ register of the first; ENC-ADRP throws E-A64EMIT-REACH past adrp's reach.
: CODE-ADDRESS! ( n n ^u8 -- )
   {: to:n at:n p:^ :}
   p LE:U32@ ADDRESS-CARRIER:ADDR-RD-MASK and  to at A64ASM:ENC-ADRP-ADD
   p 4 + LE:U32!  p LE:U32! ;


\ dst holds emission e's bytes and runs from link-time address at.
: PATCH ( n ^u8 n [ n -- n ] [ n -- n ] [ n n -- bool ] -- )
   {: e:n dst:^ at:n data resolve live :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      e s live execute if
         e i NSHADOW:CALL-TARGET@ resolve execute  e i NSHADOW:CALL-KIND@
         at s +  dst s +  CALL!
      then
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      dst s + {: p:^ :}
      e i NSHADOW:ADDR-SITE-VALUE@ {: v:n :}
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = {: data?:bool :}
      e s live execute 0= if
         data? if 0 p ADDRESS-CARRIER:SET-CHAIN-VALUE
         else 0 0 p CODE-ADDRESS! then
      else
         data? if
            v data execute  p ADDRESS-CARRIER:SET-CHAIN-VALUE
         else
            v resolve execute  at s +  p CODE-ADDRESS!
         then
      then
   loop ;


\ v written as the address value at site of a row of kind in emission bytes b, s of them;
\ a code site, whose value depends on where it runs, becomes `adrp rd, 0; add rd, rd, 0`.
: ADDRESS! ( n ^u8 n n n -- )
   {: v:n b:^ s:n site:n kind:n :}
   kind NEMIT:ADDR-CODE = if 0 0 b site + CODE-ADDRESS! exit then
   v  b site +  ADDRESS-CARRIER:SET-CHAIN-VALUE ;

;package
;package
