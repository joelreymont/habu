\ link.f - an arm64 shadow emission copied to its link-time address: calls
\ become bl/b and code addresses adrp+add relative to where the code runs, data
\ addresses keep their absolute DATA chain. Words visible only inside LINKER.

require src/lib/errors.f
require src/lib/le.f
require src/core/bytes.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/address-carrier.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package ARM64

\ The data address chain at p, inside bytes ending at end, moved by delta.
: DATA-ROW ( ptr u8 ptr u8 n -- )
   {: p:ptr end:ptr delta:n :}
   p end ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   size 0= if s" a64link: a data address row holds no address chain" 74 die then
   p  p size ADDRESS-CARRIER:CHAIN-VALUE delta +  size
   ADDRESS-CARRIER:SET-CHAIN-VALUE ;

public

\ The word at p, a call site whose link-time address is at, becomes bl
\ (NEMIT:CALL) or b (NEMIT:TAIL) to to.
: CALL! ( n n n ptr u8 -- )
   {: to:n kind:n at:n p:ptr :}
   to at - {: d:n :}
   d 3 and 0<> if E-A64EMIT-REACH throw then
   d 4 / {: w:n :}
   w A64ICODE:REL26-OK? 0= if E-A64EMIT-REACH throw then
   kind NEMIT:CALL = if w A64ASM:ENC-BL else w A64ASM:ENC-B then  p LE:U32! ;


\ The 16-byte chain at p, link-time address at, becomes adrp rd, page(to);
\ add rd, rd, #(to & $FFF); nop; nop, in the chain's own register.
: CODE-ADDRESS! ( n n ptr u8 -- )
   {: to:n at:n p:ptr :}
   to 12 rshift  at 12 rshift - {: pages:n :}
   pages A64ICODE:ADR-OK? 0= if E-A64EMIT-REACH throw then
   p LE:U32@ ADDRESS-CARRIER:ADDR-RD-MASK and {: rd:n :}
   rd pages A64ASM:ENC-ADRP  p LE:U32!
   rd rd to $FFF and A64ASM:ENC-ADDI  p 4 + LE:U32!
   A64ASM:ENC-NOP  p 8 + LE:U32!
   A64ASM:ENC-NOP  p 12 + LE:U32! ;


\ Emission e copied to dst, whose first byte runs at link-time address at: calls
\ and code address chains to resolve of their target, data addresses by delta.
: PLACE ( n ptr u8 n n [ n -- n ] -- )
   {: e:n dst:ptr at:n delta:n resolve :}
   e NSHADOW:BYTES dst e NSHADOW:SIZE BYTE-COPY
   dst e NSHADOW:SIZE + {: end:ptr :}
   e NSHADOW:CALL-SITES 0 ?do
      e i NSHADOW:CALL-SITE@ {: s:n :}
      e i NSHADOW:CALL-TARGET@ resolve execute  e i NSHADOW:CALL-KIND@
      at s +  dst s +  CALL!
   loop
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE@ {: s:n :}
      dst s + {: p:ptr :}
      e i NSHADOW:ADDR-SITE-KIND@ {: kind:n :}
      kind NEMIT:ADDR-DATA = if
         p end delta DATA-ROW
      else
         kind NEMIT:ADDR-CODE <> if
            s" a64link: an address row of no known kind" 74 die
         then
         p end ADDRESS-CARRIER:CHAIN-SIZE ADDRESS-CARRIER:ADDR-CHAIN-BYTES <> if
            s" a64link: a code address row holds no 16-byte chain" 74 die
         then
         p ADDRESS-CARRIER:ADDR-CHAIN-BYTES ADDRESS-CARRIER:CHAIN-VALUE
         resolve execute  at s +  p CODE-ADDRESS!
      then
   loop ;

;package
;package
