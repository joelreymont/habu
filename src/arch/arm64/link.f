\ link.f - an arm64 shadow emission patched in place for its link-time address:
\ calls become bl/b and code addresses adrp+add relative to where the code runs,
\ data address chains take the value a data resolver gives. Words visible only
\ inside LINKER.

require src/core/exit-status.f
require src/lib/le.f
require src/arch/arm64/asm.f
require src/habu/address-carrier.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f

package LINKER
private
package ARM64

\ The data address chain at p, inside bytes ending at end, its value through data.
: DATA-ROW ( ^u8 ^u8 [ n -- n ] -- )
   {: p:^ end:^ data :}
   p end ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   size 0= if s" a64link: a data address row holds no address chain" EXIT-STATUS:INTERNAL die then
   p  p size ADDRESS-CARRIER:CHAIN-VALUE data execute  size
   ADDRESS-CARRIER:SET-CHAIN-VALUE ;

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
      e i NSHADOW:ADDR-SITE-KIND@ {: kind:n :}
      kind NEMIT:ADDR-DATA = if
         p end data DATA-ROW
      else
         kind NEMIT:ADDR-CODE <> if
            s" a64link: an address row of no known kind" EXIT-STATUS:INTERNAL die
         then
         p end ADDRESS-CARRIER:CHAIN-SIZE ADDRESS-CARRIER:ADDR-CHAIN-BYTES <> if
            s" a64link: a code address row holds no 16-byte chain" EXIT-STATUS:INTERNAL die
         then
         p ADDRESS-CARRIER:ADDR-CHAIN-BYTES ADDRESS-CARRIER:CHAIN-VALUE
         resolve execute  at s +  p CODE-ADDRESS!
      then
   loop ;


private

\ The size of the chain a row of kind holds at site in emission bytes b, s of them: a code
\ chain is ADDR-CHAIN-BYTES long, a data chain whole; 0 when no such chain is there.
: SITE-SIZE ( ^u8 n n n -- n )
   {: b:^ s:n site:n kind:n :}
   site 0<  site 3 and 0<>  or  site s >=  or if 0 exit then
   b site +  b s +  ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   kind NEMIT:ADDR-CODE = if
      size ADDRESS-CARRIER:ADDR-CHAIN-BYTES = if size else 0 then exit
   then
   kind NEMIT:ADDR-DATA = if size exit then
   0 ;

public

\ The address value emission bytes b, s of them, hold at site for a row of kind, and whether
\ they hold one there.
: ADDRESS@ ( ^u8 n n n -- n bool )
   {: b:^ s:n site:n kind:n :}
   b s site kind SITE-SIZE {: size:n :}
   size 0= if 0 false exit then
   b site +  size ADDRESS-CARRIER:CHAIN-VALUE  true ;


\ v written as the address value at site of a row of kind in emission bytes b, s of them;
\ false, with nothing written, where ADDRESS@ answers false.
: ADDRESS! ( n ^u8 n n n -- bool )
   {: v:n b:^ s:n site:n kind:n :}
   b s site kind SITE-SIZE {: size:n :}
   size 0= if false exit then
   b site +  v size ADDRESS-CARRIER:SET-CHAIN-VALUE  true ;

;package
;package
