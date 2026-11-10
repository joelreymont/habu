\ link-site.f - the Wasm part of the linker's site tests and address values
\ (src/habu/t0-wasm.f "THE ROWS"). An address site is `i64.const` and a padded
\ ten-byte s64 after it, for a code or a data address alike; a call site, for a
\ call or a tail, is `call` and the padded five-byte u32 0; the does> slot is
\ `drop; i32.const 0; nop; nop; nop` (SLOT-CLOSE).
require src/lib/span.f
require src/arch/wasm/leb.f
require src/arch/wasm/code.f

package LINKER
private

package WASM

\ Whether emission bytes b, s of them, hold the opcode and its whole field at site.
: SITE? ( ptr u8 n n -- bool )
   {: b:ptr s:n site:n :}
   site 0<  site 11 + s >  or if false exit then
   b site + c@ WCODE:OP-I64-CONST = ;


\ Whether site and the five bytes after it lie inside s bytes.
: ROW-SITE? ( n n -- bool )
   {: s:n site:n :}
   site 0 >=  site 6 + s <=  and ;


\ Whether the five bytes at p are the padded u32 0.
: PAD0? ( ptr u8 -- bool )
   {: p:ptr :}
   p c@ $80 =  p 1 + c@ $80 =  and  p 2 + c@ $80 =  and  p 3 + c@ $80 =  and
   p 4 + c@ 0=  and ;

public

\ The address value emission bytes b, s of them, hold at site, and whether they hold
\ one there; a field that is not a padded s64 throws WLEB:E-UNPADDED.
: ADDRESS@ ( ptr u8 n n n -- n bool )
   {: b:ptr s:n site:n kind:n :}
   b s site SITE? 0= if 0 false exit then
   b s site 1+ WLEB:S64-PAD@ true ;


\ v written as the address value at site in emission bytes b, s of them; false, with
\ nothing written, where ADDRESS@ answers false.
: ADDRESS! ( n ptr u8 n n n -- bool )
   {: v:n b:ptr s:n site:n kind:n :}
   b s site SITE? 0= if false exit then
   v  b s SPAN:MAKE  site 1+ WLEB:S64-PATCH  true ;


\ Whether emission bytes b, s of them, hold at site the compile's unplaced call, of
\ either kind.
: CALL? ( ptr u8 n n n -- bool )
   {: b:ptr s:n site:n kind:n :}
   s site ROW-SITE? 0= if false exit then
   b site + c@ WCODE:OP-CALL =  b site + 1 + PAD0?  and ;


\ Whether emission bytes b, s of them, hold at site the compile's does> slot, which a clause
\ patches.
: SLOT? ( ptr u8 n n -- bool )
   {: b:ptr s:n site:n :}
   s site ROW-SITE? 0= if false exit then
   b site + {: p:ptr :}
   p c@ WCODE:OP-DROP =  p 1 + c@ WCODE:OP-I32-CONST =  and  p 2 + c@ 0=  and
   p 3 + c@ WCODE:OP-NOP =  and  p 4 + c@ WCODE:OP-NOP =  and  p 5 + c@ WCODE:OP-NOP =  and ;


\ Whether s bytes can be an emission.
: SIZE? ( n -- bool )
   {: s:n :}
   s 0 > ;

;package
;package
