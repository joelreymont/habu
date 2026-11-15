\ link-site.f - the Wasm part of the linker's address values
\ (src/habu/t0-wasm.f "THE ROWS"). An address site is `i64.const` and a padded
\ ten-byte s64 after it, for a code or a data address alike.
require src/lib/span.f
require src/arch/wasm/leb.f
require src/arch/wasm/code.f

package LINKER
private

package WASM

\ Whether emission bytes b, s of them, hold the opcode and its whole field at site.
: SITE? ( ptr u8 n n -- bool )
   {: b:ptr s:n site:n :}
   site 0<  site s 11 - >  or if false exit then
   b site + c@ WCODE:OP-I64-CONST = ;


\ Whether the ten bytes at p are a padded s64, as WLEB:S64-PAD@ reads one: nine
\ with the more bit, then a last byte that only copies the sign.
: PAD64? ( ptr u8 -- bool )
   {: p:ptr :}
   9 0 ?do  p i + c@ $80 and 0= if false unloop exit then  loop
   p 9 + c@ {: last:n :}
   last 0=  last $7F =  or ;

public

\ The address value emission bytes b, s of them, hold at site, and whether they hold
\ one there.
: ADDRESS@ ( ptr u8 n n n -- n bool )
   {: b:ptr s:n site:n kind:n :}
   b s site SITE? 0= if 0 false exit then
   b site + 1+ PAD64? 0= if 0 false exit then
   b s site 1+ WLEB:S64-PAD@ true ;


\ v written as the address value at site in emission bytes b, s of them; false, with
\ nothing written, where ADDRESS@ answers false.
: ADDRESS! ( n ptr u8 n n n -- bool )
   {: v:n b:ptr s:n site:n kind:n :}
   b s site SITE? 0= if false exit then
   v  b s SPAN:MAKE  site 1+ WLEB:S64-PATCH  true ;

;package
;package
