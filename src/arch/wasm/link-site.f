\ link-site.f - the Wasm part of the linker's address values. An address site is
\ `i64.const` and a padded ten-byte s64 after it, for a code or a data address
\ alike (src/habu/t0-wasm.f "THE ROWS").
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

;package
;package
