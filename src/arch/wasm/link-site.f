split-str
\ link-site.f - the Wasm part of the linker's address values
\ (src/habu/t0-wasm.f "THE ROWS"). An address site is `i64.const` and a padded
\ ten-byte s64 after it, for a code or a data address alike.
require src/lib/span.f
require src/arch/wasm/leb.f

package LINKER
private

package WASM

public

\ v written as the address value at site in emission bytes b, s of them.
: ADDRESS! ( n ^u8 n n n -- )
   {: v:n b:^ s:n site:n kind:n :}
   v  b s SPAN:MAKE  site 1+ WLEB:S64-PATCH ;

;package
;package
