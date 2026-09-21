\ bootstrap-strip.f - CLI used after the Gforth seed exists.
\ Run: <engine> --load tools/bootstrap-strip.f -- <input> <output>

require lib/fs.f
require lib/memory.f
require tools/bootstrap-strip-lib.f

package BOOTSTRIP-CLI
private

74 constant RC

: USAGE ( -- )
   s" bootstrap-strip: input and output paths are required" RC die ;

public

: MAIN ( -- )
   SCRIPT-ARGC 2 <> if USAGE then
   0 SCRIPT-ARGV$ {: in:ptr inu:n :}
   1 SCRIPT-ARGV$ {: out:ptr outu:n :}
   in inu FILE-SIZE {: size:n :}
   size MEM-ALLOC-BYTES {: src:ptr cap:n :}
   size MEM-ALLOC-BYTES {: dst:ptr outcap:n :}
   in inu src cap READ-ALL {: got:n :}
   src got dst outcap BOOTSTRIP:STRIP-BYTES {: outn:n :}
   out outu dst outn WRITE-ALL
   s" bootstrap-strip: " type outn . s" bytes" type cr ;

;package

BOOTSTRIP-CLI:MAIN
