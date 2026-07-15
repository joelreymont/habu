\ diff-file.f - exact-size OS-backed framed-artifact loader.

require lib/errors.f
require lib/memory.f
require lib/fs.f

package DIFF-FILE
public

: LOAD ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: u:n :}
   u 0= if 1 else u then MEM-ALLOC-BYTES drop {: a:ptr :}
   path pathu a u READ-ALL u <> if E-FS-IO throw then
   a u ;

;package
