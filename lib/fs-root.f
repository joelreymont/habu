\ fs-root.f - checked filesystem root readiness predicates.
\ Load after lib/errors.f and lib/fs.f.

require lib/errors.f
require lib/string.f
require lib/fs.f

package FS

2 constant W-OK

public

: WRITABLE-ROOT? ( ptr u8 n -- bool )
   2dup DIR? 0= if 2drop 0 0= 0= exit then
   FS-PATHZ W-OK access 0= ;

;package
