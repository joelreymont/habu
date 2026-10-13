\ fs-root.f - checked filesystem root readiness predicates.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fs.f

package FS

1 constant X-OK
2 constant W-OK
W-OK X-OK or constant WRITE-SEARCH

public

: WRITABLE-ROOT? ( ptr u8 n -- bool )
   2dup DIR? 0= if 2drop 0 0= 0= exit then
   FS-PATHZ WRITE-SEARCH access 0= ;

;package
