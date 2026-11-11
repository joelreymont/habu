\ The optional native-origin-cell is a kernel primitive. A later source
\ definition of the same name is not a capability.

package NATIVE-BODY
private
CAST: BODY-CELLS-XT ( n -- [ -- ptr u8 ] )

public
: BODY-CAPABILITY ( -- ptr u8 )
   s" native-origin-cell" WL:PRIMITIVES search-wl
   dup 0= if drop NULL-PTR exit then
   {: entry:n :}
   data-base RBASE-CELL + @ {: base:n :}
   data-base CODE-END-CELL + @ {: end:n :}
   end base <= if NULL-PTR exit then
   entry base >= entry end < and 0= if NULL-PTR exit then
   entry BODY-CELLS-XT execute ;
;package
