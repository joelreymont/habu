\ The optional native-origin-cell is a kernel primitive. A later source
\ definition of the same name is not a capability.

package NATIVE-BODY
private
CAST: BODY-CELLS-XT ( n -- [ -- ptr u8 ] )

public
: BODY-CAPABILITY ( -- ptr u8 )
   s" native-origin-cell" 0 search-wl
   dup 0= if drop NULL-PTR exit then
   {: entry:n :}
   data-base $1C0 + @ {: base:n :}
   data-base $2CE0 + @ {: end:n :}
   end base <= if NULL-PTR exit then
   entry base >= entry end < and 0= if NULL-PTR exit then
   entry BODY-CELLS-XT execute ;
;package
