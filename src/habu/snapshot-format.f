\ Snapshot wire format and the baked loader's immutable capability.
\ Keep this separate from the address-cell storage ABI: an old donor may load
\ current source while its native startup still understands only v10 images.
require src/habu/layout.f

package SNAPSHOT-FORMAT
public

11 constant VERSION

private
TRUSTED: VERSION-XT ( n -- [ -- n ] ) ;
TRUSTED: TEXT-BASE ( -- n ) data-base RBASE-CELL + @ ;
TRUSTED: TEXT-SIZE ( -- n )
   TEXT-BASE CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - ;

public

\ Resolve wordlist zero directly: a source definition with the same spelling
\ cannot supply the loader's capability. Retained engine text is the authority.
: SUPPORTED? ( -- bool )
   s" snapshot-format" 0 search-wl {: xt:n :}
   xt 0= if false exit then
   xt TEXT-BASE < xt TEXT-BASE - TEXT-SIZE >= or if
      s" snap: format capability is not an engine primitive" 74 die
   then
   xt VERSION-XT execute VERSION = ;

: VERIFY ( -- )
   SUPPORTED? 0= if
      s" snap: donor does not support snapshot format" 74 die
   then ;

;package
