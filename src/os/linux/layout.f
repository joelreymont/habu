\ layout.f -- linux-aarch64 executable/data layout constants.

96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$340000000 constant DATA-VA
$2000000 constant DATA-SIZE
$1000 constant CODE-OFF
$B0 constant LINUX-DLOPEN-SLOT-OFF
$B8 constant LINUX-DLSYM-SLOT-OFF

package IMAGE-CELL
private
\ The running image's cells are addressed by byte offset from its base, an integer.
CAST: N>CELL ( n -- ptr n )
public
\ The cell at byte offset n into the running image.
: AT ( n -- ptr n ) rbase CODE-OFF - + N>CELL ;
;package

: LINUX-TEXT-CELL ( -- ptr n )
   IMAGE-TEXT-SIZE-OFF IMAGE-CELL:AT ;

: LINUX-TEXT-SIZE ( -- n )
   LINUX-TEXT-CELL @ ;

: DLOPEN-SLOT ( -- ptr n )
   LINUX-TEXT-SIZE LINUX-DLOPEN-SLOT-OFF + IMAGE-CELL:AT ;

: DLSYM-SLOT ( -- ptr n )
   LINUX-TEXT-SIZE LINUX-DLSYM-SLOT-OFF + IMAGE-CELL:AT ;
