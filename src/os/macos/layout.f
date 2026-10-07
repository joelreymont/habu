\ layout.f -- macos-aarch64 executable/data layout constants.

$D8 constant IMAGE-TEXT-SIZE-OFF
0 constant IMAGE-TEXT-CONTENT-ADJ
$1000 constant IMAGE-TEXT-TRAILER-ADJ
$44000000000 constant DATA-VA
$10000000000 constant DATA-SIZE
$1000 constant CODE-OFF
$3FFF constant MACHO-PAGE-MASK

package IMAGE-CELL
private
\ The running image's cells are addressed by byte offset from its base, an integer.
CAST: N>CELL ( n -- ptr n )
public
\ The cell at byte offset n into the running image.
: AT ( n -- ptr n ) rbase CODE-OFF - + N>CELL ;
;package

: MACHO-TEXT-CELL ( -- ptr n )
   IMAGE-TEXT-SIZE-OFF IMAGE-CELL:AT ;

: MACHO-TEXT-CONTENT ( -- n )
   MACHO-TEXT-CELL @ ;

: MACHO-PAGE-ALIGN ( n -- n )
   MACHO-PAGE-MASK + MACHO-PAGE-MASK invert and ;

: MACHO-TEXT-SIZE ( -- n )
   CODE-OFF MACHO-TEXT-CONTENT + MACHO-PAGE-ALIGN ;

: DLOPEN-SLOT ( -- ptr n )
   MACHO-TEXT-SIZE IMAGE-CELL:AT ;

: DLSYM-SLOT ( -- ptr n )
   DLOPEN-SLOT $8 + ;
