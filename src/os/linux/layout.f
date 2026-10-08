\ layout.f -- the executable/data layout of both Linux targets (aarch64,
\ x86-64). Its constants are src/os/linux/layout-constants.f, which every
\ loader names just before this file: the boot prefix and the build window
\ load both ahead of src/core/include.f, which defines `require`.

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
