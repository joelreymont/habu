\ layout.f -- macos-aarch64 executable/data layout constants.

216 constant IMAGE-TEXT-SIZE-OFF
0 constant IMAGE-TEXT-CONTENT-ADJ
$1000 constant IMAGE-TEXT-TRAILER-ADJ
$44000000000 constant DATA-VA
$10000000000 constant DATA-SIZE
$1000 constant CODE-OFF
$3FFF constant MACHO-PAGE-MASK

TRUSTED: MACHO>N-PTR ( n -- ptr n ) ;

: MACHO-IMAGE-BASE ( -- n )
   rbase CODE-OFF - ;

: MACHO-TEXT-CELL ( -- ptr n )
   MACHO-IMAGE-BASE IMAGE-TEXT-SIZE-OFF + MACHO>N-PTR ;

: MACHO-TEXT-CONTENT ( -- n )
   MACHO-TEXT-CELL @ ;

: MACHO-PAGE-ALIGN ( n -- n ) {: n:n :}
   n MACHO-PAGE-MASK + MACHO-PAGE-MASK invert and ;

: MACHO-TEXT-SIZE ( -- n )
   CODE-OFF MACHO-TEXT-CONTENT + MACHO-PAGE-ALIGN ;

: DLOPEN-SLOT ( -- ptr n )
   MACHO-IMAGE-BASE MACHO-TEXT-SIZE + MACHO>N-PTR ;

: DLSYM-SLOT ( -- ptr n )
   DLOPEN-SLOT 8 + ;

s" IMAGE-TEXT-SIZE-OFF" s" -- n" TRUST
s" IMAGE-TEXT-CONTENT-ADJ" s" -- n" TRUST
s" IMAGE-TEXT-TRAILER-ADJ" s" -- n" TRUST
s" DATA-VA" s" -- ptr a" TRUST
s" DATA-SIZE" s" -- n" TRUST
s" CODE-OFF" s" -- n" TRUST
s" DLOPEN-SLOT" s" -- ptr n" TRUST
s" DLSYM-SLOT" s" -- ptr n" TRUST
