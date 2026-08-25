\ layout.f -- linux-aarch64 executable/data layout constants.
\ Trusted rows publish fixed ELF image offsets/sizes and refine computed runtime
\ image/GOT addresses for header and loader-slot reads.
\ Retirement: habu-builder-trust-rows-c5d41af6.

96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$340000000 constant DATA-VA
$2000000 constant DATA-SIZE
$1000 constant CODE-OFF
$B0 constant LINUX-DLOPEN-SLOT-OFF
$B8 constant LINUX-DLSYM-SLOT-OFF

TRUSTED: LINUX-VA>PTR ( va -- ptr n ) ;

: LINUX-IMAGE-BASE ( -- n )
   rbase CODE-OFF - ;

: LINUX-TEXT-CELL ( -- ptr n )
   LINUX-IMAGE-BASE IMAGE-TEXT-SIZE-OFF + LINUX-VA>PTR ;

: LINUX-TEXT-SIZE ( -- n )
   LINUX-TEXT-CELL @ ;

: LINUX-RW-VA ( -- va )
   LINUX-IMAGE-BASE LINUX-TEXT-SIZE + ;

: DLOPEN-SLOT-VA ( -- va )
   LINUX-RW-VA LINUX-DLOPEN-SLOT-OFF + ;

: DLSYM-SLOT-VA ( -- va )
   LINUX-RW-VA LINUX-DLSYM-SLOT-OFF + ;

: DLOPEN-SLOT ( -- ptr n )
   DLOPEN-SLOT-VA LINUX-VA>PTR ;

: DLSYM-SLOT ( -- ptr n )
   DLSYM-SLOT-VA LINUX-VA>PTR ;
