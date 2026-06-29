\ layout.f -- linux-aarch64 executable/data layout constants.

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
   LINUX-IMAGE-BASE IMAGE-TEXT-SIZE-OFF + >VA LINUX-VA>PTR ;

: LINUX-TEXT-SIZE ( -- n )
   LINUX-TEXT-CELL @ ;

: LINUX-RW-VA ( -- va )
   LINUX-IMAGE-BASE LINUX-TEXT-SIZE + >VA ;

: DLOPEN-SLOT-VA ( -- va )
   LINUX-RW-VA VA>N LINUX-DLOPEN-SLOT-OFF + >VA ;

: DLSYM-SLOT-VA ( -- va )
   LINUX-RW-VA VA>N LINUX-DLSYM-SLOT-OFF + >VA ;

: DLOPEN-SLOT ( -- ptr n )
   DLOPEN-SLOT-VA LINUX-VA>PTR ;

: DLSYM-SLOT ( -- ptr n )
   DLSYM-SLOT-VA LINUX-VA>PTR ;

s" IMAGE-TEXT-SIZE-OFF" s" -- n" TRUST
s" IMAGE-TEXT-CONTENT-ADJ" s" -- n" TRUST
s" IMAGE-TEXT-TRAILER-ADJ" s" -- n" TRUST
s" DATA-VA" s" -- ptr a" TRUST
s" DATA-SIZE" s" -- n" TRUST
s" CODE-OFF" s" -- n" TRUST
s" LINUX-DLOPEN-SLOT-OFF" s" -- n" TRUST
s" LINUX-DLSYM-SLOT-OFF" s" -- n" TRUST
s" LINUX-IMAGE-BASE" s" -- n" TRUST
s" LINUX-TEXT-CELL" s" -- ptr n" TRUST
s" LINUX-TEXT-SIZE" s" -- n" TRUST
s" LINUX-RW-VA" s" -- va" TRUST
s" DLOPEN-SLOT-VA" s" -- va" TRUST
s" DLSYM-SLOT-VA" s" -- va" TRUST
s" DLOPEN-SLOT" s" -- ptr n" TRUST
s" DLSYM-SLOT" s" -- ptr n" TRUST
