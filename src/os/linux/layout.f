\ layout.f -- linux-aarch64 executable/data layout constants.

96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$340000000 constant DATA-VA
$300000 constant DATA-SIZE
$1000 constant CODE-OFF
$5000B0 constant DLOPEN-SLOT
$5000B8 constant DLSYM-SLOT

s" IMAGE-TEXT-SIZE-OFF" s" -- n" TRUST
s" IMAGE-TEXT-CONTENT-ADJ" s" -- n" TRUST
s" IMAGE-TEXT-TRAILER-ADJ" s" -- n" TRUST
s" DATA-VA" s" -- ptr a" TRUST
s" DATA-SIZE" s" -- n" TRUST
s" CODE-OFF" s" -- n" TRUST
s" DLOPEN-SLOT" s" -- ptr n" TRUST
s" DLSYM-SLOT" s" -- ptr n" TRUST
