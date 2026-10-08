\ layout-constants.f -- the Linux executable and data layout constants of both
\ Linux targets (linux-aarch64, linux-x86-64).
\ The image offsets are ELF64 format facts and read the same as the aarch64
\ seam's: IMAGE-TEXT-SIZE-OFF is the first program header's p_filesz (64-byte
\ header, then type, flags, offset, vaddr and paddr), and the two loader slots
\ sit past the read-write segment's dynamic table. DATA-VA is a Linux fixed
\ mapping, not an architecture fact: 13 GiB is as free above a non-PIE image at
\ $400000 and its brk on x86_64 as it is on aarch64, and both Linux seams keep
\ one mapping model rather than two addresses to reconcile.

96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$340000000 constant DATA-VA
$2000000 constant DATA-SIZE
$1000 constant CODE-OFF
$B0 constant LINUX-DLOPEN-SLOT-OFF
$B8 constant LINUX-DLSYM-SLOT-OFF
