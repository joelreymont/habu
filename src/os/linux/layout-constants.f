\ layout-constants.f -- the Linux executable and data layout constants of both
\ Linux targets (linux-aarch64, linux-x86-64).
\ The image offsets are ELF64 format facts and read the same as the aarch64
\ seam's: IMAGE-TEXT-SIZE-OFF is the first program header's p_filesz (64-byte
\ header, then type, flags, offset, vaddr and paddr), and the two loader slots
\ sit past the read-write segment's dynamic table. DATA-VA is a Linux fixed
\ mapping, not an architecture fact: 13 GiB is as free above a non-PIE image at
\ $400000 and its brk on x86_64 as it is on aarch64, and both Linux seams keep
\ one mapping model rather than two addresses to reconcile.
\
\ DATA is one lazy anonymous reservation of DATA-SIZE bytes at DATA-VA, taken by
\ the boot before Habu code runs (sys.f MAP-DATA-RESERVE), charged only for the
\ pages it touches and never rebased, as macOS reserves its 1 TiB. The size is
\ a model fact, not a demand: the window must end inside the smallest user
\ address space an arm64 Linux kernel configures, CONFIG_ARM64_VA_BITS_39's 512
\ GiB, below its top-down mmap base, and DATA-VA + 2^38 = $4340000000 is the
\ largest power-of-two window from DATA-VA that fits. A 38-bit width also meets
\ the compiler's 48-bit address bound (src/compiler/native/select.f
\ MATERIALISE, a64ir.f DATA-OFFSET). Where the kernel will not reserve it lazily
\ - vm.overcommit_memory=2, or RLIMIT_AS or RLIMIT_DATA below the window - the
\ boot refuses with rc 78, which an engine names as "hb: cannot map fixed data
\ region".

96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$340000000 constant DATA-VA
$4000000000 constant DATA-SIZE
$1000 constant CODE-OFF
$B0 constant LINUX-DLOPEN-SLOT-OFF
$B8 constant LINUX-DLSYM-SLOT-OFF
