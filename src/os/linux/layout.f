\ layout.f -- the executable and data layout constants of both Linux targets
\ (linux-aarch64, linux-x86-64), loaded at boot before `require` exists.
\ The image offsets are ELF64 format facts: IMAGE-TEXT-SIZE-OFF is the first
\ program header's p_filesz (64-byte header, then type, flags, offset, vaddr and
\ paddr), and dlsym's slot is the GOT's second cell, past the read-write
\ segment's dynamic table. DATA-VA is a Linux fixed mapping, not an architecture
\ fact: 13 GiB is as free above a non-PIE image at $400000 and its brk on x86_64
\ as it is on aarch64.
\
\ DATA is one lazy anonymous reservation of DATA-SIZE bytes at DATA-VA, taken by
\ the boot before Habu code runs, charged only for the pages it touches and
\ never rebased. The size is a model fact, not a demand: the window must end
\ inside the smallest user address space an arm64 Linux kernel configures
\ outside CONFIG_EXPERT, CONFIG_ARM64_VA_BITS_39's 512 GiB, below its top-down
\ mmap base, and DATA-VA + 2^38 = $4340000000 is the largest power-of-two window
\ from DATA-VA that fits. A 38-bit width also meets the compiler's 48-bit
\ address bound. The boot refuses with rc 78, which an engine names as
\ "E-MAP-FAIL habu: cannot map fixed data region", where the kernel will not
\ reserve the window lazily (vm.overcommit_memory=2, or RLIMIT_AS or RLIMIT_DATA
\ below it)
\ or cannot place it: the expert-only 36-bit arm64 address space ends inside it,
\ and a 39-bit kernel in the legacy bottom-up layout (vm.legacy_va_layout=1,
\ setarch -L or an unlimited stack) maps the loader and libraries from
\ TASK_SIZE/4 = 128 GiB up, inside it.

96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$340000000 constant DATA-VA
$4000000000 constant DATA-SIZE
$1000 constant CODE-OFF
$400000 constant IMAGE-VA
$B8 constant LINUX-DLSYM-SLOT-OFF
