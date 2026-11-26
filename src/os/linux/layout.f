\ layout.f -- the executable and data layout constants of both Linux targets
\ (linux-aarch64, linux-x86-64), as package LINUX-LAYOUT any host can load: a
\ Linux engine loads it at boot before `require` exists. A host's bare layout
\ names are its own (a macOS host's DATA-VA differs), so x86-64 code reads these
\ by prefix, as LINUX-LAYOUT:DATA-VA.
\ The image offsets are ELF64 format facts: IMAGE-TEXT-SIZE-OFF is the first
\ program header's p_filesz (64-byte header, then type, flags, offset, vaddr and
\ paddr). DATA-VA is a Linux fixed mapping, not an architecture fact: on
\ x86_64 as on aarch64 nothing maps from the code region's end, $4700000 above
\ a non-PIE image at $400000, up to the window, and at 2 GiB the window's first
\ 2 GiB lie below 4 GiB.
\
\ DATA is one lazy anonymous reservation of DATA-SIZE bytes at DATA-VA, taken by
\ the boot before Habu code runs, charged only for the pages it touches and
\ never rebased. The boot moves the image's carried pages into it, each a clean
\ file page until the program writes it. The size is a model fact, not a demand:
\ the window must end inside the smallest user address space an arm64 Linux
\ kernel configures outside CONFIG_EXPERT, CONFIG_ARM64_VA_BITS_39's 512 GiB,
\ below its top-down mmap base. The window ends at DATA-VA + 2^38 = $4080000000,
\ and 2^38 is the largest power of two that fits: 2^39 bytes alone fill that
\ space. A 38-bit width also meets the compiler's 48-bit address bound.
\ The boot rejects with rc 71, which an engine
\ names as "hb: mapping the fixed data region failed; raise the
\ address-space limit (ulimit -v) above its size in bytes: 0x0000004000000000",
\ where the kernel will not reserve the window lazily (vm.overcommit_memory=2,
\ or RLIMIT_AS or RLIMIT_DATA below it) or cannot place it: the expert-only
\ 36-bit arm64 address space ends inside it, and a 39-bit kernel in the legacy
\ bottom-up layout (vm.legacy_va_layout=1, setarch -L or an unlimited stack)
\ maps the loader and libraries from TASK_SIZE/4 = 128 GiB up, inside it.
\
\ The image's carrier, the read-write load that holds DATA's initial pages,
\ loads at CARRIER-VA, just past the window, so no mapping bounds its size; the
\ kernel starts the brk heap after it.

package LINUX-LAYOUT
public
96 constant IMAGE-TEXT-SIZE-OFF
$1000 constant IMAGE-TEXT-CONTENT-ADJ
0 constant IMAGE-TEXT-TRAILER-ADJ
$80000000 constant DATA-VA
$4000000000 constant DATA-SIZE
DATA-VA DATA-SIZE + constant CARRIER-VA
$1000 constant CODE-OFF
$400000 constant IMAGE-VA
;package
