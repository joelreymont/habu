\ target-layout.f -- the linux-x86-64 target's layout for a cross build,
\ package X64LAYOUT.
\ The host's layout names the host's own DATA (a macOS host maps it at
\ $44000000000), and the engine refuses a second CODE-OFF beside it, so the
\ target's layout.f is replayed here, privately, once for every x86-64 file that
\ builds against it: the image writer (elf.f), the boot (src/habu/boot-x64.f)
\ and the kernel (src/habu/kernel-x64.f).
\ Read each value qualified, X64LAYOUT:DATA-VA, never bare: a bare read binds
\ the host's global, which a Linux host gives the same value and a macOS host
\ does not. So each x86 source opens `using X64LAYOUT` after its last load and
\ closes it before its end: under that guard a bare DATA-VA, DATA-SIZE, CODE-OFF
\ or IMAGE-TEXT-SIZE-OFF refuses by name on every host, rc 67 in a definition
\ and rc 105 at top level. A macOS host has no LINUX-DLSYM-SLOT-OFF global, so
\ there a bare one reads this package's value. Bodies nothing certifies
\ (TRUSTED:, 0 set-check) keep global-first, outside the guard.
\ The public constants copy the replayed values rather than EXPORT them:
\ tools/check.f preverifies without replaying another target's layout, so there
\ DATA-VA is the engine's own, and an EXPORT of it is a word checked code may
\ not call (E-CAP-TRUSTED).
\ It opens a package, and packages do not nest, so it loads at top level.

package X64LAYOUT
s" src/os/linux-x86-64/layout.f" included
public
DATA-VA constant DATA-VA
DATA-SIZE constant DATA-SIZE
CODE-OFF constant CODE-OFF
IMAGE-TEXT-SIZE-OFF constant IMAGE-TEXT-SIZE-OFF
LINUX-DLSYM-SLOT-OFF constant LINUX-DLSYM-SLOT-OFF
;package
