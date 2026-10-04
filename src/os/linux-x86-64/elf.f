\ elf.f -- dynamic Linux/x86-64 ELF executable writer.
\ Provides the same image-builder surface as the aarch64 ELF and the Mach-O
\ writers: MBUF, MLEN@/!, MPAGE, ASM-CODE, BUILD-IMAGE.
\ Every PT_LOAD sits on a PROT-PAGE-MAX boundary so the text and the read-write
\ tail never share a kernel page on any supported page size.
\ Snapshot extras name the staged dynamic/GOT tail and its fixed byte size.
\ What differs from the aarch64 writer is what the architecture owns - e_machine,
\ the interpreter the dynamic loader is named by (one byte longer, which is why
\ ELF-INTERP-SZ is 28 and not 27) and the GOT relocation type - and two fixed
\ segments: this image maps its code region and DATA by PT_LOAD, where the
\ aarch64 boot maps both itself (docs/x86-64.md "Fixed segments"). Its six
\ program headers end at $190, so the metadata behind them starts there and not
\ at the aarch64 writer's $120.
\ Everything else here is ELF64 format, and reads the same under both machines.
\ The code stream it wraps is package X64CODE's (src/arch/x86-64/icode.f).
\ src/os/image-bytes.f sizes MSIZE from a bare CODE-CAP-BYTES at load, so this
\ file requires it under `using X64CODE`, and loads before src/arch/arm64/icode.f,
\ whose global CODE-CAP-BYTES the engine would refuse a bare X64CODE one beside.
\ The fixed DATA segment and CODE-OFF are the target's, read qualified from
\ package X64LAYOUT (src/os/linux-x86-64/target-layout.f), whose `using` below
\ is the guard that refuses a bare one and closes before this file ends. The
\ require below opens a package, and packages do not nest, so this file loads
\ at top level.
\ Retirement: habu-campaign-c2-mem-c3d7662b.
require src/arch/x86-64/icode.f
require src/os/linux-x86-64/target-layout.f

using X64CODE
require src/os/image-bytes.f
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

$7F constant ELF-MAG0
69 constant ELF-MAG1
76 constant ELF-MAG2
70 constant ELF-MAG3
2 constant ELFCLASS64
1 constant ELFDATA2LSB
1 constant EV-CURRENT
2 constant ET-EXEC
62 constant EM-X86-64
1 constant PT-LOAD
2 constant PT-DYNAMIC
3 constant PT-INTERP
1 constant PF-X
2 constant PF-W
4 constant PF-R
5 constant PF-RX
6 constant PF-RW
64 constant ELF-HDR-SZ
56 constant ELF-PHDR-SZ
6 constant ELF-PHDR-N
$400000 constant VMBASE
$C0 constant ELF-RW-SZ
$B0 constant ELF-DYNAMIC-SZ
$B0 constant ELF-DLOPEN-SLOT-OFF
$B8 constant ELF-DLSYM-SLOT-OFF
$190 constant ELF-INTERP-OFF
28 constant ELF-INTERP-SZ
$1B0 constant ELF-HASH-OFF
$1C8 constant ELF-DYNSYM-OFF
$210 constant ELF-DYNSTR-OFF
24 constant ELF-DYNSTR-SZ
$228 constant ELF-RELA-OFF
48 constant ELF-RELA-SZ
24 constant ELF-SYM-SZ
24 constant ELF-RELA-ENT-SZ
6 constant ELF-R-X86-64-GLOB-DAT
4 constant DT-HASH
5 constant DT-STRTAB
6 constant DT-SYMTAB
7 constant DT-RELA
8 constant DT-RELASZ
9 constant DT-RELAENT
10 constant DT-STRSZ
11 constant DT-SYMENT
1 constant DT-NEEDED
30 constant DT-FLAGS
8 constant DF-BIND-NOW
\ The generated executable window: everything the code layer admits, behind the
\ header page it sits at. It is DERIVED from X64CODE's window rather than
\ written out again, because the two are one fact and a second spelling of one
\ fact is a drift waiting for an editor.
CODE-CAP-BYTES X64LAYOUT:CODE-OFF + constant MPAGE
variable CODELEN
variable ELF-TEXT-SIZE
VMBASE REGION-OFF + constant ELF-REGION-VA

: ELF-PAGE-UP ( n -- n )
   PROT-PAGE-MAX 1- + PROT-PAGE-MAX 1- invert and ;

\ The image buffer includes the maximum-page-rounded text and its RW tail.
: ELF-MSIZE-CHECK ( -- )
   MPAGE ELF-PAGE-UP ELF-RW-SZ + MSIZE >
   IF s" elf: MSIZE below max image" 73 die THEN ;
ELF-MSIZE-CHECK

\ The region starts past the largest text and its RW tail, and DATA past the
\ region, so the loadable segments ascend by address, as ELF orders them, and
\ never overlap.
: ELF-FIXED-CHECK ( -- )
   MPAGE ELF-PAGE-UP ELF-RW-SZ + REGION-OFF >
   ELF-REGION-VA REGION + X64LAYOUT:DATA-VA > or
   IF s" elf: fixed segments overlap the image" 73 die THEN ;
ELF-FIXED-CHECK

: ASM-CODELEN! ( -- )
   ASM-LEN CODELEN ! ;

\ Assembly ends by linking the stream's labels at the address the text loads
\ at, so no image is written with a label site still zero.
: ASM-CODE ( -- asm )
   VMBASE X64LAYOUT:CODE-OFF + ASM-LINK
   ASM-CODELEN!
   ASM-PHASE ;

: TEXTSZ ( -- n )  X64LAYOUT:CODE-OFF CODELEN @ + ELF-PAGE-UP ;

: ELF-VA ( n -- n )
   VMBASE + ;

: ELF-RW-VA ( -- n )
   VMBASE ELF-TEXT-SIZE @ + ;

: ELF-DLOPEN-SLOT-VA ( -- n )
   ELF-RW-VA ELF-DLOPEN-SLOT-OFF + ;

: ELF-DLSYM-SLOT-VA ( -- n )
   ELF-RW-VA ELF-DLSYM-SLOT-OFF + ;

: ELF-IDENT ( -- )
   ELF-MAG0 IMG-M8  ELF-MAG1 IMG-M8  ELF-MAG2 IMG-M8  ELF-MAG3 IMG-M8
   ELFCLASS64 IMG-M8  ELFDATA2LSB IMG-M8  EV-CURRENT IMG-M8  0 IMG-M8  0 IMG-M8
   7 M-LEN M-ZEROS-LEN ;

: ELF-HDR, ( -- )
   ELF-IDENT
   ET-EXEC IMG-M16  EM-X86-64 IMG-M16  EV-CURRENT IMG-M32
   VMBASE X64LAYOUT:CODE-OFF + IMG-M64
   ELF-HDR-SZ IMG-M64
   0 IMG-M64
   0 IMG-M32
   ELF-HDR-SZ IMG-M16  ELF-PHDR-SZ IMG-M16  ELF-PHDR-N IMG-M16
   0 IMG-M16  0 IMG-M16  0 IMG-M16 ;

\ The kernel maps memsz bytes at va and zero-fills those past filesz.
: ELF-PHDR, ( n n n n n n n -- ) {: typ flags off va filesz memsz align :}
   typ IMG-M32
   flags IMG-M32
   off IMG-M64
   va IMG-M64
   va IMG-M64
   filesz IMG-M64
   memsz IMG-M64
   align IMG-M64 ;

: ELF-RX-PHDR, ( -- )
   PT-LOAD PF-RX 0 VMBASE ELF-TEXT-SIZE @ dup PROT-PAGE-MAX ELF-PHDR, ;

: ELF-RW-PHDR, ( -- )
   PT-LOAD PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-RW-SZ dup PROT-PAGE-MAX
   ELF-PHDR, ;

: ELF-INTERP-PHDR, ( -- )
   PT-INTERP PF-R ELF-INTERP-OFF ELF-INTERP-OFF ELF-VA ELF-INTERP-SZ dup 1
   ELF-PHDR, ;

: ELF-DYNAMIC-PHDR, ( -- )
   PT-DYNAMIC PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-DYNAMIC-SZ dup 8 ELF-PHDR, ;

\ A linked image carries leading region and DATA bytes; a bare image has
\ zero-filled fixed segments which its boot maps itself.
variable ELF-REGION-BYTES
variable ELF-DATA-BYTES

: ELF-REGION-AT ( -- n ) ELF-TEXT-SIZE @ ELF-RW-SZ + ELF-PAGE-UP ;
: ELF-DATA-AT ( -- n ) ELF-REGION-AT ELF-REGION-BYTES @ + ELF-PAGE-UP ;

: ELF-FIXED-PHDR, ( n n n n -- ) {: off:n va:n filesz:n memsz:n :}
   filesz 0= if 0 else off then {: at:n :}
   PT-LOAD PF-RW at va filesz memsz PROT-PAGE-MAX ELF-PHDR, ;

: ELF-REGION-PHDR, ( -- )
   ELF-REGION-AT ELF-REGION-VA ELF-REGION-BYTES @ REGION ELF-FIXED-PHDR, ;

: ELF-DATA-PHDR, ( -- )
   ELF-DATA-AT X64LAYOUT:DATA-VA ELF-DATA-BYTES @ X64LAYOUT:DATA-SIZE ELF-FIXED-PHDR, ;

\ The loadable segments ascend by address: text, RW tail, region, DATA.
: ELF-PHDRS, ( -- )
   ELF-RX-PHDR,
   ELF-RW-PHDR,
   ELF-INTERP-PHDR,
   ELF-DYNAMIC-PHDR,
   ELF-REGION-PHDR,
   ELF-DATA-PHDR, ;

: ELF-INTERP, ( -- )
   ELF-INTERP-OFF M-OFF M-PAD-OFF
   s" /lib64/ld-linux-x86-64.so.2" M-BYTES
   0 IMG-M8 ;

: ELF-HASH, ( -- )
   ELF-HASH-OFF M-OFF M-PAD-OFF
   1 IMG-M32  3 IMG-M32  1 IMG-M32  0 IMG-M32  2 IMG-M32  0 IMG-M32 ;

: ELF-SYM-NULL, ( -- )
   ELF-SYM-SZ M-ZEROS ;

: ELF-SYM, ( n -- ) {: nameoff :}
   nameoff IMG-M32  $12 IMG-M8  0 IMG-M8  0 IMG-M16  0 IMG-M64  0 IMG-M64 ;

: ELF-DYNSYM, ( -- )
   ELF-DYNSYM-OFF M-OFF M-PAD-OFF
   ELF-SYM-NULL,
   1 ELF-SYM,
   8 ELF-SYM, ;

: ELF-DYNSTR, ( -- )
   ELF-DYNSTR-OFF M-OFF M-PAD-OFF
   0 IMG-M8
   s" dlopen" M-BYTES 0 IMG-M8
   s" dlsym" M-BYTES 0 IMG-M8
   s" libc.so.6" M-BYTES 0 IMG-M8 ;

: ELF-R-INFO ( n -- n )
   32 lshift ELF-R-X86-64-GLOB-DAT or ;

: ELF-RELA, ( -- )
   ELF-RELA-OFF M-OFF M-PAD-OFF
   ELF-DLOPEN-SLOT-VA IMG-M64  1 ELF-R-INFO IMG-M64  0 IMG-M64
   ELF-DLSYM-SLOT-VA IMG-M64   2 ELF-R-INFO IMG-M64  0 IMG-M64 ;

: ELF-RX-META, ( -- )
   ELF-INTERP,
   ELF-HASH,
   ELF-DYNSYM,
   ELF-DYNSTR,
   ELF-RELA,
   X64LAYOUT:CODE-OFF M-OFF M-PAD-OFF ;

: ELF-DYN, ( n n -- ) {: tag val :}
   tag IMG-M64
   val IMG-M64 ;

: ELF-DYNAMIC, ( -- )
   DT-HASH     ELF-HASH-OFF ELF-VA ELF-DYN,
   DT-STRTAB   ELF-DYNSTR-OFF ELF-VA ELF-DYN,
   DT-SYMTAB   ELF-DYNSYM-OFF ELF-VA ELF-DYN,
   DT-STRSZ    ELF-DYNSTR-SZ ELF-DYN,
   DT-SYMENT   ELF-SYM-SZ ELF-DYN,
   DT-RELA     ELF-RELA-OFF ELF-VA ELF-DYN,
   DT-RELASZ   ELF-RELA-SZ ELF-DYN,
   DT-RELAENT  ELF-RELA-ENT-SZ ELF-DYN,
   DT-NEEDED   14 ELF-DYN,
   DT-FLAGS    DF-BIND-NOW ELF-DYN,
   0 0 ELF-DYN, ;

: ELF-GOT, ( -- )
   0 IMG-M64  0 IMG-M64 ;

: ELF-RW-AT, ( n -- ) {: off :}
   off M-OFF M-PAD-OFF
   ELF-DYNAMIC,
   ELF-GOT, ;

\ A caller-owned text span can be larger than the assembler's CODE buffer.
\ Build only its fixed header and dynamic tail here; the caller streams the
\ text and page padding between them at the offsets the same header computes.
: ELF-HEADER-FOR ( n -- )
   CODELEN !  M-RESET
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   M-HERE MLEN! ;

: ELF-RW-TAIL ( -- )
   M-RESET
   ELF-DYNAMIC,
   ELF-GOT,
   M-HERE MLEN! ;

: SNAP-EXTRA-PTR ( -- ptr u8 )
   MBUF X64LAYOUT:CODE-OFF + ;
s" SNAP-EXTRA-PTR" s" -- ptr u8" TRUST

$C0 constant SNAP-EXTRA-SIZE
s" SNAP-EXTRA-SIZE" s" -- n" TRUST

: ELF-BUILD-SPAN ( ptr u8 n -- ) {: text:ptr bytes:n :}
   bytes CODELEN !  M-RESET
   CODELEN @  MPAGE X64LAYOUT:CODE-OFF -  > IF s" elf: code exceeds text window" 73 die THEN
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   text bytes M-LEN M-BYTES-LEN
   TEXTSZ M-OFF M-PAD-OFF
   ELF-TEXT-SIZE @ ELF-RW-AT,
   M-HERE MLEN! ;

: BUILD-ELF ( -- )
   ASM-CODELEN!
   CODE CODELEN @ ELF-BUILD-SPAN ;

: BUILD-IMAGE ( asm -- img )
   ASM-DROP
   BUILD-ELF
   IMG-PHASE ;

: BUILD-SNAP-HDR ( n -- snap n ) {: snl :}
   X64LAYOUT:CODE-OFF snl + ELF-PAGE-UP {: sfts:n :}
   sfts ELF-TEXT-SIZE !
   M-RESET
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   X64LAYOUT:CODE-OFF ELF-RW-AT,
   SNAP-PHASE sfts ;

\ ---------------------------------------------------------------------------
\ Size-attribution tail: the bytes past the page-aligned text that the driver
\ (src/habu/driver-io.f DRV-SIZE-MARKS) has to attribute so the emitted size map
\ reconciles to the exact file length. Mirrors the Mach-O IMG-TAIL surface in
\ src/os/macos/sign2.f; a Linux image has no code signature, so its whole tail is
\ the read-write segment (DYNAMIC + GOT) BUILD-ELF appends after the text pad.
73 constant ELF-TAIL-RC
1 constant ELF-TAIL-N

: IMG-TAIL-N ( -- n )
   ELF-TAIL-N ;

: IMG-TAIL-NAME ( n -- ptr u8 n ) {: i:n :}
   i 0 = if s" container/rw-segment" exit then
   s" elf: size tail index out of range" ELF-TAIL-RC die ;

: IMG-TAIL-BYTES ( n -- n ) {: i:n :}
   i 0 = if ELF-RW-SZ exit then
   s" elf: size tail index out of range" ELF-TAIL-RC die ;

;using   \ X64LAYOUT
;using
