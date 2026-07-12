\ elf.f -- dynamic Linux/aarch64 ELF executable writer.
\ Provides the same image-builder surface as the Mach-O writer: MBUF, MLEN@/!,
\ CODE-OFF, MPAGE, ASM-CODE, BUILD-IMAGE.

$7F constant ELF-MAG0
69 constant ELF-MAG1
76 constant ELF-MAG2
70 constant ELF-MAG3
2 constant ELFCLASS64
1 constant ELFDATA2LSB
1 constant EV-CURRENT
2 constant ET-EXEC
$B7 constant EM-AARCH64
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
4 constant ELF-PHDR-N
$400000 constant VMBASE
$C0 constant ELF-RW-SZ
$B0 constant ELF-DYNAMIC-SZ
$B0 constant ELF-DLOPEN-SLOT-OFF
$B8 constant ELF-DLSYM-SLOT-OFF
$120 constant ELF-INTERP-OFF
27 constant ELF-INTERP-SZ
$140 constant ELF-HASH-OFF
$158 constant ELF-DYNSYM-OFF
$1A0 constant ELF-DYNSTR-OFF
24 constant ELF-DYNSTR-SZ
$1B8 constant ELF-RELA-OFF
48 constant ELF-RELA-SZ
24 constant ELF-SYM-SZ
24 constant ELF-RELA-ENT-SZ
$401 constant ELF-R-AARCH64-GLOB-DAT
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
$200000 constant MPAGE
variable CODELEN
variable ELF-TEXT-SIZE

\ Fail closed at load time: the image buffer must fit the largest emittable
\ image (text at MPAGE + the RW tail) so the loud MPAGE code-window guard
\ stays the binding constraint, never a silent M-BOUNDS-RC throw from the
\ byte cursor.
: ELF-MSIZE-CHECK ( -- )
   CODE-CAP-BYTES CODE-OFF + MPAGE <>
   IF s" elf: code capacity differs from MPAGE" 73 die THEN
   MPAGE ELF-RW-SZ + MSIZE >
   IF s" elf: MSIZE below max image" 73 die THEN ;
ELF-MSIZE-CHECK

: ASM-CODELEN! ( -- )
   ASM-LEN CODELEN ! ;

: ASM-CODE ( -- asm )
   ASM-CODELEN!
   ASM-PHASE ;

: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $FFF +  $FFF invert and ;

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
   ET-EXEC IMG-M16  EM-AARCH64 IMG-M16  EV-CURRENT IMG-M32
   VMBASE CODE-OFF + IMG-M64
   ELF-HDR-SZ IMG-M64
   0 IMG-M64
   0 IMG-M32
   ELF-HDR-SZ IMG-M16  ELF-PHDR-SZ IMG-M16  ELF-PHDR-N IMG-M16
   0 IMG-M16  0 IMG-M16  0 IMG-M16 ;

: ELF-PHDR, ( n n n n n n -- ) {: typ flags off va filesz align :}
   typ IMG-M32
   flags IMG-M32
   off IMG-M64
   va IMG-M64
   va IMG-M64
   filesz IMG-M64
   filesz IMG-M64
   align IMG-M64 ;

: ELF-RX-PHDR, ( -- )
   PT-LOAD PF-RX 0 VMBASE ELF-TEXT-SIZE @ $1000 ELF-PHDR, ;

: ELF-RW-PHDR, ( -- )
   PT-LOAD PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-RW-SZ $1000 ELF-PHDR, ;

: ELF-INTERP-PHDR, ( -- )
   PT-INTERP PF-R ELF-INTERP-OFF ELF-INTERP-OFF ELF-VA ELF-INTERP-SZ 1
   ELF-PHDR, ;

: ELF-DYNAMIC-PHDR, ( -- )
   PT-DYNAMIC PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-DYNAMIC-SZ 8 ELF-PHDR, ;

: ELF-PHDRS, ( -- )
   ELF-RX-PHDR,
   ELF-RW-PHDR,
   ELF-INTERP-PHDR,
   ELF-DYNAMIC-PHDR, ;

: ELF-INTERP, ( -- )
   ELF-INTERP-OFF M-OFF M-PAD-OFF
   s" /lib/ld-linux-aarch64.so.1" M-BYTES
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
   32 lshift ELF-R-AARCH64-GLOB-DAT or ;

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
   CODE-OFF M-OFF M-PAD-OFF ;

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

: SNAP-EXTRA-PTR ( -- ptr u8 )
   MBUF CODE-OFF + ;
s" SNAP-EXTRA-PTR" s" -- ptr u8" TRUST

$C0 constant SNAP-EXTRA-SIZE
s" SNAP-EXTRA-SIZE" s" -- n" TRUST

: BUILD-ELF ( -- )
   ASM-CODELEN!  M-RESET
   CODELEN @  MPAGE CODE-OFF -  > IF s" elf: code exceeds text window" 73 die THEN
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   CODE CODELEN @ M-LEN M-BYTES-LEN
   TEXTSZ M-OFF M-PAD-OFF
   ELF-TEXT-SIZE @ ELF-RW-AT,
   M-HERE MLEN! ;

: BUILD-IMAGE ( asm -- img )
   ASM-DROP
   BUILD-ELF
   IMG-PHASE ;

: BUILD-SNAP-HDR ( n -- snap n ) {: snl :}
   CODE-OFF snl + $FFF + $FFF invert and {: sfts :}
   sfts ELF-TEXT-SIZE !
   M-RESET
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   CODE-OFF ELF-RW-AT,
   SNAP-PHASE sfts ;
