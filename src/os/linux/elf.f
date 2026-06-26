\ elf.f -- dynamic Linux/aarch64 ELF executable writer.
\ Provides the same image-builder surface as the Mach-O writer: MBUF, MLEN,
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
$500000 constant ELF-RW-VA
$C0 constant ELF-RW-SZ
$B0 constant ELF-DYNAMIC-SZ
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
$80000 constant MPAGE
variable CODELEN
variable ELF-TEXT-SIZE

: ASM-CODELEN! ( -- )
   ASM-LEN CODELEN ! ;

: ASM-CODE ( -- asm )
   ASM-CODELEN!
   ASM-PHASE ;

: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $FFF +  $FFF invert and ;

: ELF-VA ( n -- n )
   VMBASE + ;

: ELF-IDENT ( -- )
   ELF-MAG0 M8  ELF-MAG1 M8  ELF-MAG2 M8  ELF-MAG3 M8
   ELFCLASS64 M8  ELFDATA2LSB M8  EV-CURRENT M8  0 M8  0 M8
   7 M-LEN M-ZEROS-LEN ;

: ELF-HDR, ( -- )
   ELF-IDENT
   ET-EXEC M16  EM-AARCH64 M16  EV-CURRENT M32
   VMBASE CODE-OFF + M64
   ELF-HDR-SZ M64
   0 M64
   0 M32
   ELF-HDR-SZ M16  ELF-PHDR-SZ M16  ELF-PHDR-N M16
   0 M16  0 M16  0 M16 ;

: ELF-PHDR, ( n n n n n n -- ) {: typ flags off va filesz align :}
   typ M32
   flags M32
   off M64
   va M64
   va M64
   filesz M64
   filesz M64
   align M64 ;

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
   0 M8 ;

: ELF-HASH, ( -- )
   ELF-HASH-OFF M-OFF M-PAD-OFF
   1 M32  3 M32  1 M32  0 M32  2 M32  0 M32 ;

: ELF-SYM-NULL, ( -- )
   ELF-SYM-SZ M-ZEROS ;

: ELF-SYM, ( n -- ) {: nameoff :}
   nameoff M32  $12 M8  0 M8  0 M16  0 M64  0 M64 ;

: ELF-DYNSYM, ( -- )
   ELF-DYNSYM-OFF M-OFF M-PAD-OFF
   ELF-SYM-NULL,
   1 ELF-SYM,
   8 ELF-SYM, ;

: ELF-DYNSTR, ( -- )
   ELF-DYNSTR-OFF M-OFF M-PAD-OFF
   0 M8
   s" dlopen" M-BYTES 0 M8
   s" dlsym" M-BYTES 0 M8
   s" libc.so.6" M-BYTES 0 M8 ;

: ELF-R-INFO ( n -- n )
   32 lshift ELF-R-AARCH64-GLOB-DAT or ;

: ELF-RELA, ( -- )
   ELF-RELA-OFF M-OFF M-PAD-OFF
   DLOPEN-SLOT-VA VA>N M64  1 ELF-R-INFO M64  0 M64
   DLSYM-SLOT-VA VA>N M64   2 ELF-R-INFO M64  0 M64 ;

: ELF-RX-META, ( -- )
   ELF-INTERP,
   ELF-HASH,
   ELF-DYNSYM,
   ELF-DYNSTR,
   ELF-RELA,
   CODE-OFF M-OFF M-PAD-OFF ;

: ELF-DYN, ( n n -- ) {: tag val :}
   tag M64
   val M64 ;

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
   0 M64  0 M64 ;

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
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   CODE CODELEN @ M-LEN M-BYTES-LEN
   TEXTSZ M-OFF M-PAD-OFF
   ELF-TEXT-SIZE @ ELF-RW-AT,
   M-HERE MLEN ! ;

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
