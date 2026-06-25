\ elf.f -- minimal Linux/aarch64 ELF executable writer.
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
5 constant PF-RX
64 constant ELF-HDR-SZ
56 constant ELF-PHDR-SZ
$400000 constant VMBASE
$80000 constant MPAGE
variable CODELEN
variable ELF-TEXT-SIZE

: ASM-CODE ( -- )  ASM-LEN CODELEN ! ;
s" ASM-CODE" s" --" TRUST

: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $FFF +  $FFF invert and ;

: ELF-IDENT ( -- )
   ELF-MAG0 M8  ELF-MAG1 M8  ELF-MAG2 M8  ELF-MAG3 M8
   ELFCLASS64 M8  ELFDATA2LSB M8  EV-CURRENT M8  0 M8  0 M8
   7 M-ZEROS ;

: ELF-HDR, ( -- )
   ELF-IDENT
   ET-EXEC M16  EM-AARCH64 M16  EV-CURRENT M32
   VMBASE CODE-OFF + M64
   ELF-HDR-SZ M64
   0 M64
   0 M32
   ELF-HDR-SZ M16  ELF-PHDR-SZ M16  1 M16
   0 M16  0 M16  0 M16 ;

: ELF-PHDR, ( -- )
   PT-LOAD M32
   PF-RX M32
   0 M64
   VMBASE M64
   VMBASE M64
   ELF-TEXT-SIZE @ M64
   ELF-TEXT-SIZE @ M64
   $1000 M64 ;

: BUILD-ELF ( -- )
   ASM-CODE  M-RESET
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDR,
   CODE-OFF M-PAD
   CODE CODELEN @ M-BYTES
   TEXTSZ M-PAD
   M-HERE MLEN ! ;

: BUILD-IMAGE ( -- )  BUILD-ELF ;
s" BUILD-IMAGE" s" --" TRUST

: BUILD-SNAP-HDR ( n -- n ) {: snl :}
   CODE-OFF snl + $FFF + $FFF invert and {: sfts :}
   sfts ELF-TEXT-SIZE !
   M-RESET
   ELF-HDR,
   ELF-PHDR,
   CODE-OFF M-PAD
   sfts ;
s" BUILD-SNAP-HDR" s" n -- n" TRUST
