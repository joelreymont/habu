\ elf.fs -- minimal Linux/aarch64 ELF executable writer for the Gforth bootstrap.

require asm.fs
require image.fs

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
$1000 constant CODE-OFF
variable CODELEN
variable SCODE   variable SCODE-CAP
2variable SIG-ID

\ The text segment follows the emitted program: PASS1 sizes the scratch buffer,
\ PASS2 fills it, and TEXTSZ carries that length into the segment headers. There
\ is no fixed text page. An image is bounded by instruction reach, which asm.fs
\ checks per instruction (?ADR, ?REL19, ?REL26), and by nothing else -- a page
\ chosen as a round number refused images the loader would have accepted.
: ASM-CODE ( -- )
   PASS1  WPOS @ 4 *  SCODE SCODE-CAP BUF-FIT
   SCODE @ ASSEMBLE CODELEN ! ;

: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $FFF +  $FFF invert and ;

: ELF-IDENT
   ELF-MAG0 M8  ELF-MAG1 M8  ELF-MAG2 M8  ELF-MAG3 M8
   ELFCLASS64 M8  ELFDATA2LSB M8  EV-CURRENT M8  0 M8  0 M8
   7 M-ZEROS ;

: ELF-HDR,
   ELF-IDENT
   ET-EXEC M16  EM-AARCH64 M16  EV-CURRENT M32
   VMBASE CODE-OFF + M64
   ELF-HDR-SZ M64
   0 M64
   0 M32
   ELF-HDR-SZ M16  ELF-PHDR-SZ M16  1 M16
   0 M16  0 M16  0 M16 ;

: ELF-PHDR,
   PT-LOAD M32
   PF-RX M32
   0 M64
   VMBASE M64
   VMBASE M64
   TEXTSZ M64
   TEXTSZ M64
   $1000 M64 ;

: BUILD-ELF ( -- )
   ASM-CODE  TEXTSZ M-FIT  M-RESET
   ELF-HDR,
   ELF-PHDR,
   CODE-OFF M-PAD
   SCODE @ CODELEN @ M-BYTES
   TEXTSZ M-PAD
   M-HERE MLEN ! ;

: BUILD-IMAGE ( -- )  BUILD-ELF ;

: SIGN-IMAGE ( -- ) ;
