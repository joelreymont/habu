\ elf.f - the ELF writer for Linux arm64: a dynamic executable of the program
\ in icode's CODE (BUILD-IMAGE), whose GOT ld.so binds to the libc names
\ BUILD-IMAGE is given.
\ Every PT_LOAD sits on a PROT-PAGE-MAX boundary so the text and the read-write
\ tail never share a kernel page on any supported page size.
\
\ OPEN reads the extents of such an executable: the ELF header, its four
\ program headers (the text's PT_LOAD, the read-write tail's PT_LOAD, PT_INTERP
\ and PT_DYNAMIC), the interpreter path and the hash, symbol, string and
\ relocation tables the dynamic section names. The code starts at the entry,
\ and the text's PT_LOAD maps the file from offset 0, so a file offset is an
\ address less that load's. Offsets come from these headers, never from the
\ inspecting host.

require src/arch/arm64/icode.f
require src/os/image-bytes.f
require src/lib/le.f
require src/lib/string.f
using A64ICODE
using IMAGE-BYTES
package ELF
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
\ Where the file's first byte maps.
variable VMBASE
$B0 constant ELF-DYNAMIC-SZ
$120 constant ELF-INTERP-OFF
27 constant ELF-INTERP-SZ
$140 constant ELF-HASH-OFF
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

\ The GOT follows the dynamic section and ends the read-write tail.
ELF-DYNAMIC-SZ constant ELF-GOT-OFF

public
\ The libc symbols the loader binds into the GOT of the tree's kernel, in slot
\ order, by C name; the kernel object carries them to the link.
: IMPORT$ ( n -- ptr u8 n )
   case
      0 of s" dlopen" endof
      1 of s" dlsym" endof
      2 of s" free" endof
      3 of s" realpath" endof
      s" elf: no such import" 73 die
   endcase ;
4 constant IMPORTS

\ The row whose C name this is; the build dies on a name the table lacks.
: IMPORT-ROW ( ptr u8 n -- n )
   {: a:ptr u:n :}
   IMPORTS 0 ?do i IMPORT$ a u STR= if i unloop exit then loop
   SB-RESET  s" elf: no such import " SB-APPEND  a u SB-APPEND
   SB$ 73 die ;

\ Import row n's cell, as a byte offset from the read-write tail's start.
: SLOT-OFF ( n -- n ) 8 * ELF-GOT-OFF + ;
private

\ The image's imports, which BUILD-IMAGE is given: C names in GOT row order,
\ each ending in a NUL.
PTR-VARIABLE NAMES
variable NAMES-U
variable NAMES-N

: NAMES@ ( -- ptr u8 ) NAMES @ ;

: NAMES! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a NAMES !  u NAMES-U !
   0  u 0 ?do a i + c@ 0= if 1 + then loop  NAMES-N ! ;

\ Whether a name starts at byte k of NAMES: the first, or one past a NUL.
: START? ( n -- bool )
   {: k:n :}
   k 0= if true exit then
   NAMES@ k + 1 - c@ 0= ;

: LIBC$ ( -- ptr u8 n ) s" libc.so.6" ;

: ELF-ALIGN8 ( n -- n ) 7 + -8 and ;

: ELF-RW-SZ ( -- n ) NAMES-N @ SLOT-OFF ;
\ One bucket, then a chain through every symbol after the null one.
: ELF-HASH-SZ ( -- n ) NAMES-N @ 4 + 4 * ;
: ELF-DYNSYM-OFF ( -- n ) ELF-HASH-OFF ELF-HASH-SZ + ELF-ALIGN8 ;
: ELF-DYNSTR-OFF ( -- n ) ELF-DYNSYM-OFF NAMES-N @ 1 + ELF-SYM-SZ * + ;
\ The string table opens with a NUL, then holds the names, then libc's.
: ELF-LIBC-OFF ( -- n ) NAMES-U @ 1 + ;
: ELF-DYNSTR-SZ ( -- n ) ELF-LIBC-OFF LIBC$ nip + 1 + ;
: ELF-RELA-OFF ( -- n ) ELF-DYNSTR-OFF ELF-DYNSTR-SZ + ELF-ALIGN8 ;
: ELF-RELA-SZ ( -- n ) NAMES-N @ ELF-RELA-ENT-SZ * ;
\ The generated executable window: everything the assembler can emit, behind the
\ header page it sits at.
CODE-CAP-BYTES CODE-OFF + constant MPAGE
public
variable CODELEN
private
variable ELF-TEXT-SIZE

: ELF-PAGE-UP ( n -- n )
   PROT-PAGE-MAX 1- + PROT-PAGE-MAX 1- invert and ;

\ The image buffer holds the maximum-page-rounded text and its RW tail.
: ELF-MSIZE! ( -- ) MPAGE ELF-PAGE-UP ELF-RW-SZ + MSIZE! ;

: ASM-CODELEN! ( -- )
   ASM-LEN CODELEN ! ;

public
: ASM-CODE ( -- asm )
   ASM-CODELEN!
   ASM-PHASE ;

: TEXTSZ ( -- n )  CODE-OFF CODELEN @ + ELF-PAGE-UP ;

private
: ELF-VA ( n -- n )
   VMBASE @ + ;

: ELF-RW-VA ( -- n )
   VMBASE @ ELF-TEXT-SIZE @ + ;

: ELF-SLOT-VA ( n -- n )
   SLOT-OFF ELF-RW-VA + ;

: ELF-IDENT ( -- )
   ELF-MAG0 IMG-M8  ELF-MAG1 IMG-M8  ELF-MAG2 IMG-M8  ELF-MAG3 IMG-M8
   ELFCLASS64 IMG-M8  ELFDATA2LSB IMG-M8  EV-CURRENT IMG-M8  0 IMG-M8  0 IMG-M8
   7 M-LEN M-ZEROS-LEN ;

: ELF-HDR, ( -- )
   ELF-IDENT
   ET-EXEC IMG-M16  EM-AARCH64 IMG-M16  EV-CURRENT IMG-M32
   VMBASE @ CODE-OFF + IMG-M64
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
   PT-LOAD PF-RX 0 VMBASE @ ELF-TEXT-SIZE @ PROT-PAGE-MAX ELF-PHDR, ;

: ELF-RW-PHDR, ( -- )
   PT-LOAD PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-RW-SZ PROT-PAGE-MAX ELF-PHDR, ;

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
   1 IMG-M32  NAMES-N @ 1 + IMG-M32  1 IMG-M32  0 IMG-M32
   NAMES-N @ 1 ?do i 1 + IMG-M32 loop
   0 IMG-M32 ;

: ELF-SYM-NULL, ( -- )
   ELF-SYM-SZ M-ZEROS ;

: ELF-SYM, ( n -- ) {: nameoff :}
   nameoff IMG-M32  $12 IMG-M8  0 IMG-M8  0 IMG-M16  0 IMG-M64  0 IMG-M64 ;

: ELF-DYNSYM, ( -- )
   ELF-DYNSYM-OFF M-OFF M-PAD-OFF
   ELF-SYM-NULL,
   NAMES-U @ 0 ?do i START? if i 1 + ELF-SYM, then loop ;

: ELF-DYNSTR, ( -- )
   ELF-DYNSTR-OFF M-OFF M-PAD-OFF
   0 IMG-M8
   NAMES@ NAMES-U @ M-BYTES
   LIBC$ M-BYTES 0 IMG-M8 ;

: ELF-R-INFO ( n -- n )
   32 lshift ELF-R-AARCH64-GLOB-DAT or ;

: ELF-RELA, ( -- )
   ELF-RELA-OFF M-OFF M-PAD-OFF
   NAMES-N @ 0 ?do
      i ELF-SLOT-VA IMG-M64  i 1 + ELF-R-INFO IMG-M64  0 IMG-M64
   loop ;

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
   DT-NEEDED   ELF-LIBC-OFF ELF-DYN,
   DT-FLAGS    DF-BIND-NOW ELF-DYN,
   0 0 ELF-DYN, ;

: ELF-GOT, ( -- )
   NAMES-N @ 0 ?do 0 IMG-M64 loop ;

: ELF-RW-AT, ( n -- ) {: off :}
   off M-OFF M-PAD-OFF
   ELF-DYNAMIC,
   ELF-GOT, ;

: BUILD-ELF ( -- )
   ASM-CODELEN!  ELF-MSIZE!  M-RESET
   CODELEN @  MPAGE CODE-OFF -  > IF s" elf: code exceeds text window" 73 die THEN
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   CODE CODELEN @ M-LEN M-BYTES-LEN
   TEXTSZ M-OFF M-PAD-OFF
   ELF-TEXT-SIZE @ ELF-RW-AT,
   M-HERE MLEN! ;

public
\ The program as an ELF file whose code maps at the address, its GOT binding
\ the C names, each ending in a NUL, in row order.
: BUILD-IMAGE ( asm n ptr u8 n -- img )
   NAMES!
   CODE-OFF - VMBASE !
   ASM-DROP
   BUILD-ELF
   IMG-PHASE ;

private

PTR-VARIABLE IMAGE
variable BYTES
variable RX-HDR
variable RW-HDR
variable INTERP-HDR
variable DYN-HDR
variable HASH-AT
variable SYM-AT
variable SYM-ENT
variable STR-AT
variable STR-BYTES
variable RELA-AT
variable RELA-ENT
variable RELA-BYTES

: CHECK ( bool ptr u8 n -- ) {: ok:bool why:ptr whyu:n :}
   ok 0= if why whyu 74 die then ;

: AT ( n n -- ptr u8 ) {: off:n size:n :}
   off 0 >= size 0 >= and off BYTES @ <= and
   size BYTES @ off - <= and
   s" elf: an extent passes the file's end" CHECK
   IMAGE @ off + ;

: U8@ ( n -- n ) 1 AT c@ ;
: U16@ ( n -- n ) 2 AT LE:U16@ ;
: U32@ ( n -- n ) 4 AT LE:U32@ ;
: U64@ ( n -- n ) 8 AT LE:U64@ ;

\ A program header's fields.
: P-OFF ( n -- n ) 8 + U64@ ;
: P-VA ( n -- n ) 16 + U64@ ;
: P-FILESZ ( n -- n ) 32 + U64@ ;
: P-MEMSZ ( n -- n ) 40 + U64@ ;
: P-ALIGN ( n -- n ) 48 + U64@ ;

: RX-VA ( -- n ) RX-HDR @ P-VA ;
: VA>OFF ( n -- n ) RX-VA - ;
: RW-OFF ( -- n ) RW-HDR @ P-OFF ;
: INTERP-BYTES ( -- n ) INTERP-HDR @ P-FILESZ ;
\ The hash table's header, buckets and chains: one chain per symbol.
: HASH-BYTES ( -- n )
   HASH-AT @ U32@ HASH-AT @ 4 + U32@ + 2 + 4 * ;
: SYM-BYTES ( -- n ) HASH-AT @ 4 + U32@ SYM-ENT @ * ;

public

: MAGIC? ( ptr u8 n -- bool ) {: image:ptr size:n :}
   size ELF-HDR-SZ < if false exit then
   image LE:U32@ $464C457F = ;

\ The ELF header and the program headers after it.
: HEADERS-BYTES ( -- n ) 32 U64@ 56 U16@ 54 U16@ * + ;
: TABLES-BYTES ( -- n )
   INTERP-BYTES HASH-BYTES + SYM-BYTES +
   STR-BYTES @ + RELA-BYTES @ + ;
: TEXT-OFF ( -- n ) 24 U64@ VA>OFF ;
: TEXT-LIMIT ( -- n ) RX-HDR @ P-FILESZ ;
: TEXT-ALIGN ( -- n ) RX-HDR @ P-ALIGN ;
: RW-BYTES ( -- n ) RW-HDR @ P-FILESZ ;

private

: CHECK-HEADER ( -- )
   4 U8@ ELFCLASS64 = 5 U8@ ELFDATA2LSB = and 6 U8@ EV-CURRENT = and
   16 U16@ ET-EXEC = and 18 U16@ EM-AARCH64 = and 20 U32@ EV-CURRENT = and
   s" elf: not a 64-bit little-endian arm64 executable" CHECK
   32 U64@ 52 U16@ = 52 U16@ ELF-HDR-SZ = and 54 U16@ ELF-PHDR-SZ = and
   56 U16@ ELF-PHDR-N = and
   s" elf: not four program headers after the header" CHECK
   40 U64@ 0= 60 U16@ 0= and
   s" elf: a section header table the linker does not write" CHECK ;

: UNIQUE! ( n ptr n -- ) {: off:n slot:ptr :}
   slot @ 0= s" elf: two program headers of one kind" CHECK
   off slot ! ;

: PHDR ( n -- ) {: off:n :}
   off U32@ off 4 + U32@ {: kind:n flags:n :}
   kind PT-LOAD = flags PF-RX = and if off RX-HDR UNIQUE! exit then
   kind PT-LOAD = flags PF-RW = and if off RW-HDR UNIQUE! exit then
   kind PT-INTERP = flags PF-R = and if off INTERP-HDR UNIQUE! exit then
   kind PT-DYNAMIC = flags PF-RW = and
   s" elf: a program header the linker does not write" CHECK
   off DYN-HDR UNIQUE! ;

: PHDRS ( -- )
   0 RX-HDR ! 0 RW-HDR ! 0 INTERP-HDR ! 0 DYN-HDR !
   56 U16@ 0 ?do 32 U64@ i ELF-PHDR-SZ * + PHDR loop ;

: POW2? ( n -- bool ) {: n:n :}
   n 0 > n n 1- and 0= and ;

\ The text's load maps the file from its first byte to a page boundary, and
\ the read-write load maps the rest at the address that follows it.
: CHECK-LOADS ( -- )
   TEXT-ALIGN POW2? s" elf: the text's page is not a power of two" CHECK
   RX-HDR @ P-OFF 0= TEXT-LIMIT RX-HDR @ P-MEMSZ = and
   RX-VA TEXT-ALIGN mod 0= and TEXT-LIMIT TEXT-ALIGN mod 0= and
   s" elf: the text's load is not on page boundaries" CHECK
   0 TEXT-LIMIT AT drop
   RW-OFF TEXT-LIMIT = RW-OFF RW-BYTES + BYTES @ = and
   RW-HDR @ P-VA RX-VA RW-OFF + = and
   RW-BYTES RW-HDR @ P-MEMSZ = and RW-HDR @ P-ALIGN TEXT-ALIGN = and
   s" elf: the read-write load does not end the file" CHECK
   TEXT-OFF HEADERS-BYTES > TEXT-OFF TEXT-LIMIT <= and
   s" elf: the entry is not in the text's load past the headers" CHECK ;

: DYN-ENTRY ( n n -- ) {: tag:n val:n :}
   tag DT-HASH = if val VA>OFF HASH-AT ! exit then
   tag DT-STRTAB = if val VA>OFF STR-AT ! exit then
   tag DT-SYMTAB = if val VA>OFF SYM-AT ! exit then
   tag DT-RELA = if val VA>OFF RELA-AT ! exit then
   tag DT-RELASZ = if val RELA-BYTES ! exit then
   tag DT-RELAENT = if val RELA-ENT ! exit then
   tag DT-STRSZ = if val STR-BYTES ! exit then
   tag DT-SYMENT = if val SYM-ENT ! then ;

\ True when the walk meets the DT_NULL entry that ends the section.
: DYN-WALK ( -- bool )
   DYN-HDR @ P-OFF {: at:n :}
   DYN-HDR @ P-FILESZ 16 / 0 ?do
      at i 16 * + {: e:n :}
      e U64@ 0= if true unloop exit then
      e U64@ e 8 + U64@ DYN-ENTRY
   loop false ;

: DYN-RESET ( -- )
   -1 HASH-AT ! -1 SYM-AT ! -1 SYM-ENT ! -1 STR-AT !
   -1 STR-BYTES ! -1 RELA-AT ! -1 RELA-ENT ! -1 RELA-BYTES ! ;

: TABLES? ( -- bool )
   HASH-AT @ 0 >= SYM-AT @ 0 >= and STR-AT @ 0 >= and
   RELA-AT @ 0 >= and STR-BYTES @ 0 >= and RELA-BYTES @ 0 >= and
   SYM-ENT @ ELF-SYM-SZ = and RELA-ENT @ ELF-RELA-ENT-SZ = and
   RELA-BYTES @ ELF-RELA-ENT-SZ mod 0= and ;

\ The dynamic section opens the read-write load.
: CHECK-DYNAMIC ( -- )
   DYN-HDR @ P-OFF RW-OFF = DYN-HDR @ P-VA RW-HDR @ P-VA = and
   DYN-HDR @ P-FILESZ RW-BYTES <= and
   DYN-HDR @ P-FILESZ 16 mod 0= and
   s" elf: the dynamic section is not at the read-write load" CHECK
   DYN-RESET DYN-WALK
   s" elf: the dynamic section has no end" CHECK
   TABLES? s" elf: the dynamic section lacks a table the linker writes"
   CHECK ;

\ Answers where the table at off with size bytes ends, given where the one
\ before it ends.
: NEXT ( n n n -- n ) {: end:n off:n size:n :}
   off end >= size 0 >= and
   s" elf: the dynamic-linking tables overlap" CHECK
   off size AT drop
   off size + ;

\ The tables lie in the linker's order between the headers and the text.
: CHECK-TABLES ( -- )
   INTERP-HDR @ P-OFF INTERP-HDR @ P-VA VA>OFF =
   s" elf: the interpreter's offset is not its address" CHECK
   HEADERS-BYTES
   INTERP-HDR @ P-OFF INTERP-BYTES NEXT
   HASH-AT @ HASH-BYTES NEXT
   SYM-AT @ SYM-BYTES NEXT
   STR-AT @ STR-BYTES @ NEXT
   RELA-AT @ RELA-BYTES @ NEXT
   TEXT-OFF <= s" elf: the dynamic-linking tables pass the text" CHECK ;

public

: OPEN ( ptr u8 n -- ) {: image:ptr size:n :}
   image size MAGIC? s" elf: not an ELF file" CHECK
   image IMAGE ! size BYTES !
   CHECK-HEADER PHDRS CHECK-LOADS CHECK-DYNAMIC CHECK-TABLES ;

;package
;using
;using
