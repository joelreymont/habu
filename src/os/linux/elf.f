\ elf.f - the ELF writer for Linux arm64 and x86-64. The kernel object
\ supplies the libc names its GOT binds.
\ Every PT_LOAD sits on a PAGE-BYTES boundary so the text and the read-write
\ tail never share a kernel page on any supported page size.
\
\ The code region is a PT_LOAD of REGION bytes at the image's base plus
\ REGION-OFF that holds no file bytes: the kernel reserves it at exec, so the
\ brk heap starts above it and nothing mapped before entry takes its range, and
\ the boot maps the region's views over it.
\
\ The carrier is a read-write PT_LOAD at the address the build's kernel names,
\ past the DATA window, that holds DATA's initial pages for the boot to move
\ into the window.
\ Its bytes end the file, from the PAGE-BYTES boundary after the read-write
\ tail.
\
\ OPEN reads the extents of such an executable: the ELF header, its six
\ program headers (the text's PT_LOAD, the read-write tail's PT_LOAD, PT_INTERP,
\ PT_DYNAMIC, the code region's PT_LOAD and the carrier's PT_LOAD), the
\ interpreter path and the hash, symbol, string and relocation tables the
\ dynamic section names. The code starts at the entry, and the text's PT_LOAD
\ maps the file from offset 0, so a file offset is an address less that
\ load's. Offsets come from these headers, never from the inspecting host.
\ CODE-OFF and DATA-VA are the target's, LINUX-LAYOUT's, which the `using`
\ below keeps from reading bare; it closes before this file ends.

require src/core/exit-status.f
require src/arch/arm64/icode.f
require src/os/linux/layout.f
require src/os/image-bytes.f
require src/os/import-names.f
require src/lib/fmt.f
require src/lib/le.f
require src/lib/string.f
using A64ICODE
using IMAGE-BYTES
using LINUX-LAYOUT   \ the guard
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
\ Where the file's first byte maps.
variable VMBASE
$C0 constant ELF-DYNAMIC-SZ
$190 constant ELF-INTERP-OFF
TYPED-VARIABLE X64 bool
: INTERPRETER$ ( -- str )
   X64 @ if s" /lib64/ld-linux-x86-64.so.2" else s" /lib/ld-linux-aarch64.so.1" then ;
: ELF-INTERP-SZ ( -- n ) INTERPRETER$ STR:LEN 1+ ;
$1B0 constant ELF-HASH-OFF
24 constant ELF-SYM-SZ
24 constant ELF-RELA-ENT-SZ
$401 constant ELF-R-AARCH64-GLOB-DAT
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
21 constant DT-DEBUG
8 constant DF-BIND-NOW

\ The GOT follows the dynamic section and ends the read-write tail.
ELF-DYNAMIC-SZ constant ELF-GOT-OFF

public
\ The bytes a load and a carried page of DATA are multiples of: the base page of
\ an x86-64 kernel, the largest one of an ARM64 kernel.
: PAGE-BYTES ( bool -- n ) if $1000 else PROT-PAGE-MAX then ;

\ The libc symbols the loader binds into the GOT of the tree's kernel, in slot
\ order, by C name; the kernel object carries them to the link.
: IMPORT$ ( n -- str )
   case
      0 of s" dlopen" endof
      1 of s" dlsym" endof
      2 of s" free" endof
      3 of s" realpath" endof
      4 of s" clock_gettime" endof
      s" elf: no such import" EXIT-STATUS:IMAGE die
   endcase ;
5 constant IMPORTS

\ The row whose C name this is; the build dies on a name the table lacks.
: IMPORT-ROW ( str -- n )
   {: s:str :}
   IMPORTS 0 ?do i IMPORT$ s STR:= if i unloop exit then loop
   SB-RESET  s" elf: no such import " SB-APPEND  s SB-APPEND
   SB$ EXIT-STATUS:IMAGE die ;

\ Import row n's cell, as a byte offset from the read-write tail's start.
: SLOT-OFF ( n -- n ) 8 * ELF-GOT-OFF + ;
private

: LIBC$ ( -- str ) s" libc.so.6" ;

: ELF-ALIGN8 ( n -- n ) 7 + -8 and ;

: ELF-RW-SZ ( -- n ) IMPORT-NAMES:COUNT SLOT-OFF ;
\ The two counts, one bucket and a chain cell per symbol, the null one included.
: ELF-HASH-SZ ( -- n ) IMPORT-NAMES:COUNT 4 + 4 * ;
: ELF-DYNSYM-OFF ( -- n ) ELF-HASH-OFF ELF-HASH-SZ + ELF-ALIGN8 ;
: ELF-DYNSTR-OFF ( -- n ) ELF-DYNSYM-OFF IMPORT-NAMES:COUNT 1 + ELF-SYM-SZ * + ;
\ The string table opens with a NUL, then holds the names, then libc's.
: ELF-LIBC-OFF ( -- n ) IMPORT-NAMES:SPAN@ STR:LEN 1 + ;
: ELF-DYNSTR-SZ ( -- n ) ELF-LIBC-OFF LIBC$ STR:LEN + 1 + ;
: ELF-RELA-OFF ( -- n ) ELF-DYNSTR-OFF ELF-DYNSTR-SZ + ELF-ALIGN8 ;
: ELF-RELA-SZ ( -- n ) IMPORT-NAMES:COUNT ELF-RELA-ENT-SZ * ;
: ELF-TABLES-END ( -- n ) ELF-RELA-OFF ELF-RELA-SZ + ;
public
variable CODELEN
private
variable ELF-TEXT-SIZE
PTR-VARIABLE TEXT
PTR-VARIABLE CARRIER-A
variable CARRIER-N
variable CARRIER-BASE                \ the carrier's load address

: ELF-PAGE-UP ( n -- n )
   X64 @ PAGE-BYTES {: page:n :}
   page 1- + page negate and ;

: ASM-CODELEN! ( -- )
   ASM-LEN CODELEN ! CODE TEXT ! ;

public
: ASM-CODE ( -- asm )
   ASM-CODELEN!
   ASM-PHASE ;

: TEXTSZ ( -- n )  LINUX-LAYOUT:CODE-OFF CODELEN @ + ELF-PAGE-UP ;

private
\ The carrier's file offset: the boundary after the read-write tail.
: ELF-CARRIER-AT ( -- n ) TEXTSZ ELF-RW-SZ + ELF-PAGE-UP ;

\ The buffer fits the file BUILD-ELF writes.
: ELF-MSIZE! ( -- ) ELF-CARRIER-AT CARRIER-N @ + MSIZE! ;

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
   ET-EXEC IMG-M16
   X64 @ if EM-X86-64 else EM-AARCH64 then IMG-M16
   EV-CURRENT IMG-M32
   VMBASE @ LINUX-LAYOUT:CODE-OFF + IMG-M64
   ELF-HDR-SZ IMG-M64
   0 IMG-M64
   0 IMG-M32
   ELF-HDR-SZ IMG-M16  ELF-PHDR-SZ IMG-M16  ELF-PHDR-N IMG-M16
   0 IMG-M16  0 IMG-M16  0 IMG-M16 ;

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
   PT-LOAD PF-RX 0 VMBASE @ ELF-TEXT-SIZE @ dup X64 @ PAGE-BYTES ELF-PHDR, ;

: ELF-RW-PHDR, ( -- )
   PT-LOAD PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-RW-SZ dup X64 @ PAGE-BYTES
   ELF-PHDR, ;

: ELF-INTERP-PHDR, ( -- )
   PT-INTERP PF-R ELF-INTERP-OFF ELF-INTERP-OFF ELF-VA ELF-INTERP-SZ dup 1
   ELF-PHDR, ;

: ELF-DYNAMIC-PHDR, ( -- )
   PT-DYNAMIC PF-RW ELF-TEXT-SIZE @ ELF-RW-VA ELF-DYNAMIC-SZ dup 8 ELF-PHDR, ;

: ELF-REGION-PHDR, ( -- )
   PT-LOAD PF-RW 0 REGION-OFF ELF-VA 0 REGION X64 @ PAGE-BYTES ELF-PHDR, ;

: ELF-CARRIER-PHDR, ( -- )
   PT-LOAD PF-RW ELF-CARRIER-AT CARRIER-BASE @ CARRIER-N @ dup
   X64 @ PAGE-BYTES ELF-PHDR, ;

\ The loads ascend by address: text, read-write tail, code region, carrier.
: ELF-PHDRS, ( -- )
   ELF-RX-PHDR,
   ELF-RW-PHDR,
   ELF-INTERP-PHDR,
   ELF-DYNAMIC-PHDR,
   ELF-REGION-PHDR,
   ELF-CARRIER-PHDR, ;

: ELF-INTERP, ( -- )
   ELF-INTERP-OFF M-OFF M-PAD-OFF
   INTERPRETER$ M-BYTES
   0 IMG-M8 ;

: NEXT-SYM ( n -- n ) 1 + dup IMPORT-NAMES:COUNT > if drop 0 then ;

\ The bucket holds the first symbol and each chain cell the next; the null
\ symbol, 0, ends the chain.
: ELF-HASH, ( -- )
   ELF-HASH-OFF M-OFF M-PAD-OFF
   1 IMG-M32  IMPORT-NAMES:COUNT 1 + IMG-M32  0 NEXT-SYM IMG-M32  0 IMG-M32
   IMPORT-NAMES:COUNT 1 + 1 ?do i NEXT-SYM IMG-M32 loop ;

: ELF-SYM-NULL, ( -- )
   ELF-SYM-SZ M-ZEROS ;

: ELF-SYM, ( n -- ) {: nameoff :}
   nameoff IMG-M32  $12 IMG-M8  0 IMG-M8  0 IMG-M16  0 IMG-M64  0 IMG-M64 ;

: ELF-DYNSYM, ( -- )
   ELF-DYNSYM-OFF M-OFF M-PAD-OFF
   ELF-SYM-NULL,
   0 begin dup IMPORT-NAMES:END? 0= while
      dup 1 + ELF-SYM,  IMPORT-NAMES:NEXT-AT
   repeat drop ;

: ELF-DYNSTR, ( -- )
   ELF-DYNSTR-OFF M-OFF M-PAD-OFF
   0 IMG-M8
   IMPORT-NAMES:SPAN@ M-BYTES
   LIBC$ M-BYTES 0 IMG-M8 ;

: ELF-R-INFO ( n -- n )
   32 lshift X64 @ if ELF-R-X86-64-GLOB-DAT else ELF-R-AARCH64-GLOB-DAT then or ;

: ELF-RELA, ( -- )
   ELF-RELA-OFF M-OFF M-PAD-OFF
   IMPORT-NAMES:COUNT 0 ?do
      i ELF-SLOT-VA IMG-M64  i 1 + ELF-R-INFO IMG-M64  0 IMG-M64
   loop ;

: ELF-RX-META, ( -- )
   ELF-INTERP,
   ELF-HASH,
   ELF-DYNSYM,
   ELF-DYNSTR,
   ELF-RELA,
   LINUX-LAYOUT:CODE-OFF M-OFF M-PAD-OFF ;

: ELF-DYN, ( n n -- ) {: tag val :}
   tag IMG-M64
   val IMG-M64 ;

\ The loader writes its r_debug's address into DT_DEBUG's value, where a
\ debugger finds the link map; the read-write load holds the section so it can.
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
   DT-DEBUG    0 ELF-DYN,
   0 0 ELF-DYN, ;

: ELF-GOT, ( -- )
   IMPORT-NAMES:COUNT 0 ?do 0 IMG-M64 loop ;

: ELF-RW-AT, ( n -- ) {: off :}
   off M-OFF M-PAD-OFF
   ELF-DYNAMIC,
   ELF-GOT, ;

: ELF-CARRIER, ( -- )
   ELF-CARRIER-AT M-OFF M-PAD-OFF
   CARRIER-A @ CARRIER-N @ >str M-BYTES ;

: BUILD-ELF ( -- )
   TEXTSZ ELF-RW-SZ + REGION-OFF > if
      s" elf: the text overlaps the runtime code region" EXIT-STATUS:IMAGE die
   then
   ELF-TABLES-END LINUX-LAYOUT:CODE-OFF > if
      SB-RESET  s" elf: " SB-APPEND  IMPORT-NAMES:COUNT FMT:SB-U
      s"  imports outgrow the header page's " SB-APPEND
      LINUX-LAYOUT:CODE-OFF FMT:SB-U
      s"  bytes" SB-APPEND  SB$ EXIT-STATUS:IMAGE die
   then
   ELF-MSIZE! M-RESET
   TEXTSZ ELF-TEXT-SIZE !
   ELF-HDR,
   ELF-PHDRS,
   ELF-RX-META,
   TEXT @ CODELEN @ >str M-BYTES
   TEXTSZ M-OFF M-PAD-OFF
   ELF-TEXT-SIZE @ ELF-RW-AT,
   ELF-CARRIER,
   M-HERE MLEN! ;

: CARRIER! ( n str -- )
   dup STR:LEN CARRIER-N ! STR:DATA CARRIER-A ! CARRIER-BASE ! ;

public
\ The program as an ELF file whose code maps at the first address, its GOT
\ binding the import names given and its carrier, loaded at the second
\ address, holding the bytes given last.
: BUILD-IMAGE ( asm n str n str -- img )
   CARRIER!
   IMPORT-NAMES:SPAN!
   false X64 ! ASM-CODELEN!
   LINUX-LAYOUT:CODE-OFF - VMBASE !
   ASM-DROP
   BUILD-ELF
   IMG-PHASE ;

\ The same ELF layout, with the x86-64 loader and relocation number.
: BUILD-X64 ( str n str n str -- img )
   CARRIER!
   IMPORT-NAMES:SPAN!
   LINUX-LAYOUT:CODE-OFF - VMBASE !
   dup STR:LEN CODELEN ! STR:DATA TEXT ! true X64 !
   BUILD-ELF IMG-PHASE ;

private

PTR-VARIABLE IMAGE
variable BYTES
variable RX-HDR
variable RW-HDR
variable REGION-HDR
variable CARRIER-HDR
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

: CHECK ( bool str -- ) {: ok:bool why:str :}
   ok 0= if why EXIT-STATUS:DATAERR die then ;

: AT ( n n -- ^u8 ) {: off:n size:n :}
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

: MAGIC? ( str -- bool ) {: image:str :}
   image STR:LEN ELF-HDR-SZ < if false exit then
   image STR:DATA LE:U32@ $464C457F = ;

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
   16 U16@ ET-EXEC = and
   18 U16@ dup EM-AARCH64 = swap EM-X86-64 = or and
   20 U32@ EV-CURRENT = and
   s" elf: not a supported 64-bit little-endian executable" CHECK
   32 U64@ 52 U16@ = 52 U16@ ELF-HDR-SZ = and 54 U16@ ELF-PHDR-SZ = and
   56 U16@ ELF-PHDR-N = and
   s" elf: not six program headers after the header" CHECK
   40 U64@ 0= 60 U16@ 0= and
   s" elf: a section header table the linker does not write" CHECK ;

: UNIQUE! ( n ^n -- ) {: off:n slot:^ :}
   slot @ 0= s" elf: two program headers of one kind" CHECK
   off slot ! ;

: RW-LOAD? ( n -- bool ) {: off:n :}
   off U32@ PT-LOAD = off 4 + U32@ PF-RW = and ;

\ Every header but a read-write load, which RW-PHDR places once the text's
\ address is known.
: PHDR ( n -- ) {: off:n :}
   off RW-LOAD? if exit then
   off U32@ off 4 + U32@ {: kind:n flags:n :}
   kind PT-LOAD = flags PF-RX = and if off RX-HDR UNIQUE! exit then
   kind PT-INTERP = flags PF-R = and if off INTERP-HDR UNIQUE! exit then
   kind PT-DYNAMIC = flags PF-RW = and
   s" elf: a program header the linker does not write" CHECK
   off DYN-HDR UNIQUE! ;

\ Of the read-write loads, the code region's holds no file bytes; of the others,
\ in header order, which ELF keeps ascending by address, the first is the
\ read-write tail and the second the carrier.
: RW-PHDR ( n -- ) {: off:n :}
   off RW-LOAD? 0= if exit then
   off P-FILESZ 0= if off REGION-HDR UNIQUE! exit then
   RW-HDR @ 0= if off RW-HDR ! exit then
   off CARRIER-HDR UNIQUE! ;

: PHDR-AT ( n -- n ) ELF-PHDR-SZ * 32 U64@ + ;

: PHDRS ( -- )
   0 RX-HDR ! 0 RW-HDR ! 0 REGION-HDR ! 0 CARRIER-HDR ! 0 INTERP-HDR !
   0 DYN-HDR !
   56 U16@ 0 ?do i PHDR-AT PHDR loop
   56 U16@ 0 ?do i PHDR-AT RW-PHDR loop ;

: POW2? ( n -- bool ) {: n:n :}
   n 0 > n n 1- and 0= and ;

: CHECK-REGION ( -- )
   REGION-HDR @ {: r:n :}
   r P-VA RX-VA REGION-OFF + = r P-MEMSZ REGION = and
   r P-ALIGN TEXT-ALIGN = and  r P-VA RW-HDR @ P-VA RW-BYTES + >= and
   s" elf: the code region's load is not REGION bytes at REGION-OFF past the text" CHECK ;

\ The carrier's load maps whole pages from the page boundary after the
\ read-write load to the file's end.
: CHECK-CARRIER ( -- )
   CARRIER-HDR @ {: c:n :}
   RW-OFF RW-BYTES + TEXT-ALIGN 1- + TEXT-ALIGN negate and {: start:n :}
   c P-OFF start = c P-OFF c P-FILESZ + BYTES @ = and
   c P-FILESZ c P-MEMSZ = and c P-FILESZ TEXT-ALIGN mod 0= and
   c P-ALIGN TEXT-ALIGN = and
   s" elf: the carrier's load does not end the file" CHECK
   c P-OFF c P-FILESZ AT drop ;

\ The text's load maps the file from its first byte to a page boundary, and
\ the read-write load maps what follows at the address that follows it.
: CHECK-LOADS ( -- )
   TEXT-ALIGN POW2? s" elf: the text's page is not a power of two" CHECK
   RX-HDR @ P-OFF 0= TEXT-LIMIT RX-HDR @ P-MEMSZ = and
   RX-VA TEXT-ALIGN mod 0= and TEXT-LIMIT TEXT-ALIGN mod 0= and
   s" elf: the text's load is not on page boundaries" CHECK
   0 TEXT-LIMIT AT drop
   RW-OFF TEXT-LIMIT = RW-HDR @ P-VA RX-VA RW-OFF + = and
   RW-BYTES RW-HDR @ P-MEMSZ = and RW-HDR @ P-ALIGN TEXT-ALIGN = and
   s" elf: the read-write load does not follow the text" CHECK
   RW-OFF RW-BYTES AT drop
   CHECK-REGION
   CHECK-CARRIER
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

: OPEN ( str -- ) {: image:str :}
   image MAGIC? s" elf: not an ELF file" CHECK
   image STR:DATA IMAGE ! image STR:LEN BYTES !
   CHECK-HEADER PHDRS CHECK-LOADS CHECK-DYNAMIC CHECK-TABLES ;

\ The opened image's carrier: its file offset and length.
: CARRIER ( -- n n ) CARRIER-HDR @ dup P-OFF swap P-FILESZ ;

;package
;using   \ LINUX-LAYOUT
;using
;using
