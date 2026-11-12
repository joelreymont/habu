\ Read the extents of Habu's Linux arm64 executables: the ELF header, its four
\ program headers (the text's PT_LOAD, the read-write tail's PT_LOAD, PT_INTERP
\ and PT_DYNAMIC), the interpreter path and the hash, symbol, string and
\ relocation tables the dynamic section names. The code starts at the entry,
\ and the text's PT_LOAD maps the file from offset 0, so a file offset is an
\ address less that load's. Offsets come from these headers, never from the
\ inspecting host.
require src/lib/le.f
require src/lib/string.f

package ELF-READ

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
   s" elf-read: an extent passes the file's end" CHECK
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
   size 64 < if false exit then
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
   4 U8@ 2 = 5 U8@ 1 = and 6 U8@ 1 = and
   16 U16@ 2 = and 18 U16@ $B7 = and 20 U32@ 1 = and
   s" elf-read: not a 64-bit little-endian arm64 executable" CHECK
   32 U64@ 52 U16@ = 52 U16@ 64 = and 54 U16@ 56 = and 56 U16@ 4 = and
   s" elf-read: not four program headers after the header" CHECK
   40 U64@ 0= 60 U16@ 0= and
   s" elf-read: a section header table the linker does not write" CHECK ;

: UNIQUE! ( n ptr n -- ) {: off:n slot:ptr :}
   slot @ 0= s" elf-read: two program headers of one kind" CHECK
   off slot ! ;

\ A program header's type and flags: PT_LOAD read-execute and read-write,
\ PT_INTERP read, PT_DYNAMIC read-write.
: PHDR ( n -- ) {: off:n :}
   off U32@ off 4 + U32@ {: kind:n flags:n :}
   kind 1 = flags 5 = and if off RX-HDR UNIQUE! exit then
   kind 1 = flags 6 = and if off RW-HDR UNIQUE! exit then
   kind 3 = flags 4 = and if off INTERP-HDR UNIQUE! exit then
   kind 2 = flags 6 = and
   s" elf-read: a program header the linker does not write" CHECK
   off DYN-HDR UNIQUE! ;

: PHDRS ( -- )
   0 RX-HDR ! 0 RW-HDR ! 0 INTERP-HDR ! 0 DYN-HDR !
   56 U16@ 0 ?do 32 U64@ i 56 * + PHDR loop ;

: POW2? ( n -- bool ) {: n:n :}
   n 0 > n n 1- and 0= and ;

\ The text's load maps the file from its first byte to a page boundary, and
\ the read-write load maps the rest at the address that follows it.
: CHECK-LOADS ( -- )
   TEXT-ALIGN POW2? s" elf-read: the text's page is not a power of two" CHECK
   RX-HDR @ P-OFF 0= TEXT-LIMIT RX-HDR @ P-MEMSZ = and
   RX-VA TEXT-ALIGN mod 0= and TEXT-LIMIT TEXT-ALIGN mod 0= and
   s" elf-read: the text's load is not on page boundaries" CHECK
   0 TEXT-LIMIT AT drop
   RW-OFF TEXT-LIMIT = RW-OFF RW-BYTES + BYTES @ = and
   RW-HDR @ P-VA RX-VA RW-OFF + = and
   RW-BYTES RW-HDR @ P-MEMSZ = and RW-HDR @ P-ALIGN TEXT-ALIGN = and
   s" elf-read: the read-write load does not end the file" CHECK
   TEXT-OFF HEADERS-BYTES > TEXT-OFF TEXT-LIMIT <= and
   s" elf-read: the entry is not in the text's load past the headers" CHECK ;

: DYN-ENTRY ( n n -- ) {: tag:n val:n :}
   tag 4 = if val VA>OFF HASH-AT ! exit then
   tag 5 = if val VA>OFF STR-AT ! exit then
   tag 6 = if val VA>OFF SYM-AT ! exit then
   tag 7 = if val VA>OFF RELA-AT ! exit then
   tag 8 = if val RELA-BYTES ! exit then
   tag 9 = if val RELA-ENT ! exit then
   tag 10 = if val STR-BYTES ! exit then
   tag 11 = if val SYM-ENT ! then ;

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
   SYM-ENT @ 24 = and RELA-ENT @ 24 = and RELA-BYTES @ 24 mod 0= and ;

\ The dynamic section opens the read-write load.
: CHECK-DYNAMIC ( -- )
   DYN-HDR @ P-OFF RW-OFF = DYN-HDR @ P-VA RW-HDR @ P-VA = and
   DYN-HDR @ P-FILESZ RW-BYTES <= and
   DYN-HDR @ P-FILESZ 16 mod 0= and
   s" elf-read: the dynamic section is not at the read-write load" CHECK
   DYN-RESET DYN-WALK
   s" elf-read: the dynamic section has no end" CHECK
   TABLES? s" elf-read: the dynamic section lacks a table the linker writes"
   CHECK ;

\ Answers where the table at off with size bytes ends, given where the one
\ before it ends.
: NEXT ( n n n -- n ) {: end:n off:n size:n :}
   off end >= size 0 >= and
   s" elf-read: the dynamic-linking tables overlap" CHECK
   off size AT drop
   off size + ;

\ The tables lie in the linker's order between the headers and the text.
: CHECK-TABLES ( -- )
   INTERP-HDR @ P-OFF INTERP-HDR @ P-VA VA>OFF =
   s" elf-read: the interpreter's offset is not its address" CHECK
   HEADERS-BYTES
   INTERP-HDR @ P-OFF INTERP-BYTES NEXT
   HASH-AT @ HASH-BYTES NEXT
   SYM-AT @ SYM-BYTES NEXT
   STR-AT @ STR-BYTES @ NEXT
   RELA-AT @ RELA-BYTES @ NEXT
   TEXT-OFF <= s" elf-read: the dynamic-linking tables pass the text" CHECK ;

public

: OPEN ( ptr u8 n -- ) {: image:ptr size:n :}
   image size MAGIC? s" elf-read: not an ELF file" CHECK
   image IMAGE ! size BYTES !
   CHECK-HEADER PHDRS CHECK-LOADS CHECK-DYNAMIC CHECK-TABLES ;

;package
