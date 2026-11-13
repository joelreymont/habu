\ macho.f - the Mach-O writer for macOS ARM64: the canonical unsigned image of
\ the program in icode's CODE (BUILD-IMAGE), which src/os/macos/sign2.f then
\ signs ad hoc.
\
\ DYNAMIC, WITH NO C. AMFI kills a static arm64 executable, so the image is
\ dyld-loaded and links libSystem, yet its code is all emitted machine code.
\ The header is MH_EXECUTE for ARM64 with NOUNDEFS, DYLDLINK, TWOLEVEL and,
\ while PIE? is set, PIE. The load commands, in order: __PAGEZERO, 4 GiB with
\ no file bytes; __TEXT at VMBASE, sized to its content in 16 KB pages, __text
\ at CODE-OFF; __DATA_CONST, whose __got holds one cell per import BUILD-IMAGE
\ is given, which dyld binds to libSystem; __LINKEDIT, the chained-fixups blob, which the signer
\ grows; LC_DYLD_CHAINED_FIXUPS, whose starts name the __DATA_CONST segment
\ offset so dyld rewrites the GOT before LC_MAIN runs; /usr/lib/dyld; LC_MAIN at
\ CODE-OFF; libSystem; and two Homebrew runpaths.
\ sign2.f adds LC_CODE_SIGNATURE in the header slack.
\
\ DETERMINISTIC. The self-rebuild gate compares successive images, so none
\ carries LC_UUID, a timestamp or another optional command a clang link adds
\ (exports trie, symbol tables, build and source version, function starts,
\ data in code).
\
\ OPEN reads the container extents of the signed executable; an unsigned one is
\ refused. Offsets come from load commands and section records, never from the
\ inspecting host.
require src/arch/arm64/icode.f
require src/os/image-bytes.f
require src/lib/le.f
require src/lib/string.f
using A64ICODE
using IMAGE-BYTES
package MACHO
$FEEDFACF constant MH-MAGIC64
$0100000C constant CPU-ARM64
2         constant MH-EXECUTE
$00000085 constant MH-FLAGS-BASE     \ NOUNDEFS|DYLDLINK|TWOLEVEL
$00200000 constant MH-PIE
variable PIE?   -1 PIE? !

: MH-FLAGS ( -- n )  MH-FLAGS-BASE  PIE? @ IF MH-PIE or THEN ;
$19       constant LC-SEG64
$0E       constant LC-DYLINKER
$80000028 constant LC-MAIN
$0C       constant LC-DYLIB
$8000001C constant LC-RPATH
$80000034 constant LC-DYLD-CHAINED-FIXUPS
$1D       constant LC-CODE-SIG
\ Where the file's first byte maps; BUILD-IMAGE stores it.
variable VMBASE
\ The maximum generated executable window: everything the assembler can emit,
\ behind the header page it sits at.
CODE-CAP-BYTES CODE-OFF + constant MPAGE
$4000     constant MACHO-PAGE
$4000     constant DATA-CONST-SIZE

public
\ The libSystem functions and variables dyld binds into the GOT of the tree's
\ kernel, in slot order, by C name; the kernel object carries them to the link.
: IMPORT$ ( n -- ptr u8 n )
   case
      0 of s" dlopen" endof
      1 of s" dlsym" endof
      2 of s" free" endof
      3 of s" realpath" endof
      4 of s" fork" endof
      5 of s" mach_vm_remap" endof
      6 of s" mach_task_self_" endof
      s" macho: no such import" 73 die
   endcase ;
7 constant IMPORTS

\ The row whose C name this is; the build dies on a name the table lacks.
: IMPORT-ROW ( ptr u8 n -- n )
   {: a:ptr u:n :}
   IMPORTS 0 ?do i IMPORT$ a u STR= if i unloop exit then loop
   SB-RESET  s" macho: no such import " SB-APPEND  a u SB-APPEND
   SB$ 73 die ;

\ Import row n's cell, as a byte offset from the GOT's start.
: SLOT-OFF ( n -- n ) 8 * ;
private

\ The image's imports, which BUILD-IMAGE is given: C names in GOT row order,
\ each ending in a NUL.
PTR-VARIABLE NAMES
variable NAMES-U
variable NAMES-N

: NAMES@ ( -- ptr u8 ) NAMES @ ;

\ The name at a byte offset in NAMES.
: NAME-AT ( n -- ptr u8 n )
   {: at:n :}
   NAMES@ at +  0
   begin 2dup + c@ 0<> while 1 + repeat ;

: NEXT-AT ( n -- n ) dup NAME-AT nip + 1 + ;

: NAMES! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a NAMES !  u NAMES-U !
   0 0 begin dup u < while NEXT-AT swap 1 + swap repeat
   drop NAMES-N ! ;

\ Darwin's plain realpath is its legacy entry, which reads every parent
\ directory of a relative path; C programs link realpath$DARWIN_EXTSN.
: SUFFIX$ ( ptr u8 n -- ptr u8 n )
   s" realpath" STR= if s" $DARWIN_EXTSN" else s" " then ;

\ A name's symbol is an underscore, the name, then the suffix.
: SYMBOL-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u SUFFIX$ nip u + 1 + ;

: SYMBOL, ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" _" M-BYTES  a u M-BYTES  a u SUFFIX$ M-BYTES ;

\ From a symbol's offset in the symbol strings and its name's in NAMES to the
\ next one's.
: STEP ( n n -- n n )
   dup NAME-AT SYMBOL-LEN 1 + rot + swap NEXT-AT ;

\ The symbol strings open with a NUL, and each symbol ends in one.
: SYMBOLS-BYTES ( -- n )
   1 0 begin dup NAMES-U @ < while STEP repeat drop ;

: MACHO-GOT-BYTES ( -- n ) NAMES-N @ SLOT-OFF ;

\ The fixups header and image starts ($38), one page's segment starts (24), the
\ imports and the symbol strings; the blob pads them to a cell.
: MACHO-FIXUPS-USED ( -- n )
   $38 24 + NAMES-N @ 4 * + SYMBOLS-BYTES + ;

: MACHO-FIXUPS-SIZE ( -- n ) MACHO-FIXUPS-USED 7 + -8 and ;

\ One chained start belongs to each __DATA_CONST page. The first starts the
\ GOT binds; no other page holds a bind.
: MACHO-FIXUPS-MAX ( -- n )
   CODE-CAP-BYTES MACHO-GOT-BYTES + MACHO-PAGE 1- + MACHO-PAGE /
   1- 2 * 3 + -4 and  MACHO-FIXUPS-SIZE + ;

\ Code and payload share the assembler window. Their two separately rounded
\ segments can therefore exceed MPAGE + one DATA_CONST page by one more page.
MPAGE DATA-CONST-SIZE + MACHO-PAGE + constant MACHO-IMAGE-MAX

\ CODESIG2's largest signature, spelt from sign2.f's CS-PAGE, CS-HASH, CD-HDR and
\ $14 SuperBlob header, which load later, and a $40 id; more throws M-BOUNDS-RC.
: MACHO-SIG-MAX ( -- n )
   MACHO-IMAGE-MAX MACHO-FIXUPS-MAX +
   $FFF + $1000 / $20 *   $14 + $58 + $40 + ;

$10       constant SG-READ-ONLY
$6        constant S-NON-LAZY-SYMBOL-POINTERS
6         constant DYLD-CHAINED-PTR-64-OFFSET
public
variable CODELEN
private

: DATA-CONST-BYTES ( -- n )
   MACHO-GOT-BYTES IMG-PAYLOAD-N @ + MACHO-PAGE 1- + MACHO-PAGE 1- invert and
   DATA-CONST-SIZE max ;

: DATA-CONST-PAGES ( -- n ) DATA-CONST-BYTES MACHO-PAGE / ;

: MACHO-FIXUPS-BYTES ( -- n )
   DATA-CONST-PAGES 1- 2 * 3 + -4 and MACHO-FIXUPS-SIZE + ;

: MACHO-STARTS-BYTES ( -- n )
   24 DATA-CONST-PAGES 1- 2 * + 3 + -4 and ;

\ The image buffer fits the largest emittable image, so the loud MPAGE
\ code-window guard stays the binding constraint, never a silent M-BOUNDS-RC
\ throw from the byte cursor.
: MACHO-MSIZE! ( -- )
   MACHO-IMAGE-MAX MACHO-FIXUPS-MAX + MACHO-SIG-MAX + MSIZE! ;

: ASM-CODELEN! ( -- )
   IMG-PAYLOAD-N @ 0 > if
      IMG-CODE-N @ ASM-LEN > if s" image: invalid code/payload boundary" 73 die then
      IMG-CODE-N @
   else ASM-LEN then CODELEN ! ;

public
: ASM-CODE ( -- asm )
   ASM-CODELEN!
   ASM-PHASE ;

: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $3FFF +  $3FFF invert and ;
private
variable LE-OFF                      \ file offset of the __LINKEDIT LC (sign post-pass)
variable LE-BASE-SIZE
32 constant MH-HDR-SZ
variable NCMDS

: LC+ ( -- )
   NCMDS @ 1 + NCMDS ! ;

: LINKOFF ( n -- n )
   DATA-CONST-BYTES + ;

: VA+ ( n -- n )
   VMBASE @ + ;

: SEGX, ( ptr u8 n n n n n n n n n -- ) {: a:ptr u:n vma:n vmsz:n foff:n fsz:n prot:n nsects:n extra:n flags:n :}
   LC-SEG64 IMG-M32   72 extra + IMG-M32
   a u M-LEN M-NAME16-LEN
   vma IMG-M64  vmsz IMG-M64  foff IMG-M64  fsz IMG-M64
   prot IMG-M32  prot IMG-M32  nsects IMG-M32  flags IMG-M32 ;

: SEG, ( ptr u8 n n n n n n n n -- )
   0 SEGX, ;

: SECT, ( ptr u8 n ptr u8 n n n n n n -- ) {: na:ptr nu:n sa:ptr su:n addr:n size:n off:n al:n fl:n :}
   na nu M-LEN M-NAME16-LEN   sa su M-LEN M-NAME16-LEN
   addr IMG-M64  size IMG-M64  off IMG-M32  al IMG-M32
   0 IMG-M32  0 IMG-M32  fl IMG-M32  0 IMG-M32 0 IMG-M32 0 IMG-M32 ;

: DYLINKER, ( -- )
   LC-DYLINKER IMG-M32  32 IMG-M32  12 IMG-M32
   s" /usr/lib/dyld" {: a:ptr u :}
   a u M-LEN M-BYTES-LEN
   32 12 - u - M-LEN M-ZEROS-LEN ;

: MAIN, ( n -- ) {: entryoff :}
   LC-MAIN IMG-M32  24 IMG-M32  entryoff IMG-M64  0 IMG-M64 ;

: DYLIB, ( -- )
   LC-DYLIB IMG-M32  56 IMG-M32  24 IMG-M32
   2 IMG-M32  $054C0000 IMG-M32  $00010000 IMG-M32     \ ts=2, cur=1356.0.0, compat=1.0.0
   s" /usr/lib/libSystem.B.dylib" {: a:ptr u :}
   a u M-LEN M-BYTES-LEN
   56 24 - u - M-LEN M-ZEROS-LEN ;

\ dyld searches these runpaths for basename dlopen calls. System libraries keep
\ their normal dyld-cache lookup; Homebrew's arm64 prefix is outside that path.
: RPATH, ( ptr u8 n -- ) {: path:ptr u:n :}
   u 13 + 7 + -8 and {: size:n :}
   LC-RPATH IMG-M32 size IMG-M32 12 IMG-M32
   path u M-BYTES
   size 12 - u - M-ZEROS ;

: CHAINED-FIXUPS, ( n -- ) {: linkoff:n :}
   LC-DYLD-CHAINED-FIXUPS IMG-M32
   16 IMG-M32
   linkoff IMG-M32
   MACHO-FIXUPS-BYTES IMG-M32 ;

: DATA-CONST-SEG, ( n -- ) {: textsz:n :}
   s" __DATA_CONST" textsz VA+ DATA-CONST-BYTES textsz DATA-CONST-BYTES 3 1 80 SG-READ-ONLY SEGX,
   s" __got" s" __DATA_CONST" textsz VA+ MACHO-GOT-BYTES textsz 3 S-NON-LAZY-SYMBOL-POINTERS SECT, ;

: LINKEDIT-SEG, ( n -- ) {: textsz:n :}
   textsz LINKOFF {: linkoff:n :}
   M-HERE LE-OFF !
   s" __LINKEDIT" linkoff VA+ MACHO-PAGE linkoff MACHO-FIXUPS-BYTES 1 0 0 SEG, ;

\ A bind per row: the import's ordinal, the bind bit and, on every row but the
\ last, a next of two 4-byte strides.
: MACHO-GOT, ( -- )
   NAMES-N @ 0 ?do
      i IMG-M32
      $80000000 i NAMES-N @ 1- < if $00100000 or then IMG-M32
   loop ;

\ Each import names libSystem (ordinal 1) and its name's offset.
: MACHO-IMPORTS, ( -- )
   1 0 begin dup NAMES-U @ < while
      over 9 lshift 1 or IMG-M32  STEP
   repeat 2drop ;

: MACHO-SYMBOLS, ( -- )
   0 IMG-M8
   0 begin dup NAMES-U @ < while
      dup NAME-AT SYMBOL, 0 IMG-M8  NEXT-AT
   repeat drop
   MACHO-FIXUPS-SIZE MACHO-FIXUPS-USED - M-ZEROS ;

: MACHO-FIXUPS, ( n -- ) {: segoff:n :}
   0 IMG-M32                 \ fixups_version
   $20 IMG-M32               \ starts_offset
   $38 MACHO-STARTS-BYTES + IMG-M32  \ imports_offset
   $38 MACHO-STARTS-BYTES + NAMES-N @ 4 * + IMG-M32  \ symbols_offset
   NAMES-N @ IMG-M32         \ imports_count
   1 IMG-M32                 \ DYLD_CHAINED_IMPORT
   0 IMG-M32                 \ uncompressed symbol strings
   4 M-ZEROS
   4 IMG-M32  0 IMG-M32  0 IMG-M32  $18 IMG-M32  0 IMG-M32
   4 M-ZEROS
   MACHO-STARTS-BYTES IMG-M32
   MACHO-PAGE IMG-M16
   DYLD-CHAINED-PTR-64-OFFSET IMG-M16
   segoff IMG-M64
   0 IMG-M32
   DATA-CONST-PAGES IMG-M16
   0 IMG-M16
   DATA-CONST-PAGES 1- 0 ?do $FFFF IMG-M16 loop
   DATA-CONST-PAGES 1 and 0= if 0 IMG-M16 then
   MACHO-IMPORTS,
   MACHO-SYMBOLS, ;

: MACHO-EXTRA, ( n n -- ) {: off:n segoff:n :}
   off M-OFF M-PAD-OFF
   MACHO-GOT,
   IMG-PAYLOAD-N @ 0 > if IMG-PAYLOAD-A @ IMG-PAYLOAD-N @ M-BYTES then
   DATA-CONST-BYTES MACHO-GOT-BYTES - IMG-PAYLOAD-N @ - M-ZEROS
   off DATA-CONST-BYTES + M-OFF M-PAD-OFF
   segoff MACHO-FIXUPS,
   MACHO-FIXUPS-BYTES LE-BASE-SIZE ! ;

: MACHO-CMDS, ( n n -- ) {: textsz:n textlen:n :}
   s" __PAGEZERO" 0 VMBASE @ 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE @ textsz 0 textsz 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE @ CODE-OFF + textlen CODE-OFF 2 $80000400 SECT,
   textsz DATA-CONST-SEG,  LC+
   textsz LINKEDIT-SEG,  LC+
   textsz LINKOFF CHAINED-FIXUPS,  LC+
   DYLINKER,  LC+
   CODE-OFF MAIN,  LC+
   DYLIB,  LC+
   s" /opt/homebrew/lib" RPATH, LC+
   s" /opt/homebrew/opt/libpq/lib" RPATH, LC+ ;

: MH-HDR, ( -- )
   MH-MAGIC64 IMG-M32  CPU-ARM64 IMG-M32  0 IMG-M32  MH-EXECUTE IMG-M32
   0 IMG-M32  0 IMG-M32  MH-FLAGS IMG-M32  0 IMG-M32 ;

: PATCH-HDR ( -- )
   NCMDS @ 16 M-OFF M-LE32!
   M-HERE MH-HDR-SZ - 20 M-OFF M-LE32! ;

: BUILD-MACHO ( -- )                        \ assumes icode's CODE holds the program
   ASM-CODELEN!  MACHO-MSIZE!  M-RESET  0 NCMDS !
   MH-HDR,
   TEXTSZ CODELEN @ MACHO-CMDS,
   PATCH-HDR
   CODELEN @  MPAGE CODE-OFF -  > IF s" macho: code exceeds __TEXT page" 73 die THEN
   CODE-OFF M-OFF M-PAD-OFF
   CODE CODELEN @ M-LEN M-BYTES-LEN
   TEXTSZ M-OFF M-PAD-OFF
   TEXTSZ TEXTSZ MACHO-EXTRA,
   M-HERE MLEN!
   IMG-PAYLOAD-CLEAR ;

public
\ The program in icode's CODE as a Mach-O file whose code maps at the address,
\ its GOT binding the C names, each ending in a NUL, in row order.
: BUILD-IMAGE ( asm n ptr u8 n -- img )
   NAMES!
   CODE-OFF - VMBASE !
   ASM-DROP
   BUILD-MACHO
   IMG-PHASE ;

private

PTR-VARIABLE IMAGE
variable BYTES
variable CMD-END
variable TEXT-CMD
variable DATA-CMD
variable LINK-CMD
variable FIX-CMD
variable SIGN-CMD
variable CURSOR

: CHECK ( bool -- )
   0= if s" macho: invalid Habu Mach-O container" 74 die then ;

: AT ( n n -- ptr u8 ) {: off:n size:n :}
   off 0 >= size 0 >= and off BYTES @ <= and CHECK
   size BYTES @ off - <= CHECK
   IMAGE @ off + ;

: U32@ ( n -- n ) 4 AT LE:U32@ ;
: U64@ ( n -- n ) 8 AT LE:U64@ ;

: NAME? ( n ptr u8 n -- bool ) {: off:n name:ptr size:n :}
   off 16 AT size name size STR= 0= if false exit then
   size 16 < if off size + 1 AT c@ 0= else true then ;

: UNIQUE! ( n ptr n -- ) {: off:n slot:ptr :}
   slot @ 0= CHECK off slot ! ;

: SEGMENT ( n n -- ) {: off:n size:n :}
   size 72 >= CHECK
   off 64 + U32@ 80 * 72 + size = CHECK
   off 8 + s" __TEXT" NAME? if off TEXT-CMD UNIQUE! exit then
   off 8 + s" __DATA_CONST" NAME? if off DATA-CMD UNIQUE! exit then
   off 8 + s" __LINKEDIT" NAME? if off LINK-CMD UNIQUE! exit then
   off 8 + s" __PAGEZERO" NAME? CHECK
   off 40 + U64@ 0= off 48 + U64@ 0= and CHECK ;

: COMMAND ( n n -- ) {: off:n size:n :}
   off U32@ {: kind:n :}
   kind LC-SEG64 = if off size SEGMENT exit then
   kind LC-DYLD-CHAINED-FIXUPS = if size 16 = CHECK off FIX-CMD UNIQUE! exit then
   kind LC-CODE-SIG = if size 16 = CHECK off SIGN-CMD UNIQUE! then ;

public

: MAGIC? ( ptr u8 n -- bool ) {: image:ptr size:n :}
   size MH-HDR-SZ < if false exit then
   image LE:U32@ MH-MAGIC64 = ;

: COMMANDS-END ( -- n ) CMD-END @ ;
: TEXT-OFF ( -- n ) TEXT-CMD @ 120 + U32@ ;
: TEXT-END ( -- n ) TEXT-OFF TEXT-CMD @ 112 + U64@ + ;
: TEXT-LIMIT ( -- n ) TEXT-CMD @ 48 + U64@ ;
: TEXT-VA ( -- n ) TEXT-CMD @ 104 + U64@ ;
: DATA-OFF ( -- n ) DATA-CMD @ 40 + U64@ ;
: DATA-BYTES ( -- n ) DATA-CMD @ 48 + U64@ ;
: GOT-BYTES ( -- n ) DATA-CMD @ 112 + U64@ ;
: LINK-OFF ( -- n ) LINK-CMD @ 40 + U64@ ;
: LINK-BYTES ( -- n ) LINK-CMD @ 48 + U64@ ;
: FIXUPS-OFF ( -- n ) FIX-CMD @ 8 + U32@ ;
: FIXUPS-BYTES ( -- n ) FIX-CMD @ 12 + U32@ ;
: SIGN-OFF ( -- n ) SIGN-CMD @ 8 + U32@ ;
: SIGN-BYTES ( -- n ) SIGN-CMD @ 12 + U32@ ;

private

: CHECK-TEXT ( -- )
   TEXT-CMD @ 4 + U32@ 152 = CHECK
   TEXT-CMD @ 72 + s" __text" NAME? CHECK
   TEXT-CMD @ 88 + s" __TEXT" NAME? CHECK
   TEXT-CMD @ 40 + U64@ 0= CHECK
   TEXT-OFF $1000 = CHECK
   TEXT-CMD @ 24 + U64@ TEXT-OFF + TEXT-VA = CHECK
   TEXT-CMD @ 112 + U64@ 0 >= CHECK
   TEXT-END TEXT-OFF >= TEXT-END TEXT-LIMIT <= and CHECK
   0 TEXT-LIMIT AT drop ;

: CHECK-DATA ( -- )
   DATA-CMD @ 4 + U32@ 152 = CHECK
   DATA-CMD @ 72 + s" __got" NAME? CHECK
   DATA-CMD @ 88 + s" __DATA_CONST" NAME? CHECK
   DATA-CMD @ 120 + U32@ DATA-OFF = CHECK
   GOT-BYTES 7 and 0= CHECK
   DATA-OFF TEXT-LIMIT = CHECK
   DATA-BYTES GOT-BYTES >= CHECK
   DATA-OFF DATA-BYTES AT drop ;

: CHECK-LINK ( -- )
   LINK-CMD @ 4 + U32@ 72 = CHECK
   LINK-OFF DATA-OFF DATA-BYTES + = CHECK
   LINK-OFF LINK-BYTES + BYTES @ = CHECK
   LINK-OFF LINK-BYTES AT drop
   FIXUPS-OFF LINK-OFF = CHECK
   FIXUPS-OFF FIXUPS-BYTES AT drop
   SIGN-OFF FIXUPS-OFF FIXUPS-BYTES + >= CHECK
   SIGN-OFF SIGN-BYTES + BYTES @ = CHECK
   SIGN-OFF SIGN-BYTES AT drop ;

public

: OPEN ( ptr u8 n -- ) {: image:ptr size:n :}
   image size MAGIC? CHECK
   image IMAGE ! size BYTES !
   4 U32@ CPU-ARM64 = 12 U32@ MH-EXECUTE = and CHECK
   0 TEXT-CMD ! 0 DATA-CMD ! 0 LINK-CMD ! 0 FIX-CMD ! 0 SIGN-CMD !
   20 U32@ MH-HDR-SZ + CMD-END !
   CMD-END @ $1000 <= CHECK
   MH-HDR-SZ CURSOR !
   16 U32@ $1000 8 / <= CHECK
   16 U32@ 0 ?do
      CURSOR @ {: off:n :}
      off 8 + CMD-END @ <= CHECK
      off 4 + U32@ {: width:n :}
      width 8 >= width 8 mod 0= and CHECK
      width CMD-END @ off - <= CHECK
      off width COMMAND
      off width + CURSOR !
   loop
   CURSOR @ CMD-END @ = CHECK
   TEXT-CMD @ 0 > DATA-CMD @ 0 > and LINK-CMD @ 0 > and
      FIX-CMD @ 0 > and SIGN-CMD @ 0 > and CHECK
   CHECK-TEXT CHECK-DATA CHECK-LINK ;

;package
;using
;using
