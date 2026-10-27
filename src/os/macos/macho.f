\ macho.f - the Mach-O writer for macOS ARM64: the canonical unsigned image of
\ the program in icode's CODE (BUILD-IMAGE) or of a snapshot (BUILD-SNAP-HDR),
\ which src/os/macos/sign2.f then signs ad hoc.
\
\ DYNAMIC, WITH NO C. AMFI kills a static arm64 executable, so the image is
\ dyld-loaded and links libSystem, yet its code is all emitted machine code.
\ The header is MH_EXECUTE for ARM64 with NOUNDEFS, DYLDLINK, TWOLEVEL and,
\ while PIE? is set, PIE. The load commands, in order: __PAGEZERO, 4 GiB with
\ no file bytes; __TEXT at VMBASE, sized to its content in 16 KB pages, __text
\ at CODE-OFF; __DATA_CONST, whose __got holds the two cells dyld binds to
\ libSystem's _dlopen and _dlsym (src/os/macos/layout.f DLOPEN-SLOT,
\ DLSYM-SLOT), then the stripped DATA payload; __LINKEDIT, the chained-fixups
\ blob, which the signer grows; LC_DYLD_CHAINED_FIXUPS, whose starts name the
\ __DATA_CONST segment offset so dyld rewrites the GOT before LC_MAIN runs;
\ /usr/lib/dyld; LC_MAIN at CODE-OFF; libSystem; and two Homebrew runpaths.
\ sign2.f adds LC_CODE_SIGNATURE in the header slack.
\
\ DETERMINISTIC. The self-rebuild gate compares successive images, so none
\ carries LC_UUID, a timestamp or another optional command a clang link adds
\ (exports trie, symbol tables, build and source version, function starts,
\ data in code). A snapshot stages its __DATA_CONST and fixups tail in MBUF at
\ CODE-OFF, but the fixups name the final segment offset, so dyld binds the
\ mapped page.
\ Mach-O constants
require src/arch/arm64/icode.f
using A64ICODE
require src/os/image-bytes.f
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
$100000000 constant VMBASE
\ The maximum generated executable window: everything the assembler can emit,
\ behind the header page it sits at. It is DERIVED from the assembler's own
\ buffer rather than written out again, because the two are one fact and a
\ second spelling of one fact is a drift waiting for an editor. What used to
\ hold them together was a load-time comparison of two hand-written literals;
\ the comparison is gone with the second literal, and the drift it watched for
\ is now unwritable.
CODE-CAP-BYTES CODE-OFF + constant MPAGE
$4000     constant MACHO-PAGE
$4000     constant DATA-CONST-SIZE
16        constant MACHO-GOT-BYTES
MACHO-GOT-BYTES IMG-PAYLOAD-PREFIX !
104       constant MACHO-FIXUPS-SIZE
\ One chained start belongs to each __DATA_CONST page. The first starts the two
\ GOT binds; remaining pages contain only the immutable stripped DATA payload.
\ The payload can fill the assembler window, with the GOT prefix taking another
\ page at that boundary.
CODE-CAP-BYTES MACHO-GOT-BYTES + MACHO-PAGE 1- + MACHO-PAGE / 1- 2 * 3 + -4 and
   MACHO-FIXUPS-SIZE + constant MACHO-FIXUPS-MAX
\ Code and payload share the assembler window. Their two separately rounded
\ segments can therefore exceed MPAGE + one DATA_CONST page by one more page.
MPAGE DATA-CONST-SIZE + MACHO-PAGE + constant MACHO-IMAGE-MAX
\ Upper bound for the CODESIG2 ad-hoc signature appended into MBUF, COMPUTED
\ from the window rather than chosen, because it is a function of the window and
\ a number chosen against one window size is wrong at the next. sign2.f hashes
\ the finished file in CS-PAGE slots ($1000) and writes one CS-HASH digest ($20,
\ SHA-256) per slot, after a 20-byte SuperBlob header ($14) and an 88-byte
\ CodeDirectory header ($58, sign2.f CD-HDR) and the id string. The largest file
\ this writer can hand the signer is a full MPAGE of __TEXT plus __DATA_CONST
\ and the chained-fixups tail, so that is the slot count to price. $40 is the id
\ allowance: every writer signs with src/habu/sign-id.f SIGN-ID:PROG$ or
\ SIGN-ID:ENGINE$, and the longer, SIGN-ID:PROG$ `hb-prog`, is 7 bytes.
\
\ THE FOUR FORMAT NUMBERS ARE A MIRROR OF sign2.f AND NOTHING CHECKS THE MIRROR.
\ They cannot be read from their owner here because sign2.f loads after this file
\ and needs MACHO-SIG-MAX at load. So the mirror is stated, not
\ enforced: if sign2.f's CS-PAGE, CS-HASH or CD-HDR ever moved without this line,
\ the failure is the loud 73 die below or the 75 image-cursor bounds throw, never
\ a bad image. A dot for the cross-check is owed.
MACHO-IMAGE-MAX MACHO-FIXUPS-MAX +
   $FFF + $1000 / $20 *   $14 + $58 + $40 +   constant MACHO-SIG-MAX
$10       constant SG-READ-ONLY
$6        constant S-NON-LAZY-SYMBOL-POINTERS
6         constant DYLD-CHAINED-PTR-64-OFFSET
variable CODELEN
variable MACHO-DATA-WROTE
variable MACHO-FIXUPS-WROTE

: DATA-CONST-BYTES ( -- n )
   MACHO-GOT-BYTES IMG-PAYLOAD-N @ + MACHO-PAGE 1- + MACHO-PAGE 1- invert and
   DATA-CONST-SIZE max ;

: DATA-CONST-PAGES ( -- n ) DATA-CONST-BYTES MACHO-PAGE / ;

: MACHO-FIXUPS-BYTES ( -- n )
   DATA-CONST-PAGES 1- 2 * 3 + -4 and MACHO-FIXUPS-SIZE + ;

: MACHO-STARTS-BYTES ( -- n )
   24 DATA-CONST-PAGES 1- 2 * + 3 + -4 and ;

\ Fail closed at load time: the image buffer must fit the largest emittable
\ image (__text at MPAGE + __DATA_CONST + chained fixups + signature) so the
\ loud MPAGE code-window guard stays the binding constraint, never a silent
\ M-BOUNDS-RC throw from the byte cursor. MSIZE belongs to the target-neutral
\ src/os/image-bytes.f, which cannot see this target's tail, so this is the
\ check that binds the two.
: MACHO-MSIZE-CHECK ( -- )
   MACHO-IMAGE-MAX MACHO-FIXUPS-MAX + MACHO-SIG-MAX + MSIZE >
   IF s" macho: MSIZE below max image" 73 die THEN ;
MACHO-MSIZE-CHECK

: ASM-CODELEN! ( -- )
   IMG-PAYLOAD-N @ 0 > if IMG-CODE-N @ else ASM-LEN then CODELEN ! ;

: ASM-CODE ( -- asm )
   ASM-CODELEN!
   ASM-PHASE ;

\ __TEXT sized to CONTENT (16 KB pages), not a fixed page count: a 24 KB
\ program is a 28 KB binary, not a fixed-cap binary. MPAGE is only the fail-closed
\ maximum generated-code window for builder images.
: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $3FFF +  $3FFF invert and ;
variable LE-OFF                      \ file offset of the __LINKEDIT LC (sign post-pass)
variable LE-BASE-SIZE
32 constant MH-HDR-SZ                \ mach_header_64 size
variable NCMDS

: LC+ ( -- )
   NCMDS @ 1 + NCMDS ! ;

: LINKOFF ( n -- n )
   DATA-CONST-BYTES + ;

: VA+ ( n -- n )
   VMBASE + ;

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

: MACHO-GOT, ( -- )
   0 IMG-M32  $80100000 IMG-M32
   1 IMG-M32  $80000000 IMG-M32 ;

: MACHO-FIXUPS, ( n -- ) {: segoff:n :}
   0 IMG-M32                 \ fixups_version
   $20 IMG-M32               \ starts_offset
   $38 MACHO-STARTS-BYTES + IMG-M32  \ imports_offset
   $40 MACHO-STARTS-BYTES + IMG-M32  \ symbols_offset
   2 IMG-M32                 \ imports_count
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
   $201 IMG-M32
   $1201 IMG-M32
   0 IMG-M8
   s" _dlopen" M-BYTES 0 IMG-M8
   s" _dlsym" M-BYTES 0 IMG-M8 ;

: MACHO-EXTRA, ( n n -- ) {: off:n segoff:n :}
   off M-OFF M-PAD-OFF
   MACHO-GOT,
   IMG-PAYLOAD-N @ 0 > if IMG-PAYLOAD-A @ IMG-PAYLOAD-N @ M-BYTES then
   DATA-CONST-BYTES MACHO-GOT-BYTES - IMG-PAYLOAD-N @ - M-ZEROS
   off DATA-CONST-BYTES + M-OFF M-PAD-OFF
   segoff MACHO-FIXUPS,
   MACHO-FIXUPS-BYTES LE-BASE-SIZE ! ;

: MACHO-CMDS, ( n n -- ) {: textsz:n textlen:n :}
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE textsz 0 textsz 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + textlen CODE-OFF 2 $80000400 SECT,
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
   ASM-CODELEN!  M-RESET  0 NCMDS !
   MH-HDR,
   TEXTSZ CODELEN @ MACHO-CMDS,
   PATCH-HDR
   CODELEN @  MPAGE CODE-OFF -  > IF s" macho: code exceeds __TEXT page" 73 die THEN
   CODE-OFF M-OFF M-PAD-OFF
   CODE CODELEN @ M-LEN M-BYTES-LEN
   TEXTSZ M-OFF M-PAD-OFF
   TEXTSZ TEXTSZ MACHO-EXTRA,
   M-HERE MLEN!
   DATA-CONST-BYTES MACHO-DATA-WROTE !
   MACHO-FIXUPS-BYTES MACHO-FIXUPS-WROTE !
   IMG-PAYLOAD-CLEAR ;

\ the target-neutral driver entry: another OS swaps in an ELF builder here
: BUILD-IMAGE ( asm -- img )
   ASM-DROP
   BUILD-MACHO
   IMG-PHASE ;

: BUILD-SNAP-HDR ( n -- snap n ) {: snl :}
   IMG-PAYLOAD-CLEAR
   CODE-OFF snl + $3FFF + $3FFF invert and {: sfts :}
   M-RESET  0 NCMDS !
   MH-HDR,
   sfts snl MACHO-CMDS,
   PATCH-HDR
   CODE-OFF M-OFF M-PAD-OFF
   CODE-OFF sfts MACHO-EXTRA,
   DATA-CONST-BYTES MACHO-DATA-WROTE !
   MACHO-FIXUPS-BYTES MACHO-FIXUPS-WROTE !
   SNAP-PHASE sfts ;

: SNAP-EXTRA-PTR ( -- ptr u8 )
   MBUF CODE-OFF + ;
s" SNAP-EXTRA-PTR" s" -- ptr u8" TRUST

DATA-CONST-SIZE MACHO-FIXUPS-SIZE + constant SNAP-EXTRA-SIZE
s" SNAP-EXTRA-SIZE" s" -- n" TRUST
;using
