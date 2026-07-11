\ macho.fs — the FULL Mach-O builder for the native engine: header + 6 load
\ commands (PAGEZERO, TEXT+__text, LINKEDIT,
\ DYLINKER, MAIN, DYLIB libSystem), slack to $1000, code at $1000, and TEXT sized
\ to content. Canonical UNSIGNED artifact; sign.fs post-pass adds the ad-hoc signature.
\ The self-rebuild gate proves deterministic output. Code comes from icode's CODE.
\ Mach-O constants
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
$80000034 constant LC-DYLD-CHAINED-FIXUPS
$100000000 constant VMBASE
$200000   constant MPAGE             \ maximum generated executable window
$4000     constant MACHO-PAGE
$4000     constant DATA-CONST-SIZE
104       constant MACHO-FIXUPS-SIZE
\ Upper bound for the CODESIG2 ad-hoc signature appended into MBUF: SuperBlob +
\ CodeDirectory + 32 B per 4 KB page of a full-MPAGE image is ~16.5 KB; $6000
\ leaves slack for the id string and header growth.
$6000     constant MACHO-SIG-MAX
$10       constant SG-READ-ONLY
$6        constant S-NON-LAZY-SYMBOL-POINTERS
6         constant DYLD-CHAINED-PTR-64-OFFSET
variable CODELEN

\ Fail closed at load time: the image buffer must fit the largest emittable
\ image (__text at MPAGE + __DATA_CONST + chained fixups + signature) so the
\ loud MPAGE code-window guard stays the binding constraint, never a silent
\ M-BOUNDS-RC throw from the byte cursor.
: MACHO-MSIZE-CHECK ( -- )
   CODE-CAP-BYTES CODE-OFF + MPAGE <>
   IF s" macho: code capacity differs from MPAGE" 73 die THEN
   MPAGE DATA-CONST-SIZE + MACHO-FIXUPS-SIZE + MACHO-SIG-MAX + MSIZE >
   IF s" macho: MSIZE below max image" 73 die THEN ;
MACHO-MSIZE-CHECK

: ASM-CODELEN! ( -- )
   ASM-LEN CODELEN ! ;  \ code already assembled in icode's CODE

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
   DATA-CONST-SIZE + ;

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

: CHAINED-FIXUPS, ( n -- ) {: linkoff:n :}
   LC-DYLD-CHAINED-FIXUPS IMG-M32
   16 IMG-M32
   linkoff IMG-M32
   MACHO-FIXUPS-SIZE IMG-M32 ;

: DATA-CONST-SEG, ( n -- ) {: textsz:n :}
   s" __DATA_CONST" textsz VA+ DATA-CONST-SIZE textsz DATA-CONST-SIZE 3 1 80 SG-READ-ONLY SEGX,
   s" __got" s" __DATA_CONST" textsz VA+ 16 textsz 3 S-NON-LAZY-SYMBOL-POINTERS SECT, ;

: LINKEDIT-SEG, ( n -- ) {: textsz:n :}
   textsz LINKOFF {: linkoff:n :}
   M-HERE LE-OFF !
   s" __LINKEDIT" linkoff VA+ MACHO-PAGE linkoff MACHO-FIXUPS-SIZE 1 0 0 SEG, ;

: MACHO-GOT, ( -- )
   0 IMG-M32  $80100000 IMG-M32
   1 IMG-M32  $80000000 IMG-M32 ;

: MACHO-FIXUPS, ( n -- ) {: segoff:n :}
   0 IMG-M32                 \ fixups_version
   $20 IMG-M32               \ starts_offset
   $50 IMG-M32               \ imports_offset
   $58 IMG-M32               \ symbols_offset
   2 IMG-M32                 \ imports_count
   1 IMG-M32                 \ DYLD_CHAINED_IMPORT
   0 IMG-M32                 \ uncompressed symbol strings
   4 M-ZEROS
   4 IMG-M32  0 IMG-M32  0 IMG-M32  $18 IMG-M32  0 IMG-M32
   4 M-ZEROS
   $18 IMG-M32
   MACHO-PAGE IMG-M16
   DYLD-CHAINED-PTR-64-OFFSET IMG-M16
   segoff IMG-M64
   0 IMG-M32
   1 IMG-M16
   0 IMG-M16
   $201 IMG-M32
   $1201 IMG-M32
   0 IMG-M8
   s" _dlopen" M-BYTES 0 IMG-M8
   s" _dlsym" M-BYTES 0 IMG-M8 ;

: MACHO-EXTRA, ( n n -- ) {: off:n segoff:n :}
   off M-OFF M-PAD-OFF
   MACHO-GOT,
   DATA-CONST-SIZE 16 - M-ZEROS
   off DATA-CONST-SIZE + M-OFF M-PAD-OFF
   segoff MACHO-FIXUPS,
   MACHO-FIXUPS-SIZE LE-BASE-SIZE ! ;

: MACHO-CMDS, ( n n -- ) {: textsz:n textlen:n :}
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE textsz 0 textsz 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + textlen CODE-OFF 2 $80000400 SECT,
   textsz DATA-CONST-SEG,  LC+
   textsz LINKEDIT-SEG,  LC+
   textsz LINKOFF CHAINED-FIXUPS,  LC+
   DYLINKER,  LC+
   CODE-OFF MAIN,  LC+
   DYLIB,  LC+ ;

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
   M-HERE MLEN! ;

\ the target-neutral driver entry: another OS swaps in an ELF builder here
: BUILD-IMAGE ( asm -- img )
   ASM-DROP
   BUILD-MACHO
   IMG-PHASE ;

: BUILD-SNAP-HDR ( n -- snap n ) {: snl :}
   CODE-OFF snl + $3FFF + $3FFF invert and {: sfts :}
   M-RESET  0 NCMDS !
   MH-HDR,
   sfts snl MACHO-CMDS,
   PATCH-HDR
   CODE-OFF M-OFF M-PAD-OFF
   CODE-OFF sfts MACHO-EXTRA,
   SNAP-PHASE sfts ;

: SNAP-EXTRA-PTR ( -- ptr u8 )
   MBUF CODE-OFF + ;
s" SNAP-EXTRA-PTR" s" -- ptr u8" TRUST

DATA-CONST-SIZE MACHO-FIXUPS-SIZE + constant SNAP-EXTRA-SIZE
s" SNAP-EXTRA-SIZE" s" -- n" TRUST
