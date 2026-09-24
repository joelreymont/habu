\ macho.fs — dynamic macOS ARM64 seed image. Mirrors src/os/macos/macho.f:
\ __TEXT, a __DATA_CONST page with dlopen/dlsym slots, and chained fixups in
\ __LINKEDIT. sign.fs appends the ad-hoc signature. See docs/macho.md.

require asm.fs
require image.fs

\ Mach-O constants
$FEEDFACF constant MH-MAGIC64
$0100000C constant CPU-ARM64
2         constant MH-EXECUTE
$00000085 constant MH-FLAGS-BASE     \ NOUNDEFS|DYLDLINK|TWOLEVEL
$00200000 constant MH-PIE
variable PIE?   PIE? on

: MH-FLAGS ( -- f )  MH-FLAGS-BASE  PIE? @ if MH-PIE or then ;
$19       constant LC-SEG64
$0E       constant LC-DYLINKER
$80000028 constant LC-MAIN
$0C       constant LC-DYLIB
$8000001C constant LC-RPATH
$80000034 constant LC-DYLD-CHAINED-FIXUPS
$4000     constant DATA-CONST-SIZE
104       constant MACHO-FIXUPS-SIZE
$100000000 constant VMBASE
$1000     constant CODE-OFF          \ entry file offset (slack below for codesign)

variable CODELEN
variable SCODE   variable SCODE-CAP   \ assembled-code scratch, grown to the emission

\ __TEXT follows the emitted program: PASS1 sizes the scratch buffer, PASS2 fills
\ it, and TEXTSZ carries that length into the load commands. There is no fixed
\ text page; an image is bounded by instruction reach, which asm.fs checks per
\ instruction (?ADR, ?REL19, ?REL26).
\ Same rule and same shape as bootstrap/cg/elf.fs.
: ASM-CODE ( -- )
   PASS1  WPOS @ 4 *  SCODE SCODE-CAP BUF-FIT
   SCODE @ ASSEMBLE CODELEN ! ;

\ __TEXT sized to CONTENT (16 KB pages): a 24 KB program is a 28 KB binary.
: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $3FFF +  $3FFF invert and ;

variable LE-OFF                       \ file offset of the __LINKEDIT LC (for sign.fs post-pass)

: SEGX, ( name$ vmaddr vmsize fileoff filesize prot nsects extrasz flags -- )
   {: addr u vma vmsz foff fsz prot nsects extra flags :}
   LC-SEG64 M32   72 extra + M32
   addr u M-NAME16
   vma M64  vmsz M64  foff M64  fsz M64
   prot M32  prot M32  nsects M32  flags M32 ;

: SEG, ( name$ vmaddr vmsize fileoff filesize prot nsects extrasz -- )
   0 SEGX, ;

: SECT, ( name$ seg$ addr size offset align flags -- )
   {: na nu sa su addr size off al fl :}
   na nu M-NAME16   sa su M-NAME16
   addr M64  size M64  off M32  al M32
   0 M32  0 M32  fl M32  0 M32 0 M32 0 M32 ;

: DYLINKER, ( -- )
   LC-DYLINKER M32  32 M32  12 M32
   s" /usr/lib/dyld" dup >r bounds ?do i c@ M8 loop  32 12 - r> - M-ZEROS ;

: MAIN, ( entryoff -- )
   LC-MAIN M32  24 M32  M64  0 M64 ;

: DYLIB, ( -- )
   LC-DYLIB M32  56 M32  24 M32
   2 M32  $054C0000 M32  $00010000 M32     \ ts=2, cur=1356.0.0, compat=1.0.0
   s" /usr/lib/libSystem.B.dylib" dup >r bounds ?do i c@ M8 loop  56 24 - r> - M-ZEROS ;

: RPATH, ( a u -- ) {: path u :}
   u 13 + 7 + -8 and {: size :}
   LC-RPATH M32 size M32 12 M32
   path u bounds ?do i c@ M8 loop
   size 12 - u - M-ZEROS ;

32 constant MH-HDR-SZ                 \ mach_header_64 size
variable NCMDS                        \ load commands counted as emitted

: LC+ ( -- )  1 NCMDS +! ;            \ each LC emitter calls this

: MH-HDR, ( -- )                      \ ncmds/sizeofcmds back-patched later
   MH-MAGIC64 M32  CPU-ARM64 M32  0 M32  MH-EXECUTE M32
   0 M32  0 M32  MH-FLAGS M32  0 M32 ;

: PATCH-HDR ( -- )                    \ fill ncmds + sizeofcmds from what was emitted
   NCMDS @  MBUF 16 +  l!
   M-HERE MH-HDR-SZ -  MBUF 20 +  l! ;

: MACHO-GOT, ( -- )
   0 M32  $80100000 M32
   1 M32  $80000000 M32 ;

: MACHO-FIXUPS, ( segoff -- ) {: segoff :}
   0 M32  $20 M32  $50 M32  $58 M32
   2 M32  1 M32  0 M32
   4 M-ZEROS
   4 M32  0 M32  0 M32  $18 M32  0 M32
   4 M-ZEROS
   $18 M32  $4000 M16  6 M16
   segoff M64  0 M32  1 M16  0 M16
   $201 M32  $1201 M32
   0 M8  s" _dlopen" M-BYTES 0 M8  s" _dlsym" M-BYTES 0 M8 ;

: BUILD-MACHO ( -- )
   ASM-CODE
   TEXTSZ DATA-CONST-SIZE + {: linkoff :}
   linkoff MACHO-FIXUPS-SIZE + M-FIT  M-RESET  0 NCMDS !
   MH-HDR,
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE TEXTSZ 0 TEXTSZ 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + CODELEN @ CODE-OFF 2 $80000400 SECT,
   s" __DATA_CONST" VMBASE TEXTSZ + DATA-CONST-SIZE TEXTSZ DATA-CONST-SIZE 3 1 80 $10 SEGX,  LC+
      s" __got" s" __DATA_CONST" VMBASE TEXTSZ + 16 TEXTSZ 3 6 SECT,
   M-HERE LE-OFF !
   s" __LINKEDIT" VMBASE linkoff + $4000 linkoff MACHO-FIXUPS-SIZE 1 0 0 SEG,  LC+
   LC-DYLD-CHAINED-FIXUPS M32  16 M32  linkoff M32  MACHO-FIXUPS-SIZE M32  LC+
   DYLINKER,  LC+   CODE-OFF MAIN,  LC+   DYLIB,  LC+
   s" /opt/homebrew/lib" RPATH, LC+
   s" /opt/homebrew/opt/libpq/lib" RPATH, LC+
   PATCH-HDR
   CODE-OFF M-PAD
   SCODE @ CODELEN @ M-BYTES
   TEXTSZ M-PAD
   MACHO-GOT,  DATA-CONST-SIZE 16 - M-ZEROS
   TEXTSZ MACHO-FIXUPS,
   M-HERE MLEN ! ;

: BUILD-IMAGE ( -- )  BUILD-MACHO ;
