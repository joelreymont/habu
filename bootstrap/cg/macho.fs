\ macho.fs — emit a minimal dynamic macOS ARM64 Mach-O executable, in Forth.
\ Layout: header + 6 load commands (PAGEZERO, TEXT+__text, LINKEDIT, DYLINKER,
\ MAIN, LOAD_DYLIB libSystem), header slack to 0x1000, the ICode-assembled code
\ at 0x1000, padded to one page (MPAGE). This is the canonical UNSIGNED binary;
\ sign.fs is a post-pass that rewrites the header to add LC_CODE_SIGNATURE + an
\ embedded ad-hoc signature (replacing external `codesign`), exactly as codesign
\ does. The drift guard compares this unsigned artifact. Static binaries are
\ SIGKILLed (AMFI); this is dynamic, dyld-loaded, zero C. See docs/macho.md.

require asm.fs

$50000 constant MSIZE
create MBUF MSIZE allot
variable MP
variable MLEN

: M-RESET ( -- )  MBUF MP ! ;

: M8  ( b -- )  MP @ c!  1 MP +! ;

: M16 ( h -- )  dup M8  8 rshift M8 ;

: M32 ( w -- )  dup M16  16 rshift M16 ;

: M64 ( x -- )  dup M32  32 rshift M32 ;

: M-HERE ( -- off )  MP @ MBUF - ;

: M-ZEROS ( n -- )  0 max 0 ?do 0 M8 loop ;

: M-NAME16 ( addr u -- )  dup >r  bounds ?do i c@ M8 loop  16 r> - M-ZEROS ;

: M-PAD ( off -- )  M-HERE - M-ZEROS ;

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
$100000000 constant VMBASE
$1000     constant CODE-OFF          \ entry file offset (slack below for codesign)
$40000    constant MPAGE              \ __TEXT file/vm size; __LINKEDIT starts here

variable CODELEN
create SCODE MPAGE allot              \ assembled-code scratch (grows with the standalone)

\ size via PASS1 (computes WPOS without writing) BEFORE PASS2 touches SCODE —
\ a post-write guard would segfault first on overflow
: ASM-CODE ( -- )
   PASS1  WPOS @ 4 *  MPAGE CODE-OFF -  > abort" cg: code exceeds __TEXT page"
   SCODE ASSEMBLE CODELEN ! ;

\ __TEXT sized to CONTENT (16 KB pages), not a fixed page count: a 24 KB
\ program is a 28 KB binary, not 264 KB. MPAGE survives as the buffer cap.
: TEXTSZ ( -- n )  CODE-OFF CODELEN @ +  $3FFF +  $3FFF invert and ;

variable LE-OFF                       \ file offset of the __LINKEDIT LC (for sign.fs post-pass)

: SEG, ( name$ vmaddr vmsize fileoff filesize prot nsects extrasz -- )
   {: addr u vma vmsz foff fsz prot nsects extra :}
   LC-SEG64 M32   72 extra + M32
   addr u M-NAME16
   vma M64  vmsz M64  foff M64  fsz M64
   prot M32  prot M32  nsects M32  0 M32 ;

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

32 constant MH-HDR-SZ                 \ mach_header_64 size
variable NCMDS                        \ load commands counted as emitted

: LC+ ( -- )  1 NCMDS +! ;            \ each LC emitter calls this

: MH-HDR, ( -- )                      \ ncmds/sizeofcmds back-patched later
   MH-MAGIC64 M32  CPU-ARM64 M32  0 M32  MH-EXECUTE M32
   0 M32  0 M32  MH-FLAGS M32  0 M32 ;

: PATCH-HDR ( -- )                    \ fill ncmds + sizeofcmds from what was emitted
   NCMDS @  MBUF 16 +  l!
   M-HERE MH-HDR-SZ -  MBUF 20 +  l! ;

: BUILD-MACHO ( -- )                 \ assumes ICODE holds the program
   ASM-CODE  M-RESET  0 NCMDS !
   MH-HDR,
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE TEXTSZ 0 TEXTSZ 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + CODELEN @ CODE-OFF 2 $80000400 SECT,
   M-HERE LE-OFF !                    \ remember __LINKEDIT LC offset for the sign post-pass
   s" __LINKEDIT" VMBASE TEXTSZ + MPAGE TEXTSZ 0 1 0 0 SEG,  LC+
   DYLINKER,  LC+   CODE-OFF MAIN,  LC+   DYLIB,  LC+
   PATCH-HDR                          \ derive ncmds/sizeofcmds (no frozen magic)
   CODELEN @  MPAGE CODE-OFF -  > abort" cg: emitted code exceeds __TEXT page"
   CODE-OFF M-PAD                    \ header slack (room for the post-pass LC_CODE_SIGNATURE)
   SCODE  MP @  CODELEN @  move      \ copy assembled code
   CODELEN @ MP +!
   TEXTSZ M-PAD                        \ pad file to one page
   M-HERE MLEN ! ;

\ the target-neutral driver entry: another OS swaps in an ELF builder here
: BUILD-IMAGE ( -- )  BUILD-MACHO ;
