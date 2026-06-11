\ macho.fs — emit a minimal dynamic macOS ARM64 Mach-O executable, in Forth.
\ Layout: header + 6 load commands (PAGEZERO, TEXT+__text, LINKEDIT, DYLINKER,
\ MAIN, LOAD_DYLIB libSystem), header slack to 0x1000, the ICode-assembled code
\ at 0x1000, padded to one page (MPAGE). This is the canonical UNSIGNED binary;
\ sign.fs is a post-pass that rewrites the header to add LC_CODE_SIGNATURE + an
\ embedded ad-hoc signature (replacing external `codesign`), exactly as codesign
\ does. The drift guard compares this unsigned artifact. Static binaries are
\ SIGKILLed (AMFI); this is dynamic, dyld-loaded, zero C. See docs/macho.md.

require asm.fs

$20000 constant MSIZE
create MBUF MSIZE allot
variable MP
variable MLEN
: M-RESET ( -- )  MBUF MP ! ;
: m8  ( b -- )  MP @ c!  1 MP +! ;
: m16 ( h -- )  dup m8  8 rshift m8 ;
: m32 ( w -- )  dup m16  16 rshift m16 ;
: m64 ( x -- )  dup m32  32 rshift m32 ;
: m-here ( -- off )  MP @ MBUF - ;
: m-zeros ( n -- )  0 max 0 ?do 0 m8 loop ;
: m-name16 ( addr u -- )  dup >r  bounds ?do i c@ m8 loop  16 r> - m-zeros ;
: m-pad ( off -- )  m-here - m-zeros ;

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
$10000    constant MPAGE              \ __TEXT file/vm size; __LINKEDIT starts here

variable CODELEN
create SCODE $18000 allot             \ assembled-code scratch (grows with the standalone)
: ASM-CODE ( -- )  SCODE ASSEMBLE CODELEN ! ;

variable LE-OFF                       \ file offset of the __LINKEDIT LC (for sign.fs post-pass)

: SEG, ( name$ vmaddr vmsize fileoff filesize prot nsects extrasz -- )
   {: addr u vma vmsz foff fsz prot nsects extra :}
   LC-SEG64 m32   72 extra + m32
   addr u m-name16
   vma m64  vmsz m64  foff m64  fsz m64
   prot m32  prot m32  nsects m32  0 m32 ;

: SECT, ( name$ seg$ addr size offset align flags -- )
   {: na nu sa su addr size off al fl :}
   na nu m-name16   sa su m-name16
   addr m64  size m64  off m32  al m32
   0 m32  0 m32  fl m32  0 m32 0 m32 0 m32 ;

: DYLINKER, ( -- )
   LC-DYLINKER m32  32 m32  12 m32
   s" /usr/lib/dyld" dup >r bounds ?do i c@ m8 loop  32 12 - r> - m-zeros ;

: MAIN, ( entryoff -- )
   LC-MAIN m32  24 m32  m64  0 m64 ;

: DYLIB, ( -- )
   LC-DYLIB m32  56 m32  24 m32
   2 m32  $054C0000 m32  $00010000 m32     \ ts=2, cur=1356.0.0, compat=1.0.0
   s" /usr/lib/libSystem.B.dylib" dup >r bounds ?do i c@ m8 loop  56 24 - r> - m-zeros ;

32 constant MH-HDR-SZ                 \ mach_header_64 size
variable NCMDS                        \ load commands counted as emitted
: LC+ ( -- )  1 NCMDS +! ;            \ each LC emitter calls this
: MH-HDR, ( -- )                      \ ncmds/sizeofcmds back-patched later
   MH-MAGIC64 m32  CPU-ARM64 m32  0 m32  MH-EXECUTE m32
   0 m32  0 m32  MH-FLAGS m32  0 m32 ;
: PATCH-HDR ( -- )                    \ fill ncmds + sizeofcmds from what was emitted
   NCMDS @  MBUF 16 +  l!
   m-here MH-HDR-SZ -  MBUF 20 +  l! ;

: BUILD-MACHO ( -- )                 \ assumes ICODE holds the program
   ASM-CODE  M-RESET  0 NCMDS !
   MH-HDR,
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE MPAGE 0 MPAGE 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + CODELEN @ CODE-OFF 2 $80000400 SECT,
   m-here LE-OFF !                    \ remember __LINKEDIT LC offset for the sign post-pass
   s" __LINKEDIT" VMBASE MPAGE + MPAGE MPAGE 0 1 0 0 SEG,  LC+
   DYLINKER,  LC+   CODE-OFF MAIN,  LC+   DYLIB,  LC+
   PATCH-HDR                          \ derive ncmds/sizeofcmds (no frozen magic)
   CODELEN @  MPAGE CODE-OFF -  > abort" cg: emitted code exceeds __TEXT page"
   CODE-OFF m-pad                    \ header slack (room for the post-pass LC_CODE_SIGNATURE)
   SCODE  MP @  CODELEN @  move      \ copy assembled code
   CODELEN @ MP +!
   MPAGE m-pad                        \ pad file to one page
   m-here MLEN ! ;
