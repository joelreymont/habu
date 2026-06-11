\ macho.fs — the FULL Mach-O builder, transcribed from src/cg/macho.fs for the
\ engine-builder port: header + 6 load commands (PAGEZERO, TEXT+__text, LINKEDIT,
\ DYLINKER, MAIN, DYLIB libSystem), slack to $1000, code at $1000, padded to one
\ page. Canonical UNSIGNED artifact; sign.fs post-pass adds the ad-hoc signature.
\ Golden byte-for-byte vs habu in test/t-sh-macho.fs. Code comes from icode's CODE.
$40000 constant MSIZE
create MBUF MSIZE allot
variable MP
variable MLEN
: M-RESET  MBUF MP ! ;
: m8  {: b :}  b MP @ c!  MP @ 1 + MP ! ;
: m16 {: h :}  h m8  h 8 rshift m8 ;
: m32 {: w :}  w m16  w 16 rshift m16 ;
: m64 {: x :}  x m32  x 32 rshift m32 ;
: m-here  MP @ MBUF - ;
: m-zeros {: n :}  n 0 > IF n BEGIN dup 0 > WHILE 0 m8 1 - REPEAT drop THEN ;
: m-bytes {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ m8  1 + REPEAT drop ;
: m-name16 {: a u :}  a u m-bytes  16 u - m-zeros ;
: m-pad {: off :}  off m-here - m-zeros ;
\ Mach-O constants
$FEEDFACF constant MH-MAGIC64
$0100000C constant CPU-ARM64
2         constant MH-EXECUTE
$00000085 constant MH-FLAGS-BASE     \ NOUNDEFS|DYLDLINK|TWOLEVEL
$00200000 constant MH-PIE
variable PIE?   -1 PIE? !
: MH-FLAGS  MH-FLAGS-BASE  PIE? @ IF MH-PIE or THEN ;
$19       constant LC-SEG64
$0E       constant LC-DYLINKER
$80000028 constant LC-MAIN
$0C       constant LC-DYLIB
$100000000 constant VMBASE
$1000     constant CODE-OFF          \ entry file offset (slack below for codesign)
$20000    constant MPAGE             \ __TEXT file/vm size; __LINKEDIT starts here
variable CODELEN
: ASM-CODE  ASM-LEN CODELEN ! ;      \ code already assembled in icode's CODE
variable LE-OFF                      \ file offset of the __LINKEDIT LC (sign post-pass)
: SEG, {: a u vma vmsz foff fsz prot nsects extra :}
   LC-SEG64 m32   72 extra + m32
   a u m-name16
   vma m64  vmsz m64  foff m64  fsz m64
   prot m32  prot m32  nsects m32  0 m32 ;
: SECT, {: na nu sa su addr size off al fl :}
   na nu m-name16   sa su m-name16
   addr m64  size m64  off m32  al m32
   0 m32  0 m32  fl m32  0 m32 0 m32 0 m32 ;
: DYLINKER,
   LC-DYLINKER m32  32 m32  12 m32
   s" /usr/lib/dyld" {: a u :}  a u m-bytes  32 12 - u - m-zeros ;
: MAIN, {: entryoff :}
   LC-MAIN m32  24 m32  entryoff m64  0 m64 ;
: DYLIB,
   LC-DYLIB m32  56 m32  24 m32
   2 m32  $054C0000 m32  $00010000 m32     \ ts=2, cur=1356.0.0, compat=1.0.0
   s" /usr/lib/libSystem.B.dylib" {: a u :}  a u m-bytes  56 24 - u - m-zeros ;
32 constant MH-HDR-SZ                \ mach_header_64 size
variable NCMDS
: LC+  NCMDS @ 1 + NCMDS ! ;
: MH-HDR,
   MH-MAGIC64 m32  CPU-ARM64 m32  0 m32  MH-EXECUTE m32
   0 m32  0 m32  MH-FLAGS m32  0 m32 ;
variable PHP
: pl! {: w a :}  a PHP !  w $FF and PHP @ c!  w 8 rshift $FF and PHP @ 1 + c!
   w 16 rshift $FF and PHP @ 2 + c!  w 24 rshift $FF and PHP @ 3 + c! ;
: PATCH-HDR
   NCMDS @  MBUF 16 +  pl!
   m-here MH-HDR-SZ -  MBUF 20 +  pl! ;
: BUILD-MACHO                        \ assumes icode's CODE holds the program
   ASM-CODE  M-RESET  0 NCMDS !
   MH-HDR,
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE MPAGE 0 MPAGE 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + CODELEN @ CODE-OFF 2 $80000400 SECT,
   m-here LE-OFF !
   s" __LINKEDIT" VMBASE MPAGE + MPAGE MPAGE 0 1 0 0 SEG,  LC+
   DYLINKER,  LC+   CODE-OFF MAIN,  LC+   DYLIB,  LC+
   PATCH-HDR
   CODELEN @  MPAGE CODE-OFF -  > IF s" macho: code exceeds __TEXT page" 73 die THEN
   CODE-OFF m-pad
   CODE CODELEN @ m-bytes
   MPAGE m-pad
   m-here MLEN ! ;
