\ macho.fs — the FULL Mach-O builder, transcribed from bootstrap/cg/macho.fs for the
\ engine-builder port: header + 6 load commands (PAGEZERO, TEXT+__text, LINKEDIT,
\ DYLINKER, MAIN, DYLIB libSystem), slack to $1000, code at $1000, padded to one
\ page. Canonical UNSIGNED artifact; sign.fs post-pass adds the ad-hoc signature.
\ Golden byte-for-byte vs habu in test/t-sh-macho.fs. Code comes from icode's CODE.
$50000 constant MSIZE
create MBUF MSIZE allot
variable MP
variable MLEN

: M-RESET  MBUF MP ! ;

: M8  {: b :}  b MP @ c!  MP @ 1 + MP ! ;

: M16 {: h :}  h M8  h 8 rshift M8 ;

: M32 {: w :}  w M16  w 16 rshift M16 ;

: M64 {: x :}  x M32  x 32 rshift M32 ;

: M-HERE  MP @ MBUF - ;

: M-ZEROS {: n :}  n 0 > IF n BEGIN dup 0 > WHILE 0 M8 1 - REPEAT drop THEN ;

: M-BYTES {: a u :}  0 BEGIN dup u < WHILE  dup a + c@ M8  1 + REPEAT drop ;

: M-NAME16 {: a u :}  a u M-BYTES  16 u - M-ZEROS ;

: M-PAD {: off :}  off M-HERE - M-ZEROS ;
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
$40000    constant MPAGE             \ __TEXT file/vm size; __LINKEDIT starts here
variable CODELEN

: ASM-CODE  ASM-LEN CODELEN ! ;      \ code already assembled in icode's CODE
variable LE-OFF                      \ file offset of the __LINKEDIT LC (sign post-pass)

: SEG, {: a u vma vmsz foff fsz prot nsects extra :}
   LC-SEG64 M32   72 extra + M32
   a u M-NAME16
   vma M64  vmsz M64  foff M64  fsz M64
   prot M32  prot M32  nsects M32  0 M32 ;

: SECT, {: na nu sa su addr size off al fl :}
   na nu M-NAME16   sa su M-NAME16
   addr M64  size M64  off M32  al M32
   0 M32  0 M32  fl M32  0 M32 0 M32 0 M32 ;

: DYLINKER,
   LC-DYLINKER M32  32 M32  12 M32
   s" /usr/lib/dyld" {: a u :}  a u M-BYTES  32 12 - u - M-ZEROS ;

: MAIN, {: entryoff :}
   LC-MAIN M32  24 M32  entryoff M64  0 M64 ;

: DYLIB,
   LC-DYLIB M32  56 M32  24 M32
   2 M32  $054C0000 M32  $00010000 M32     \ ts=2, cur=1356.0.0, compat=1.0.0
   s" /usr/lib/libSystem.B.dylib" {: a u :}  a u M-BYTES  56 24 - u - M-ZEROS ;
32 constant MH-HDR-SZ                \ mach_header_64 size
variable NCMDS

: LC+  NCMDS @ 1 + NCMDS ! ;

: MH-HDR,
   MH-MAGIC64 M32  CPU-ARM64 M32  0 M32  MH-EXECUTE M32
   0 M32  0 M32  MH-FLAGS M32  0 M32 ;
variable PHP

: PL! {: w a :}  a PHP !  w $FF and PHP @ c!  w 8 rshift $FF and PHP @ 1 + c!
   w 16 rshift $FF and PHP @ 2 + c!  w 24 rshift $FF and PHP @ 3 + c! ;

: PATCH-HDR
   NCMDS @  MBUF 16 +  PL!
   M-HERE MH-HDR-SZ -  MBUF 20 +  PL! ;

: BUILD-MACHO                        \ assumes icode's CODE holds the program
   ASM-CODE  M-RESET  0 NCMDS !
   MH-HDR,
   s" __PAGEZERO" 0 VMBASE 0 0 0 0 0 SEG,  LC+
   s" __TEXT" VMBASE MPAGE 0 MPAGE 5 1 80 SEG,  LC+
      s" __text" s" __TEXT" VMBASE CODE-OFF + CODELEN @ CODE-OFF 2 $80000400 SECT,
   M-HERE LE-OFF !
   s" __LINKEDIT" VMBASE MPAGE + MPAGE MPAGE 0 1 0 0 SEG,  LC+
   DYLINKER,  LC+   CODE-OFF MAIN,  LC+   DYLIB,  LC+
   PATCH-HDR
   CODELEN @  MPAGE CODE-OFF -  > IF s" macho: code exceeds __TEXT page" 73 die THEN
   CODE-OFF M-PAD
   CODE CODELEN @ M-BYTES
   MPAGE M-PAD
   M-HERE MLEN ! ;

\ the target-neutral driver entry: another OS swaps in an ELF builder here
: BUILD-IMAGE  BUILD-MACHO ;
