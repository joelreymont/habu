\ macho-min.fs — minimal Mach-O exit(42) builder in the STANDALONE's Forth, shared
\ by the unsigned drift test (exit42.fs) and the self-signing demo (sign.fs). Builds
\ the unsigned image into the dictionary at MSTART; BUILD leaves it ready to write or
\ to post-process (sign.fs). 6 load commands, byte-identical to caf's macho.fs.
4294967296 constant VMBASE  65536 constant MPAGE  4096 constant CODEOFF
variable MSTART
: MOFF here MSTART @ - ;
: M8 c, ;
: M32 {: w :} w 255 and M8 w 8 rshift 255 and M8 w 16 rshift 255 and M8 w 24 rshift 255 and M8 ;
: M64 {: x :} x M32 x 32 rshift M32 ;
: SPAD {: a u total :} 0 BEGIN dup total < WHILE dup u < IF dup a + c@ M8 ELSE 0 M8 THEN 1 + REPEAT drop ;
: MNAME 16 SPAD ;
: MPAD {: target :} BEGIN MOFF target < WHILE 0 M8 REPEAT ;
: P32 {: w a :} w 255 and a c! w 8 rshift 255 and a 1 + c! w 16 rshift 255 and a 2 + c! w 24 rshift 255 and a 3 + c! ;
create PB 64 allot   variable PL
: PSET 0 PL ! BEGIN dup PL @ > WHILE over PL @ + c@ PB PL @ + c! PL @ 1 + PL ! REPEAT 2drop 0 PB PL @ + c! ;
: BUILD
  here MSTART !
  4277009103 M32 16777228 M32 0 M32 2 M32 0 M32 0 M32 2097285 M32 0 M32
  25 M32 72 M32 s" __PAGEZERO" MNAME 0 M64 VMBASE M64 0 M64 0 M64 0 M32 0 M32 0 M32 0 M32
  25 M32 152 M32 s" __TEXT" MNAME VMBASE M64 MPAGE M64 0 M64 MPAGE M64 5 M32 5 M32 1 M32 0 M32
  s" __text" MNAME s" __TEXT" MNAME VMBASE CODEOFF + M64 12 M64 CODEOFF M32 2 M32 0 M32 0 M32 2147484672 M32 0 M32 0 M32 0 M32
  25 M32 72 M32 s" __LINKEDIT" MNAME VMBASE MPAGE + M64 MPAGE M64 MPAGE M64 0 M64 1 M32 1 M32 0 M32 0 M32
  14 M32 32 M32 12 M32 s" /usr/lib/dyld" 20 SPAD
  2147483688 M32 24 M32 CODEOFF M64 0 M64
  12 M32 56 M32 24 M32 2 M32 88866816 M32 65536 M32 s" /usr/lib/libSystem.B.dylib" 32 SPAD
  6 MSTART @ 16 + P32  MOFF 32 - MSTART @ 20 + P32
  CODEOFF MPAD
  3531605312 M32 3531604016 M32 3556773889 M32
  MPAGE MPAD ;
