\ t-sh-asm.fs — the standalone's ARM64 encoders (selfhost/asm.fs) match caf's, byte
\ for byte. caf computes the reference encodings; the standalone computes its own;
\ assert identical. First milestone of the codegen port. Run: gforth test/t-sh-asm.fs -e bye
require ../src/cg/asm.fs
require nf.fs
require tester.fs
\ caf reference encoders (operand form; same base constants + bit layout as asm.fs)
: R3 ( base rd rn rm -- w )  >r >r >r  r> swap r> 5 lshift or r> 16 lshift or or ;
\ build the expected decimal-per-line string the standalone's . prints
create EB16 65536 allot   variable EL
: e+ ( a u -- )  bounds ?do i c@ EB16 EL @ + c! 1 EL +! loop ;
: n+ ( n -- )  0 <# 10 hold #s #> e+ ;   \ "decimal\n"
: REF ( -- )
   0 EL !
   5 42 0 MOVZHW n+   5 7 2 MOVKHW n+   3 1 0 MOVNHW n+
   $8B000000 1 2 3 R3 n+   $CB000000 1 2 3 R3 n+   $8A000000 1 2 3 R3 n+
   $AA000000 1 2 3 R3 n+   $CA000000 1 2 3 R3 n+   $9B007C00 1 2 3 R3 n+
   $91000000 1 or 2 5 lshift or 10 10 lshift or n+
   $D1000000 1 or 2 5 lshift or 10 10 lshift or n+
   $D3400000 5 or 3 5 lshift or 64 2 - 63 and 16 lshift or 63 2 - 10 lshift or n+
   $D340FC00 5 or 3 5 lshift or 2 16 lshift or n+
   $EB00001F 2 5 lshift or 3 16 lshift or n+
   $F100001F 2 5 lshift or 5 10 lshift or n+
   $D4000001 0 5 lshift or n+
   $D65F03C0 n+ ;
create CBUF 32768 allot   variable CL
: +B {: a u -- }  a  CBUF CL @ +  u move  u CL +! ;
: ASM-OUT ( -- a u )
   0 CL !
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/asm-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ ASM-OUT  EB16 EL @  compare 0= -> true }T
