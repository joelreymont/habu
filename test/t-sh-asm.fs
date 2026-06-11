\ t-sh-asm.fs — the standalone's ARM64 encoders (selfhost/asm.f) match habu's, byte
\ for byte. habu computes the reference encodings; the standalone computes its own;
\ assert identical. First milestone of the codegen port. Run: gforth test/t-sh-asm.fs -e bye
require ../src/cg/asm.fs
require sh-driver.fs
\ habu reference encoders (operand form; same base constants + bit layout as asm.fs)
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
   $D65F03C0 n+
   $F9400000 1 or 2 5 lshift or 16 8 / 10 lshift or n+
   $F9000000 1 or 2 5 lshift or 16 8 / 10 lshift or n+
   $39400000 1 or 2 5 lshift or 3 10 lshift or n+
   $39000000 1 or 2 5 lshift or 3 10 lshift or n+
   $B9400000 1 or 2 5 lshift or 8 4 / 10 lshift or n+
   $B9000000 1 or 2 5 lshift or 8 4 / 10 lshift or n+
   $14000000 5 or n+   $94000000 5 or n+
   $54000000 3 5 lshift or 11 or n+
   $B4000000 4 5 lshift or 9 or n+
   $B5000000 4 5 lshift or 9 or n+
   $9E670000 1 or 2 5 lshift or n+
   $9E660000 1 or 2 5 lshift or n+
   $1E602800 0 or 1 5 lshift or 2 16 lshift or n+
   $1E603800 0 or 1 5 lshift or 2 16 lshift or n+
   $1E600800 0 or 1 5 lshift or 2 16 lshift or n+ ;
: ASM-OUT ( -- a u )
   0 CL !
   s" selfhost/asm.f"      slurp-file +B   s"  " +B
   s" selfhost/asm-demo.f" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ ASM-OUT  EB16 EL @  compare 0= -> true }T
