\ t-sh-asm.fs — the standalone's ARM64 encoders (src/arch/arm64/asm.f) match habu's, byte
\ for byte. habu computes the reference encodings; the standalone computes its own;
\ assert identical. First milestone of the codegen port. Run: gforth test/t-sh-asm.fs -e bye
require ../bootstrap/cg/asm.fs
require sh-driver.fs
\ habu reference encoders (operand form; same base constants + bit layout as asm.fs)
: R3 ( base rd rn rm -- w )  >r >r >r  r> swap r> 5 lshift or r> 16 lshift or or ;
\ build the expected decimal-per-line string the standalone's . prints
create EB16 65536 allot   variable EL
: E+ ( a u -- )  bounds ?do i c@ EB16 EL @ + c! 1 EL +! loop ;
: N+ ( n -- )  0 <# 10 hold #s #> E+ ;   \ "decimal\n"
: REF ( -- )
   0 EL !
   5 42 0 MOVZHW N+   5 7 2 MOVKHW N+   3 1 0 MOVNHW N+
   $8B000000 1 2 3 R3 N+   $CB000000 1 2 3 R3 N+   $8A000000 1 2 3 R3 N+
   $AA000000 1 2 3 R3 N+   $CA000000 1 2 3 R3 N+   $9B007C00 1 2 3 R3 N+
   $91000000 1 or 2 5 lshift or 10 10 lshift or N+
   $D1000000 1 or 2 5 lshift or 10 10 lshift or N+
   $D3400000 5 or 3 5 lshift or 64 2 - 63 and 16 lshift or 63 2 - 10 lshift or N+
   $D340FC00 5 or 3 5 lshift or 2 16 lshift or N+
   $EB00001F 2 5 lshift or 3 16 lshift or N+
   $F100001F 2 5 lshift or 5 10 lshift or N+
   $D4000001 0 5 lshift or N+
   $D65F03C0 N+
   $F9400000 1 or 2 5 lshift or 16 8 / 10 lshift or N+
   $F9000000 1 or 2 5 lshift or 16 8 / 10 lshift or N+
   $39400000 1 or 2 5 lshift or 3 10 lshift or N+
   $39000000 1 or 2 5 lshift or 3 10 lshift or N+
   $B9400000 1 or 2 5 lshift or 8 4 / 10 lshift or N+
   $B9000000 1 or 2 5 lshift or 8 4 / 10 lshift or N+
   $14000000 5 or N+   $94000000 5 or N+
   $54000000 3 5 lshift or 11 or N+
   $B4000000 4 5 lshift or 9 or N+
   $B5000000 4 5 lshift or 9 or N+
   $9E670000 1 or 2 5 lshift or N+
   $9E660000 1 or 2 5 lshift or N+
   $1E602800 0 or 1 5 lshift or 2 16 lshift or N+
   $1E603800 0 or 1 5 lshift or 2 16 lshift or N+
   $1E600800 0 or 1 5 lshift or 2 16 lshift or N+ ;
: ASM-OUT ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f"      slurp-file +B   s"  " +B
   s" test/demos/asm-demo.f" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
REF
T{ ASM-OUT  EB16 EL @  compare 0= -> true }T
