\ t-cg-exe.fs — end-to-end: ICode -> ARM64 -> Mach-O -> ad-hoc sign -> native run.
\ Asserts the generated executables produce the right exit codes and stdout.
\ Slow (codesign+exec per case) and needs the macOS toolchain, so it is NOT in
\ all.fs — run explicitly:  gforth test/t-cg-exe.fs -e bye
require tester.fs
require ../src/cg/exec.fs

: R-EXIT42 ( -- rc )  ICODE-RESET  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,
   s" /tmp/caf-t42" RUN-EXE ;
T{ R-EXIT42 -> 42 }T

: R-ADD ( -- rc )  ICODE-RESET  0 7 MOVZ,  1 5 MOVZ,  0 0 1 ADD,  16 1 MOVZ,  $80 SVC,
   s" /tmp/caf-tadd" RUN-EXE ;
T{ R-ADD -> 12 }T

: R-MUL ( -- rc )  ICODE-RESET  0 6 MOVZ,  1 7 MOVZ,  0 0 1 MUL,  16 1 MOVZ,  $80 SVC,
   s" /tmp/caf-tmul" RUN-EXE ;
T{ R-MUL -> 42 }T                       \ 6*7 native multiply

\ countdown loop: x0=5; loop { x0-- } until zero; exit(x0)=0  — exercises a
\ back-edge branch + CBNZ resolved by the label pass.
: R-LOOP ( -- rc )  ICODE-RESET
   0 5 MOVZ,                            \ x0 = 5
   NEWLBL dup LBL,                      \ L:
   0 0 1 SUBI,                          \ x0 = x0 - 1
   0 swap CBNZ,                         \ cbnz x0, L
   16 1 MOVZ,  $80 SVC,                 \ exit(0)
   s" /tmp/caf-tloop" RUN-EXE ;
T{ R-LOOP -> 0 }T

\ stdout: write(1,"caf\n",4); exit(0). Capture via shell redirect + read back.
2variable OUT$
: CAPTURE ( -- )
   ICODE-RESET
   2 $0A666163 LIT64,  2 31 -16 STR-PRE,  1 31 0 ADDI,
   0 1 MOVZ,  2 4 MOVZ,  16 4 MOVZ,  $80 SVC,
   0 0 MOVZ,  16 1 MOVZ,  $80 SVC,
   s" /tmp/caf-thello" EMIT-EXE
   s" /tmp/caf-thello > /tmp/caf-tout" system
   s" /tmp/caf-tout" slurp-file OUT$ 2! ;
CAPTURE
T{ OUT$ 2@ nip -> 4 }T                              \ wrote 4 bytes ("caf\n")
: FIRST3 ( -- f )  OUT$ 2@ drop 3  s" caf" compare 0= ;
T{ FIRST3 -> true }T                                \ first three bytes = "caf"
