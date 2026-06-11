\ spike-exe.fs — prove the full pipeline: ICode -> ARM64 -> Mach-O -> native run.
require ../src/cg/exec.fs

\ exit(42)
ICODE-RESET  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,
s" /tmp/habu-exit42" RUN-EXE  ." exit(42)  -> rc=" . cr

\ exit(7+5) — arithmetic in native code
ICODE-RESET  0 7 MOVZ,  1 5 MOVZ,  0 0 1 ADD,  16 1 MOVZ,  $80 SVC,
s" /tmp/habu-add" RUN-EXE  ." exit(7+5) -> rc=" . cr

\ write(1,"habu\n",4) then exit(0) — I/O via syscall, buffer on the stack
ICODE-RESET
2 $0A666163 LIT64,   \ x2 = "habu\n" little-endian
2 31 -16 STR-PRE,    \ [sp,#-16]! = x2  (sp = reg 31 as base)
1 31 0 ADDI,         \ x1 = sp  (add x1,sp,#0)
0 1 MOVZ,            \ x0 = 1 (stdout)
2 4 MOVZ,            \ x2 = 4 (length)
16 4 MOVZ,           \ x16 = 4 (SYS_write)
$80 SVC,
0 0 MOVZ,  16 1 MOVZ,  $80 SVC,   \ exit(0)
." write demo prints: "
s" /tmp/habu-hello" RUN-EXE  ." (rc=" . ." )" cr

bye
