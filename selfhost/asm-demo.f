\ asm-demo.fs — print a fixed set of ARM64 encodings (decimal u32, one per line) for
\ test/t-sh-asm.fs to verify against habu's encoders. Needs asm.fs.
: GO
  5 42 0 MOVZHW .   5 7 2 MOVKHW .   3 1 0 MOVNHW .
  1 2 3 ENC-ADD .   1 2 3 ENC-SUB .   1 2 3 ENC-AND .
  1 2 3 ENC-ORR .   1 2 3 ENC-EOR .   1 2 3 ENC-MUL .
  1 2 10 ENC-ADDI .   1 2 10 ENC-SUBI .
  5 3 2 ENC-LSLI .   5 3 2 ENC-LSRI .
  2 3 ENC-CMP .   2 5 ENC-CMPI .   0 ENC-SVC .   ENC-RET .
  1 2 16 ENC-LDR .   1 2 16 ENC-STR .   1 2 3 ENC-LDRB .   1 2 3 ENC-STRB .
  1 2 8 ENC-LDRW .   1 2 8 ENC-STRW .
  5 ENC-B .   5 ENC-BL .   3 11 ENC-BCOND .   9 4 ENC-CBZ .   9 4 ENC-CBNZ .
  1 2 ENC-FMOVXD .   1 2 ENC-FMOVDX .   0 1 2 ENC-FADD .   0 1 2 ENC-FSUB .   0 1 2 ENC-FMUL . ;
GO
