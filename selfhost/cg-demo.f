\ cg-demo.fs — the standalone GENERATES native code with its own encoders+assembler:
\ assembles a loop computing exit(5+4+3+2+1=15), emits a self-signed Mach-O to
\ /tmp/sh-cg-bin. Proves encoder + assembler (labels/branches) + Mach-O + sign end to
\ end. Concatenated after sha256.fs macho.fs sign.fs asm.fs icode.fs.
: ASM-PROG
  ASM-INIT
  0 0 0 MOVZHW EMITW            \ mov x0, #0   (acc)
  1 5 0 MOVZHW EMITW            \ mov x1, #5   (i)
  NEWLBL {: lp :}  lp LBL,
  0 0 1 ENC-ADD EMITW           \ add x0, x0, x1
  1 1 1 ENC-SUBI EMITW          \ sub x1, x1, #1
  1 lp CBNZ,                    \ cbnz x1, lp
  16 1 0 MOVZHW EMITW           \ mov x16, #1
  0 ENC-SVC EMITW ;             \ svc #0  -> exit(x0)
: GO
  ASM-PROG
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-cg-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO
