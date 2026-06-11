\ walk-demo.fs — compile the Forth body "7 dup *" to native code with GEN-BODY and
\ emit a self-signed binary to /tmp/sh-walk-bin (exit 49). Needs sha256+macho-min+sign
\ +asm+icode+walk. Proves the standalone COMPILES source (not hand-assembled/copied).
: GO
  ASM-INIT
  s" 7 dup *" GEN-BODY
  ASM-LEN HDR
  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD  CODESIG
  s" /tmp/sh-walk-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
GO
