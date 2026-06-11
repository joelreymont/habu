\ spill-demo.fs — force the register pool (x9..x15, 7 regs) to EXHAUST: dup a runtime
\ input 8 times (9 live copies > 7 regs), so R-ALLOC must spill the deepest reg to its
\ canonical x19 slot and reload it. Then sum the 9 copies -> 9*input. Self-signed binary
\ exits with that sum, proving spill+reload is correct. Input 5 -> exit 45.
: GO  ASM-INIT  s" dup dup dup dup dup dup dup dup + + + + + + + +" INPUTVAL GEN-VS-N
  ASM-LEN HDR  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD CODESIG  s" /tmp/sh-spill-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
GO
