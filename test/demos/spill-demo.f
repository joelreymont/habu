\ spill-demo.fs — force the register pool (x9..x15, 7 regs) to EXHAUST: dup a runtime
\ input 8 times (9 live copies > 7 regs), so R-ALLOC must spill the deepest reg to its
\ canonical x19 slot and reload it. Then sum the 9 copies -> 9*input. Self-signed binary
\ exits with that sum, proving spill+reload is correct. Input 5 -> exit 45.
: GO  ASM-INIT  s" dup dup dup dup dup dup dup dup + + + + + + + +" INPUTVAL GEN-VS-N
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-spill-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO
