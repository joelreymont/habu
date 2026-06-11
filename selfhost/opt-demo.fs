\ opt-demo.fs — compile "5 dup *", run the peephole optimizer (opt.fs), and emit a
\ self-signed binary to /tmp/sh-opt-bin. Prints the instruction count before and after
\ optimization (store-to-load forwarding removes the redundant ldr). Driven by t-sh-opt.
: GO
  ASM-INIT  s" 5 dup *" GEN-BODY
  CP @ .  OPT  CP @ .                      \ before, after (after < before)
  ASM-LEN HDR
  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD  CODESIG
  s" /tmp/sh-opt-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
GO
