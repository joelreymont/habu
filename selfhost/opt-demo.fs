\ opt-demo.fs — compile "5 dup *", run the peephole optimizer (opt.fs), and emit a
\ self-signed binary to /tmp/sh-opt-bin. Prints the instruction count before and after
\ optimization (store-to-load forwarding removes the redundant ldr). Driven by t-sh-opt.
: GO
  ASM-INIT  s" 5 dup *" GEN-BODY
  CP @ .  OPT  CP @ .                      \ before, after (after < before)
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-opt-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO
