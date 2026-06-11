\ vs-demo.fs — compile "5 dup *" with the REGISTER allocator (vs.fs), print the
\ instruction count, and emit a self-signed binary to /tmp/sh-vs-bin (exit 25). The
\ register-allocated code has zero ldr/str (vs 16 instructions for the memory model).
: GO
  ASM-INIT  s" 5 dup *" GEN-VS
  CP @ .                                   \ instruction count (8, no memory traffic)
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-vs-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO
