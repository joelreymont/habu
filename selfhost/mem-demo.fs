\ mem-demo.fs — compile a memory round-trip ("here 42 over ! @") with the register
\ allocator's memory ops (@ ! via ldr/str, HERE = scratch buffer at x19+256), self-sign
\ to /tmp/sh-mem-bin. Stores 42, loads it back -> exit 42. Memory codegen in the standalone.
: GO  ASM-INIT  s" here 42 over ! @" 0 GEN-VS-N
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-mem-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO
