\ mem-demo.fs — compile a memory round-trip ("here 42 over ! @") with the register
\ allocator's memory ops (@ ! via ldr/str, HERE = scratch buffer at x19+256), self-sign
\ to /tmp/sh-mem-bin. Stores 42, loads it back -> exit 42. Memory codegen in the standalone.
: GO  ASM-INIT  s" here 42 over ! @" 0 GEN-VS-N
  ASM-LEN HDR  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD CODESIG  s" /tmp/sh-mem-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
GO
