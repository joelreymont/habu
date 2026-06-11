\ cmp-demo.fs — compile SRC$ on runtime input INPUTVAL with the register allocator's
\ comparison ops (= < > and 1+ 1-), emit a self-signed binary to /tmp/sh-cmp-bin.
\ The test prepends : INPUTVAL n ; and : SRC$ s" ..." ; per case (fold + runtime paths).
: GO  ASM-INIT  SRC$ INPUTVAL GEN-VS-N
  ASM-LEN HDR  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD CODESIG  s" /tmp/sh-cmp-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
GO
