\ cmp-demo.fs — compile SRC$ on runtime input INPUTVAL with the register allocator's
\ comparison ops (= < > and 1+ 1-), emit a self-signed binary to /tmp/sh-cmp-bin.
\ The test prepends : INPUTVAL n ; and : SRC$ s" ..." ; per case (fold + runtime paths).
: GO  ASM-INIT  SRC$ INPUTVAL GEN-VS-N
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-cmp-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
GO
