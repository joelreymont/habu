\ if-demo.fs — compile abs() = "dup 0< if negate then" on a runtime input with the
\ register allocator's IF/THEN (spill-to-memory at boundaries), emit a self-signed
\ binary to /tmp/sh-if-bin. Input is negated first so the conditional is real (-N -> N).
: GO  ASM-INIT  s" negate dup 0< if negate then" INPUTVAL GEN-VS-N
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-if-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
