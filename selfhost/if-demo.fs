\ if-demo.fs — compile abs() = "dup 0< if negate then" on a runtime input with the
\ register allocator's IF/THEN (spill-to-memory at boundaries), emit a self-signed
\ binary to /tmp/sh-if-bin. Input is negated first so the conditional is real (-N -> N).
: GO  ASM-INIT  s" negate dup 0< if negate then" INPUTVAL GEN-VS-N
  ASM-LEN HDR  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD CODESIG  s" /tmp/sh-if-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
