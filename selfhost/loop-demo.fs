\ loop-demo.fs — compile a BEGIN/UNTIL countdown ("begin 1 - dup 0= until 42 +") on a
\ runtime input, self-sign to /tmp/sh-loop-bin. Counts the input down to 0, then +42 ->
\ exit 42. Proves loop codegen (spill at the loop top + back-edge) in the standalone.
: GO  ASM-INIT  s" begin 1 - dup 0= until 42 +" INPUTVAL GEN-VS-N
  ASM-LEN HDR  0 BEGIN dup ASM-LEN < WHILE dup CODE + c@ M8 1 + REPEAT drop
  MPAGE MPAD CODESIG  s" /tmp/sh-loop-bin" PSET PB 1537 493 open dup MSTART @ MOFF write drop close ;
