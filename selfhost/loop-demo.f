\ loop-demo.fs — compile a BEGIN/UNTIL countdown ("begin 1 - dup 0= until 42 +") on a
\ runtime input, self-sign to /tmp/sh-loop-bin. Counts the input down to 0, then +42 ->
\ exit 42. Proves loop codegen (spill at the loop top + back-edge) in the standalone.
: GO  ASM-INIT  s" begin 1 - dup 0= until 42 +" INPUTVAL GEN-VS-N
  BUILD-MACHO  s" sh" SET-SIGID CODESIG2
  s" /tmp/sh-loop-bin" PATH0 1537 493 open dup MBUF MLEN @ write drop close ;
