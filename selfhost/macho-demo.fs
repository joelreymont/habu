\ macho-demo.fs — drive the FULL Mach-O builder port: assemble exit(42), build the
\ unsigned binary, write it to /tmp/sh-macho-got.bin. t-sh-macho.fs compares it
\ byte-for-byte against caf's src/cg/macho.fs output for the same program.
create PZ 32 allot
: SETP  s" /tmp/sh-macho-got.bin" {: a u :}
   0 BEGIN dup u < WHILE  dup a + c@  over PZ + c!  1 + REPEAT drop  0 PZ u + c! ;
: GO  ASM-INIT  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,
   BUILD-MACHO
   SETP  PZ 1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
