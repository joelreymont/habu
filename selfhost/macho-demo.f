\ macho-demo.fs — drive the FULL Mach-O builder port: assemble exit(42), build the
\ unsigned binary, write it to /tmp/sh-macho-got.bin. t-sh-macho.fs compares it
\ byte-for-byte against habu's src/cg/macho.fs output for the same program.
create PZ 32 allot
: GO  ASM-INIT  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,
   BUILD-MACHO
   s" /tmp/sh-macho-got.bin" PZ PATHZ
   PZ 1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
