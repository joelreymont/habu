\ t-sh-macho.fs — the FULL Mach-O builder port (selfhost/macho.fs) produces a
\ byte-identical unsigned binary to caf's src/cg/macho.fs for the same program
\ (exit(42)). Run: gforth test/t-sh-macho.fs -e bye
require ../src/cg/macho.fs
require sh-driver.fs
: REF ( -- )  ICODE-RESET  0 42 MOVZ,  16 1 MOVZ,  $80 SVC,  BUILD-MACHO
   s" /tmp/sh-macho-ref.bin" w/o create-file throw {: fd :}
   MBUF MLEN @ fd write-file throw  fd close-file throw ;
: GEN ( -- )
   0 CL !
   s" selfhost/asm.fs" +F  s" selfhost/icode.fs" +F  s" selfhost/mnem.fs" +F
   s" selfhost/util.fs" +F  s" selfhost/macho.fs" +F  s" selfhost/macho-demo.fs" +F
   CBUF CL @ NF-RUN ;
REF GEN
T{ s" /tmp/sh-macho-ref.bin" slurp-file s" /tmp/sh-macho-got.bin" slurp-file compare 0= -> true }T
