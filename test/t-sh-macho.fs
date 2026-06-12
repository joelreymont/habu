\ t-sh-macho.fs — the FULL Mach-O builder port (src/os/macos/macho.f) produces a
\ byte-identical unsigned binary to habu's bootstrap/cg/macho.fs for the same program
\ (exit(42)). Run: gforth test/t-sh-macho.fs -e bye
require ../bootstrap/cg/macho.fs
require sh-driver.fs
: REF ( -- )  ICODE-RESET  0 42 MOVZ,  NR-EXIT SYS,  BUILD-MACHO
   s" /tmp/sh-macho-ref.bin" w/o create-file throw {: fd :}
   MBUF MLEN @ fd write-file throw  fd close-file throw ;
: GEN ( -- )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/arch/arm64/mnem.f" +F  s" src/os/macos/sys.f" +F
   s" src/core/util.f" +F  s" src/os/macos/macho.f" +F  s" test/demos/macho-demo.f" +F
   CBUF CL @ NF-RUN ;
REF GEN
T{ s" /tmp/sh-macho-ref.bin" slurp-file s" /tmp/sh-macho-got.bin" slurp-file compare 0= -> true }T
