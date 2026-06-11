\ t-sh-balance.fs — the vs codegen DIES (exit 72) on control-flow stack-depth bugs
\ instead of silently miscompiling: an IF body that nets a pop makes the canonical
\ spill slots disagree between the branch paths. Also: THEN without IF. A balanced
\ body still compiles (covered by t-sh-if). Run: gforth test/t-sh-balance.fs -e bye
require sh-driver.fs
: CG+ ( -- )  0 CL !
   s" selfhost/sha256.fs" +F  s" selfhost/macho-min.fs" +F  s" selfhost/sign.fs" +F
   s" selfhost/util.fs" +F  s" selfhost/asm.fs" +F  s" selfhost/icode.fs" +F  s" selfhost/walk.fs" +F
   s" selfhost/vs.fs" +F ;
: GEN-RC ( frag-a frag-u -- code )         \ codegen stack + fragment -> build+run, exit code
   CG+ +B  CBUF CL @ s" /tmp/sh-bal-bin" FORTH-EXE
   s" /tmp/sh-bal-bin >/dev/null 2>/dev/null; echo $? > /tmp/sh-bal-rc" system
   s" /tmp/sh-bal-rc" slurp-file s>number? 2drop ;
T{ s\" : GO ASM-INIT s\" dup if drop then\" 7 GEN-VS-N ; GO"  GEN-RC -> 72 }T  \ body nets a pop
T{ s\" : GO ASM-INIT s\" 5 then\" 7 GEN-VS-N ; GO"            GEN-RC -> 72 }T  \ then without if
T{ s\" : GO ASM-INIT s\" 1- dup 0= until\" 7 GEN-VS-N ; GO"   GEN-RC -> 72 }T  \ until without begin
