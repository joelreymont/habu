\ t-sh-bodycap.fs — the body-capture buffer is 8 KB and OVERFLOW IS FATAL (exit 71):
\ a silently truncated capture would let the check hook certify code it never saw.
\ Bodies over the old 900-byte cap (engine-builder-port sized) now compile fine.
\ Run: gforth test/t-sh-bodycap.fs -e bye
require sh-driver.fs
: RC-OF ( a u -- code )  s" /tmp/nf-bc-bin" FORTH-EXE
   s" /tmp/nf-bc-bin >/dev/null 2>/dev/null; echo $? > /tmp/nf-bc-rc" system
   s" /tmp/nf-bc-rc" slurp-file s>number? 2drop ;
: REPS ( n -- a u )  0 CL !  s" : GO 1 " +B  0 do s" dup drop " +B loop  s" . ; GO" +B
   CBUF CL @ ;
T{ 250 REPS RC-OF ->  0 }T     \ ~2.3 KB body (old cap 900) compiles + runs
T{ 900 REPS RC-OF -> 71 }T     \ ~8.1 KB body -> loud death, never truncation
