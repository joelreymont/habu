\ t-sh-stage2.fs — THE COMPILER FIXPOINT, from source. SRC = the complete ported
\ compiler (encoders, assembler, runtime, crash handler, Mach-O builder, engine
\ builder parts 1+2) + the stage2 driver. gforth builds stage1 from SRC; running
\ stage1 makes it COMPILE SRC (its own source) with the ported engine builder and
\ emit stage2. Assert: stage2 == the gforth-built engine image for the same SRC,
\ byte for byte. The standalone compiles its own compiler — and the output is the
\ canonical artifact. Run: gforth test/t-sh-stage2.fs -e bye
require ../src/cg/forth.fs
require ../src/cg/exec.fs
require sh-driver.fs
: SRC+ ( -- )  0 CL !
   s" selfhost/sha256.fs" +F  s" selfhost/asm.fs" +F  s" selfhost/icode.fs" +F
   s" selfhost/mnem.fs" +F  s" selfhost/util.fs" +F  s" selfhost/walk.fs" +F
   s" selfhost/rt.fs" +F  s" selfhost/crash.fs" +F  s" selfhost/macho.fs" +F
   s" selfhost/sign2.fs" +F  s" selfhost/engine.fs" +F  s" selfhost/engine2.fs" +F
   s" selfhost/stage2.fs" +F ;
\ write SRC where stage1 will read it back as data
: SAVE-SRC ( -- )
   s" /tmp/stage2-src" w/o create-file throw {: fd :}
   CBUF CL @ fd write-file throw  fd close-file throw ;
\ reference: gforth builds + SIGNS the engine image for SRC (same fixed identifier)
: REF ( -- )
   CBUF CL @ EMIT-FORTH  BUILD-MACHO
   s" stage2" SIG-ID 2!  CODESIG
   s" /tmp/stage2-ref" w/o create-file throw {: fd :}
   MBUF MLEN @ fd write-file throw  fd close-file throw ;
SRC+  SAVE-SRC  REF
\ stage1: the gforth-built standalone whose baked program IS the compiler source;
\ running it compiles the compiler and emits a SIGNED stage2
CBUF CL @ s" /tmp/stage1-bin" FORTH-EXE
: RUN-STAGE1 ( -- )  s" rm -f /tmp/stage2-got; /tmp/stage1-bin" system ;
RUN-STAGE1
T{ s" /tmp/stage2-ref" slurp-file  s" /tmp/stage2-got" slurp-file  compare 0= -> true }T
\ the loop WITHOUT gforth: run stage2 itself -> it compiles the same source and
\ emits stage3; the chain is self-sustaining when stage3 == stage2.
: RUN-STAGE2 ( -- )
   s" cp /tmp/stage2-got /tmp/stage2-bin; chmod +x /tmp/stage2-bin; rm -f /tmp/stage2-got; /tmp/stage2-bin" system ;
RUN-STAGE2
T{ s" /tmp/stage2-bin" slurp-file  s" /tmp/stage2-got" slurp-file  compare 0= -> true }T
