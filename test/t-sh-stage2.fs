\ t-sh-stage2.fs — THE COMPILER FIXPOINT, from source. SRC = the complete ported
\ compiler (encoders, assembler, runtime, crash handler, Mach-O builder, engine
\ builder parts 1+2) + the stage2 driver. gforth builds stage1 from SRC; running
\ stage1 makes it COMPILE SRC (its own source) with the ported engine builder and
\ emit stage2. Assert: stage2 == the gforth-built engine image for the same SRC,
\ byte for byte. The standalone compiles its own compiler — and the output is the
\ canonical artifact. Run: gforth test/t-sh-stage2.fs -e bye
require ../bootstrap/cg/forth.fs
require ../bootstrap/cg/exec.fs
require sh-driver.fs
: SRC+ ( -- )  0 CL !                                \ mirrors tools/srclist.sh
   s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
   s" : HOOK CHECK ; ' HOOK set-check " +B            \ DOGFOOD: every word below is
                            \ checked as the compiler compiles itself;
   s" src/core/sha256.f" +F  s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F
   s" src/arch/arm64/mnem.f" +F
   s" src/os/macos/sys.f" +F  s" src/habu/rt.f" +F  s" src/habu/crash.f" +F  s" src/os/macos/macho.f" +F
   s" src/os/macos/sign2.f" +F  s" src/habu/habu1.f" +F  s" src/habu/prof.f" +F  s" src/habu/regalloc.f" +F  s" src/habu/jit.f" +F  s" src/habu/habu2.f" +F
   s" src/habu/stage2.f" +F ;                        \ a type error rejects -> exit 70
\ write SRC where stage1 will read it back as data
: SAVE-SRC ( -- )
   s" /tmp/stage2-src" w/o create-file throw {: fd :}
   CBUF CL @ fd write-file throw  fd close-file throw ;
\ reference: gforth builds + SIGNS the engine image for SRC (same fixed identifier)
: REF ( -- )
   CBUF CL @ EMIT-FORTH  BUILD-MACHO
   s" hb" SIG-ID 2!  CODESIG
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
