\ t-sh-walk.fs — the standalone COMPILES a Forth body to native code: GEN-BODY (walk.fs)
\ turns "7 dup *" into ARM64 via the encoders+assembler, emits a self-signed binary,
\ which runs and exits 49. Source -> native, zero external tools, no hand-assembly.
\ Run: gforth test/t-sh-walk.fs -e bye
require sh-driver.fs
: GEN ( -- )
   0 CL !
   s" src/core/sha256.f"   slurp-file +B   s"  " +B
   s" src/arch/arm64/asm.f"      slurp-file +B   s"  " +B
   s" src/arch/arm64/icode.f"    slurp-file +B   s"  " +B
   s" src/core/util.f"    slurp-file +B   s"  " +B
   s" src/arch/arm64/walk.f"     slurp-file +B   s"  " +B
   s" src/os/macos/macho.f"    slurp-file +B   s"  " +B
   s" src/os/macos/sign2.f"    slurp-file +B   s"  " +B
   s" test/demos/walk-demo.f" slurp-file +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-walk-bin; echo $? > /tmp/sh-walk-rc" system
   s" /tmp/sh-walk-rc" slurp-file  s>number? 2drop ;
GEN
T{ RC -> 49 }T
