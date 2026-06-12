\ t-sh-cg.fs — the standalone GENERATES native code with its own toolchain: encoders
\ (asm.fs) + single-pass assembler with labels/branches (icode.fs) assemble a loop
\ computing exit(5+4+3+2+1=15); it emits a self-signed Mach-O (full Mach-O builder (macho.fs+sign2.fs)) with
\ ZERO external tools. Assert the generated binary runs and exits 15.
\ Run: gforth test/t-sh-cg.fs -e bye
require sh-driver.fs
: GEN ( -- )                              \ build the standalone codegen + run it -> /tmp/sh-cg-bin
   0 CL !
   s" src/core/util.f"     slurp-file +B   s"  " +B
   s" src/core/sha256.f"   slurp-file +B   s"  " +B
   s" src/arch/arm64/asm.f"      slurp-file +B   s"  " +B
   s" src/arch/arm64/icode.f"    slurp-file +B   s"  " +B
   s" src/os/macos/macho.f"    slurp-file +B   s"  " +B
   s" src/os/macos/sign2.f"    slurp-file +B   s"  " +B
   s" test/demos/cg-demo.f"  slurp-file +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-cg-bin; echo $? > /tmp/sh-cg-rc" system
   s" /tmp/sh-cg-rc" slurp-file  s>number? 2drop ;
s" rm -f /tmp/sh-cg-bin /tmp/sh-cg-rc" system
GEN
T{ RC -> 15 }T
