\ t-sh-loop.fs — the standalone codegen with LOOPS: compiles a BEGIN/UNTIL countdown
\ (register allocator + spill at the loop top and back-edge), emits a self-signed binary
\ that counts a runtime input down to 0 then +42 -> exit 42. Run: gforth test/t-sh-loop.fs -e bye
require sh-driver.fs
: BUILD-LOOP {: input -- }
   0 CL !
   s" src/core/sha256.f"   slurp-file +B   s"  " +B
   s" src/arch/arm64/asm.f"      slurp-file +B   s"  " +B
   s" src/arch/arm64/icode.f"    slurp-file +B   s"  " +B
   s" src/core/util.f"    slurp-file +B   s"  " +B
   s" src/arch/arm64/walk.f"     slurp-file +B   s"  " +B
   s" src/arch/arm64/vs.f"       slurp-file +B   s"  " +B
   s" : INPUTVAL " +B  input 0 <# #s #> +B  s"  ; " +B
   s" src/os/macos/macho.f"    slurp-file +B   s"  " +B
   s" src/os/macos/sign2.f"    slurp-file +B   s"  " +B
   s" test/demos/loop-demo.f" slurp-file +B  s"  GO" +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-loop-bin; echo $? > /tmp/sh-loop-rc" system
   s" /tmp/sh-loop-rc" slurp-file  s>number? 2drop ;
5 BUILD-LOOP   T{ RC -> 42 }T
