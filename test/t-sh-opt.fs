\ t-sh-opt.fs — the standalone's peephole OPTIMIZER (opt.fs): store-to-load forwarding
\ removes a redundant ldr from the token compiler's output (16 -> 15 instructions), and
\ the optimized self-signed binary still computes correctly (5*5 = 25). First step of
\ porting habu's optimizer to the standalone. Run: gforth test/t-sh-opt.fs -e bye
require sh-driver.fs
: GEN ( -- a u )
   0 CL !
   s" src/core/sha256.f"   slurp-file +B   s"  " +B
   s" src/arch/arm64/asm.f"      slurp-file +B   s"  " +B
   s" src/arch/arm64/icode.f"    slurp-file +B   s"  " +B
   s" src/core/util.f"    slurp-file +B   s"  " +B
   s" src/arch/arm64/walk.f"     slurp-file +B   s"  " +B
   s" src/arch/arm64/opt.f"      slurp-file +B   s"  " +B
   s" src/os/macos/macho.f"    slurp-file +B   s"  " +B
   s" src/os/macos/sign2.f"    slurp-file +B   s"  " +B
   s" test/demos/opt-demo.f" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
: RC ( -- n )  s" /tmp/sh-opt-bin; echo $? > /tmp/sh-opt-rc" system
   s" /tmp/sh-opt-rc" slurp-file  s>number? 2drop ;
T{ GEN  s\" 16\n15\n" compare 0= -> true }T   \ optimizer removed 1 instruction
T{ RC -> 25 }T                                 \ optimized binary still correct (5*5)
