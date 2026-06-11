\ t-sh-vs.fs — the standalone's REGISTER-ALLOCATING code generator (vs.fs, habu's VS
\ allocator ported): the data stack lives in registers (x9..x15), so "5 dup *" compiles
\ to 5 instructions (the 5*5 multiply is folded at compile time) (the memory model needs 16), and the self-signed
\ binary still computes 5*5 = 25. Run: gforth test/t-sh-vs.fs -e bye
require sh-driver.fs
: GEN ( -- a u )
   0 CL !
   s" src/core/sha256.f"   slurp-file +B   s"  " +B
   s" src/arch/arm64/asm.f"      slurp-file +B   s"  " +B
   s" src/arch/arm64/icode.f"    slurp-file +B   s"  " +B
   s" src/core/util.f"    slurp-file +B   s"  " +B
   s" src/arch/arm64/walk.f"     slurp-file +B   s"  " +B
   s" src/arch/arm64/vs.f"       slurp-file +B   s"  " +B
   s" src/os/macos/macho.f"    slurp-file +B   s"  " +B
   s" src/os/macos/sign2.f"    slurp-file +B   s"  " +B
   s" test/demos/vs-demo.f"  slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
: RC ( -- n )  s" /tmp/sh-vs-bin; echo $? > /tmp/sh-vs-rc" system
   s" /tmp/sh-vs-rc" slurp-file  s>number? 2drop ;
T{ GEN  s\" 6\n" compare 0= -> true }T     \ register+folded: 6 instructions (incl. frame setup) (no ldr/str)
T{ RC -> 25 }T                              \ correct (5*5)
