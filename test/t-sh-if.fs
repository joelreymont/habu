\ t-sh-if.fs — the standalone codegen with CONTROL FLOW: compiles abs() with a real
\ runtime IF/THEN (register allocator + spill-to-memory at the branch merge), emits a
\ self-signed binary, and it computes abs correctly. Run: gforth test/t-sh-if.fs -e bye
require sh-driver.fs
: BUILD-IF {: input -- }                       \ compile abs(-input) -> /tmp/sh-if-bin
   0 CL !
   s" selfhost/sha256.f"   slurp-file +B   s"  " +B
   s" selfhost/asm.f"      slurp-file +B   s"  " +B
   s" selfhost/icode.f"    slurp-file +B   s"  " +B
   s" selfhost/util.f"    slurp-file +B   s"  " +B
   s" selfhost/walk.f"     slurp-file +B   s"  " +B
   s" selfhost/vs.f"       slurp-file +B   s"  " +B
   s" : INPUTVAL " +B  input 0 <# #s #> +B  s"  ; " +B
   s" selfhost/macho.f"    slurp-file +B   s"  " +B
   s" selfhost/sign2.f"    slurp-file +B   s"  " +B
   s" selfhost/if-demo.f"  slurp-file +B  s"  GO" +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-if-bin; echo $? > /tmp/sh-if-rc" system
   s" /tmp/sh-if-rc" slurp-file  s>number? 2drop ;
7 BUILD-IF    T{ RC -> 7 }T                     \ abs(-7) = 7   (IF taken, negate)
