\ t-sh-loop.fs — the standalone codegen with LOOPS: compiles a BEGIN/UNTIL countdown
\ (register allocator + spill at the loop top and back-edge), emits a self-signed binary
\ that counts a runtime input down to 0 then +42 -> exit 42. Run: gforth test/t-sh-loop.fs -e bye
require sh-driver.fs
: BUILD-LOOP {: input -- }
   0 CL !
   s" selfhost/sha256.fs"   slurp-file +B   s"  " +B
   s" selfhost/macho-min.fs" slurp-file +B   s"  " +B
   s" selfhost/sign.fs"     slurp-file +B   s"  " +B
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/icode.fs"    slurp-file +B   s"  " +B
   s" selfhost/util.fs"    slurp-file +B   s"  " +B
   s" selfhost/walk.fs"     slurp-file +B   s"  " +B
   s" selfhost/vs.fs"       slurp-file +B   s"  " +B
   s" : INPUTVAL " +B  input 0 <# #s #> +B  s"  ; " +B
   s" selfhost/loop-demo.fs" slurp-file +B  s"  GO" +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-loop-bin; echo $? > /tmp/sh-loop-rc" system
   s" /tmp/sh-loop-rc" slurp-file  s>number? 2drop ;
5 BUILD-LOOP   T{ RC -> 42 }T
