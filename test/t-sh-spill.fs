\ t-sh-spill.fs — register-SPILL machinery: 9 live register copies exceed the 7-reg
\ pool (x9..x15), so R-ALLOC spills the deepest reg to its canonical x19 slot and
\ V-FORCE reloads it. "dup x8 + x8" of a runtime input N computes 9*N; the self-signed
\ binary exits 9*N, proving spill+reload are correct. Run: gforth test/t-sh-spill.fs -e bye
require sh-driver.fs
: GEN {: input -- :}
   0 CL !
   s" selfhost/sha256.fs"   slurp-file +B   s"  " +B
   s" selfhost/macho-min.fs" slurp-file +B   s"  " +B
   s" selfhost/sign.fs"     slurp-file +B   s"  " +B
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/icode.fs"    slurp-file +B   s"  " +B
   s" selfhost/walk.fs"     slurp-file +B   s"  " +B
   s" selfhost/vs.fs"       slurp-file +B   s"  " +B
   s" : INPUTVAL " +B  input 0 <# #s #> +B  s"  ; " +B
   s" selfhost/spill-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN ;
: BUILD-SPILL {: input -- }  input GEN ;
: RC ( -- n )  s" /tmp/sh-spill-bin; echo $? > /tmp/sh-spill-rc" system
   s" /tmp/sh-spill-rc" slurp-file  s>number? 2drop ;
5 BUILD-SPILL    T{ RC -> 45 }T                 \ 9 * 5 = 45  (spill + reload)
