\ t-sh-cmp.fs — comparison ops (= < >) and 1+/1- in the register-allocating codegen,
\ BOTH paths each: constant-folded (both operands known -> flag folded at compile time,
\ must be the same 0/-1 Forth flag the runtime cset path yields) and runtime (input in
\ a register -> cmp/cset/sub). Exit code 255 = -1 (true flag). Run: gforth test/t-sh-cmp.fs -e bye
require sh-driver.fs
: GEN {: sa su input -- :}
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
   s\" : SRC$ s\" " +B  sa su +B  s\" \" ; " +B
   s" selfhost/cmp-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-cmp-bin; echo $? > /tmp/sh-cmp-rc" system
   s" /tmp/sh-cmp-rc" slurp-file  s>number? 2drop ;
s" 9 9 = 42 and"  0 GEN  T{ RC ->  42 }T   \ EQ folded: -1 and 42 = 42 (negate bug -> 0)
s" 1- 1- 5 ="     7 GEN  T{ RC -> 255 }T   \ 1- runtime x2, EQ runtime: 5=5 -> -1
s" 3 9 < 42 and"  0 GEN  T{ RC ->  42 }T   \ LT folded
s" 10 <"          7 GEN  T{ RC -> 255 }T   \ LT runtime: 7<10
s" 1+ 7 >"        7 GEN  T{ RC -> 255 }T   \ 1+ runtime, GT runtime: 8>7
s" 9 3 > 42 and"  0 GEN  T{ RC ->  42 }T   \ GT folded
s" 8 1+ 1-"       0 GEN  T{ RC ->   8 }T   \ 1+/1- folded round-trip
s" 5 ="           7 GEN  T{ RC ->   0 }T   \ EQ runtime false: 7<>5 -> 0
