\ t-sh-vs.fs — the standalone's REGISTER-ALLOCATING code generator (vs.fs, caf's VS
\ allocator ported): the data stack lives in registers (x9..x15), so "5 dup *" compiles
\ to 5 instructions (the 5*5 multiply is folded at compile time) (the memory model needs 16), and the self-signed
\ binary still computes 5*5 = 25. Run: gforth test/t-sh-vs.fs -e bye
require nf.fs
require tester.fs
create CBUF 131072 allot   variable CL
: +B {: a u -- }  a  CBUF CL @ +  u move  u CL +! ;
: GEN ( -- a u )
   0 CL !
   s" selfhost/sha256.fs"   slurp-file +B   s"  " +B
   s" selfhost/macho-min.fs" slurp-file +B   s"  " +B
   s" selfhost/sign.fs"     slurp-file +B   s"  " +B
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/icode.fs"    slurp-file +B   s"  " +B
   s" selfhost/walk.fs"     slurp-file +B   s"  " +B
   s" selfhost/vs.fs"       slurp-file +B   s"  " +B
   s" selfhost/vs-demo.fs"  slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
: RC ( -- n )  s" /tmp/sh-vs-bin; echo $? > /tmp/sh-vs-rc" system
   s" /tmp/sh-vs-rc" slurp-file  s>number? 2drop ;
T{ GEN  s\" 5\n" compare 0= -> true }T     \ register-allocated + constant-folded: 5 instructions (no ldr/str)
T{ RC -> 25 }T                              \ correct (5*5)
