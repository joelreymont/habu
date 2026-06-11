\ t-sh-walk.fs — the standalone COMPILES a Forth body to native code: GEN-BODY (walk.fs)
\ turns "7 dup *" into ARM64 via the encoders+assembler, emits a self-signed binary,
\ which runs and exits 49. Source -> native, zero external tools, no hand-assembly.
\ Run: gforth test/t-sh-walk.fs -e bye
require nf.fs
require tester.fs
create CBUF 131072 allot   variable CL
: +B {: a u -- }  a  CBUF CL @ +  u move  u CL +! ;
: GEN ( -- )
   0 CL !
   s" selfhost/sha256.fs"   slurp-file +B   s"  " +B
   s" selfhost/macho-min.fs" slurp-file +B   s"  " +B
   s" selfhost/sign.fs"     slurp-file +B   s"  " +B
   s" selfhost/asm.fs"      slurp-file +B   s"  " +B
   s" selfhost/icode.fs"    slurp-file +B   s"  " +B
   s" selfhost/walk.fs"     slurp-file +B   s"  " +B
   s" selfhost/walk-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN ;
: RC ( -- n )  s" /tmp/sh-walk-bin; echo $? > /tmp/sh-walk-rc" system
   s" /tmp/sh-walk-rc" slurp-file  s>number? 2drop ;
GEN
T{ RC -> 49 }T
