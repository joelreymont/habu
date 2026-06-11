\ t-selfrebuild.fs — the self-host FIXPOINT (dot 10). The standalone re-emits its
\ OWN binary: it rebuilds the Mach-O (header + load commands) deterministically and
\ copies its compiled code from its loaded image, producing a byte-identical copy.
\ gforth builds stage2; stage2 (run) emits stage3; assert stage2 == stage3.
\ Slow (build + codesign + exec); run explicitly:  gforth test/t-selfrebuild.fs -e bye
require ../src/cg/forth.fs
require ../src/cg/exec.fs
require tester.fs

s" selfhost/rebuild.fs" slurp-file 2constant SRC

\ stage2 = the standalone built by gforth (unsigned bytes)
SRC EMIT-FORTH BUILD-MACHO  s" /tmp/sr-stage2" WRITE-EXE
\ a signed, runnable copy of stage2
SRC s" /tmp/sr-bin" FORTH-EXE

\ run stage2 -> it executes REBUILD, writing /tmp/self-out (= stage3, unsigned)
: RUN-STAGE2 ( -- )  s" rm -f /tmp/self-out; /tmp/sr-bin" system ;
RUN-STAGE2

\ assert stage2 == stage3, byte-identical
: SAME? ( -- f )
   s" /tmp/sr-stage2" slurp-file  s" /tmp/self-out" slurp-file  compare 0= ;
T{ SAME? -> true }T
