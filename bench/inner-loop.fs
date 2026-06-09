\ inner-loop.fs — Phase 0.3 speed-gate microbench.
\ A serial scalar LCG step (mul + add, wrapping 64-bit): no closed form, no
\ vectorization, data-dependent — representative of a VM/decoder inner step.
\ Run: gforth inner-loop.fs -e "BENCH bye"   (and likewise gforth-fast)

6364136223846793005 constant LCG-A
1442695040888963407 constant LCG-C
1000000000           constant ITERS          \ 1e9 iterations

variable RES

: LCG ( seed n -- x )  0 ?do  LCG-A *  LCG-C +  loop ;

: BENCH ( -- )
  utime  1 ITERS LCG RES !  utime              \ ( s.lo s.hi e.lo e.hi ), RES=x
  2swap d-  d>s                                 \ elapsed microseconds
  ." us=" .  ." result=" RES @ u.  cr ;
