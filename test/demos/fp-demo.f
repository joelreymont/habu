\ fp-demo.f — checked floating point: Newton's method square root converging on
\ the fsqrt prim. Verdicts print first (-1 = certified), then the results.
\ Driven by test/t-sh-fp.fs (checker prepended there).
: HOOK CHECK dup . ;
' HOOK set-check
: NSTEP over over f/ f+ 0.5 f* ;
: NSQRT 1.0 NSTEP NSTEP NSTEP NSTEP NSTEP NSTEP nip ;
2.0 NSQRT f.
2.0 fsqrt f.
9.0 NSQRT f>s .
0.0 1.0 f< .
