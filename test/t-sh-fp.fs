\ t-sh-fp.fs — floating point in habu (fp umbrella gate): doubles as bit-cells on
\ the data stack, d.d literals, f-prims, f. printing, and the checker's r type
\ (mixed int/float arithmetic is a REJECTED program). Run: gforth test/t-sh-fp.fs -e bye
require sh-driver.fs
: OUT ( a u -- a u )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" 2.5 4 s>f f* f>s ."                OUT s\" 10\n"   compare 0= -> true }T
T{ s" 0.5 0.25 f+ 4 s>f f* f>s ."        OUT s\" 3\n"    compare 0= -> true }T
T{ s" 10 s>f 4 s>f f/ f."                OUT s\" 2.500000\n" compare 0= -> true }T
T{ s" -2.25 fabs 4 s>f f* f>s ."         OUT s\" 9\n"    compare 0= -> true }T
T{ s" 9 s>f fsqrt f>s ."                 OUT s\" 3\n"    compare 0= -> true }T
T{ s" 1.5 2.5 f< . 2.5 1.5 f< . 7.5 7.5 f= ."  OUT s\" -1\n0\n-1\n" compare 0= -> true }T
T{ s" 0.0 f0= . -3.5 f0< . 3.7 f>s ."    OUT s\" -1\n-1\n3\n" compare 0= -> true }T
T{ s" -0.125 f. 0.0 f. 3 s>f f."         OUT s\" -0.125000\n0.000000\n3.000000\n" compare 0= -> true }T
T{ s" : T {: a :} a s>f 2.5 f* f>s . ; 4 T"  OUT s\" 10\n" compare 0= -> true }T
\ checker: float typing — good certifies (-1), mixes reject (0)
: CHK ( a u -- a u )  0 CL !
   s" src/core/util.f" +F  s" src/core/checker.f" +F
   s" : HOOK CHECK dup . ; ' HOOK set-check " +B  +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" : G 1.5 2.5 f+ ; : B 1.5 2 + ; : B2 1 2 f+ ;"  CHK s\" -1\n0\n0\n" compare 0= -> true }T
\ fp-demo: Newton sqrt vs fsqrt prim — NSQRT certifies via NSTEP's recorded sig
: DEMO ( -- a u )  0 CL !
   s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
   s" test/demos/fp-demo.f" +F  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ DEMO s\" -1\n-1\n1.414213\n1.414213\n3\n-1\n" compare 0= -> true }T
