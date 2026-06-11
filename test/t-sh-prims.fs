\ t-sh-prims.fs — the standalone checker's expanded prim table + generic signature
\ parser (PARSE-SIG). Each definition's verdict: -1 well-typed, 0 type error, 1
\ uncheckable. Exercises stack shuffles, arithmetic, comparisons, and numeric
\ literals — all driven by the data-table FIND-SIG, not a giant dispatch word.
\ Run: gforth test/t-sh-prims.fs -e bye
require sh-driver.fs
: PRIMS-OUT ( -- a u )
   0 CL !
   s" selfhost/util.fs"    slurp-file +B   s"  " +B
   s" selfhost/checker.fs"   slurp-file +B   s"  " +B
   s" selfhost/prim-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
\ T1 swap drop=-1, T2 over nip=-1, T3 dup 1+ +=-1, T4 dup 0= +=0 (type error),
\ T5 0= 1+=0, T6 5 dup +=-1 (numeric literal checkable), T7 ... if ... then=1, then 42.
T{ PRIMS-OUT s\" -1\n-1\n-1\n0\n0\n-1\n1\n42\n" compare 0= -> true }T
