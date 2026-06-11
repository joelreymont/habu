\ t-sh-prof.fs — the in-binary sampling profiler: `n prof-on` + busy loop ->
\ prof-report names the hot word with a plausible count; the sample-limit path
\ dumps and exits 99 (the hang diagnoser). Run: gforth test/t-sh-prof.fs -e bye
require sh-driver.fs
: OUT ( a u -- a u )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" : BUSY 80000000 begin 1- dup dup * drop dup 0= until drop ; : GO 100000 prof-on BUSY prof-report ; GO"
   OUT  drop 5 s" BUSY " compare 0=  -> true }T    \ report starts "BUSY <count>"
: RC-OF ( a u -- code )  s" /tmp/nf-pr-bin" FORTH-EXE
   s" /tmp/nf-pr-bin >/dev/null 2>/dev/null; echo $? > /tmp/nf-pr-rc" system
   s" /tmp/nf-pr-rc" slurp-file s>number? 2drop ;
T{ s" : SPIN begin 1 drop again ; : GO 200 prof-on SPIN ; GO"  RC-OF -> 99 }T  \ limit -> dump+exit(99)
