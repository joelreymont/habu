\ validate.fs — prove every reference solution typechecks (CHECK! => -1).
\ Builds one native engine = util+checker+render + a CHECK! hook (prints each
\ definition's verdict) + solutions.f, runs it, and the verdict stream is checked
\ by run.sh. Run from the repo root: gforth bench/llm/validate.fs -e bye
require ../../test/sh-driver.fs
0 CL !
s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
s" : HOOK CHECK! dup . ; ' HOOK set-check " +B
s" bench/llm/solutions.f" +F
CBUF CL @ NF-RUN  NFOUT 2@ type
