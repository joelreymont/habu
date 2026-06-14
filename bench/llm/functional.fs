\ functional.fs — run reference-solution behavior tests in the native engine.
\ Run from repo root: gforth bench/llm/functional.fs -e bye
require ../../test/sh-driver.fs
0 CL !
s" src/core/util.f" +F  s" src/core/checker.f" +F  s" src/core/render.f" +F
s" : HOOK CHECK! ; ' HOOK set-check " +B
s" bench/llm/solutions.f" +F
s" bench/llm/tests.f" +F
CBUF CL @ NF-RUN  NFOUT 2@ type
