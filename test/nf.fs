\ nf.fs — native-Forth (Part F) build/run/capture harness. Reusable by tests and
\ from the REPL. Builds a standalone Mach-O from a habu source line, runs it, and
\ captures stdout. Use NFX interactively; NF-RUN + NF= in T{ … }T tests.

require ../src/cg/forth.fs

2variable NFOUT
: NF-RUN ( src-a src-u -- )            \ build native Forth on src, run, capture stdout
   s" /tmp/nf-bin" FORTH-EXE
   s" /tmp/nf-bin > /tmp/nf-out 2>/dev/null" system
   s" /tmp/nf-out" slurp-file NFOUT 2! ;
: NF= ( a u -- f )  NFOUT 2@ compare 0= ;

: NFX ( src-a src-u -- )               \ build+run+show (interactive: `s" 5 SQ ." NFX`)
   2dup cr ." nf< " type  NF-RUN  cr ." nf> " NFOUT 2@ type ;

: NF-REPL ( src-a src-u -- )           \ build a stdin REPL, pipe src in, capture stdout
   s" /tmp/nf-src" w/o create-file throw {: fh :}
   fh write-file throw  fh close-file throw
   s" /tmp/nf-repl" FORTH-REPL-EXE
   s" /tmp/nf-repl < /tmp/nf-src > /tmp/nf-out 2>/dev/null" system
   s" /tmp/nf-out" slurp-file NFOUT 2! ;
