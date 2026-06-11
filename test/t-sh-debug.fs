\ t-sh-debug.fs — the standalone's `.s` data-stack inspector (forth.fs b.s): prints
\ the whole stack base..top, one signed decimal per line, non-destructively. The
\ primary debugging tool for standalone Forth (interleave `.s` to inspect state).
\ Run: gforth test/t-sh-debug.fs -e bye
require nf.fs
require tester.fs
\ 11 22 33 .s leaves the stack intact; then + + sums to 66.
: DBG-OUT ( -- a u )  s" : GO 11 22 33 .s + + . ; GO" NF-RUN  NFOUT 2@ ;
T{ DBG-OUT s\" 11\n22\n33\n66\n" compare 0= -> true }T
\ negative + empty-stack handling
: DBG2 ( -- a u )  s" : GO -5 .s drop ; GO" NF-RUN  NFOUT 2@ ;
T{ DBG2 s\" -5\n" compare 0= -> true }T
