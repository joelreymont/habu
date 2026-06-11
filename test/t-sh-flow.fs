\ t-sh-flow.fs — runtime behavior of control flow in standalone-COMPILED words:
\ DO/LOOP/I (incl. the documented do-while zero-trip), nested loops, ELSE both arms,
\ WHILE/REPEAT, AGAIN-less BEGIN/UNTIL. Output-compared, not just exit codes.
\ Run: gforth test/t-sh-flow.fs -e bye
require sh-driver.fs
: OUT ( a u -- a u )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ s" : GO 5 0 do i . loop ; GO"                   OUT s\" 0\n1\n2\n3\n4\n" compare 0= -> true }T
T{ s" : GO 0 0 do 7 . loop ; GO"                   OUT s\" 7\n"             compare 0= -> true }T  \ do-while: zero-trip runs once (documented)
T{ s" : IN 2 0 do i . loop ; : GO 2 0 do IN loop ; GO"  OUT s\" 0\n1\n0\n1\n" compare 0= -> true }T \ nested via a called word
T{ s" : GO 1 if 1 . else 2 . then ; GO"            OUT s\" 1\n"             compare 0= -> true }T
T{ s" : GO 0 if 1 . else 2 . then ; GO"            OUT s\" 2\n"             compare 0= -> true }T
T{ s" : GO 3 begin dup . 1- dup 0= until drop ; GO" OUT s\" 3\n2\n1\n"      compare 0= -> true }T
T{ s" : GO 3 begin dup 0 > while dup . 1- repeat drop ; GO" OUT s\" 3\n2\n1\n" compare 0= -> true }T
