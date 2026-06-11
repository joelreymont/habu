\ t-sh-catch.fs — catch/throw across the new BL call frames: every word saves/restores
\ x30 on the machine stack, and throw unwinds by restoring sp wholesale from the handler
\ frame, abandoning the intermediate frames. Prove: deep unwind (3 frames), normal path
\ (exc 0), data-stack restore below the handler, and uncaught throw = exit(exc).
\ Run: gforth test/t-sh-catch.fs -e bye
require sh-driver.fs
: OUT ( a u -- a u )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;
: RC-OF ( a u -- code )  s" /tmp/nf-cat-bin" FORTH-EXE
   s" /tmp/nf-cat-bin >/dev/null 2>/dev/null; echo $? > /tmp/nf-cat-rc" system
   s" /tmp/nf-cat-rc" slurp-file s>number? 2drop ;
T{ s" : BOOM 7 throw ; : TOP ['] BOOM catch . ; TOP"             OUT s\" 7\n"  compare 0= -> true }T
T{ s" : OK ; : TOP ['] OK catch . ; TOP"                          OUT s\" 0\n"  compare 0= -> true }T
T{ s" : C3 9 throw ; : C2 C3 ; : C1 C2 ; : TOP ['] C1 catch . ; TOP" OUT s\" 9\n" compare 0= -> true }T
T{ s" : B 5 throw ; : TOP 42 ['] B catch drop . ; TOP"            OUT s\" 42\n" compare 0= -> true }T
T{ s" : B 3 throw ; : TOP ['] B catch . 42 . ; TOP"               OUT s\" 3\n42\n" compare 0= -> true }T
T{ s" : B 8 throw ; B"                                            RC-OF -> 8 }T   \ uncaught -> exit(exc)
