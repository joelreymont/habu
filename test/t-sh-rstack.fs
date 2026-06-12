\ t-sh-rstack.fs — the engine's user return stack (>R R> R@): a data-region
\ stack at [x20+RSTK-OFF] with depth at [x20+RSP-CELL] (the DO/LOOP frame-stack
\ pattern — word frames on the machine stack would unbalance the epilogue).
\ Run: gforth test/t-sh-rstack.fs -e bye
require sh-driver.fs

: RS-OUT ( a u -- a2 u2 )  0 CL !  +B  CBUF CL @ NF-RUN  NFOUT 2@ ;

T{ s" : T 1 2 >r 10 + r> + ; T ."             RS-OUT  s\" 13\n" compare 0= -> true }T
T{ s" : T 5 >r r@ r@ * r> + ; T ."            RS-OUT  s\" 30\n" compare 0= -> true }T
T{ s" : U >r 2 * r> + ; : T 10 3 U ; T ."     RS-OUT  s\" 23\n" compare 0= -> true }T
T{ s" : T 0 5 0 do i >r loop 5 0 do r> + loop ; T ."  RS-OUT  s\" 10\n" compare 0= -> true }T
T{ s" : T 7 >r 1 if r@ else 0 then r> drop ; T ."     RS-OUT  s\" 7\n"  compare 0= -> true }T
