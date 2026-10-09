\ A local declared inside begin/until is gone after until on the trusted path
\ too, though Gforth's own locals still see it there.
trusted: D ( n -- n ) begin {: x :} x 1 - dup 0 = until x ;
3 D . cr
