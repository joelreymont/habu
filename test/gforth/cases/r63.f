\ A local declared inside begin/until is gone after until (checked).
: D ( n -- n ) begin {: x :} x 1 - dup 0 = until x ;
3 D . cr
