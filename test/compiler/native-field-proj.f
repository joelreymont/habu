\ A generated address accessor must compile and run through the native tier.
require lib/test.f

1 set-tier
package NFP
public

STRUCTURE rec 0 DERIVE addr
   FIELD a n
   FIELD b n
;STRUCTURE

2 LAYOUT-BUFFER BUF rec

: STORE ( n n n -- )
   {: a:n b:n ix:n :}
   a b NFP-REC:MAKE ix BUF ! ;
: GET-A ( n -- n ) BUF NFP-REC:A @ ;
: GET-B ( n -- n ) BUF NFP-REC:B @ ;

;package
0 set-tier

T-RESET
10 20 0 NFP:STORE
30 40 1 NFP:STORE
0 NFP:GET-A 10 T=
0 NFP:GET-B 20 T=
1 NFP:GET-A 30 T=
1 NFP:GET-B 40 T=
T-REPORT
