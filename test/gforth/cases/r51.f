\ r50 with a call of the bare name in the clause: refused
\ E-USING-SHADOW-GLOBAL in the clause's check, before the head is pending.
: MK ( n -- ) drop ;
package P
public
: MK ( n -- ) drop ;
;package
package Q
public
using P
: MK ( n -- ) create , does> ( -- n ) dup @ MK @ ;
;using
;package
5 Q:MK X
X . cr
