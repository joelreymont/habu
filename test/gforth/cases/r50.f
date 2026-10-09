\ A does> clause ticks its definer's bare name under `using`, beside a global
\ of that name. The clause is checked before its head (habu2.f
\ EM-COMPILE-PUBLISH-TRUSTED), so MK is not yet the pending definer and the
\ clause is refused E-USING-SHADOW-GLOBAL, as native refuses it.
: MK ( n -- ) drop ;
package P
public
: MK ( n -- ) drop ;
;package
package Q
public
using P
: MK ( n -- ) create , does> ( -- n ) ['] MK drop @ ;
;using
;package
5 Q:MK X
X . cr
