\ The saved effect carries both the byte ceiling and the value dependency.
require lib/c2-memory.f

package C2-INIT-WRAPPER
public

STRUCTURE c2isaved 0 DERIVE init FIELD left n FIELD right n ;STRUCTURE
STRUCTURE c2isource 2 DERIVE init
   FIELD source read-view<a,b,u8>
   FIELD mark n
;STRUCTURE

: APPLY ( R mut-view<p,l,a,u8> c2isaved forall<i inside [l,c2isaved],[ R mut-view<p,i,a,init<i,c2isaved>> -- S mut-view<p,i,a,init<i,c2isaved>> | U -- U ]> | U -- S mut-view<p,l,a,u8> | U )
   C2-MEM:WITH-INIT ;

;package
