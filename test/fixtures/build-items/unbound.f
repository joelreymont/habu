\ unbound.f - a definer whose built body drops from an empty stack, then
\ reads local 9, which no `{:` group bound, called on line 13. The read
\ takes the pin after the drop's failure: unbound.f:13.

package BITEM-UNBOUND
private

\ : NAME ( -- ) drop l9 ;
: MAKE-UNBOUND ( ^u8 n -- )
   EFFECT:EMPTY define-word  s" drop" EFFECT:CALL  9 EFFECT:LOCAL@
   postpone ; ;

s" BI-UNBOUND" MAKE-UNBOUND

;package
