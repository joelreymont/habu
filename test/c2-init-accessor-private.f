\ The native compiler compiles the exact receiver and field representation
\ helpers for scoped and nested private initialized accessors.
1 set-tier

package C2-INIT-ACCESSOR-PRIVATE
private

STRUCTURE pair 0 FIELD left n FIELD right n ;STRUCTURE
STRUCTURE shelf 2 DERIVE init
   FIELD source read-view<a,b,u8>
   FIELD nested pair
;STRUCTURE

: READ-SOURCE ( mut-view<c,d,e,init<g,shelf<a,b>>> -- mut-view<c,d,e,init<g,shelf<a,b>>> read-view<a,b,u8> )
   SHELF-SOURCE@ ;

: WRITE-SOURCE ( mut-view<c,d,e,init<g,shelf<a,b>>> read-view<a,b,u8> -- mut-view<c,d,e,init<g,shelf<a,b>>> )
   SHELF-SOURCE! ;

: READ-NESTED ( mut-view<c,d,e,init<g,shelf<a,b>>> -- mut-view<c,d,e,init<g,shelf<a,b>>> pair )
   SHELF-NESTED@ ;

: WRITE-NESTED ( mut-view<c,d,e,init<g,shelf<a,b>>> pair -- mut-view<c,d,e,init<g,shelf<a,b>>> )
   SHELF-NESTED! ;

;package

s" c2-init-accessor-private: ok" type cr
