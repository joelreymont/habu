\ Generated initialized-record accessors load and type-check from source.
\ WITH-INIT is a separate operation; this fixture checks the declaration and
\ calls through their public effects without constructing a live initialized view.

require lib/test.f

package C2IA

public
STRUCTURE shelf 2 DERIVE init
   FIELD source read-view<a,b,u8>
   FIELD count n
;STRUCTURE

STRUCTURE pair 0 DERIVE addr init
   FIELD first n
   FIELD second n
;STRUCTURE

STRUCTURE nested 0 DERIVE init
   FIELD pair pair
   FIELD count n
;STRUCTURE

PRODUCT scalar 0 DERIVE init FIELD value n ;PRODUCT

: GET-SOURCE ( mut-view<c,d,e,init<g,shelf<a,b>>> -- mut-view<c,d,e,init<g,shelf<a,b>>> read-view<a,b,u8> ) C2IA-SHELF:SOURCE@ ;

: SET-SOURCE ( mut-view<c,d,e,init<g,shelf<a,b>>> read-view<a,b,u8> -- mut-view<c,d,e,init<g,shelf<a,b>>> ) C2IA-SHELF:SOURCE! ;

: GET-PAIR ( mut-view<a,b,c,init<d,nested>> -- mut-view<a,b,c,init<d,nested>> pair ) C2IA-NESTED:PAIR@ ;

: SET-PAIR ( mut-view<a,b,c,init<d,nested>> pair -- mut-view<a,b,c,init<d,nested>> ) C2IA-NESTED:PAIR! ;

: GET-SCALAR ( mut-view<a,b,c,init<d,scalar>> -- mut-view<a,b,c,init<d,scalar>> n ) C2IA-SCALAR:VALUE@ ;

: SET-SCALAR ( mut-view<a,b,c,init<d,scalar>> n -- mut-view<a,b,c,init<d,scalar>> ) C2IA-SCALAR:VALUE! ;

: QUOTED-COUNT ( mut-view<a,b,c,init<d,shelf<a,b>>> -- mut-view<a,b,c,init<d,shelf<a,b>>> n )
   [: C2IA-SHELF:COUNT@ ;] execute ;

;package

s" c2-init-accessors: ok" type cr
