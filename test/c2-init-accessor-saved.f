\ A fresh saved-image consumer: the declaration source is already in the image.
\ These checked calls resolve the captured public field effects at either tier.
require lib/test.f

package C2-INIT-ACCESSOR-SAVED

: SOURCE ( mut-view<c,d,e,init<g,C2IA:shelf<a,b>>> -- mut-view<c,d,e,init<g,C2IA:shelf<a,b>>> read-view<a,b,u8> )
   C2IA-SHELF:SOURCE@ ;

: SET-SOURCE ( mut-view<c,d,e,init<g,C2IA:shelf<a,b>>> read-view<a,b,u8> -- mut-view<c,d,e,init<g,C2IA:shelf<a,b>>> )
   C2IA-SHELF:SOURCE! ;

: NESTED ( mut-view<a,b,c,init<d,C2IA:nested>> -- mut-view<a,b,c,init<d,C2IA:nested>> C2IA:pair )
   C2IA-NESTED:PAIR@ ;

: SCALAR ( mut-view<a,b,c,init<d,C2IA:scalar>> n -- mut-view<a,b,c,init<d,C2IA:scalar>> )
   C2IA-SCALAR:VALUE! ;

: QUOTED ( mut-view<a,b,c,init<d,C2IA:shelf<a,b>>> -- mut-view<a,b,c,init<d,C2IA:shelf<a,b>>> n )
   [: C2IA-SHELF:COUNT@ ;] execute ;

;package

s" c2-init-accessor-saved: ok" type cr
