\ A saved callback with two binders specializes to the live scope and region
\ while WITH-INIT consumes a concrete quotation inside the owner path.
require lib/c2-owner.f
require lib/memory.f

package C2-FORALL-INPUT
public

STRUCTURE state 1 DERIVE init FIELD source read-view<a,a,u8> ;STRUCTURE
STRUCTURE control 5
   FIELD owner C2-MEM:owner<a,b,c>
   FIELD cache mut-view<a,d,e,init<d,state<a>>>
;STRUCTURE

TRUSTED: SEED ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> state<p> ) 0 0 ;

: BODY
   ( R C2-MEM:owner<p,i,a> [ R control<p,i,a,j,b> -- S control<p,i,a,j,b> | U -- U ] mut-view<p,j,b,init<j,state<p>>> | U -- S C2-MEM:owner<p,i,a> mut-view<p,j,b,init<j,state<p>>> | U )
   swap {: cb :} C2--FORALL--INPUT-CONTROL:MAKE cb execute
   C2--FORALL--INPUT-CONTROL:UNMAKE ;

: START
   ( R C2-MEM:owner<p,i,a> forall-region<b,forall<j inside [p,state<p>], [ R control<p,i,a,j,b> -- S control<p,i,a,j,b> | U -- U ]>> | U -- S C2-MEM:owner<p,i,a> | U )
   {: cb :}
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   swap cb rot SEED [: BODY ;] C2-MEM:WITH-INIT
   C2-MEM:PUBLISH drop ;

: TAKE-CHILD
   ( R C2-MEM:owner<p,i,a> mut-view<p,p,b,u8> forall<j inside [p,state<p>], [ R control<p,i,a,j,b> -- S control<p,i,a,j,b> | U -- U ]> | U -- S C2-MEM:owner<p,i,a> mut-view<p,p,b,u8> | U )
   drop ;

: PARTIAL
   ( R C2-MEM:owner<p,i,a> forall-region<b,forall<j inside [p,state<p>], [ R control<p,i,a,j,b> -- S control<p,i,a,j,b> | U -- U ]>> | U -- S C2-MEM:owner<p,i,a> | U )
   {: cb :}
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   cb TAKE-CHILD
   C2-MEM:PUBLISH drop ;

: TAKE-SCOPE
   ( read-view<p,p,u8> [ read-view<p,p,u8> -- read-view<p,p,u8> ] -- read-view<p,p,u8> )
   drop ;

: GENERIC-SCOPE
   ( read-view<p,p,u8> forall<q,[ read-view<q,q,u8> -- read-view<q,q,u8> ]> -- read-view<p,p,u8> )
   TAKE-SCOPE ;

: TAKE-REGION
   ( mut-view<p,p,a,u8> [ mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ] -- mut-view<p,p,a,u8> )
   drop ;

: GENERIC-REGION
   ( mut-view<p,p,a,u8> forall-region<b,[ mut-view<p,p,b,u8> -- mut-view<p,p,b,u8> ]> -- mut-view<p,p,a,u8> )
   TAKE-REGION ;

\ The surplus callback specializes before the loan. Only an extra parent view
\ may leave that loan; the matched child-view escape is refused below.
: LOAN-PARENT-EXTRA
   ( read-view<p,p,u8> forall<q,[ read-view<q,q,u8> -- read-view<q,q,u8> ]> forall<j inside p,[ read-view<p,j,u8> -- read-view<p,p,u8> read-view<p,j,u8> ]> -- read-view<p,p,u8> read-view<p,p,u8> )
   {: loan :} GENERIC-SCOPE loan C2-MEM:WITH-READ ;

;package

s" c2-forall-input: ok" type cr
