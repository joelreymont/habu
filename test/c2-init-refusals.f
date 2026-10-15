\ Initialization admits only a complete, non-owning record and a live byte view.
require src/lib/test.f
require src/lib/test/subject.f
require src/lib/c2-memory.f

STRUCTURE c2irec FIELD x n FIELD y n ;STRUCTURE
STRUCTURE c2iopen<a> FIELD value a ;STRUCTURE
STRUCTURE c2iempty ;STRUCTURE
STRUCTURE c2iexchanged<a,b> FIELD first a FIELD second b ;STRUCTURE

package C2-INIT-REFUSALS
public

: RUN ( -- )
   T-RESET
   s" raw storage cannot initialize a scoped view" T-LABEL
   s" : C2-INIT-RAW ( ptr u8 n c2irec -- ptr u8 n ) [: ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" a shared view cannot initialize a scoped view" T-LABEL
   s" : C2-INIT-SHARED ( read-view<p,q,u8> c2irec -- read-view<p,q,u8> ) [: ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" an open record field cannot initialize storage" T-LABEL
   s" : C2-INIT-OPEN ( mut-view<p,l,a,u8> c2iopen<t> -- mut-view<p,l,a,u8> ) [: ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" a record containing unique authority cannot initialize storage" T-LABEL
   s" : C2-INIT-UNIQUE ( mut-view<p,l,a,u8> c2iopen<mut-view<p,l,b,u8>> -- mut-view<p,l,a,u8> ) [: ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" a stale record cannot initialize storage" T-LABEL
   s" : C2-INIT-STALE-THROW ( c2irec n -- c2irec n ) swap 1 throw ; : C2-INIT-STALE ( mut-view<p,l,a,u8> c2irec n -- mut-view<p,l,a,u8> ) ['] C2-INIT-STALE-THROW catch drop drop [: ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" opposing field widths cannot cancel to a fixed schema" T-LABEL
   s" : C2-INIT-SWAPPED ( mut-view<p,l,a,u8> c2iexchanged<c2irec,c2iempty> -- mut-view<p,l,a,u8> ) [: ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" a callback with an incompatible initialized element is refused" T-LABEL
   s" : C2-INIT-BAD-CALLBACK ( mut-view<p,l,a,u8> c2irec -- mut-view<p,l,a,u8> ) [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   s" the fresh initialization scope cannot escape as the parent scope" T-LABEL
   s" : C2-INIT-ESCAPE ( mut-view<p,l,a,u8> c2irec forall<i inside [l,c2irec],[ mut-view<p,i,a,init<i,c2irec>> -- read-view<p,i,u8> mut-view<p,i,a,init<i,c2irec>> ]> -- read-view<p,l,u8> mut-view<p,l,a,u8> ) C2-MEM:WITH-INIT ;" 70 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-init-refusals: ok" type cr ;

;package

C2-INIT-REFUSALS:RUN
