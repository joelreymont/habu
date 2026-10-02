\ Initialization admits only a complete, non-owning record and a live byte view.
require lib/test.f
require lib/test/subject.f
require lib/c2-memory.f

STRUCTURE c2irec 0 FIELD x n FIELD y n ;STRUCTURE
STRUCTURE c2iopen 1 FIELD value a ;STRUCTURE
STRUCTURE c2iempty 0 ;STRUCTURE
STRUCTURE c2iexchanged 2 FIELD first a FIELD second b ;STRUCTURE

package C2-INIT-REFUSALS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: REJECT ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public

: RUN ( -- )
   T-RESET
   s" raw storage cannot initialize a scoped view" T-LABEL
   s" : C2-INIT-RAW ( ptr u8 n c2irec -- ptr u8 n ) [: ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" a shared view cannot initialize a scoped view" T-LABEL
   s" : C2-INIT-SHARED ( read-view<p,q,u8> c2irec -- read-view<p,q,u8> ) [: ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" an open record field cannot initialize storage" T-LABEL
   s" : C2-INIT-OPEN ( mut-view<p,l,a,u8> c2iopen<t> -- mut-view<p,l,a,u8> ) [: ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" a record containing unique authority cannot initialize storage" T-LABEL
   s" : C2-INIT-UNIQUE ( mut-view<p,l,a,u8> c2iopen<mut-view<p,l,b,u8>> -- mut-view<p,l,a,u8> ) [: ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" a stale record cannot initialize storage" T-LABEL
   s" : C2-INIT-STALE-THROW ( c2irec n -- c2irec n ) swap 1 throw ; : C2-INIT-STALE ( mut-view<p,l,a,u8> c2irec n -- mut-view<p,l,a,u8> ) ['] C2-INIT-STALE-THROW catch drop drop [: ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" opposing field widths cannot cancel to a fixed schema" T-LABEL
   s" : C2-INIT-SWAPPED ( mut-view<p,l,a,u8> c2iexchanged<c2irec,c2iempty> -- mut-view<p,l,a,u8> ) [: ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" a callback with an incompatible initialized element is refused" T-LABEL
   s" : C2-INIT-BAD-CALLBACK ( mut-view<p,l,a,u8> c2irec -- mut-view<p,l,a,u8> ) [: 0 C2-MEM:MUT-BYTE@ ;] C2-MEM:WITH-INIT ;" REJECT TTRUE
   s" the fresh initialization scope cannot escape as the parent scope" T-LABEL
   s" : C2-INIT-ESCAPE ( mut-view<p,l,a,u8> c2irec forall<i inside [l,c2irec],[ mut-view<p,i,a,init<i,c2irec>> -- read-view<p,i,u8> mut-view<p,i,a,init<i,c2irec>> ]> -- read-view<p,l,u8> mut-view<p,l,a,u8> ) C2-MEM:WITH-INIT ;" REJECT TTRUE
   T-REPORT
   s" c2-init-refusals: ok" type cr ;

;package

C2-INIT-REFUSALS:RUN
