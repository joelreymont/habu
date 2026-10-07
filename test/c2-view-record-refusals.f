\ Refusals through fresh evaluators in the native C2 image.
require lib/test.f
require lib/test/subject.f
require lib/c2-memory.f
require test/c2-view-record-defs.f
require test/c2-view-record-use-refusals.f

package C2-VIEW-RECORD-REFUSALS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: STATUS? ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

: SCOPE-ESCAPE? ( ptr u8 n -- bool )
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: out-u:len err-u:len rejected:bool :}
   rejected out-u LEN>N 0= and
   ERR err-u LEN>N S\" \"code\":\"E-C2-SCOPE-ESCAPE\"" CONTAINS? and ;

public

\ A refused STRUCTURE exits 70: the checker renders the declaration's refusal
\ before it throws the refusal's code.
: RUN ( -- )
   T-RESET
   s" a prior value use cannot become a scope" T-LABEL
   s" STRUCTURE c2mixfirst 2 FIELD value a FIELD source read-view<a,b,u8> ;STRUCTURE" 70 STATUS? TTRUE
   s" a later value use cannot consume a scope" T-LABEL
   s" STRUCTURE c2mixlast 2 FIELD source read-view<a,b,u8> FIELD value a ;STRUCTURE" 70 STATUS? TTRUE
   s" an element type parameter cannot become a scope" T-LABEL
   s" STRUCTURE c2mixelement 3 FIELD source read-view<a,b,c> FIELD extra read-view<c,b,u8> ;STRUCTURE" 70 STATUS? TTRUE
   s" a phantom element cannot become an owning field" T-LABEL
   s" STRUCTURE c2ownstype 3 FIELD source read-view<a,b,c> FIELD value c ;STRUCTURE" 70 STATUS? TTRUE
   s" a raw pointer cannot hide a borrowed field" T-LABEL
   s" STRUCTURE c2ptrview 2 FIELD source ptr read-view<a,b,u8> ;STRUCTURE" 70 STATUS? TTRUE
   s" a parametric field cannot recursively name its owner" T-LABEL
   s" STRUCTURE c2recursive 1 FIELD next ptr c2recursive<a> ;STRUCTURE" 70 STATUS? TTRUE
   s" nested record cannot escape an owner callback" T-LABEL
   s" -1 JSON-DIAGS ! : C2V-S ( read-view<p,l,u8> -- c2vinner<p,l> read-view<p,l,u8> ) dup 1 C2VINNER:MAKE swap ; : C2V-B ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) [: C2V-S ;] C2-MEM:WITH-READ swap drop ;" SCOPE-ESCAPE? TTRUE
   s" raw global storage cannot retain a borrowed record" T-LABEL
   s" variable C2V-SLOT : C2V-STORE ( c2vinner<p,q> -- ) C2V-SLOT ! ;" 70 STATUS? TTRUE
   s" raw pointer field access cannot expose a view" T-LABEL
   s" STRUCTURE c2vaddr 2 DERIVE addr FIELD source read-view<a,b,u8> ;STRUCTURE" 70 STATUS? TTRUE
   C2-VIEW-RECORD-USE-REFUSALS:CASES
   T-REPORT
   s" c2-view-record-refusals: ok" type cr ;

;package

C2-VIEW-RECORD-REFUSALS:RUN
