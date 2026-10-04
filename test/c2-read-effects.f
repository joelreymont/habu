\ C2 shared-view foundation. Run through the native source load path:
\   bin/hb --load test/c2-read-effects.f
\ Each view is two cells (address and bound); no source in this suite creates one.
\ Checked words may transport views supplied by their callers, including through
\ a by-value sum, but may not invent scopes or erase their dependencies.

require lib/test.f
require lib/adt/option.f
require lib/test/subject.f
require src/core/quotation-storage.f

package C2-READ-EFFECTS
private

: KEEP ( read-view<p,q,u8> -- read-view<p,q,u8> ) ;
: COPY ( read-view<p,q,u8> -- read-view<p,q,u8> read-view<p,q,u8> ) dup ;
: LOCAL ( read-view<p,q,u8> -- read-view<p,q,u8> ) {: value :} value ;
: WRAP ( read-view<p,q,u8> -- option<read-view<p,q,u8>> ) OPTION:SOME ;
: UNWRAP ( option<read-view<p,q,u8>> -- read-view<p,q,u8> )
   MATCH option
      none OF 1 throw ENDOF
      some OF ENDOF
   ;MATCH ;
: GENERIC-COPY ( read-view<p,q,a> -- read-view<p,q,a> read-view<p,q,a> ) dup ;
: GENERIC-WRAP ( read-view<p,q,a> -- option<read-view<p,q,a>> ) OPTION:SOME ;
: GENERIC-UNWRAP ( option<read-view<p,q,a>> -- read-view<p,q,a> )
   MATCH option
      none OF 1 throw ENDOF
      some OF ENDOF
   ;MATCH ;
: NEST ( option<option<read-view<p,q,u8>>> -- option<option<read-view<p,q,u8>>> ) ;
STRUCTURE box 1 FIELD value a ;STRUCTURE
: RECORD ( box<read-view<p,q,u8>> -- box<read-view<p,q,u8>> ) ;

TYPED-VARIABLE N-SLOT n
TYPED-VARIABLE N-CALLBACK [ n -- n ]
: STORE-N ( n ptr n -- ) QUOTATION-STORAGE:STORE ;
: INSTALL-N ( [ n -- n ] -- ) N-CALLBACK ! ;
: APPLY-N ( read-view<p,q,u8> -- read-view<p,q,u8> )
   41 N-CALLBACK @ execute drop ;
: APPLY-R ( read-view<p,q,u8> -- read-view<p,q,u8> )
   >r 41 N-CALLBACK @ execute drop r> ;

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

\ Each rejected program runs in its own native evaluator process. A signal or
\ timeout fails the test; each case names its normal evaluator refusal status.
: REJECT ( ptr u8 n n -- bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   >r 2drop r> ;

public

: CHECK-STORAGE ( -- )
   41 N-SLOT STORE-N N-SLOT @ 41 T=
   [: 1+ ;] INSTALL-N 41 N-CALLBACK @ execute 42 T= ;

: CHECK-REFUSALS ( -- )
   s" source cannot replace the reserved view family identity" T-LABEL
   s" -1 C2-READ-FAM !" 70 REJECT TTRUE
   s" a view cannot be erased into a raw pointer" T-LABEL
   s" : C2-RAW ( read-view<p,q,u8> -- ptr u8 ) ;" 70 REJECT TTRUE
   s" the element type is invariant" T-LABEL
   s" : C2-WIDEN ( read-view<p,q,u8> -- read-view<p,q,n> ) ;" 70 REJECT TTRUE
   s" distinct scopes cannot be exchanged" T-LABEL
   s" : C2-SWAP-SCOPES ( read-view<p,q,u8> -- read-view<q,p,u8> ) ;" 70 REJECT TTRUE
   s" a checked word cannot mint an owner scope" T-LABEL
   s" : C2-FORGE ( -- read-view<p,q,u8> ) 0 0 ;" 70 REJECT TTRUE
   s" a trusted declaration cannot mint an owner scope" T-LABEL
   \ The bad stored signature is rendered before its code is thrown, so it exits 70.
   s" TRUSTED: C2-TRUST-FORGE ( -- read-view<p,q,u8> ) 0 0 ;" 70 REJECT TTRUE
   s" a rigid allocation identity is not a scope" T-LABEL
   s" : C2-REGION ( read-view<fresh-region-a,fresh-region-a,u8> -- ) drop ;" 70 REJECT TTRUE
   s" raw storage cannot hold a scoped view" T-LABEL
   s" variable C2-RAW-SLOT : C2-RAW-STORE ( read-view<p,q,u8> -- ) C2-RAW-SLOT ! ;" 70 REJECT TTRUE
   s" typed pointer storage cannot hold a scoped view" T-LABEL
   s" : C2-TYPED-STORE ( read-view<p,q,u8> ptr read-view<p,q,u8> -- ) ! ;" 70 REJECT TTRUE
   s" a generic store cannot hide a view dependency" T-LABEL
   s" : C2-GENERIC-STORE ( a ptr a -- ) ! ; : C2-GENERIC-BAD ( read-view<p,q,u8> ptr read-view<p,q,u8> -- ) C2-GENERIC-STORE ;" 70 REJECT TTRUE
   s" a typed global cannot hold a scoped view" T-LABEL
   s" TYPED-VARIABLE C2-TYPED-SLOT read-view<p,q,u8>" 70 REJECT TTRUE
   s" a dynamic buffer cannot hold a scoped view" T-LABEL
   s" DYNAMIC-BUFFER C2-DYNAMIC-SLOT read-view<p,q,u8>" 70 REJECT TTRUE
   s" a quotation dependency cannot enter typed state" T-LABEL
   s" TYPED-VARIABLE C2-CALLBACK [ read-view<p,q,u8> -- ]" 70 REJECT TTRUE
   s" xt! cannot retain a scoped quotation in raw state" T-LABEL
   s" variable C2-XT-SLOT : C2-XT-STORE ( [ read-view<p,q,u8> -- ] -- ) C2-XT-SLOT xt! ;" 70 REJECT TTRUE
   s" a generic xt! helper cannot retain a scoped quotation" T-LABEL
   s" variable C2-GXT-SLOT : C2-GXT-STORE ( [ R -- R ] -- ) C2-GXT-SLOT xt! ; : C2-GXT-BAD ( [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- ) C2-GXT-STORE ;" 70 REJECT TTRUE
   s" a second generic helper preserves the storage restriction" T-LABEL
   s" variable C2-HXT-SLOT : C2-HXT-STORE ( [ R -- R ] -- ) C2-HXT-SLOT xt! ; : C2-HXT-NEXT ( [ R -- R ] -- ) C2-HXT-STORE ; : C2-HXT-BAD ( [ read-view<p,q,u8> -- read-view<p,q,u8> ] -- ) C2-HXT-NEXT ;" 70 REJECT TTRUE
   s" typed quotation state cannot acquire a nested scope by unification" T-LABEL
   s" NEWTYPE c2-holder 1 TYPED-VARIABLE C2-Q-SLOT [ a -- a ] : C2-Q-STORE ( [ c2-holder<read-view<p,q,u8>> -- c2-holder<read-view<p,q,u8>> ] -- ) C2-Q-SLOT ! ;" 70 REJECT TTRUE
   s" defer state cannot acquire a scoped quotation" T-LABEL
   s" defer C2-DEFER ( a -- a ) NEWTYPE c2-dholder 1 : C2-DEFER-STORE ( [ c2-dholder<read-view<p,q,u8>> -- c2-dholder<read-view<p,q,u8>> ] -- ) is C2-DEFER ;" 70 REJECT TTRUE
   s" a phantom wrapper cannot erase a scoped dependency" T-LABEL
   s" NEWTYPE c2-phantom 1 CAST: C2-ERASE ( c2-phantom<read-view<p,q,u8>> -- n )" 67 REJECT TTRUE
   s" exclusive views remain inadmissible until loan accounting" T-LABEL
   s" : C2-MUT ( mut-view<p,q,u8> -- ) drop ;" 70 REJECT TTRUE
   s" direct borrowed schemas wait for scoped storage" T-LABEL
   \ A rendered declaration refusal exits 70; the CAST: refusal above renders none.
   s" SUMTYPE c2-schema 2 VARIANT some read-view<a,b,u8> ;VARIANT ;SUMTYPE" 70 REJECT TTRUE
   s" legacy value records cannot declare a scoped field" T-LABEL
   s" VALUE-RECORD c2-vrec value read-view<p,q,u8> END-VALUE-RECORD" 70 REJECT TTRUE
   s" legacy value records cannot hide a scoped field in a sum" T-LABEL
   s" VALUE-RECORD c2-nvrec value option<read-view<p,q,u8>> END-VALUE-RECORD" 70 REJECT TTRUE ;

;package

T-RESET
C2-READ-EFFECTS:CHECK-STORAGE
C2-READ-EFFECTS:CHECK-REFUSALS
T-REPORT
s" c2-read-effects: ok" type cr
