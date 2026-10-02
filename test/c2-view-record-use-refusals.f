\ A phantom element type must stay separate from a value or fetched cell.
require lib/test.f
require lib/test/subject.f

package C2-VIEW-RECORD-USE-REFUSALS
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

public

: CASES ( -- )
   s" captured generic cell effects accept a one-cell element" T-LABEL
   s" : C2V-OK ( n read-view<p,q,n> -- read-view<p,q,n> ) C2-VIEW-RECORD-GENERICS:DROP-ELEM ; : C2V-OK-FETCH ( ptr n read-view<p,q,n> -- read-view<p,q,n> ) C2-VIEW-RECORD-GENERICS:FETCH-DROP ;" 0 STATUS? TTRUE
   s" captured generic cell effects reject a wide element" T-LABEL
   s" : C2V-BAD ( c2vpair read-view<p,q,c2vpair> -- read-view<p,q,c2vpair> ) C2-VIEW-RECORD-GENERICS:DROP-ELEM ;" 70 STATUS? TTRUE
   s" captured generic fetch effects reject a wide element" T-LABEL
   s" : C2V-BAD ( ptr c2vpair read-view<p,q,c2vpair> -- read-view<p,q,c2vpair> ) C2-VIEW-RECORD-GENERICS:FETCH-DROP ;" 70 STATUS? TTRUE
   s" a defer cannot retain a narrower cell-copy implementation" T-LABEL
   s" defer C2V-COPY-T ( ptr t ptr t -- ) : C2V-COPY-CELL ( ptr t ptr t -- ) swap @ swap ! ; : C2V-INSTALL ( -- ) [: C2V-COPY-CELL ;] is C2V-COPY-T ; C2V-INSTALL : C2V-CALL-ONE ( ptr n ptr n -- ) C2V-COPY-T ;" 70 STATUS? TTRUE
   s" a typed quotation cell cannot retain a narrower cell-copy implementation" T-LABEL
   s" TYPED-VARIABLE C2V-SLOT [ ptr t ptr t -- ] : C2V-COPY-CELL ( ptr t ptr t -- ) swap @ swap ! ; : C2V-INSTALL ( -- ) [: C2V-COPY-CELL ;] C2V-SLOT ! ; C2V-INSTALL : C2V-CALL-ONE ( ptr n ptr n -- ) C2V-SLOT @ execute ;" 70 STATUS? TTRUE
   s" a defer accepts a fully generic pointer-only implementation" T-LABEL
   s" defer C2V-PASS-T ( ptr t ptr t -- ) : C2V-PASS ( ptr t ptr t -- ) 2drop ; : C2V-INSTALL ( -- ) [: C2V-PASS ;] is C2V-PASS-T ; C2V-INSTALL : C2V-CALL-ONE ( ptr n ptr n -- ) C2V-PASS-T ;" 0 STATUS? TTRUE
   s" a typed quotation cell accepts a fully generic pointer-only implementation" T-LABEL
   s" TYPED-VARIABLE C2V-SLOT [ ptr t ptr t -- ] : C2V-PASS ( ptr t ptr t -- ) 2drop ; : C2V-INSTALL ( -- ) [: C2V-PASS ;] C2V-SLOT ! ; C2V-INSTALL : C2V-CALL-ONE ( ptr n ptr n -- ) C2V-SLOT @ execute ;" 0 STATUS? TTRUE
   s" a defer cannot retain a fixed-row implementation as a generic row" T-LABEL
   s" defer C2V-D ( R -- R ) : C2V-INC ( n -- n ) 1+ ; : C2V-INSTALL ( -- ) [: C2V-INC ;] is C2V-D ; C2V-INSTALL : C2V-EMPTY ( -- ) C2V-D ;" 70 STATUS? TTRUE
   s" a typed quotation cell cannot retain a fixed-row implementation as a generic row" T-LABEL
   s" TYPED-VARIABLE C2V-SLOT [ R -- R ] : C2V-INC ( n -- n ) 1+ ; : C2V-INSTALL ( -- ) [: C2V-INC ;] C2V-SLOT ! ; C2V-INSTALL : C2V-EMPTY ( -- ) C2V-SLOT @ execute ;" 70 STATUS? TTRUE
   s" generic row implementations still fit retained contracts" T-LABEL
   s" defer C2V-D ( R -- R ) : C2V-ID ( R -- R ) ; : C2V-INSTALL ( -- ) [: C2V-ID ;] is C2V-D ; C2V-INSTALL : C2V-EMPTY ( -- ) C2V-D ;" 0 STATUS? TTRUE
   s" a retained contract admits 33 independent record field variables" T-LABEL
   s" require test/c2-retained-contract-wide.f" 0 STATUS? TTRUE
   s" a pointer-only use keeps a wide view element phantom" T-LABEL
   s" : C2V-PTR-PASS ( ptr t read-view<p,q,t> -- ptr t read-view<p,q,t> ) ; : C2V-WIDE-PTR ( ptr c2vpair read-view<p,q,c2vpair> -- ptr c2vpair read-view<p,q,c2vpair> ) C2V-PTR-PASS ;" 0 STATUS? TTRUE
   s" generic view consumers retain their one-cell instances" T-LABEL
   s" : C2V-DROP-ELEM ( t read-view<p,q,t> -- read-view<p,q,t> ) swap drop ; : C2V-FETCH-DROP ( ptr t read-view<p,q,t> -- read-view<p,q,t> ) swap @ drop ;" 0 STATUS? TTRUE
   s" a physical input shared with a view element rejects a wide type" T-LABEL
   s" : C2V-DROP-ELEM ( t read-view<p,q,t> -- read-view<p,q,t> ) swap drop ; : C2V-BAD ( c2vpair read-view<p,q,c2vpair> -- read-view<p,q,c2vpair> ) C2V-DROP-ELEM ;" 70 STATUS? TTRUE
   s" a physical input shared with a view element rejects in either order" T-LABEL
   s" : C2V-DROP-ELEM-REV ( read-view<p,q,t> t -- read-view<p,q,t> ) drop ; : C2V-BAD ( read-view<p,q,c2vpair> c2vpair -- read-view<p,q,c2vpair> ) C2V-DROP-ELEM-REV ;" 70 STATUS? TTRUE
   s" a fetched cell shared with a view element rejects a wide type" T-LABEL
   s" : C2V-FETCH ( ptr t read-view<p,q,t> -- t read-view<p,q,t> ) swap @ swap ; : C2V-BAD ( ptr c2vpair read-view<p,q,c2vpair> -- c2vpair read-view<p,q,c2vpair> ) C2V-FETCH ;" 70 STATUS? TTRUE
   s" a discarded fetched cell still rejects a wide element type" T-LABEL
   s" : C2V-FETCH-DROP ( ptr t read-view<p,q,t> -- read-view<p,q,t> ) swap @ drop ; : C2V-BAD ( ptr c2vpair read-view<p,q,c2vpair> -- read-view<p,q,c2vpair> ) C2V-FETCH-DROP ;" 70 STATUS? TTRUE ;

;package
