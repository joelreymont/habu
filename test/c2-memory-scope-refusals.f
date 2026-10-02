\ Checker boundaries for the public C2 owner and shared loan entries.

require lib/test.f
require lib/test/subject.f
require lib/adt/option.f
require lib/memory.f
require lib/c2-memory.f

package C2-MEMORY-SCOPE-REFUSALS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

\ Run the source in a child: the lengths of what it wrote to OUT and ERR, and
\ whether it exited with the expected status.
: EXITED? ( ptr u8 n n -- len len bool ) {: expected:n :}
   OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH ;

: STATUS? ( ptr u8 n n -- bool )
   EXITED? >r 2drop r> ;

\ Exited with the expected status, WHY in the diagnostic and SAID in the output.
: REPORTED? ( ptr u8 n n ptr u8 n ptr u8 n -- bool )
   {: expected:n why:ptr whyu:n said:ptr saidu:n :}
   expected EXITED? {: outu:len erru:len exited:bool :}
   exited ERR erru LEN>N why whyu CONTAINS? and
   OUT outu LEN>N said saidu CONTAINS? and ;

\ A typed global holds a quotation and never a scheme. An unclosed spelling
\ ends with its line, and the next line is the next statement's: the refusal is
\ caught so the definition after it runs.
: STORED ( -- )
   s" an ordinary quotation may be stored in a typed global" T-LABEL
   s" TYPED-VARIABLE C2-QUOTE-SLOT [ n -- n ]" 0 STATUS? TTRUE
   s" a scheme cannot be stored in a typed global" T-LABEL
   s" TYPED-VARIABLE C2-SCOPE-SLOT forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" in C2-SCOPE-SLOT: scheme in a stored type 'forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>'" s" " REPORTED? TTRUE
   s" a scheme cannot be stored behind a pointer" T-LABEL
   s" 2 TYPED-BUFFER C2-SCOPE-SLOTS ptr forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" in C2-SCOPE-SLOTS: scheme in a stored type 'ptr forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>'" s" " REPORTED? TTRUE
   s" a stored type ends at the end of its line" T-LABEL
   s\" NEWTYPE c2-line-pair 2\n' TYPED-VARIABLE catch C2-LINE-SLOT c2-line-pair<n,n\n: C2-LINE-NEXT ( -- n ) 4242 >r r> ;\nC2-LINE-NEXT . throw"
   70 s" in C2-LINE-SLOT: malformed type 'c2-line-pair<n,n'" s" 4242" REPORTED? TTRUE
   s" a stored quotation ends at the end of its line" T-LABEL
   s\" ' TYPED-VARIABLE catch C2-LINE-XT [ n -- n\n: C2-LINE-XT-NEXT ( -- n ) 4343 ;\nC2-LINE-XT-NEXT . throw"
   70 s" in C2-LINE-XT: malformed type '[ n -- n'" s" 4343" REPORTED? TTRUE ;

public

: RUN ( -- )
   T-RESET
   s" a monomorphic callback cannot claim the fresh scope" T-LABEL
   s" : C2-MUT-MONO ( [ mut-view<q,q,a,u8> -- mut-view<q,q,a,u8> | U -- U ] -- ) 1 MEM:BYTES-ALLOC-LEN swap C2-MEM:WITH-MUT ;" 70 STATUS? TTRUE
   s" an exterior copy of an inferred callback stays monomorphic" T-LABEL
   s" : C2-MUT-COPIED ( [ mut-view<q,q,a,u8> -- mut-view<q,q,a,u8> | U -- U ] -- ) dup 1 MEM:BYTES-ALLOC-LEN swap C2-MEM:WITH-MUT ;" 70 STATUS? TTRUE
   s" the authenticated root cannot be ticked in a checked word" T-LABEL
   s" : C2-SCOPE-TICK ( -- ) ['] C2-MEM:WITH-MUT drop ;" 70 STATUS? TTRUE
   s" the authenticated root cannot be ticked at the prompt" T-LABEL
   s" ' C2-MEM:WITH-MUT drop" 70 STATUS? TTRUE
   s" ordinary quotations remain executable with the same view operand" T-LABEL
   s" : C2-QUOTE-EXECUTE ( read-view<q,q,u8> [ read-view<q,q,u8> -- read-view<q,q,u8> | U -- U ] | U -- read-view<q,q,u8> | U ) execute ;" 0 STATUS? TTRUE
   s" a scheme cannot pass through execute" T-LABEL
   s" : C2-SCOPE-EXECUTE ( read-view<q,q,u8> forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> | U -- U ]> | U -- read-view<q,q,u8> | U ) execute ;" 70 STATUS? TTRUE
   s" ordinary quotations remain catchable with the same view operand" T-LABEL
   s" : C2-QUOTE-CATCH ( read-view<q,q,u8> [ read-view<q,q,u8> -- read-view<q,q,u8> | U -- U ] | U -- | U ) catch drop drop ;" 0 STATUS? TTRUE
   s" a scheme cannot pass through catch" T-LABEL
   s" : C2-SCOPE-CATCH ( read-view<q,q,u8> forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> | U -- U ]> | U -- | U ) catch drop drop ;" 70 STATUS? TTRUE
   s" ordinary quotations remain finally bodies with the same view operand" T-LABEL
   s" : C2-QUOTE-FINALLY ( read-view<q,q,u8> [ read-view<q,q,u8> -- read-view<q,q,u8> | U -- U ] | U -- | U ) [: ;] finally drop ;" 0 STATUS? TTRUE
   s" a scheme cannot pass through finally" T-LABEL
   s" : C2-SCOPE-FINALLY ( read-view<q,q,u8> forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> | U -- U ]> | U -- | U ) [: ;] finally drop ;" 70 STATUS? TTRUE
   s" an ordinary same-spelled word receives no owner scope" T-LABEL
   s" package C2-SCOPE-SHADOW TRUSTED: WITH-MUT ( R NUM:alloc-byte-len [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S | U ) C2-MEM:WITH-MUT ; : CALL ( -- n ) 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] WITH-MUT ; ;package" 70 STATUS? TTRUE
   s" a trusted declaration cannot publish a scheme" T-LABEL
   s" TRUSTED: C2-SCOPE-FORGE ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- ) drop ;" 76 STATUS? TTRUE
   s" a cast declaration cannot publish a scheme" T-LABEL
   s" CAST: C2-SCOPE-CAST ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- n )" 67 STATUS? TTRUE
   s" an ordinary quotation may pass through a data output" T-LABEL
   s" : C2-QUOTE-OUTPUT ( [ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ] -- [ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ] ) ;" 0 STATUS? TTRUE
   s" a scheme cannot be declared as an effect output" T-LABEL
   s" : C2-SCOPE-OUTPUT ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> ) ;" 70 STATUS? TTRUE
   s" an ordinary quotation may pass through a return-stack output" T-LABEL
   s" : C2-QUOTE-RETURN-OUTPUT ( [ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ] | U -- | U [ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ] ) >r ;" 0 STATUS? TTRUE
   s" a scheme cannot be declared on the return-stack output" T-LABEL
   s" : C2-SCOPE-RETURN-OUTPUT ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> | U -- | U forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> ) >r ;" 70 STATUS? TTRUE
   s" a scheme cannot be declared on the return-stack input" T-LABEL
   s" : C2-SCOPE-RETURN-INPUT ( | U forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | V -- V ]> -- | U ) r> drop ;" 70 STATUS? TTRUE
   s" an ordinary quotation may be a pointer referent" T-LABEL
   s" : C2-QUOTE-POINTER ( ptr [ n -- n ] -- ) drop ;" 0 STATUS? TTRUE
   s" a scheme cannot be placed behind a pointer" T-LABEL
   s" : C2-SCOPE-POINTER ( ptr forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- ) drop ;" 70 STATUS? TTRUE
   s" an ordinary quotation may be a family argument" T-LABEL
   s" NEWTYPE c2-quote-holder 1 : C2-QUOTE-FAMILY ( c2-quote-holder<[ n -- n ]> -- ) drop ;" 0 STATUS? TTRUE
   s" a family argument cannot contain a scheme" T-LABEL
   s" NEWTYPE c2-scope-holder 1 : C2-SCOPE-FAMILY ( c2-scope-holder<forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>> -- ) drop ;" 70 STATUS? TTRUE
   STORED
   s" an extra child view cannot leave its loan" T-LABEL
   s" : C2-CHILD-DIRECT ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup ;] C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" a nested aggregate cannot hide a child view" T-LABEL
   s" : C2-CHILD-AGG ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup OPTION:SOME swap ;] C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" a quotation cannot retain a child view" T-LABEL
   s" : C2-CHILD-QUOTE ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: 0 ['] C2-MEM:BYTE@ dup >r execute r> swap ;] C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" the return stack cannot retain a child view" T-LABEL
   s" : C2-CHILD-RETURN ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup >r ;] C2-MEM:WITH-READ r> drop ;" 70 STATUS? TTRUE
   s" an exterior local cannot retain a child view" T-LABEL
   s" : C2-CHILD-LOCAL ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup ;] C2-MEM:WITH-READ {: escaped restored :} escaped drop restored ;" 70 STATUS? TTRUE
   s" raw storage cannot retain a child view" T-LABEL
   s" variable C2-CHILD-RAW-SLOT : C2-CHILD-RAW ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup C2-CHILD-RAW-SLOT ! ;] C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" a generic raw store cannot hide a child view" T-LABEL
   s" variable C2-CHILD-GENERIC-SLOT : C2-CHILD-STORE ( a ptr a -- ) ! ; : C2-CHILD-GENERIC ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup C2-CHILD-GENERIC-SLOT C2-CHILD-STORE ;] C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" typed pointer storage cannot retain a child ceiling" T-LABEL
   s" : C2-CHILD-TYPED ( read-view<p,l,u8> ptr read-view<p,l,u8> -- ) ! ;" 70 STATUS? TTRUE
   s" a global cannot declare a child-scoped view" T-LABEL
   s" TYPED-VARIABLE C2-CHILD-SLOT read-view<p,l,u8>" 70 STATUS? TTRUE
   s" an ordinary helper cannot relabel the loan ceiling" T-LABEL
   s" : C2-CHILD-RELABEL ( read-view<p,q,u8> -- read-view<p,r,u8> ) ;" 70 STATUS? TTRUE
   s" a fixed existing-loan callback cannot claim a fresh child" T-LABEL
   s" : C2-CHILD-MONO ( read-view<p,q,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> | U -- U ] | U -- read-view<p,q,u8> | U ) C2-MEM:WITH-READ ;" 70 STATUS? TTRUE
   s" a same-spelled ordinary word receives no child authority" T-LABEL
   s" package C2-S TRUSTED: WITH-READ ( R ptr u8 n [ R ptr u8 n -- S ptr u8 n | U -- U ] | U -- S ptr u8 n | U ) C2-MEM:WITH-READ ; : X ( read-view<p,q,u8> -- n read-view<p,q,u8> ) [: 0 C2-MEM:BYTE@ ;] WITH-READ ; ;package" 70 STATUS? TTRUE
   T-REPORT
   s" c2-memory-scope-refusals: ok" type cr ;

;package

C2-MEMORY-SCOPE-REFUSALS:RUN
