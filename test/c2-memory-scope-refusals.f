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

\ The layout definers read their type by the same rule: a scheme is refused
\ whole and the rest of its line is the next statement's, a family closed over
\ a quotation is stored, a pointer type is refused whole, and no type at all is
\ a named refusal in every storage definer.
: LAYOUT-STORED ( -- )
   s" a scheme cannot fill a layout buffer" T-LABEL
   s" 4 LAYOUT-BUFFER C2-SCOPE-ROWS forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" in C2-SCOPE-ROWS: scheme in a stored type 'forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>'" s" " REPORTED? TTRUE
   s" a scheme cannot fill a deferred layout column" T-LABEL
   s" DEFER-LAYOUT-BUFFER C2-SCOPE-COLUMN forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" in C2-SCOPE-COLUMN: scheme in a stored type 'forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>'" s" " REPORTED? TTRUE
   s" a refused layout type leaves the rest of its line to the next statement" T-LABEL
   s" 4 ' LAYOUT-BUFFER catch C2-REST-ROWS forall<p,[ n -- n ]> nip 4444 . throw"
   70 s" in C2-REST-ROWS: scheme in a stored type 'forall<p,[ n -- n ]>'" s" 4444" REPORTED? TTRUE
   s" layout storage holds a family closed over a quotation" T-LABEL
   s\" STRUCTURE c2hook 1 DERIVE addr FIELD fn a ;STRUCTURE\n2 LAYOUT-BUFFER C2-HOOKS c2hook<[ n -- n ]>\n: C2-H ( -- n ) [: 1 + ;] C2HOOK:MAKE 1 C2-HOOKS ! 41 1 C2-HOOKS C2HOOK:FN @ execute ;\nC2-H ."
   0 s" " s" 42" REPORTED? TTRUE
   s\" STRUCTURE c2hook 1 DERIVE addr FIELD fn a ;STRUCTURE\nDEFER-LAYOUT-BUFFER C2-HOOKC c2hook<[ n -- n ]>\n: C2-H ( -- n ) 2 C2-HOOKC-BIND [: 2 + ;] C2HOOK:MAKE 1 C2-HOOKC ! 41 1 C2-HOOKC C2HOOK:FN @ execute ;\nC2-H ."
   0 s" " s" 43" REPORTED? TTRUE
   s" a layout pointer type is refused whole" T-LABEL
   s" 4 LAYOUT-BUFFER C2-PTR-ROWS ptr n"
   70 s" in C2-PTR-ROWS: type this definer cannot store 'ptr n'" s" " REPORTED? TTRUE
   s" DEFER-LAYOUT-BUFFER C2-PTR-COLUMN ptr n"
   70 s" in C2-PTR-COLUMN: type this definer cannot store 'ptr n'" s" " REPORTED? TTRUE
   s" a missing stored type is a named refusal" T-LABEL
   s" 4 LAYOUT-BUFFER C2-NO-ROWS-TYPE"
   70 s" in C2-NO-ROWS-TYPE: no type for 'C2-NO-ROWS-TYPE'" s" " REPORTED? TTRUE
   s" DEFER-LAYOUT-BUFFER C2-NO-COLUMN-TYPE"
   70 s" in C2-NO-COLUMN-TYPE: no type for 'C2-NO-COLUMN-TYPE'" s" " REPORTED? TTRUE
   s" TYPED-VARIABLE C2-NO-SLOT-TYPE"
   70 s" in C2-NO-SLOT-TYPE: no type for 'C2-NO-SLOT-TYPE'" s" " REPORTED? TTRUE
   s" 4 TYPED-BUFFER C2-NO-SLOTS-TYPE"
   70 s" in C2-NO-SLOTS-TYPE: no type for 'C2-NO-SLOTS-TYPE'" s" " REPORTED? TTRUE
   s" DYNAMIC-BUFFER C2-NO-DYN-TYPE"
   70 s" in C2-NO-DYN-TYPE: no type for 'C2-NO-DYN-TYPE'" s" " REPORTED? TTRUE ;

\ Every storage definer reads its name on its own line and the first token of
\ its type on the name's. A name or type on the next line is missing there,
\ refused by name at the definer or the name, and that line is the next
\ statement's: the refusal is caught so the statement runs.
: LINE-STORED ( -- )
   s" a stored type on the next line is no type" T-LABEL
   s\" TYPED-VARIABLE C2-NL-N\nn"
   70 s" in C2-NL-N: no type for 'C2-NL-N'" s" " REPORTED? TTRUE
   s\" ' TYPED-VARIABLE catch C2-NL-SLOT\n4501 . throw"
   70 s" in C2-NL-SLOT: no type for 'C2-NL-SLOT'" s" 4501" REPORTED? TTRUE
   s\" 4 ' TYPED-BUFFER catch C2-NL-SLOTS\nnip 4502 . throw"
   70 s" in C2-NL-SLOTS: no type for 'C2-NL-SLOTS'" s" 4502" REPORTED? TTRUE
   s\" ' DYNAMIC-BUFFER catch C2-NL-DYN\n4503 . throw"
   70 s" in C2-NL-DYN: no type for 'C2-NL-DYN'" s" 4503" REPORTED? TTRUE
   s\" 4 ' LAYOUT-BUFFER catch C2-NL-ROWS\nnip 4504 . throw"
   70 s" in C2-NL-ROWS: no type for 'C2-NL-ROWS'" s" 4504" REPORTED? TTRUE
   s\" ' DEFER-LAYOUT-BUFFER catch C2-NL-COLUMN\n4505 . throw"
   70 s" in C2-NL-COLUMN: no type for 'C2-NL-COLUMN'" s" 4505" REPORTED? TTRUE
   s" a storage name on the next line is no name" T-LABEL
   s\" TYPED-VARIABLE\nC2-NL-NAMED n"
   70 s" in TYPED-VARIABLE: no name for 'TYPED-VARIABLE'" s" " REPORTED? TTRUE
   s\" ' TYPED-VARIABLE catch\n4511 . throw"
   70 s" in TYPED-VARIABLE: no name for 'TYPED-VARIABLE'" s" 4511" REPORTED? TTRUE
   s\" 4 ' TYPED-BUFFER catch\nnip 4512 . throw"
   70 s" in TYPED-BUFFER: no name for 'TYPED-BUFFER'" s" 4512" REPORTED? TTRUE
   s\" ' DYNAMIC-BUFFER catch\n4513 . throw"
   70 s" in DYNAMIC-BUFFER: no name for 'DYNAMIC-BUFFER'" s" 4513" REPORTED? TTRUE
   s\" 4 ' LAYOUT-BUFFER catch\nnip 4514 . throw"
   70 s" in LAYOUT-BUFFER: no name for 'LAYOUT-BUFFER'" s" 4514" REPORTED? TTRUE
   s\" ' DEFER-LAYOUT-BUFFER catch\n4515 . throw"
   70 s" in DEFER-LAYOUT-BUFFER: no name for 'DEFER-LAYOUT-BUFFER'" s" 4515" REPORTED? TTRUE ;

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
   LAYOUT-STORED
   LINE-STORED
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
