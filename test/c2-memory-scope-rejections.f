\ Checker boundaries for the public C2 owner and shared loan entries.

require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/subject.f
require src/lib/test/ending.f
require src/lib/adt/option.f
require src/lib/memory.f
require src/lib/c2-memory.f
require src/lib/process.f

package C2-MEMORY-SCOPE-REJECTIONS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

\ Run the source in a child: the lengths of what it wrote to OUT and ERR, and
\ whether it exited with the expected status.
: EXITED? ( str n -- len len bool )
   {: src:str expected:n :}
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   outu erru
   oc MATCH PROCESS:ending
      exited OF expected = ENDOF
      signaled OF drop false ENDOF
      timeout OF
         src OUT outu LEN>N >str ERR erru LEN>N >str T-TIMED-OUT
      ENDOF
   ;MATCH ;

\ Exited with the expected status, WHY in the diagnostic and SAID in the output.
: REPORTED? ( str n str str -- bool )
   {: expected:n why:str said:str :}
   expected EXITED? {: outu:len erru:len exited:bool :}
   exited ERR erru LEN>N >str why STR:CONTAINS? and
   OUT outu LEN>N >str said STR:CONTAINS? and ;

\ Exited with the expected status, FIELDS and HELP in its JSON rejection.
: JSON-REPORTED? ( str n str str -- bool )
   {: expected:n fields:str help:str :}
   expected EXITED? {: outu:len erru:len exited:bool :}
   exited ERR erru LEN>N >str fields STR:CONTAINS? and
   ERR erru LEN>N >str help STR:CONTAINS? and ;

: STORED ( -- )
   s" an ordinary quotation may be stored in a typed global" T-LABEL
   s" TYPED-VARIABLE C2-QUOTE-SLOT [ n -- n ]" 0 SUBJECT:EXITED? TTRUE
   s" a scheme cannot be stored in a typed global" T-LABEL
   s" TYPED-VARIABLE C2-SCOPE-SLOT forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" error[E-BAD-STORAGE]: the type of `C2-SCOPE-SLOT` holds a scheme" s" " REPORTED? TTRUE
   s" a scheme cannot be stored behind a pointer" T-LABEL
   s" 2 TYPED-BUFFER C2-SCOPE-SLOTS ^forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" error[E-BAD-STORAGE]: the type of `C2-SCOPE-SLOTS` holds a scheme" s" " REPORTED? TTRUE ;

: LAYOUT-STORED ( -- )
   s" a scheme cannot fill a layout buffer" T-LABEL
   s" 4 LAYOUT-BUFFER C2-SCOPE-ROWS forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" error[E-BAD-STORAGE]: the type of `C2-SCOPE-ROWS` holds a scheme" s" " REPORTED? TTRUE
   s" a scheme cannot fill a deferred layout column" T-LABEL
   s" DEFER-LAYOUT-BUFFER C2-SCOPE-COLUMN forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>"
   70 s" error[E-BAD-STORAGE]: the type of `C2-SCOPE-COLUMN` holds a scheme" s" " REPORTED? TTRUE
   s" layout storage holds a family closed over a quotation" T-LABEL
   s\" STRUCTURE c2hook<a> DERIVE addr FIELD fn a ;STRUCTURE\n2 LAYOUT-BUFFER C2-HOOKS c2hook<[ n -- n ]>\n: C2-H ( -- n ) [: 1 + ;] C2HOOK:MAKE 1 C2-HOOKS ! 41 1 C2-HOOKS C2HOOK:FN @ execute ;\nC2-H ."
   0 s" " s" 42" REPORTED? TTRUE
   s\" STRUCTURE c2hook<a> DERIVE addr FIELD fn a ;STRUCTURE\nDEFER-LAYOUT-BUFFER C2-HOOKC c2hook<[ n -- n ]>\n: C2-H ( -- n ) 2 C2-HOOKC-BIND [: 2 + ;] C2HOOK:MAKE 1 C2-HOOKC ! 41 1 C2-HOOKC C2HOOK:FN @ execute ;\nC2-H ."
   0 s" " s" 43" REPORTED? TTRUE
   s" an xt in a layout family's argument is an xt in a stored type" T-LABEL
   s\" true REJECTION:JSON !\nSTRUCTURE c2hook<a> DERIVE addr FIELD fn a ;STRUCTURE\n4 LAYOUT-BUFFER C2-XT-HOOKS c2hook<xt>"
   70 s\" \"code\":\"E-BAD-STORAGE\",\"message\":\"the type of `C2-XT-HOOKS` holds an `xt`, which no storage holds"
   s\" \"help\":\"declare `C2-XT-HOOKS` with a quotation type such as `[ n -- n ]` in place of `xt`" JSON-REPORTED? TTRUE
   s" a layout pointer type is rejected whole" T-LABEL
   s" 4 LAYOUT-BUFFER C2-PTR-ROWS ^n"
   70 s" error[E-BAD-STORAGE]: the type of `C2-PTR-ROWS` is no type a layout buffer can lay out" s" " REPORTED? TTRUE
   s" DEFER-LAYOUT-BUFFER C2-PTR-COLUMN ^n"
   70 s" error[E-BAD-STORAGE]: the type of `C2-PTR-COLUMN` is no type a layout buffer can lay out" s" " REPORTED? TTRUE
   s" an xt layout buffer's help is the layout help" T-LABEL
   s\" true REJECTION:JSON !\n4 LAYOUT-BUFFER C2-XT-ROWS xt"
   70 s\" \"code\":\"E-BAD-STORAGE\",\"message\":\"the type of `C2-XT-ROWS` is no type a layout buffer can lay out"
   s\" \"help\":\"give `C2-XT-ROWS` a declared family: a NEWTYPE or DEFTYPE with no arguments" JSON-REPORTED? TTRUE
   s" a missing stored type is a named rejection" T-LABEL
   s" 4 LAYOUT-BUFFER C2-NO-ROWS-TYPE"
   70 s" error[E-BAD-STORAGE]: `C2-NO-ROWS-TYPE` has no type on its line" s" " REPORTED? TTRUE
   s" DEFER-LAYOUT-BUFFER C2-NO-COLUMN-TYPE"
   70 s" error[E-BAD-STORAGE]: `C2-NO-COLUMN-TYPE` has no type on its line" s" " REPORTED? TTRUE
   s" TYPED-VARIABLE C2-NO-SLOT-TYPE"
   70 s" error[E-BAD-STORAGE]: `C2-NO-SLOT-TYPE` has no type on its line" s" " REPORTED? TTRUE
   s" 4 TYPED-BUFFER C2-NO-SLOTS-TYPE"
   70 s" error[E-BAD-STORAGE]: `C2-NO-SLOTS-TYPE` has no type on its line" s" " REPORTED? TTRUE
   s" DYNAMIC-BUFFER C2-NO-DYN-TYPE"
   70 s" error[E-BAD-STORAGE]: `C2-NO-DYN-TYPE` has no type on its line" s" " REPORTED? TTRUE ;

\ Every storage definer reads its name on its own line and the first token of
\ its type on the name's. A name or type on the next line is missing there,
\ rejected by name at the definer or the name, though the next line would
\ complete it.
: LINE-STORED ( -- )
   s" a stored type on the next line is no type" T-LABEL
   s\" TYPED-VARIABLE C2-NL-N\nn"
   70 s" error[E-BAD-STORAGE]: `C2-NL-N` has no type on its line" s" " REPORTED? TTRUE
   s\" 4 TYPED-BUFFER C2-NL-SLOTS\nn"
   70 s" error[E-BAD-STORAGE]: `C2-NL-SLOTS` has no type on its line" s" " REPORTED? TTRUE
   s\" DYNAMIC-BUFFER C2-NL-DYN\nn"
   70 s" error[E-BAD-STORAGE]: `C2-NL-DYN` has no type on its line" s" " REPORTED? TTRUE
   s\" STRUCTURE c2row FIELD v n ;STRUCTURE\n4 LAYOUT-BUFFER C2-NL-ROWS\nc2row"
   70 s" error[E-BAD-STORAGE]: `C2-NL-ROWS` has no type on its line" s" " REPORTED? TTRUE
   s\" STRUCTURE c2row FIELD v n ;STRUCTURE\nDEFER-LAYOUT-BUFFER C2-NL-COLUMN\nc2row"
   70 s" error[E-BAD-STORAGE]: `C2-NL-COLUMN` has no type on its line" s" " REPORTED? TTRUE
   s" a storage name on the next line is no name" T-LABEL
   s\" TYPED-VARIABLE\nC2-NL-NAMED n"
   70 s" error[E-BAD-STORAGE]: `TYPED-VARIABLE` has no name on its line" s" " REPORTED? TTRUE
   s\" 4 TYPED-BUFFER\nC2-NL-SLOTS n"
   70 s" error[E-BAD-STORAGE]: `TYPED-BUFFER` has no name on its line" s" " REPORTED? TTRUE
   s\" DYNAMIC-BUFFER\nC2-NL-DYN n"
   70 s" error[E-BAD-STORAGE]: `DYNAMIC-BUFFER` has no name on its line" s" " REPORTED? TTRUE
   s\" STRUCTURE c2row FIELD v n ;STRUCTURE\n4 LAYOUT-BUFFER\nC2-NL-ROWS c2row"
   70 s" error[E-BAD-STORAGE]: `LAYOUT-BUFFER` has no name on its line" s" " REPORTED? TTRUE
   s\" STRUCTURE c2row FIELD v n ;STRUCTURE\nDEFER-LAYOUT-BUFFER\nC2-NL-COLUMN c2row"
   70 s" error[E-BAD-STORAGE]: `DEFER-LAYOUT-BUFFER` has no name on its line" s" " REPORTED? TTRUE ;

public

: RUN ( -- )
   T-RESET
   s" a monomorphic callback cannot claim the fresh scope" T-LABEL
   s" : C2-MUT-MONO ( [ mut-view<q,q,a,u8> -- mut-view<q,q,a,u8> | U -- U ] -- ) 1 MEM:BYTES-ALLOC-LEN swap C2-MEM:WITH-MUT ;" 70 SUBJECT:EXITED? TTRUE
   s" an exterior copy of an inferred callback stays monomorphic" T-LABEL
   s" : C2-MUT-COPIED ( [ mut-view<q,q,a,u8> -- mut-view<q,q,a,u8> | U -- U ] -- ) dup 1 MEM:BYTES-ALLOC-LEN swap C2-MEM:WITH-MUT ;" 70 SUBJECT:EXITED? TTRUE
   s" the authenticated root cannot be ticked in a checked word" T-LABEL
   s" : C2-SCOPE-TICK ( -- ) ['] C2-MEM:WITH-MUT drop ;" 70 SUBJECT:EXITED? TTRUE
   s" the authenticated root cannot be ticked at the prompt" T-LABEL
   s" ' C2-MEM:WITH-MUT drop" 70 SUBJECT:EXITED? TTRUE
   s" ordinary quotations remain executable with the same view operand" T-LABEL
   s" : C2-QUOTE-EXECUTE ( read-view<q,q,u8> [ read-view<q,q,u8> -- read-view<q,q,u8> | U -- U ] | U -- read-view<q,q,u8> | U ) execute ;" 0 SUBJECT:EXITED? TTRUE
   s" a scheme cannot pass through execute" T-LABEL
   s" : C2-SCOPE-EXECUTE ( read-view<q,q,u8> forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> | U -- U ]> | U -- read-view<q,q,u8> | U ) execute ;" 70 SUBJECT:EXITED? TTRUE
   s" ordinary quotations remain catchable with the same view operand" T-LABEL
   \ the quotation is over the word's own return row, which catch returns only
   \ on its success arm
   s" : C2-QUOTE-CATCH ( read-view<q,q,u8> [ read-view<q,q,u8> -- read-view<q,q,u8> | U -- U ] | U -- | U ) catch {: code:n :} drop code 0<> if code throw then ;" 0 SUBJECT:EXITED? TTRUE
   s" a scheme cannot pass through catch" T-LABEL
   s" : C2-SCOPE-CATCH ( read-view<q,q,u8> forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> | U -- U ]> | U -- | U ) catch {: code:n :} drop code 0<> if code throw then ;"
      70 s" error[E-MISMATCH]: `catch` takes [ -- ]; the stack holds forall<a,[ read-view<a,a,u8> -- read-view<a,a,u8> ]>" SUBJECT:REJECTED? TTRUE
   s" ordinary quotations remain finally bodies with the same view operand" T-LABEL
   s" : C2-QUOTE-FINALLY ( read-view<q,q,u8> [ read-view<q,q,u8> -- read-view<q,q,u8> | U -- U ] | U -- | U ) [: ;] finally drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a scheme cannot pass through finally" T-LABEL
   s" : C2-SCOPE-FINALLY ( read-view<q,q,u8> forall<p,[ read-view<p,p,u8> -- read-view<p,p,u8> | U -- U ]> | U -- | U ) [: ;] finally drop ;" 70 SUBJECT:EXITED? TTRUE
   s" an ordinary same-spelled word receives no owner scope" T-LABEL
   s" package C2-SCOPE-SHADOW TRUSTED: WITH-MUT ( R NUM:alloc-byte-len [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S | U ) C2-MEM:WITH-MUT ; : CALL ( -- n ) 1 MEM:BYTES-ALLOC-LEN [: 0 C2-MEM:MUT-BYTE@ ;] WITH-MUT ; ;package" 70 SUBJECT:EXITED? TTRUE
   s" a trusted declaration cannot publish a scheme" T-LABEL
   s" TRUSTED: C2-SCOPE-FORGE ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- ) drop ;"
      70 s" error[E-BAD-SIGNATURE]: the declared signature of `C2-SCOPE-FORGE` binds a callback scope" SUBJECT:REJECTED? TTRUE
   s" a cast declaration cannot publish a scheme" T-LABEL
   s" CAST: C2-SCOPE-CAST ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- n )"
      70 s" error[E-CAST-SCOPE]: in `cast: C2-SCOPE-CAST`, a scoped cast is not two cells on each side" SUBJECT:REJECTED? TTRUE
   s" an ordinary quotation may pass through a data output" T-LABEL
   s" : C2-QUOTE-OUTPUT ( [ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ] -- [ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ] ) ;" 0 SUBJECT:EXITED? TTRUE
   s" a scheme cannot be declared as an effect output" T-LABEL
   s" : C2-SCOPE-OUTPUT ( forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> ) ;" 70 SUBJECT:EXITED? TTRUE
   s" an ordinary quotation may be a pointer referent" T-LABEL
   s" : C2-QUOTE-POINTER ( ^[ n -- n ] -- ) drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a scheme cannot be placed behind a pointer" T-LABEL
   s" : C2-SCOPE-POINTER ( ^forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]> -- ) drop ;" 70 SUBJECT:EXITED? TTRUE
   s" an ordinary quotation may be a family argument" T-LABEL
   s" NEWTYPE c2-quote-holder<a> : C2-QUOTE-FAMILY ( c2-quote-holder<[ n -- n ]> -- ) drop ;" 0 SUBJECT:EXITED? TTRUE
   s" a family argument cannot contain a scheme" T-LABEL
   s" NEWTYPE c2-scope-holder<a> : C2-SCOPE-FAMILY ( c2-scope-holder<forall<p,[ R read-view<p,p,u8> -- S read-view<p,p,u8> | U -- U ]>> -- ) drop ;" 70 SUBJECT:EXITED? TTRUE
   STORED
   LAYOUT-STORED
   LINE-STORED
   s" an extra child view cannot leave its loan" T-LABEL
   s" : C2-CHILD-DIRECT ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup ;] C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" a nested aggregate cannot hide a child view" T-LABEL
   s" : C2-CHILD-AGG ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup OPTION:SOME swap ;] C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" a quotation cannot retain a child view" T-LABEL
   s" : C2-CHILD-QUOTE ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: 0 ['] C2-MEM:BYTE@ dup >r execute r> swap ;] C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" an exterior local cannot retain a child view" T-LABEL
   s" : C2-CHILD-LOCAL ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup ;] C2-MEM:WITH-READ {: escaped restored :} escaped drop restored ;" 70 SUBJECT:EXITED? TTRUE
   s" raw storage cannot retain a child view" T-LABEL
   s" variable C2-CHILD-RAW-SLOT : C2-CHILD-RAW ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup C2-CHILD-RAW-SLOT ! ;] C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" a generic raw store cannot hide a child view" T-LABEL
   s" variable C2-CHILD-GENERIC-SLOT : C2-CHILD-STORE ( a ^a -- ) ! ; : C2-CHILD-GENERIC ( read-view<p,q,u8> -- read-view<p,q,u8> ) [: dup C2-CHILD-GENERIC-SLOT C2-CHILD-STORE ;] C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" typed pointer storage cannot retain a child ceiling" T-LABEL
   s" : C2-CHILD-TYPED ( read-view<p,l,u8> ^read-view<p,l,u8> -- ) ! ;" 70 SUBJECT:EXITED? TTRUE
   s" a global cannot declare a child-scoped view" T-LABEL
   s" TYPED-VARIABLE C2-CHILD-SLOT read-view<p,l,u8>" 70 SUBJECT:EXITED? TTRUE
   s" an ordinary helper cannot relabel the loan ceiling" T-LABEL
   s" : C2-CHILD-RELABEL ( read-view<p,q,u8> -- read-view<p,r,u8> ) ;" 70 SUBJECT:EXITED? TTRUE
   s" a fixed existing-loan callback cannot claim a fresh child" T-LABEL
   s" : C2-CHILD-MONO ( read-view<p,q,u8> [ read-view<p,q,u8> -- read-view<p,q,u8> | U -- U ] | U -- read-view<p,q,u8> | U ) C2-MEM:WITH-READ ;" 70 SUBJECT:EXITED? TTRUE
   s" a same-spelled ordinary word receives no child authority" T-LABEL
   s" package C2-S TRUSTED: WITH-READ ( R ^u8 n [ R ^u8 n -- S ^u8 n | U -- U ] | U -- S ^u8 n | U ) C2-MEM:WITH-READ ; : X ( read-view<p,q,u8> -- n read-view<p,q,u8> ) [: 0 C2-MEM:BYTE@ ;] WITH-READ ; ;package" 70 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-memory-scope-rejections: ok" type cr ;

;package

C2-MEMORY-SCOPE-REJECTIONS:RUN
