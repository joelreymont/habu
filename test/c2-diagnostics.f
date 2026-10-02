\ C2 refusals retain the operation and the logical owner or loan in both
\ diagnostic formats. Every candidate runs through the real evaluator.
require lib/test.f
require lib/test/subject.f
require lib/c2-memory.f
require lib/c2-owner.f
require test/c2-records-types.f
require test/c2-forall-input.f

package C2-DIAGNOSTICS
private

$4000 constant CAP
create OUT CAP allot
create ERR CAP allot

: REFUSAL ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: source:ptr source-u:n operation:ptr operation-u:n culprit:ptr culprit-u:n reason:ptr reason-u:n :}
   source source-u OUT CAP >LEN ERR CAP >LEN 10000 >MS SUBJECT:RUN
   MATCH outcome
      exited OF 70 = ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH
   {: out-u:len err-u:len rejected:bool :}
   rejected TTRUE
   ERR err-u LEN>N operation operation-u CONTAINS? TTRUE
   ERR err-u LEN>N culprit culprit-u CONTAINS? TTRUE
   ERR err-u LEN>N reason reason-u CONTAINS? TTRUE ;

public
: RUN ( -- )
   T-RESET
   s" exclusive copy names the copied view and over" T-LABEL
   s" : C2-DIAG-COPY ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) 0 over drop ;"
   s" at 'over'" s" actual: mut-view" s" exclusive value copied" REFUSAL
   s" JSON exclusive copy names the copied view and over" T-LABEL
   s" -1 JSON-DIAGS ! : C2-DIAG-COPY-J ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) 0 over drop ;"
   S\" \"token\":\"over\"" S\" \"actual_type\":\"mut-view" S\" \"code\":\"E-C2-COPY\"" REFUSAL
   s" exclusive drop names the discarded view and drop" T-LABEL
   s" : C2-DIAG-DROP ( mut-view<p,p,a,u8> -- ) drop ;"
   s" at 'drop'" s" actual: mut-view" s" exclusive value discarded" REFUSAL
   s" JSON exclusive drop names the discarded view and drop" T-LABEL
   s" -1 JSON-DIAGS ! : C2-DIAG-DROP-J ( mut-view<p,p,a,u8> -- ) drop ;"
   S\" \"token\":\"drop\"" S\" \"actual_type\":\"mut-view" S\" \"code\":\"E-C2-DROP\"" REFUSAL
   s" nested owner names the exclusive carrier" T-LABEL
   s" -1 JSON-DIAGS ! : C2-DIAG-OWNER ( C2-MEM:owner<p,i,a> -- C2-MEM:owner<p,i,a> ) dup drop ;"
   S\" \"token\":\"dup\"" S\" \"actual_type\":\"c2-mem:owner" S\" \"code\":\"E-C2-COPY\"" REFUSAL
   s" escaping child names the loan and the boundary operation" T-LABEL
   s" package C2-FORALL-INPUT public : C2-DIAG-ESCAPE ( R read-view<p,p,u8> forall<q,[ read-view<q,q,u8> -- read-view<q,q,u8> ]> forall<j inside p,[ read-view<p,j,u8> -- read-view<p,j,u8> read-view<p,j,u8> ]> | U -- S read-view<p,p,u8> | U ) {: loan :} GENERIC-SCOPE loan C2-MEM:WITH-READ ; ;package"
   s" at 'C2-MEM:WITH-READ'" s" actual: read-view" s" scoped value escapes its owner or loan" REFUSAL
   s" JSON escaping child names the loan and the boundary operation" T-LABEL
   s" -1 JSON-DIAGS ! package C2-FORALL-INPUT public : C2-DIAG-ESCAPE-J ( R read-view<p,p,u8> forall<q,[ read-view<q,q,u8> -- read-view<q,q,u8> ]> forall<j inside p,[ read-view<p,j,u8> -- read-view<p,j,u8> read-view<p,j,u8> ]> | U -- S read-view<p,p,u8> | U ) {: loan :} GENERIC-SCOPE loan C2-MEM:WITH-READ ; ;package"
   S\" \"token\":\"C2-MEM:WITH-READ\"" S\" \"actual_type\":\"read-view" S\" \"code\":\"E-C2-SCOPE-ESCAPE\"" REFUSAL
   s" stale table names the stale parent and record operation" T-LABEL
   s" : C2-DIAG-THROW ( records<b,i,a,C2-RECORDS-TYPES:pair> n -- records<b,i,a,C2-RECORDS-TYPES:pair> n ) swap 1 throw ; : C2-DIAG-STALE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) 0 ['] C2-DIAG-THROW catch drop drop 0 [: ;] C2-MEM:WITH-RECORD ;"
   s" at 'C2-MEM:WITH-RECORD'" s" actual: stale<records" s" stale cell:" REFUSAL
   s" JSON stale table names the stale parent and record operation" T-LABEL
   s" -1 JSON-DIAGS ! : C2-DIAG-THROW-J ( records<b,i,a,C2-RECORDS-TYPES:pair> n -- records<b,i,a,C2-RECORDS-TYPES:pair> n ) swap 1 throw ; : C2-DIAG-STALE-J ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) 0 ['] C2-DIAG-THROW-J catch drop drop 0 [: ;] C2-MEM:WITH-RECORD ;"
   S\" \"token\":\"C2-MEM:WITH-RECORD\"" S\" \"actual_type\":\"stale<records" S\" \"code\":\"E-STALE-READ\"" REFUSAL
   T-REPORT
   s" c2-diagnostics: ok" type cr ;
;package

C2-DIAGNOSTICS:RUN
