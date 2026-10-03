\ Checker refusals for the opaque table control and its element loans.
require lib/test.f
require lib/test/subject.f
require lib/c2-memory.f
require test/c2-records-types.f

package C2-RECORDS-REFUSALS
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

: RUN ( -- )
   T-RESET
   s" a table control cannot be copied" T-LABEL
   s" : C2-TABLE-COPY ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) dup drop ;" 70 STATUS? TTRUE
   s" a table control cannot be discarded" T-LABEL
   s" : C2-TABLE-DROP ( records<b,i,a,C2-RECORDS-TYPES:pair> -- ) drop ;" 70 STATUS? TTRUE
   s" a table control cannot be held in an ordinary local" T-LABEL
   s" : C2-TABLE-LOCAL ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) {: table :} table ;" 70 STATUS? TTRUE
   s" the owner scope cannot be replaced" T-LABEL
   s" : C2-TABLE-OWNER ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<c,i,a,C2-RECORDS-TYPES:pair> ) ;" 70 STATUS? TTRUE
   s" the table's fixed initialization scope cannot be replaced" T-LABEL
   s" : C2-TABLE-INIT ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,j,a,C2-RECORDS-TYPES:pair> ) ;" 70 STATUS? TTRUE
   s" the table's allocation region cannot be replaced" T-LABEL
   s" : C2-TABLE-REGION ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,c,C2-RECORDS-TYPES:pair> ) ;" 70 STATUS? TTRUE
   s" another record type cannot be substituted" T-LABEL
   s" : C2-TABLE-VALUE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:other> ) ;" 70 STATUS? TTRUE
   s" the same record type remains usable" T-LABEL
   s" : C2-TABLE-VALUE-SAME ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) ;" 0 STATUS? TTRUE
   s" an initialized record's view element stays exact" T-LABEL
   s" : C2-TABLE-BOX-CHANGE ( records<b,i,a,C2-RECORDS-TYPES:box<c,u8>> -- records<b,i,a,C2-RECORDS-TYPES:box<c,n>> ) ;" 70 STATUS? TTRUE
   s" the same initialized view element remains usable" T-LABEL
   s" : C2-TABLE-BOX-SAME ( records<b,i,a,C2-RECORDS-TYPES:box<c,u8>> -- records<b,i,a,C2-RECORDS-TYPES:box<c,u8>> ) ;" 0 STATUS? TTRUE
   s" a stored source view cannot change its owner identity" T-LABEL
   s" : C2-TABLE-SOURCE ( records<b,i,a,C2-RECORDS-TYPES:shelf<p,q,t,u>> -- records<b,i,a,C2-RECORDS-TYPES:shelf<r,q,t,u>> ) ;" 70 STATUS? TTRUE
   s" a table cannot be relabeled as a raw byte span" T-LABEL
   s" : C2-TABLE-RAW ( records<b,i,a,C2-RECORDS-TYPES:pair> -- ptr u8 n ) ;" 70 STATUS? TTRUE
   s" a table cannot be stored in a global" T-LABEL
   s" TYPED-VARIABLE C2-TABLE-GLOBAL records<b,i,a,C2-RECORDS-TYPES:pair>" 70 STATUS? TTRUE
   s" an extra table authority cannot leave its owner callback" T-LABEL
   s" : C2-TABLE-ESCAPE ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> ) 1 1 2 C2--RECORDS--TYPES-PAIR:MAKE [: dup ;] C2-MEM:WITH-RECORDS ;" 70 STATUS? TTRUE
   s" an element view cannot be copied" T-LABEL
   s" : C2-ELEMENT-COPY ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> -- mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> ) dup drop ;" 70 STATUS? TTRUE
   s" a table cannot be used while an element loan is active" T-LABEL
   s" : C2-TABLE-ACTIVE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) dup 0 [: ;] C2-MEM:WITH-RECORD swap drop ;" 70 STATUS? TTRUE
   s" an element view cannot escape its loan callback" T-LABEL
   s" : C2-ELEMENT-ESCAPE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) 0 [: dup ;] C2-MEM:WITH-RECORD ;" 70 STATUS? TTRUE
   s" an element loan cannot change the fixed init lifetime" T-LABEL
   s" : C2-ELEMENT-INIT ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> -- mut-view<b,j,a,init<j,C2-RECORDS-TYPES:pair>> ) ;" 70 STATUS? TTRUE
   s" a live byte view can open a record table" T-LABEL
   s" : C2-TABLE-LIVE ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> ) 1 1 2 C2--RECORDS--TYPES-PAIR:MAKE [: ;] C2-MEM:WITH-RECORDS ;" 0 STATUS? TTRUE
   s" a caught throw cannot reopen a stale byte view as a table" T-LABEL
   s" : C2-TABLE-STALE-THROW ( mut-view<b,l,a,u8> n -- mut-view<b,l,a,u8> n ) swap 1 throw ; : C2-TABLE-STALE ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> ) 0 ['] C2-TABLE-STALE-THROW catch drop drop 1 1 2 C2--RECORDS--TYPES-PAIR:MAKE [: ;] C2-MEM:WITH-RECORDS ;" 70 STATUS? TTRUE
   s" a live table can loan an element" T-LABEL
   s" : C2-ELEMENT-LIVE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) 0 [: ;] C2-MEM:WITH-RECORD ;" 0 STATUS? TTRUE
   s" a caught throw cannot loan from a stale table" T-LABEL
   s" : C2-ELEMENT-STALE-THROW ( records<b,i,a,C2-RECORDS-TYPES:pair> n -- records<b,i,a,C2-RECORDS-TYPES:pair> n ) swap 1 throw ; : C2-ELEMENT-STALE ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> ) 0 ['] C2-ELEMENT-STALE-THROW catch drop drop 0 [: ;] C2-MEM:WITH-RECORD ;" 70 STATUS? TTRUE
   s" a cursor-scoped view cannot overwrite a longer-lived table field" T-LABEL
   s" : C2-CURSOR-STORE ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> read-view<p,j,u8> -- mut-view<b,j,a,init<i,C2-RECORDS-TYPES:shelf<p,q,t,u>>> ) C2--RECORDS--TYPES-SHELF:SOURCE! ;" 70 STATUS? TTRUE
   s" a normal trusted declaration cannot mint a table" T-LABEL
   s" TRUSTED: C2-TABLE-FORGE ( -- records<b,i,a,C2-RECORDS-TYPES:pair> ) 0 0 ;" 76 STATUS? TTRUE
   T-REPORT
   s" c2-records-refusals: ok" type cr ;

;package

C2-RECORDS-REFUSALS:RUN
