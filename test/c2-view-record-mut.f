\ A declared mutable view can move through a by-value record. No typed
\ address, constructor for the view, or mutable field access is introduced.
require lib/test.f
require lib/test/subject.f

STRUCTURE c2vmut 4 FIELD view mut-view<a,b,c,d> ;STRUCTURE

package C2-VIEW-RECORD-MUT
private

: ROUND ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   C2VMUT:MAKE C2VMUT:UNMAKE ;

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
   s" a region parameter cannot also be a value" T-LABEL
   s" STRUCTURE c2vregfirst 4 FIELD value c FIELD view mut-view<a,b,c,d> ;STRUCTURE" 67 STATUS? TTRUE
   s" a region parameter cannot become a scope" T-LABEL
   s" STRUCTURE c2vregscope 4 FIELD view mut-view<a,b,c,d> FIELD source read-view<a,c,u8> ;STRUCTURE" 67 STATUS? TTRUE
   s" a raw pointer cannot hide an exclusive view" T-LABEL
   s" STRUCTURE c2vptrmut 4 FIELD view ptr mut-view<a,b,c,d> ;STRUCTURE" 67 STATUS? TTRUE
   T-REPORT
   s" c2-view-record-mut: ok" type cr ;

;package

C2-VIEW-RECORD-MUT:RUN
