\ A declared mutable view can move through a by-value record. No typed
\ address, constructor for the view, or mutable field access is introduced.
require src/lib/test.f
require src/lib/test/subject.f

STRUCTURE c2vmut<a,b,c,d> FIELD view mut-view<a,b,c,d> ;STRUCTURE

package C2-VIEW-RECORD-MUT
private

: ROUND ( mut-view<p,q,a,u8> -- mut-view<p,q,a,u8> )
   C2VMUT:MAKE C2VMUT:UNMAKE ;

public

\ A refused STRUCTURE exits 70: the checker renders the declaration's refusal
\ before it throws the refusal's code.
: RUN ( -- )
   T-RESET
   s" a region parameter cannot also be a value" T-LABEL
   s" STRUCTURE c2vregfirst<a,b,c,d> FIELD value c FIELD view mut-view<a,b,c,d> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" a region parameter cannot become a scope" T-LABEL
   s" STRUCTURE c2vregscope<a,b,c,d> FIELD view mut-view<a,b,c,d> FIELD source read-view<a,c,u8> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" a raw pointer cannot hide an exclusive view" T-LABEL
   s" STRUCTURE c2vptrmut<a,b,c,d> FIELD view ptr mut-view<a,b,c,d> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   T-REPORT
   s" c2-view-record-mut: ok" type cr ;

;package

C2-VIEW-RECORD-MUT:RUN
