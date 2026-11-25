\ Rejections through fresh evaluators in the native C2 image.
require src/lib/test/assert.f
require src/lib/test/subject.f
require src/lib/c2-memory.f
require test/c2-view-record-defs.f
require test/c2-view-record-use-rejections.f

package C2-VIEW-RECORD-REJECTIONS
public

\ A rejected STRUCTURE exits 70: the checker renders the declaration's rejection
\ before it throws the rejection's code.
: RUN ( -- )
   T-RESET
   s" a prior value use cannot become a scope" T-LABEL
   s" STRUCTURE c2mixfirst<a,b> FIELD value a FIELD source read-view<a,b,u8> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" a later value use cannot consume a scope" T-LABEL
   s" STRUCTURE c2mixlast<a,b> FIELD source read-view<a,b,u8> FIELD value a ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" an element type parameter cannot become a scope" T-LABEL
   s" STRUCTURE c2mixelement<a,b,c> FIELD source read-view<a,b,c> FIELD extra read-view<c,b,u8> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" a phantom element cannot become an owning field" T-LABEL
   s" STRUCTURE c2ownstype<a,b,c> FIELD source read-view<a,b,c> FIELD value c ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" a raw pointer cannot hide a borrowed field" T-LABEL
   s" STRUCTURE c2ptrview<a,b> FIELD source ^read-view<a,b,u8> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" nested record cannot escape an owner callback" T-LABEL
   s" -1 REJECTION:JSON ! : C2V-S ( read-view<p,l,u8> -- c2vinner<p,l> read-view<p,l,u8> ) dup 1 C2VINNER:MAKE swap ; : C2V-B ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> ) [: C2V-S ;] C2-MEM:WITH-READ swap drop ;"
   70 S\" \"code\":\"E-C2-SCOPE-ESCAPE\"" SUBJECT:REJECTED? TTRUE
   s" raw global storage cannot retain a borrowed record" T-LABEL
   s" variable C2V-SLOT : C2V-STORE ( c2vinner<p,q> -- ) C2V-SLOT ! ;" 70 SUBJECT:EXITED? TTRUE
   s" raw pointer field access cannot expose a view" T-LABEL
   s" STRUCTURE c2vaddr<a,b> DERIVE addr FIELD source read-view<a,b,u8> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" raw pointer field access cannot expose a nested record's view" T-LABEL
   s" STRUCTURE c2vi<a,b> FIELD source read-view<a,b,u8> ;STRUCTURE STRUCTURE c2vo<a,b> DERIVE addr FIELD inner c2vi<a,b> ;STRUCTURE" 70 SUBJECT:EXITED? TTRUE
   s" a word cannot state a payloadless variant's unsupplied scopes" T-LABEL
   s" -1 REJECTION:JSON ! : C2V-NONE ( -- c2vmaybe<p,q> ) C2VMAYBE:VACANT ;"
   70 S\" \"code\":\"E-BAD-SIGNATURE\"" SUBJECT:REJECTED? TTRUE
   C2-VIEW-RECORD-USE-REJECTIONS:CASES
   T-REPORT
   s" c2-view-record-rejections: ok" type cr ;

;package

C2-VIEW-RECORD-REJECTIONS:RUN
