\ Exact spans must not acquire the adjacent RET or an address chain's first word.
require lib/test.f
require src/habu/app-image.f
require src/habu/aot-decl.f
require src/habu/aot-closure.f
require src/habu/aot-lib.f

1 set-tier
package CODE-SPAN-FIXTURE
public
: SPIN-A ( -- ) begin again ;
: NIL-A ( -- ) ;
: SPIN-B ( -- ) begin again ;
: LIT-B ( -- ptr u8 n ) s" span literal" ;
: BACK ( -- n ) 7 ;
: MAKE ( n -- ) create , does> ( -- n ) @ ;
42 MAKE MADE
defer LATER ( -- n )
;package

package AOT-LINK

: SPAN-REC ( ptr u8 n -- ptr n )
   XREF-FIND dup XREF-FOUND? TTRUE ;

: SPAN-END ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START rec REC-BYTES + ;

: ADJACENT ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n b:ptr v:n :}
   a u SPAN-REC {: rec:ptr :}
   b v SPAN-REC {: next:ptr :}
   rec 8 + @ CODE-SPAN:FULL? TTRUE
   rec SPAN-END next XREF-START T=
   next XREF-START ADDRESS-OWNER next = TTRUE
   rec SPAN-END 4 - ADDRESS-OWNER rec = TTRUE
   \ Retain the real root closure, including emitted stack-guard helpers.
   a u ENTRY-NAME! CLOSURE
   ROOTREC @ rec = TTRUE
   ASM-INIT PLAN-BLOBS
   ASM-LEN {: before:n :}
   rec REC-CODE-PTR@ MEMBER-AT {: i:n :}
   i CLO-BYTES rec REC-BYTES T=
   i COPY-COMPACT-BLOB
   ASM-LEN before - i CLO-BYTES T= ;

: SPAN-RUN ( -- )
   T-RESET
   s" no-return body ends before an adjacent empty RET" T-LABEL
   s" CODE-SPAN-FIXTURE:SPIN-A" s" CODE-SPAN-FIXTURE:NIL-A" ADJACENT
   s" no-return body ends before an adjacent address chain" T-LABEL
   s" CODE-SPAN-FIXTURE:SPIN-B" s" CODE-SPAN-FIXTURE:LIT-B" ADJACENT
   s" ordinary returning record preserves its legacy final slot" T-LABEL
   s" CODE-SPAN-FIXTURE:BACK" SPAN-REC {: back:ptr :}
   back 8 + @ CODE-SPAN:FULL? TFALSE
   back REC-BYTES back 8 + @ 4 + T=
   s" DOES retains its patched final slot and companion extent" T-LABEL
   s" CODE-SPAN-FIXTURE:MADE" SPAN-REC {: made:ptr :}
   made 8 + @ CODE-SPAN:FULL? TFALSE
   made REC-BYTES made 8 + @ 4 + T=
   made SPAN-END 4 - ADDRESS-OWNER made = TTRUE
   s" CODE-SPAN-FIXTURE:MAKE" SPAN-REC SPAN-END
   s" CODE-SPAN-FIXTURE:MAKE;does" SPAN-REC SPAN-END T=
   CODE-SPAN-FIXTURE:MADE 42 T=
   s" deferred code excludes its metadata trailer" T-LABEL
   s" CODE-SPAN-FIXTURE:LATER" SPAN-REC {: later:ptr :}
   later 8 + @ CODE-SPAN:FULL? TTRUE
   later SPAN-END ADDRESS-OWNER later = TFALSE
   T-REPORT ;

' SPAN-RUN
;package
execute
