\ Capture must see an active initialized frame, including its clear-only owner.
require src/habu/app-image.f
require lib/test.f
require lib/memory.f
require lib/errors.f
require lib/c2-memory.f

package C2-INIT-LIVE-SAVE
public
STRUCTURE c2ilive 0 FIELD x n FIELD y n ;STRUCTURE

private

: ACTIVE ( mut-view<p,i,a,init<i,c2ilive>> -- mut-view<p,i,a,init<i,c2ilive>> )
   [: s" hb-live-init" APP-IMAGE:SAVE ;] catch E-C2-CAPTURE = TTRUE ;

: OWNER ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   1 2 C2--INIT--LIVE--SAVE-C2ILIVE:MAKE [: ACTIVE ;] C2-MEM:WITH-INIT ;

public

: RUN ( -- )
   T-RESET
   s" live initialization refuses image capture" T-LABEL
   16 MEM:BYTES-ALLOC-LEN [: OWNER ;] C2-MEM:WITH-MUT
   T-REPORT
   s" c2-init-live-save: closed" type cr ;

;package
