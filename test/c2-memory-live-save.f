\ Load this file, then invoke RUN from stdin after all requires return.
require src/habu/app-image.f
require lib/test.f
require lib/memory.f
require lib/errors.f
require lib/c2-memory.f
require lib/c2-owner.f

package C2-MEMORY-LIVE-SAVE
private

: LOAN ( mut-view<p,l,a,u8> -- mut-view<p,l,a,u8> )
   [: s" hb-live-loan" APP-IMAGE:SAVE ;] catch E-C2-CAPTURE = TTRUE ;

: ACTIVE ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   [: LOAN ;] C2-MEM:WITH-MUT-LOAN
   [: s" hb-live-owner" APP-IMAGE:SAVE ;] catch E-C2-CAPTURE = TTRUE ;

: APPEND-OWNER ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   65488 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop
   17 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC C2-MEM:PUBLISH drop
   [: s" hb-live-append" APP-IMAGE:SAVE ;] catch E-C2-CAPTURE = TTRUE
   C2-MEM:UNBIND ;

: APPEND-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: APPEND-OWNER ;] C2-MEM:WITH-INIT ;

public

: RUN ( -- )
   T-RESET
   s" image capture refuses live owner and exclusive loan scopes" T-LABEL
   1 MEM:BYTES-ALLOC-LEN [: ACTIVE ;] C2-MEM:WITH-MUT
   s" capture refuses a live appended owner after chunk rollover" T-LABEL
   C2-MEM:OWNER-SIZE [: APPEND-ROOT ;] C2-MEM:WITH-MUT
   T-REPORT
   s" c2-memory-live-save: closed" type cr
   s" hb-after" APP-IMAGE:SAVE ;

;package
