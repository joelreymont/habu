\ A capture attempted inside an ownerless loan must be refused and retryable.
require lib/test.f
require lib/image-lifecycle.f
require lib/c2-memory.f

package C2-MEM-CAPTURE
private

create BYTES 65 c, 66 c, 67 c,

: TRY-CAPTURE ( ptr u8 n -- ptr u8 n )
   [: IMAGE-LIFECYCLE:PREPARE ;] catch E-C2-CAPTURE T= ;

TRUSTED: INSIDE-READ ( -- )
   BYTES 3 [: TRY-CAPTURE ;] C2-MEM:WITH-READ 2drop ;

TRUSTED: INSIDE-MUT-LOAN ( -- )
   BYTES 3 [: TRY-CAPTURE ;] C2-MEM:WITH-MUT-LOAN 2drop ;

: TRY-OWNER ( ptr u8 n -- ptr u8 n ) TRY-CAPTURE ;

: FAIL-LOAN ( ptr u8 n -- ptr u8 n ) -9363 throw ;

TRUSTED: THROW-READ ( -- )
   BYTES 3 [: FAIL-LOAN ;] C2-MEM:WITH-READ 2drop ;

TRUSTED: THROW-MUT-LOAN ( -- )
   BYTES 3 [: FAIL-LOAN ;] C2-MEM:WITH-MUT-LOAN 2drop ;

TRUSTED: INSIDE-OWNER ( -- )
   16 MEM:BYTES-ALLOC-LEN [: TRY-OWNER ;] C2-MEM:WITH-MUT ;

public
: RUN ( -- )
   T-RESET
   s" ownerless read loan refuses capture" T-LABEL INSIDE-READ
   s" ownerless mutable loan refuses capture" T-LABEL INSIDE-MUT-LOAN
   s" allocated owner refuses capture" T-LABEL INSIDE-OWNER
   s" failed loans restore the capture guard depth" T-LABEL
   [: THROW-READ ;] catch -9363 T=
   [: THROW-MUT-LOAN ;] catch -9363 T=
   s" closed scopes allow a retry with guard still armed" T-LABEL
   IMAGE-LIFECYCLE:PREPARE
   INSIDE-READ
   IMAGE-LIFECYCLE:PREPARE
   T-REPORT
   s" c2-memory-capture: ok" type cr ;
;package

C2-MEM-CAPTURE:RUN
