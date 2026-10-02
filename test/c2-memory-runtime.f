\ Real allocation and loan runtime through the native source load path.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f

package C2-MEM-RUNTIME
private

: CHANGE-ROW ( n ptr u8 n -- n ptr u8 n ) {: seed:n p:ptr len:n :}
   73 p c!
   seed 1+ p 1+ len 1- ;

: ALTER-READ ( ptr u8 n -- n ptr u8 n ) {: p:ptr len:n :}
   p c@ p 1+ len 1- ;

: ALTER-MUT ( ptr u8 n -- n ptr u8 n ) {: p:ptr len:n :}
   84 p c! 84 p 1+ len 1- ;

TRUSTED: ALLOCATED ( n -- n )
   16 MEM:BYTES-ALLOC-LEN [: CHANGE-ROW ;] C2-MEM:WITH-MUT ;

TRUSTED: READ-ROOT ( ptr u8 n -- n n ptr u8 n ) {: p:ptr len:n :}
   67 p c!
   p len [: ALTER-READ ;] C2-MEM:WITH-READ {: value:n restored:ptr bound:n :}
   restored p = TTRUE
   value bound p len ;

TRUSTED: READ-RESTORED ( -- n n )
   16 MEM:BYTES-ALLOC-LEN [: READ-ROOT ;] C2-MEM:WITH-MUT ;

TRUSTED: MUT-ROOT ( ptr u8 n -- n n ptr u8 n ) {: p:ptr len:n :}
   67 p c!
   p len [: ALTER-MUT ;] C2-MEM:WITH-MUT-LOAN {: value:n restored:ptr bound:n :}
   restored p = TTRUE
   value bound p len ;

TRUSTED: MUT-RESTORED ( -- n n )
   16 MEM:BYTES-ALLOC-LEN [: MUT-ROOT ;] C2-MEM:WITH-MUT ;

TRUSTED: FAIL-CALLBACK ( ptr u8 n -- ptr u8 n )
   -9363 throw ;

TRUSTED: FAIL-ALLOC ( -- )
   16 MEM:BYTES-ALLOC-LEN [: FAIL-CALLBACK ;] C2-MEM:WITH-MUT ;

TRUSTED: GROW-ROW ( n -- n n )
   [: dup 1+ ;] [: ;] [: drop ;] c2-invoke ;

TRUSTED: SHRINK-ROW ( n n -- n )
   [: + ;] [: ;] [: drop ;] c2-invoke ;

TRUSTED: CLEANUP-WINS ( -- )
   [: -9365 throw ;] [: -9366 throw ;] [: drop ;] c2-invoke ;

public
: RUN ( -- )
   T-RESET
   s" allocation accepts changed callback rows" T-LABEL
   40 ALLOCATED 41 T=
   s" read loan restores the original parent span" T-LABEL
   READ-RESTORED 16 T= 67 T=
   s" mutable loan restores the original parent span" T-LABEL
   MUT-RESTORED 16 T= 84 T=
   s" callback failure propagates after owner cleanup" T-LABEL
   [: FAIL-ALLOC ;] catch -9363 T=
   s" scoped invocation preserves a growing result row" T-LABEL
   777 91 GROW-ROW 92 T= 91 T= 777 T=
   s" scoped invocation preserves a shrinking result row" T-LABEL
   888 17 25 SHRINK-ROW 42 T= 888 T=
   s" cleanup failure supersedes callback failure" T-LABEL
   [: CLEANUP-WINS ;] catch -9366 T=
   T-REPORT
   s" c2-memory-runtime: ok" type cr ;
;package

C2-MEM-RUNTIME:RUN
