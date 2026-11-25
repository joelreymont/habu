\ Real allocation and scoped-invocation runtime through the native source load path.
require src/lib/test/assert.f
require src/lib/memory.f
require src/lib/c2-memory.f

package C2-MEM-RUNTIME
private

: CHANGE-ROW ( n ^u8 n -- n ^u8 n ) {: seed:n p:^ len:n :}
   73 p c!
   seed 1+ p 1+ len 1- ;

TRUSTED: ALLOCATED ( n -- n )
   16 MEM:BYTES-ALLOC-LEN [: CHANGE-ROW ;] C2-MEM:WITH-MUT ;

: ADD-CELL ( n mut-view<p,p,a,u8> -- n n mut-view<p,p,a,u8> )
   swap dup 1+ rot ;

: GROW-ROW ( n -- n n )
   16 MEM:BYTES-ALLOC-LEN [: ADD-CELL ;] C2-MEM:WITH-MUT ;

: SUM-CELLS ( n n mut-view<p,p,a,u8> -- n mut-view<p,p,a,u8> )
   -rot + swap ;

: SHRINK-ROW ( n n -- n )
   16 MEM:BYTES-ALLOC-LEN [: SUM-CELLS ;] C2-MEM:WITH-MUT ;

public
: RUN ( -- )
   T-RESET
   s" allocation accepts changed callback rows" T-LABEL
   40 ALLOCATED 41 T=
   s" scoped invocation preserves a growing result row" T-LABEL
   777 91 GROW-ROW 92 T= 91 T= 777 T=
   s" scoped invocation preserves a shrinking result row" T-LABEL
   888 17 25 SHRINK-ROW 42 T= 888 T=
   T-REPORT
   s" c2-memory-runtime: ok" type cr ;
;package

C2-MEM-RUNTIME:RUN
