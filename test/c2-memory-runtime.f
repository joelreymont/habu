\ Real allocation and scoped-invocation runtime through the native source load path.
require src/lib/test.f
require src/lib/memory.f
require src/lib/c2-memory.f

package C2-MEM-RUNTIME
private

: CHANGE-ROW ( n ptr u8 n -- n ptr u8 n ) {: seed:n p:ptr len:n :}
   73 p c!
   seed 1+ p 1+ len 1- ;

TRUSTED: ALLOCATED ( n -- n )
   16 MEM:BYTES-ALLOC-LEN [: CHANGE-ROW ;] C2-MEM:WITH-MUT ;

TRUSTED: GROW-ROW ( n -- n n )
   [: dup 1+ ;] [: ;] [: drop ;] c2-invoke ;

TRUSTED: SHRINK-ROW ( n n -- n )
   [: + ;] [: ;] [: drop ;] c2-invoke ;

TRUSTED: BODY-FIRST ( -- )
   [: -9365 throw ;] [: -9366 throw ;] [: drop ;] c2-invoke ;

TRUSTED: CLEANUP-ONLY ( -- ) [: ;] [: -9366 throw ;] [: drop ;] c2-invoke ;

public
: RUN ( -- )
   T-RESET
   s" allocation accepts changed callback rows" T-LABEL
   40 ALLOCATED 41 T=
   s" scoped invocation preserves a growing result row" T-LABEL
   777 91 GROW-ROW 92 T= 91 T= 777 T=
   s" scoped invocation preserves a shrinking result row" T-LABEL
   888 17 25 SHRINK-ROW 42 T= 888 T=
   s" the body's failure passes before the cleanup's" T-LABEL
   [: BODY-FIRST ;] catch -9365 T=
   s" a cleanup's failure passes when the body returned" T-LABEL
   [: CLEANUP-ONLY ;] catch -9366 T=
   T-REPORT
   s" c2-memory-runtime: ok" type cr ;
;package

C2-MEM-RUNTIME:RUN
