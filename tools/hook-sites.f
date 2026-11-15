\ hook-sites.f - immutable checker-hook installation registry.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/string.f

package HOOK-SITES

private

1 constant ROW-N

: ROW ( n -- ^u8 n ^u8 n ) {: k:n :}
   k 0 < if E-TBL-BOUNDS throw then
   k ROW-N >= if E-TBL-BOUNDS throw then
   k case
      0 of s" src/core/top-row.f" s" TOP-LEVEL" endof
      drop E-TBL-BOUNDS throw
   endcase ;

: ROW-MATCH? ( ^u8 n ^u8 n n -- bool )
   {: pa:^ pu:n na:^ nu:n k:n :}
   k ROW {: rpa:^ rpu:n rna:^ rnu:n :}
   pa pu rpa rpu STR= dup 0= if exit then drop
   na nu rna rnu STR= ;

public

: TOP-MATCH? ( ^u8 n ^u8 n -- bool )
   {: pa:^ pu:n na:^ nu:n :}
   ROW-N 0 ?do
      pa pu na nu i ROW-MATCH? dup if unloop exit then drop
   loop false ;

;package
