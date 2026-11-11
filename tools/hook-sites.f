\ hook-sites.f - immutable checker-hook installation registry.

require src/lib/error.f
require src/lib/prelude.f
require src/lib/string.f

package HOOK-SITES

private

2 constant ROW-N

0 constant KIND-CHECK
1 constant KIND-TOP

: ROW ( n -- ptr u8 n ptr u8 n n ) {: k:n :}
   k 0 < if E-TBL-BOUNDS throw then
   k ROW-N >= if E-TBL-BOUNDS throw then
   k case
      0 of s" src/core/check-hook.f" s" DEFINITION" KIND-CHECK endof
      1 of s" src/core/top-row.f" s" TOP-LEVEL" KIND-TOP endof
      drop E-TBL-BOUNDS throw
   endcase ;

: ROW-MATCH? ( ptr u8 n ptr u8 n n n -- bool )
   {: pa:ptr pu:n na:ptr nu:n kind:n k:n :}
   k ROW {: rpa:ptr rpu:n rna:ptr rnu:n rkind:n :}
   kind rkind = dup 0= if exit then drop
   pa pu rpa rpu STR= dup 0= if exit then drop
   na nu rna rnu STR= ;

: MATCH-KIND? ( ptr u8 n ptr u8 n n -- bool )
   {: pa:ptr pu:n na:ptr nu:n kind:n :}
   ROW-N 0 ?do
      pa pu na nu kind i ROW-MATCH? dup if unloop exit then drop
   loop false ;

public

: CHECK-MATCH? ( ptr u8 n ptr u8 n -- bool )
   KIND-CHECK MATCH-KIND? ;

: TOP-MATCH? ( ptr u8 n ptr u8 n -- bool )
   KIND-TOP MATCH-KIND? ;

;package
