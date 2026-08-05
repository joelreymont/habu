\ hook-sites.f - immutable checker-hook installation registry.

require lib/errors.f
require lib/prelude.f
require lib/string.f

package HOOK-SITES

private

11 constant ROW-N

0 constant KIND-CHECK
1 constant KIND-TOP

: ROW ( n -- ptr u8 n ptr u8 n ptr u8 n ptr u8 n n ) {: k:n :}
   k 0 < if E-TBL-BOUNDS throw then
   k ROW-N >= if E-TBL-BOUNDS throw then
   k case
      0 of s" src/core/check-hook.f" s" HOOK" s" stdlib-boundary" s" cap:checker-hook-identity" KIND-CHECK endof
      1 of s" src/habu/aot.f" s" USER-HOOK" s" builder-emit" s" cap:checker-hook-identity" KIND-CHECK endof
      2 of s" src/habu/snap-lib.f" s" CHECK-HOOK" s" builder-emit" s" cap:checker-hook-identity" KIND-CHECK endof
      3 of s" test/compile-preflight-recovery.f" s" CPR-HOOK" s" test-metaprog" s" cap:checker-hook-identity" KIND-CHECK endof
      4 of s" test/engine-suite.f" s" ES-VERDICT-HOOK" s" test-metaprog" s" cap:checker-hook-identity" KIND-CHECK endof
      5 of s" test/prop-test-core.f" s" PROP-CHECK-HOOK" s" test-metaprog" s" cap:checker-hook-identity" KIND-CHECK endof
      6 of s" tools/check-core.f" s" CHK-CHECK-HOOK" s" stdlib-boundary" s" cap:checker-hook-identity" KIND-CHECK endof
      7 of s" tools/codegen-role.f" s" CGR-HOOK" s" test-metaprog" s" cap:checker-hook-identity" KIND-CHECK endof
      8 of s" tools/lint/text.f" s" LINT-CHECK-HOOK" s" stdlib-boundary" s" cap:checker-hook-identity" KIND-CHECK endof
      9 of s" src/core/top-row.f" s" TR-HOOK" s" stdlib-boundary" s" cap:checker-hook-identity" KIND-TOP endof
     10 of s" test/top-row-hook-test.f" s" TRH-LOG" s" test-metaprog" s" cap:checker-hook-identity" KIND-TOP endof
      drop E-TBL-BOUNDS throw
   endcase ;

: ROW-MATCH? ( ptr u8 n ptr u8 n n n -- bool )
   {: pa:ptr pu:n na:ptr nu:n kind:n k:n :}
   k ROW
   {: rpa:ptr rpu:n rna:ptr rnu:n rca:ptr rcu:n roa:ptr rou:n rkind:n :}
   rca rcu 2drop
   roa rou 2drop
   kind rkind = dup 0= if exit then drop
   pa pu rpa rpu STR= dup 0= if exit then drop
   na nu rna rnu STR= ;

: MATCH-KIND? ( ptr u8 n ptr u8 n n -- bool )
   {: pa:ptr pu:n na:ptr nu:n kind:n :}
   ROW-N 0 ?do
      pa pu na nu kind i ROW-MATCH? dup if unloop exit then drop
   loop false ;

public

: COUNT ( -- n )
   ROW-N ;

: PATH$ ( n -- ptr u8 n )
   ROW {: pa:ptr pu:n na:ptr nu:n ca:ptr cu:n oa:ptr ou:n kind:n :}
   na nu 2drop ca cu 2drop oa ou 2drop kind drop
   pa pu ;

: NAME$ ( n -- ptr u8 n )
   ROW {: pa:ptr pu:n na:ptr nu:n ca:ptr cu:n oa:ptr ou:n kind:n :}
   pa pu 2drop ca cu 2drop oa ou 2drop kind drop
   na nu ;

: CLASS$ ( n -- ptr u8 n )
   ROW {: pa:ptr pu:n na:ptr nu:n ca:ptr cu:n oa:ptr ou:n kind:n :}
   pa pu 2drop na nu 2drop oa ou 2drop kind drop
   ca cu ;

: OWNER$ ( n -- ptr u8 n )
   ROW {: pa:ptr pu:n na:ptr nu:n ca:ptr cu:n oa:ptr ou:n kind:n :}
   pa pu 2drop na nu 2drop ca cu 2drop kind drop
   oa ou ;

: CHECK? ( n -- bool )
   ROW {: pa:ptr pu:n na:ptr nu:n ca:ptr cu:n oa:ptr ou:n kind:n :}
   pa pu 2drop na nu 2drop ca cu 2drop oa ou 2drop
   kind KIND-CHECK = ;

: TOP? ( n -- bool )
   ROW {: pa:ptr pu:n na:ptr nu:n ca:ptr cu:n oa:ptr ou:n kind:n :}
   pa pu 2drop na nu 2drop ca cu 2drop oa ou 2drop
   kind KIND-TOP = ;

: CHECK-MATCH? ( ptr u8 n ptr u8 n -- bool )
   KIND-CHECK MATCH-KIND? ;

: TOP-MATCH? ( ptr u8 n ptr u8 n -- bool )
   KIND-TOP MATCH-KIND? ;

;package
