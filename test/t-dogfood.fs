\ t-dogfood.fs — the codegen's stack-juggling bugs ARE catchable by caf. The
\ register allocator / EMIT-TOKEN are metaprogramming (search-wordlist, execute,
\ raw stacks) so they can't be fully checked, but the bug class that actually bit
\ the build — `s>number?` leaves its DOUBLE on the stack even on the false path,
\ and a branch that forgets it is unbalanced — IS caught once s>number? is
\ charted. Loaded by all.fs (after caf.fs). See LESSONS.md § Debugging.

\ s>number? : ( str -- i64 bool ).  Both IF arms must keep the i64 -> balanced.
: SN-OK   s" SNOK"  s" R str -- R i64" s" S>NUMBER? IF THEN" CHECK-DEF ;
T{ ' SN-OK  catch -> 0 }T

\ The real bug shape: one branch drops the value, the other keeps it -> the two
\ IF arms leave different stacks -> caf REFUSES the definition.
: SN-BAD  s" SNBAD" s" R str -- R i64" s" S>NUMBER? IF DROP THEN" CHECK-DEF ;
T{ ' SN-BAD catch -> E-BRANCH }T
