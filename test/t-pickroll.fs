\ t-pickroll.fs — literal-argument PICK/ROLL fold to a concrete shuffle; dynamic
\ index stays untypeable (E-UNCHECKED). Loaded by all.fs (after caf.fs).

\ N PICK copies the N-deep item:  0 PICK = DUP, 1 PICK = OVER, 2 PICK = third
: P-DUP   s" PDUP"  s" R a -- R a a"       s" 0 PICK" CHECK-DEF ;
T{ ' P-DUP   catch -> 0 }T
: P-OVER  s" POVER" s" R a b -- R a b a"   s" 1 PICK" CHECK-DEF ;
T{ ' P-OVER  catch -> 0 }T
: P-3RD   s" P3RD"  s" R a b c -- R a b c a" s" 2 PICK" CHECK-DEF ;
T{ ' P-3RD   catch -> 0 }T

\ N ROLL rotates the N-deep item up:  1 ROLL = SWAP, 2 ROLL = ROT
: R-SWAP  s" RSWAP" s" R a b -- R b a"     s" 1 ROLL" CHECK-DEF ;
T{ ' R-SWAP  catch -> 0 }T
: R-ROT   s" RROT"  s" R a b c -- R b c a" s" 2 ROLL" CHECK-DEF ;
T{ ' R-ROT   catch -> 0 }T

\ wrong arity is rejected: 2 PICK adds an item, so a no-growth output can't fit
: P-BAD   s" PBAD"  s" R a b c -- R a b c" s" 2 PICK" CHECK-DEF ;
T{ ' P-BAD   catch -> E-ARITY }T

\ dynamic (no literal before) PICK -> not foldable -> falls through to E-UNCHECKED
: P-DYN   s" PDYN"  s" R i64 a -- R a"     s" DROP PICK" CHECK-DEF ;
T{ ' P-DYN   catch -> E-UNCHECKED }T

\ out-of-range literal (> 25) -> not folded -> E-UNCHECKED
: P-BIG   s" PBIG"  s" R i64 -- R i64"     s" 99 PICK" CHECK-DEF ;
T{ ' P-BIG   catch -> E-UNCHECKED }T
