\ t-locals.fs — typed locals via CHECK-DEF. ( -- ) words for clean catch.

\ {: a b :} pops two; body `a b` pushes them back in a-then-b order.
: L-AB s" LAB" s" R i64 i64 -- R i64 i64" s" {: a b :} a b" CHECK-DEF ;
T{ ' L-AB catch -> 0 }T

\ swapped use with distinct types: body `b a` reverses order -> mismatch.
: L-SWAP s" LSWAP" s" R i64 bool -- R i64 bool" s" {: a b :} b a" CHECK-DEF ;
T{ ' L-SWAP catch -> E-MISMATCH }T

\ single untyped local round-trips its type.
: L-ONE s" LONE" s" R i64 -- R i64" s" {: a :} a" CHECK-DEF ;
T{ ' L-ONE catch -> 0 }T

\ typed local matching the input type: ok.
: L-TY s" LTY" s" R i64 -- R i64" s" {: x:i64 :} x" CHECK-DEF ;
T{ ' L-TY catch -> 0 }T

\ typed local conflicting with the input type -> E-MISMATCH.
: L-TYBAD s" LTYBAD" s" R i64 -- R i64" s" {: x:bool :} x" CHECK-DEF ;
T{ ' L-TYBAD catch -> E-MISMATCH }T

\ `{ a -- r }` brace form: name before --, output name after is a comment.
: L-BRACE s" LBRACE" s" R i64 -- R i64" s" { a -- r } a" CHECK-DEF ;
T{ ' L-BRACE catch -> 0 }T

\ undeclared name routes to the checker's E-UNKNOWN (hook returns false).
: L-UNK s" LUNK" s" R i64 -- R i64" s" zz" CHECK-DEF ;
T{ ' L-UNK catch -> E-UNKNOWN }T

\ table does not leak: second def cannot see the first def's names.
: L-LEAK s" LLEAK" s" R i64 -- R i64" s" a" CHECK-DEF ;
T{ ' L-LEAK catch -> E-UNKNOWN }T

\ scope holds for repeated uses: each name push is independent.
: L-USE s" LUSE" s" R i64 i64 -- R i64 i64 i64 i64" s" {: a b :} a b a b" CHECK-DEF ;
T{ ' L-USE catch -> 0 }T
