\ bad.f — words the checker REJECTS. Each carries a \ rejected: reason. Feeding
\ any of these through verify mode (CHECK!) yields 0 (rejected); the word is
\ dropped, never defined. The lesson: FIX THE BODY, not the signature.

\ rejected: body leaves two i64 ( dup => i64 i64 ), sig declares one out.
: BAD1 ( i64 -- i64 ) dup ;

\ rejected: >r pushes to the return stack and never pops — the return stack is
\ left unbalanced (ANS 3.2.3.3). Balance is enforced even in infer mode.
: BAD2 ( i64 -- ) >r ;

\ rejected: 1.5 is a float (r); + is the integer add ( n n -- n ). A float
\ flowing into an int op cannot unify — a type error, not just arity.
: BAD3 ( i64 -- i64 ) 1.5 + ;

\ rejected: body consumes its input and leaves nothing ( i64 -- ), sig declares
\ one out.
: BAD4 ( i64 -- i64 ) drop ;

\ rejected: an empty body is the identity ( u8 -- u8 ); u8 and u32 are distinct
\ concrete widths and do not unify.
: BAD5 ( u8 -- u32 ) ;

\ rejected: + + needs three inputs but the sig supplies two — the second + has
\ no operand (stack underflow against the declared row).
: BAD6 ( i64 i64 -- i64 ) + + ;
