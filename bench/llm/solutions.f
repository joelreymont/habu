\ solutions.f — reference answer key for the habu LLM benchmark. Each is a
\ checked definition: `run.sh` proves all 15 typecheck (CHECK! => -1 certified).
\ The spec on each line is the prompt a model is given (without the body).
\ Categories: arithmetic, control flow, locals, polymorphic stack, loops,
\ return stack, quotations/combinators, recursion-as-loop.

: SQUARE ( i64 -- i64 ) dup * ;                                  \ n -> n*n
: CUBE   ( i64 -- i64 ) dup dup * * ;                            \ n -> n*n*n
: ABSV   ( i64 -- i64 ) dup 0 < if negate then ;                 \ |n|
: NEG?   ( i64 -- bool ) 0 < ;                                   \ n -> n<0
: CLAMP0 ( i64 -- i64 ) dup 0 < if drop 0 then ;                 \ max(n,0)
: SUM3   ( i64 i64 i64 -- i64 ) + + ;                            \ a+b+c
: AVG2   ( i64 i64 -- i64 ) {: a b :} a b + 2 / ;                \ (a+b)/2 with locals
: MAX2   ( i64 i64 -- i64 ) {: a b :} a b > if a else b then ;   \ larger of a,b
: SWAP2  ( a b -- b a ) swap ;                                   \ polymorphic exchange
: ROT3   ( a b c -- b c a ) rot ;                               \ polymorphic 3-rotate
: SUMTO  ( i64 -- i64 ) 0 swap 1+ 1 ?do i + loop ;              \ 1+2+...+n
: FACT   ( i64 -- i64 ) 1 swap 1+ 1 ?do i * loop ;             \ n!
: KEEP1  ( i64 -- i64 ) dup >r 10 * r> + ;                      \ n*10+n via the return stack
: TWICE  ( i64 -- i64 ) [: dup + ;] execute ;                   \ apply a quotation
: APPLY  ( i64 [ i64 -- i64 ] -- i64 ) execute ;                \ a quotation parameter
\ harder: deeper control flow, more locals, combinators, return-stack
: MIN2   ( i64 i64 -- i64 ) {: a b :} a b < if a else b then ;  \ smaller of a,b
: SIGNUM ( i64 -- i64 ) dup 0 > if drop 1 else 0 < if -1 else 0 then then ;  \ sign of n
: 2DUP2  ( a b -- a b a b ) over over ;                         \ duplicate the top pair
: POW    ( i64 i64 -- i64 ) {: b e :} 1 e 0 ?do b * loop ;      \ b raised to e
: COUNTDOWN ( i64 -- i64 ) 0 swap 0 ?do 1+ loop ;             \ count up to n (== n)
: DIP    ( R x [ R -- S ] -- S x ) {: x q :} q execute x ;     \ run q under the top item
: KEEP   ( x [ x -- a ] -- a x ) {: x q :} x q execute x ;     \ run q on x, keep x
: BI     ( x [ x -- a ] [ x -- b ] -- a b ) {: x q1 q2 :} x q1 execute x q2 execute ;  \ apply two quots to x
