\ solutions.f — reference answer key for the habu LLM benchmark. Each definition
\ is checked: run.sh proves all tasks typecheck (CHECK! => -1 certified).
\ Categories: arithmetic, control flow, locals, polymorphic stack, loops,
\ return stack, quotations/combinators, stack introspection, memory, strings,
\ files, processes, time, recursion, parser edge cases.

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
: INC    ( i64 -- i64 ) 1 + ;                                  \ n+1
: DEC    ( i64 -- i64 ) 1 - ;                                  \ n-1
: DOUBLE ( i64 -- i64 ) dup + ;                                \ n*2
: EVEN?  ( i64 -- bool ) 1 and 0 = ;                           \ true iff even
: BETWEEN? ( i64 i64 i64 -- bool ) {: x lo hi :} x lo >= x hi <= and ;  \ lo <= x <= hi
: SUMSQ  ( i64 i64 -- i64 ) {: a b :} a dup * b dup * + ;      \ a^2+b^2
: COMPOSE2 ( i64 [ i64 -- i64 ] [ i64 -- i64 ] -- i64 ) {: x q1 q2 :} x q1 execute q2 execute ;  \ q2(q1(x))
: DEPTHNOW ( R -- R n ) depth ;                                \ report current stack depth
: HAS2? ( R -- R bool ) depth 2 >= ;                           \ true iff at least two cells live
: ADDDEPTH ( R i64 -- R i64 ) depth + ;                        \ add current depth to top value
: MEMCELL ( i64 -- i64 ) here {: p :} 8 allot p ! p @ ;        \ store and fetch one cell
: BYTECELL ( u8 -- u8 ) here {: p :} 1 allot p c! p c@ ;       \ store and fetch one byte
: STRLEN ( -- i64 ) s" habu" nip ;                             \ string literal length
: FIRSTCH ( -- u8 ) s" habu" drop c@ ;                         \ first byte of a string literal
: TWOCELLS ( i64 -- i64 ) here {: p :} 16 allot p ! 99 p cell+ ! p @ p cell+ @ + ;  \ two-cell access
: MONO? ( -- bool ) mono-ns mono-ns <= ;                       \ monotonic clock ordering
: TRUE-RC ( -- i64 ) s" /usr/bin/true" path0 -1 -1 -1 spawn-io wait-rc ;  \ run a process and report rc
: TRUE-EXISTS? ( -- bool ) s" /usr/bin/true" path0 0 access 0= ;  \ path exists
: STATTRUE ( -- bool ) here {: st :} 144 allot s" /usr/bin/true" path0 st stat64 0= ;  \ stat a path
: FIB ( i64 -- i64 ) dup 2 < if exit then dup 1 - recurse swap 2 - recurse + ;  \ recursive fibonacci
: SEVEN-CHAR ( -- i64 ) [char] 7 ;                             \ parse-next word with a digit char
