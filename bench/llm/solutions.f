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
: AOT-STR ( -- i64 ) s" hi" nip [char] 0 + ;                  \ AOT-safe string length + char literal
: CALL-TWICE ( i64 [ i64 -- i64 ] -- i64 ) {: x q :} x q execute q execute ;  \ execute one quotation twice
: R-KEEP2 ( i64 -- i64 ) dup >r dup + r> + ;                  \ double n while preserving n on return stack
: ROW-DUP ( R x -- R x x ) dup ;                              \ duplicate the top item over any stack row
: UNTIL5 ( i64 -- i64 ) 0 swap 0 ?do i 5 = if leave then 1+ loop ;  \ count up to min(n,5)
: MEM-SWAPCELL ( i64 i64 -- i64 i64 ) here {: p :} 16 allot p ! p cell+ ! p @ p cell+ @ ;  \ swap through data memory
: TRI ( x [ x -- a ] [ x -- b ] [ x -- c ] -- a b c ) {: x q1 q2 q3 :} x q1 execute x q2 execute x q3 execute ;  \ apply three quotations to x
: DIAG-REMOVE-PRODUCER ( i64 -- i64 ) dup * ;                \ corrected remove_producer diagnostic bait
: DIAG-ADD-PRODUCER ( i64 -- i64 ) 1 + ;                     \ corrected add_producer diagnostic bait
: DIAG-FIX-TYPE ( i64 -- i64 ) 0= if 1 else 0 then ;         \ bool converted to numeric result
: DIAG-FIX-RSTACK ( i64 -- ) >r r> drop ;                    \ balanced return-stack traffic, no data result
: DIAG-TRUSTED-BOUNDARY ( -- i64 ) 42 ;                      \ modeled checked code, no unsafe evaluation
: DIAG-TRUST-BOUNDARY ( -- i64 ) 42 ;                        \ modeled checked code, no TRUST declaration
: DIAG-SET-CHECK-BOUNDARY ( -- i64 ) 42 ;                    \ modeled checked code, no hook replacement
: DIAG-SIGNATURE-SYNTAX ( i64 -- i64 ) 1 + ;                 \ exact checked signature syntax
: DIAG-REWRITE-UNCHECKABLE ( i64 -- i64 ) dup * ;            \ modeled checked stack code
