\ good.f — idiomatic CHECKED words. Each carries an explicit typed ( in -- out );
\ each is accepted by the checker (CHECK! => -1 certified). UPPER-CASE per
\ convention; built-in gforth words stay lower-case.

: SQUARE ( i64 -- i64 ) dup * ;                        \ 7 SQUARE => 49

: CUBE   ( i64 -- i64 ) dup dup * * ;                  \ 3 CUBE => 27

: ABSV   ( i64 -- i64 ) dup 0 < if negate then ;       \ -5 ABSV => 5

: NEG?   ( i64 -- bool ) 0 < ;                         \ -5 NEG? => -1 (true)

: SUM3   ( i64 i64 i64 -- i64 ) + + ;                  \ 1 2 3 SUM3 => 6

\ {: a b :} locals replace stack juggling — read top-to-bottom, no rot/pick.
: AVG2   ( i64 i64 -- i64 ) {: a b :} a b + 2 / ;      \ 10 20 AVG2 => 15

: MAX2   ( i64 i64 -- i64 ) {: a b :} a b > if a else b then ;   \ 3 9 MAX2 => 9

\ a branch that consumes its input on one arm and falls through on the other —
\ both arms must leave the same stack ( i64 ), and they do.
: CLAMP0 ( i64 -- i64 ) dup 0 < if drop 0 then ;       \ -7 CLAMP0 => 0, 4 => 4

\ polymorphic: type vars a b — SWAP2 works for any two cells.
: SWAP2  ( a b -- b a ) swap ;

\ a counted loop; ?do/loop joins are typed.
: SUMTO  ( i64 -- i64 ) 0 swap 1+ 1 ?do i + loop ;     \ 5 SUMTO => 15 (1..5)

\ quotation: [: … ;] builds a quot, execute applies it.
: TWICE  ( i64 -- i64 ) [: dup + ;] execute ;          \ 21 TWICE => 42

\ a quotation PARAMETER, typed [ i64 -- i64 ]; the caller passes a quote.
: APPLY  ( i64 [ i64 -- i64 ] -- i64 ) execute ;       \ 5 [: dup * ;] APPLY => 25

\ a combinator whose quot sub-sig is RECORDED, so call sites are checked:
: DIP    swap >r execute r> ;
: ADD10  ( i64 i64 -- i64 ) [: 10 + ;] dip + ;         \ 5 7 ADD10 => 22
