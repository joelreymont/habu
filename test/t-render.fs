\ t-render.fs — RENDER-EFFECT canonical text. Assumes config+arena+types+rows
TV-CLEAR RV-CLEAR ARENA-RESET   \ clear stale bindings from earlier suites
\ +effects-repr+render loaded. Effects built by hand; trivial return rows share
\ one row-var id so the | clause is suppressed.

\ R0 i64 -- R0 i64  (trivial return: rin=rout=R1)
: E-ID ( -- c-addr u )
   ARENA-RESET
   0 MK-ROW TC-I64 MK-CON MK-PUSH        \ din  = R0 , i64
   0 MK-ROW TC-I64 MK-CON MK-PUSH        \ dout = R0 , i64
   1 MK-ROW 1 MK-ROW MK-EFFECT           \ rin=rout=R1
   RENDER-EFFECT ;
T{ E-ID  s" R i64 -- R i64" compare -> 0 }T

\ polymorphic: R0 a -- R0 a a  (first-appearance naming, var reused -> same 'a')
: E-DUP ( -- c-addr u )
   ARENA-RESET  TV-RESET 1 TV-ALLOC drop
   0 MK-ROW 0 MK-VAR MK-PUSH                          \ din  = R0 , a
   0 MK-ROW 0 MK-VAR MK-PUSH 0 MK-VAR MK-PUSH         \ dout = R0 , a , a
   1 MK-ROW 1 MK-ROW MK-EFFECT
   RENDER-EFFECT ;
T{ E-DUP  s" R a -- R a a" compare -> 0 }T

\ two distinct vars name a then b by first appearance
: E-TWO ( -- c-addr u )
   ARENA-RESET  TV-RESET 2 TV-ALLOC drop
   0 MK-ROW 0 MK-VAR MK-PUSH 1 MK-VAR MK-PUSH         \ din  = R0 , a , b
   0 MK-ROW 1 MK-VAR MK-PUSH 0 MK-VAR MK-PUSH         \ dout = R0 , b , a
   1 MK-ROW 1 MK-ROW MK-EFFECT
   RENDER-EFFECT ;
T{ E-TWO  s" R a b -- R b a" compare -> 0 }T

\ ptr<a>: R0 ptr<a> -- R0 a   renders ptr prefix
: E-PTR ( -- c-addr u )
   ARENA-RESET  TV-RESET 1 TV-ALLOC drop
   0 MK-ROW  0 MK-VAR MK-PTR MK-PUSH                  \ din  = R0 , ptr a
   0 MK-ROW  0 MK-VAR MK-PUSH                         \ dout = R0 , a
   1 MK-ROW 1 MK-ROW MK-EFFECT
   RENDER-EFFECT ;
T{ E-PTR  s" R ptr a -- R a" compare -> 0 }T

\ quotation on the stack: R0 quot<( R1 i64 -- R1 i64 )> -- R0
: Q-QUOT ( -- c-addr u )
   ARENA-RESET
   1 MK-ROW TC-I64 MK-CON MK-PUSH                     \ inner din  = R1 , i64
   1 MK-ROW TC-I64 MK-CON MK-PUSH                     \ inner dout = R1 , i64
   2 MK-ROW 2 MK-ROW MK-EFFECT MK-QUOT {: q :}        \ inner ret trivial -> [ R i64 -- R i64 ]
   0 MK-ROW q MK-PUSH                                 \ din  = R0 , quot
   0 MK-ROW                                           \ dout = R0
   3 MK-ROW 3 MK-ROW MK-EFFECT
   RENDER-EFFECT ;
T{ Q-QUOT  s" R [ S i64 -- S i64 ] -- R" compare -> 0 }T

\ non-trivial return clause: R0 a | R1 -- R0 | R1 a   ( >R-like )  appends |
: E-RET ( -- c-addr u )
   ARENA-RESET  TV-RESET 1 TV-ALLOC drop
   0 MK-ROW 0 MK-VAR MK-PUSH                          \ din  = R0 , a
   0 MK-ROW                                           \ dout = R0
   1 MK-ROW                                           \ rin  = R1
   1 MK-ROW 0 MK-VAR MK-PUSH                          \ rout = R1 , a
   MK-EFFECT
   RENDER-EFFECT ;
T{ E-RET  s" R a -- R | S -- S a" compare -> 0 }T

\ buffer reused across calls: ' RENDER-EFFECT on a ( -- ) wrapper throws nothing
: W-RENDER  ARENA-RESET 0 MK-ROW 0 MK-ROW 1 MK-ROW 1 MK-ROW MK-EFFECT RENDER-EFFECT 2drop ;
T{ ' W-RENDER catch -> 0 }T
