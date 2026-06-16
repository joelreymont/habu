\ combinators.f — higher-order library words baked into bin/hb.

: DIP ( R a [ R -- S ] -- S a )
   swap >r execute r> ;
s" DIP" s" R a [ R -- S ] -- S a" TRUST

: KEEP ( R a [ R a -- S ] -- S a )
   over >r execute r> ;
s" KEEP" s" R a [ R a -- S ] -- S a" TRUST

\ BI/TRI and the iterators must keep quotations available across calls. Modeling
\ that in checked code would require recursive quotation types, so these are
\ audited boundaries with call-site signatures recorded by TRUST and covered by
\ hb-suite/run.sh.
0 set-check
: BI
   >r KEEP r> execute ;
s" BI" s" R a [ R a -- R b ] [ R b a -- R b c ] -- R b c" TRUST

: TRI
   >r >r KEEP r> KEEP r> execute ;
s" TRI" s" R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d" TRUST

: TIMES
   >r 0 ?do r@ execute loop r> drop ;
s" TIMES" s" R i64 [ R -- R ] -- R" TRUST

: EACH {: a n q :}
   n 0 ?do a i cells + @ q execute loop ;
s" EACH" s" R addr i64 [ R i64 -- R ] -- R" TRUST

: MAP {: a n q :}
   n 0 ?do a i cells + dup @ q execute swap ! loop ;
s" MAP" s" R addr i64 [ R i64 -- R i64 ] -- R" TRUST

: FOLD {: a n acc q :}
   acc n 0 ?do a i cells + @ q execute loop ;
s" FOLD" s" R addr i64 b [ R b i64 -- R b ] -- R b" TRUST

' HOOK set-check
