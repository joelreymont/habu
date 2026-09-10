\ combinators.f — higher-order library words baked into bin/hb.

: DIP ( R a [ R -- S ] -- S a )
   swap >r execute r> ;

: KEEP ( R a [ R a -- S ] -- S a )
   over >r execute r> ;

: BI ( R a [ R a -- R b ] [ R b a -- R b c ] -- R b c )
   >r KEEP r> execute ;

: TRI ( R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d )
   >r >r KEEP r> KEEP r> execute ;

: TIMES ( R i64 [ R -- R ] -- R )
   {: n:i64 q :}
   n 0 ?do q execute loop ;

: EACH ( R ptr a i64 [ R a -- R ] -- R )
   {: a n q :}
   n 0 ?do a i cells + @ q execute loop ;

: MAP ( R ptr a i64 [ R a -- R a ] -- R )
   {: a n q :}
   n 0 ?do a i cells + @ q execute a i cells + ! loop ;

: FOLD ( R ptr a i64 b [ R b a -- R b ] -- R b )
   {: a n acc q :}
   acc n 0 ?do a i cells + @ q execute loop ;
