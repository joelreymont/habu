\ combinators.f — higher-order library words baked into bin/hb.

: DIP ( R a [ R -- S ] -- S a )
   swap >r execute r> ;

: KEEP ( R a [ R a -- S ] -- S a )
   over >r execute r> ;

\ BI/TRI and the iterators must keep quotations available across calls. Modeling
\ that in checked code would require recursive quotation types, so these are
\ audited TRUSTED: boundaries covered by
\ engine-suite plus the native gate runner.
TRUSTED: BI ( R a [ R a -- R b ] [ R b a -- R b c ] -- R b c )
   >r KEEP r> execute ;

TRUSTED: TRI ( R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d )
   >r >r KEEP r> KEEP r> execute ;

TRUSTED: TIMES ( R i64 [ R -- R ] -- R )
   >r 0 ?do r@ execute loop r> drop ;

TRUSTED: EACH ( R ptr a i64 [ R a -- R ] -- R )
   {: a n q :}
   n 0 ?do a i cells + @ q execute loop ;

TRUSTED: MAP ( R ptr a i64 [ R a -- R a ] -- R )
   {: a n q :}
   n 0 ?do a i cells + dup @ q execute swap ! loop ;

TRUSTED: FOLD ( R ptr a i64 b [ R b a -- R b ] -- R b )
   {: a n acc q :}
   acc n 0 ?do a i cells + @ q execute loop ;
