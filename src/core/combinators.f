\ combinators.f — higher-order library words baked into bin/hb.

: DIP ( R a [ R -- S ] -- S a )
   swap >r execute r> ;

: KEEP ( R a [ R a -- S ] -- S a )
   over >r execute r> ;

: BI ( R a [ R a -- R b ] [ R b a -- R b c ] -- R b c )
   >r KEEP r> execute ;

: TRI ( R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d )
   >r >r KEEP r> KEEP r> execute ;

\ The loop iterators re-execute a stored quotation (r@/local fetch per
\ iteration); typing that needs the multishot-quotation capability
\ (dot habu-multishot-quotations-typed-8832cace), so these stay audited
\ TRUSTED: boundaries covered by engine-suite plus the native gate runner.

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
