\ t-checker.fs — CHECK-DEF: composition, classification, generalize-on-store.
\ Each scenario is a ( -- ) word so ' W catch yields a clean code.

\ SQUARE ( i64 -- i64 ) DUP *  — passes, gets charted
: DEF-SQUARE  s" SQUARE" s" R i64 -- R i64" s" DUP *" CHECK-DEF ;
T{ ' DEF-SQUARE catch -> 0 }T
T{ s" SQUARE" EFFECT-OF s" R i64 -- R i64" compare -> 0 }T

\ HYP2 uses the just-charted SQUARE  (a*a + b*b)
: DEF-HYP2  s" HYP2" s" R i64 i64 -- R i64" s" SQUARE SWAP SQUARE +" CHECK-DEF ;
T{ ' DEF-HYP2 catch -> 0 }T

\ polymorphic ID ( a -- a ) with empty body; then used at TWO different types
\ (generalize-on-store regression: must re-instantiate fresh per call)
: DEF-ID   s" ID" s" R a -- R a" s" " CHECK-DEF ;
T{ ' DEF-ID catch -> 0 }T
: USE-ID-I  s" FI" s" R i64 -- R i64" s" ID" CHECK-DEF ;
: USE-ID-B  s" FB" s" R bool -- R bool" s" ID" CHECK-DEF ;
T{ ' USE-ID-I catch -> 0 }T
T{ ' USE-ID-B catch -> 0 }T

\ return-stack round trip ( a -- a ) via >R R>
: DEF-RT  s" RT" s" R a -- R a" s" >R R>" CHECK-DEF ;
T{ ' DEF-RT catch -> 0 }T

\ FAIL: declared too few inputs -> underflow surfaces as occurs at final unify
: DEF-UF  s" UF" s" R -- R" s" DROP" CHECK-DEF ;
T{ ' DEF-UF catch -> E-UNDERFLOW }T

\ FAIL: type mismatch ( bool -- i64 ) 1+
: DEF-MM  s" MM" s" R bool -- R i64" s" 1+" CHECK-DEF ;
T{ ' DEF-MM catch -> E-MISMATCH }T

\ FAIL: unknown word
: DEF-UK  s" UK" s" R -- R" s" NOSUCHWORD" CHECK-DEF ;
T{ ' DEF-UK catch -> E-UNKNOWN }T

\ FAIL: wrong output arity ( i64 -- i64 ) but body leaves two -> DUP alone
: DEF-AR  s" AR" s" R i64 -- R i64" s" DUP" CHECK-DEF ;
T{ ' DEF-AR catch -> E-ARITY }T

\ case-insensitive: uppercase type name I64, lowercase primitives dup *
: DEF-CI  s" CI" s" R I64 -- R I64" s" dup *" CHECK-DEF ;
T{ ' DEF-CI catch -> 0 }T
\ lowercase forbidden word still caught
: DEF-EV  s" EV" s" R -- R" s" evaluate" CHECK-DEF ;
T{ ' DEF-EV catch -> E-UNSAFE }T

\ F5 regression: a depth-neutral body that needs an input the declaration omits
\ ( -- ) 1+ must be REJECTED (the declared prefix is rigid, can't be extended)
: DEF-MD  s" MD" s" R -- R" s" 1+" CHECK-DEF ;
T{ ' DEF-MD catch -> E-UNDERFLOW }T
