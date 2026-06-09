\ t-quots.fs — quotation literals, tick, combinators via CHECK-DEF.

\ a quotation executed: [: 1+ ;] EXECUTE  on an i64
: Q-EXEC s" QE" s" R i64 -- R i64" s" [: 1+ ;] EXECUTE" CHECK-DEF ;
T{ ' Q-EXEC catch -> 0 }T

\ DIP runs the quotation below the top, restoring it
: Q-DIP  s" QD" s" R i64 i64 -- R i64 i64" s" [: 1+ ;] DIP" CHECK-DEF ;
T{ ' Q-DIP catch -> 0 }T

\ tick a charted word and execute it:  ' DUP EXECUTE  ~ DUP
: Q-TICK s" QT" s" R a -- R a a" s" ' DUP EXECUTE" CHECK-DEF ;
T{ ' Q-TICK catch -> 0 }T

\ ['] form too
: Q-BTICK s" QBT" s" R a -- R a a" s" ['] DUP EXECUTE" CHECK-DEF ;
T{ ' Q-BTICK catch -> 0 }T

\ effect mismatch: quotation result doesn't fit declared output
: Q-BAD  s" QBAD" s" R i64 -- R bool" s" [: 1+ ;] EXECUTE" CHECK-DEF ;
T{ ' Q-BAD catch -> E-MISMATCH }T

\ tick an unknown word -> E-UNCHECKED
: Q-UK   s" QUK" s" R -- R" s" ' NOSUCH EXECUTE" CHECK-DEF ;
T{ ' Q-UK catch -> E-UNCHECKED }T
