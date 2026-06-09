\ t-db.fs — chart an effect, look it back up. Assumes repr+render+sigparse+db.

\ chart DUP's effect, retrieve canonical scheme string
: CHART-ZD   ARENA-RESET  s" R a -- R a a" PARSE-SIG  s" ZD" CHART ;
T{ CHART-ZD  s" ZD" EFFECT-OF  s" R a -- R a a" compare -> 0 }T

\ a second, distinct entry
: CHART-ZA  ARENA-RESET  s" R i64 i64 -- R i64" PARSE-SIG  s" ZA" CHART ;
T{ CHART-ZA  s" ZA" EFFECT-OF  s" R i64 i64 -- R i64" compare -> 0 }T
\ earlier entry still intact
T{ s" ZD" EFFECT-OF  s" R a -- R a a" compare -> 0 }T

\ unknown name -> 0
T{ s" NOPE" EFFECT-OF -> 0 }T

\ stored scheme re-parses (INST) to a usable effect
T{ ARENA-RESET  s" ZD" EFFECT-OF PARSE-SIG  EFF>DOUT STACK-REST STACK-TOP TYVAR? -> true }T
