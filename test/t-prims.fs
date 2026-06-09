\ t-prims.fs — the closed v1 primitive table is charted and looked up.
\ Assumes config+arena+types+rows+effects-repr+render+sigparse+db+prims loaded.
\ EFFECT-OF returns ( sa su ) for a charted name, or ( 0 ) for an unknown one.
\ For a charted word the length su is nonzero; CHARTED? drops the addr and asks
\ "is the scheme length zero?" -- false for every present primitive.

: CHARTED?  ( c-addr u -- f )  EFFECT-OF nip 0= ;

\ --- representative spread is present ( EFFECT-OF length nonzero -> false ) ---
T{ s" DUP"     CHARTED? -> false }T
T{ s" SWAP"    CHARTED? -> false }T
T{ s" +"       CHARTED? -> false }T
T{ s" ="       CHARTED? -> false }T
T{ s" @"       CHARTED? -> false }T
T{ s" >R"      CHARTED? -> false }T
T{ s" EXECUTE" CHARTED? -> false }T
T{ s" DIP"     CHARTED? -> false }T
T{ s" CR"      CHARTED? -> false }T
T{ s" [CHAR]"  CHARTED? -> false }T

\ --- ?DUP is deliberately excluded ( value-dependent, not statically typeable ) ---
T{ s" ?DUP" EFFECT-OF -> 0 }T

\ --- canonical scheme round-trips ( stored text is canonical ) ---
T{ s" DUP" EFFECT-OF s" R a -- R a a"        compare -> 0 }T
T{ s" +"   EFFECT-OF s" R i64 i64 -- R i64"  compare -> 0 }T
T{ s" @"   EFFECT-OF s" R ptr a -- R a"      compare -> 0 }T
T{ s" >R"  EFFECT-OF s" R a -- R | S -- S a" compare -> 0 }T
T{ s" DIP" EFFECT-OF s" R a [ R -- S ] -- S a" compare -> 0 }T

\ --- a charted scheme re-parses cleanly ( INST yields a usable effect ) ---
: REPARSE-DUP  ( -- f )
   ARENA-RESET  s" DUP" EFFECT-OF PARSE-SIG
   EFF>DOUT STACK-TOP TYVAR? ;
T{ REPARSE-DUP -> true }T
