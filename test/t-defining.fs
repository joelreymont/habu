\ t-defining.fs — TRUSTED: charts an effect without checking the body and still
\ defines a runnable word; CHK-CONSTANT / CHK-VARIABLE chart the right effect and
\ run the native defining word. Assumes config..checker..defining are loaded.

\ --- TRUSTED: charts the declared effect and defines a real word ----------
TRUSTED: FOO ( R a -- R a a ) dup ;

\ effect is charted, canonical scheme matches, lookup is non-zero
T{ s" FOO" EFFECT-OF nip 0> -> true }T
T{ s" FOO" EFFECT-OF s" R a -- R a a" compare -> 0 }T

\ the defined word actually runs: 5 FOO leaves 5 5, so 5 FOO + = 10
T{ 5 FOO + -> 10 }T
T{ 7 FOO -> 7 7 }T

\ --- CHK-CONSTANT runs gforth constant AND charts "R -- R i64" -----------
42 CHK-CONSTANT ANSWER

T{ s" ANSWER" EFFECT-OF nip 0> -> true }T
T{ s" ANSWER" EFFECT-OF s" R -- R i64" compare -> 0 }T
T{ ANSWER -> 42 }T

\ --- CHK-VARIABLE runs gforth variable AND charts "R -- R ptr i64" ------
CHK-VARIABLE MYVAR

T{ s" MYVAR" EFFECT-OF nip 0> -> true }T
T{ s" MYVAR" EFFECT-OF s" R -- R ptr i64" compare -> 0 }T
\ the variable is a usable cell: store then fetch round-trips
T{ 99 MYVAR ! MYVAR @ -> 99 }T

\ --- a second TRUSTED: with a multi-word body charts and runs correctly --
TRUSTED: ADD3 ( R i64 -- R i64 ) 1 + 1 + 1 + ;
T{ s" ADD3" EFFECT-OF s" R i64 -- R i64" compare -> 0 }T
T{ 10 ADD3 -> 13 }T
