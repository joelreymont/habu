\ finally is the Gforth host's record 0: a checked word calls it and ticks it.
: BODY ( -- ) ." body" cr ;
: CLEAN ( -- ) ." clean" cr ;
: BOOM ( -- ) ." boom" cr 7 throw ;
: T ( -- n ) ['] BODY ['] CLEAN finally ['] finally ;
: U ( -- n ) [: ['] BOOM ['] CLEAN finally ;] catch ;
T ' finally = . cr U . cr
