\ defining.fs — top-level defining words that register effects in the DB.
\ TRUSTED: charts a declared effect for a word WITHOUT checking its body, then
\ compiles the body as an ordinary Forth definition. CHK-CONSTANT / CHK-VARIABLE
\ run gforth's native constant/variable AND chart the corresponding effect.
\ Depends on: config arena sigparse db (CHART) and gforth core (parse, evaluate).

\ --- scratch buffers (parse-name/parse addresses die at the next parse) ---
256 constant DEF-NAME-MAX
256 constant DEF-BODY-MAX
create DEF-NAME  DEF-NAME-MAX chars allot   variable DEF-NAME-LEN
create DEF-BODY  DEF-BODY-MAX chars allot   variable DEF-BODY-LEN
create DEF-EVAL  512 chars allot            \ ": NAME body ;" assembly buffer

: DEF-NAME!  ( c-addr u -- )   \ copy a name token out of transient parse space
   DEF-NAME-MAX min dup DEF-NAME-LEN !  DEF-NAME swap  move ;
: DEF-NAME@  ( -- c-addr u )   DEF-NAME  DEF-NAME-LEN @ ;
: DEF-BODY!  ( c-addr u -- )   \ copy the body text out of transient parse space
   DEF-BODY-MAX min dup DEF-BODY-LEN !  DEF-BODY swap  move ;
: DEF-BODY@  ( -- c-addr u )   DEF-BODY  DEF-BODY-LEN @ ;

\ Append c-addr u to the eval buffer at offset off, returning the new offset.
: +EVAL  ( off c-addr u -- off' )
   {: off ca cu :}  ca  DEF-EVAL off chars +  cu  move  off cu + ;

\ Assemble ": NAME body ;" into DEF-EVAL and run it, defining NAME for real.
: DEFINE-BODY  ( name-a name-u body-a body-u -- )
   {: na nu ba bu :}
   0                          ( off )
   s" : "    +EVAL
   na nu     +EVAL
   s"  "     +EVAL
   ba bu     +EVAL
   s"  ;"    +EVAL            ( off' )
   DEF-EVAL swap  evaluate ;

\ TRUSTED: NAME ( eff ) ... ;  — chart eff under NAME without checking the body,
\ then compile the body as an ordinary definition. Parsing word, top level only.
: TRUSTED:  ( -- )      \ parses: NAME eff-in-parens body ;
   parse-name DEF-NAME!            \ copy the name out immediately
   parse-name s" (" compare if E-BADTYPE throw then   \ next token must be (
   [char] ) parse  DEF-BODY!       \ effect text between ( and ) -> reuse buffer
   ARENA-RESET
   DEF-BODY@ PARSE-SIG {: eff :}
   eff DEF-NAME@ CHART             \ register the trusted effect
   [char] ; parse  DEF-BODY!       \ body text up to ; (consumed)
   DEF-NAME@ DEF-BODY@ DEFINE-BODY ;

\ --- constant / variable charting -------------------------------------------
\ A constant pushes its i64 value; a variable pushes a cell-pointer. Charted
\ under the given name so checked callers can use them.
: CHART-CONST  ( c-addr u -- )  \ chart "R -- R i64" under name
   {: na nu :}  ARENA-RESET
   s" R -- R i64"      PARSE-SIG  na nu CHART ;
: CHART-VAR    ( c-addr u -- )  \ chart "R -- R ptr i64" under name
   {: na nu :}  ARENA-RESET
   s" R -- R ptr i64" PARSE-SIG  na nu CHART ;

\ Top-level conveniences: run the native defining word AND chart the effect.
\ Avoid redefining gforth's constant/variable; capture the name before constant
\ consumes it.
: CHK-CONSTANT  ( n "name" -- )
   parse-name DEF-NAME!
   DEF-NAME@ nextname  constant     \ native: define the constant
   DEF-NAME@ CHART-CONST ;

: CHK-VARIABLE  ( "name" -- )
   parse-name DEF-NAME!
   DEF-NAME@ nextname  variable      \ native: define the variable
   DEF-NAME@ CHART-VAR ;
