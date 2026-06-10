\ t-repl.fs — smoke test for the interactive caf REPL. Pipes a session through a
\ real gforth subprocess and checks the captured stdout (ANSI stripped). Slow;
\ run explicitly:  gforth test/t-repl.fs -e bye
require test/tester.fs

2variable RO
: SESSION ( a u -- )                       \ run the session text through the REPL
   s" /tmp/repl-in" w/o create-file throw {: fh :}
   fh write-file throw  fh close-file throw
   s" ~/.local/bin/gforth caf-repl.fs -e REPL < /tmp/repl-in 2>/dev/null | perl -pe 's/\e\[[0-9]*m//g' > /tmp/repl-out" system  \ strip ANSI
   s" /tmp/repl-out" slurp-file RO 2! ;
: HAS ( a u -- f )  RO 2@ 2swap search nip nip ;

s\" : SQ ( i64 -- i64 ) DUP * ;\n5 SQ . cr\n: BADD ( i64 -- i64 ) DUP ;\nEFFECT SQ\nNOSUCH\n"
   SESSION

T{ s" SQ  ( R i64 -- R i64 )"        HAS -> true }T   \ inferred effect shown
T{ s" 25"                            HAS -> true }T   \ word actually runs
T{ s" in BADD: arity mismatch"       HAS -> true }T   \ type error reported
T{ s" SQ : R i64 -- R i64"           HAS -> true }T   \ EFFECT command
T{ s" undefined word"                HAS -> true }T   \ runtime error caught
