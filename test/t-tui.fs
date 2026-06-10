\ t-tui.fs — TUI core: the def-line parser and the non-charting dry check that
\ drive the as-you-type feedback. The raw-mode key loop needs a real terminal and
\ is exercised by hand; this covers everything testable. Standalone:
\   gforth test/t-tui.fs -e bye
require ../src/tui.fs
require test/tester.fs

\ --- PARSE-DEF: recognises a complete `: NAME ( eff ) body ;` ---
: PD? ( a u -- f )  PARSE-DEF if 2drop 2drop 2drop true else false then ;
: PD-NAME ( a u -- na nu )  PARSE-DEF if 2drop 2drop else 2drop s" " then ;

T{ s" : SQ ( i64 -- i64 ) DUP * ;"  PD? -> true  }T
T{ s" : ABSV ( i64 -- i64 ) DUP 0< IF NEGATE THEN ;"  PD? -> true }T
T{ s" : SQ ( i64 -- i64 ) DUP *"    PD? -> false }T   \ no closing ;
T{ s" 5 SQ ."                       PD? -> false }T   \ not a definition
T{ s" : SQ ( i64 -- i64 ) DUP * ;"  PD-NAME s" SQ" compare -> 0 }T

\ --- CHECK-DRY: checks like CHECK-DEF but does NOT chart (live preview) ---
: DZ-BODY ( -- )  s" DZ" s" R i64 -- R i64" s" DUP *" CHECK-DRY ;
: DB-BODY ( -- )  s" DB" s" R i64 -- R i64" s" DUP"   CHECK-DRY ;
: DRY-OK  ( -- code )  ['] DZ-BODY catch ;
: DRY-BAD ( -- code )  ['] DB-BODY catch ;
T{ DRY-OK  -> 0 }T
T{ DRY-BAD -> E-ARITY }T
T{ s" DZ" EFFECT-OF -> 0 }T            \ dry check left nothing in the effect DB (single 0)
