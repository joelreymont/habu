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

\ --- golden render tests (pz-style): feed a scripted key sequence into TUI-LOOP,
\ capture the rendered output (ANSI + text) via outfile-execute, assert on it.
\ No real terminal: TKEY is rebound to a feeder, output is redirected to a file.
create KEYS 512 allot   variable #KEYS   variable KEY-IX
: KEYS! ( a u -- )  512 min dup #KEYS !  KEYS swap move  0 KEY-IX ! ;
: FEED-KEY ( -- c )                    \ next scripted byte; Ctrl-D (4) once exhausted -> loop ends
   KEY-IX @ #KEYS @ < if  KEYS KEY-IX @ + c@  1 KEY-IX +!  else  4  then ;
2variable TCAP
: DRIVE ( keys-a keys-u -- )           \ run TUI-LOOP on the script, capture the render
   KEYS!  ['] FEED-KEY is TKEY
   s" /tmp/tui-cap" w/o create-file throw {: fh :}
   ['] TUI-LOOP fh outfile-execute  fh close-file throw
   ['] key is TKEY
   s" /tmp/tui-cap" slurp-file TCAP 2! ;
: SHOWS ( a u -- f )  TCAP 2@ 2swap search nip nip ;   \ rendered output contains substring?

\ type a complete, well-typed def (no Enter; Ctrl-D ends) -> live ✓ effect renders
s\" : SQ ( i64 -- i64 ) DUP * ;" DRIVE
T{ s" habu> "                       SHOWS -> true }T   \ prompt drawn
T{ s" : SQ ( i64 -- i64 ) DUP * ;" SHOWS -> true }T   \ buffer echoed
T{ s" ✓ SQ"                        SHOWS -> true }T   \ live success marker + name
T{ s" i64 -- i64"                  SHOWS -> true }T   \ inferred effect shown
T{ s" ✗"                           SHOWS -> false }T  \ no error marker for a good def

\ a type-incorrect def renders the live ✗ diagnostic, not a ✓
s\" : BADD ( i64 -- i64 ) DUP ;" DRIVE
T{ s" ✗ "             SHOWS -> true }T
T{ s" arity mismatch" SHOWS -> true }T
T{ s" ✓ "             SHOWS -> false }T

\ an incomplete def (no closing ;) renders the "typing…" placeholder
s\" : SQ ( i64 -- i64 ) DUP *" DRIVE
T{ s" …"   SHOWS -> true }T
T{ s" ✓ "  SHOWS -> false }T
