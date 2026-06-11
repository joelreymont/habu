\ repl.fs — interactive habu REPL (gforth-hosted, drives the live checker). Enter
\ a checked definition and see its inferred effect (✓) or the habu diagnostic (✗);
\ non-definition lines are evaluated so you can exercise words. Commands are plain
\ words: HELP, WORDS (charted effects), EFFECT <name>, bye. All Forth, no C/Zig.
\ The REPL driver itself uses EVALUATE/terminal I/O, so it is ordinary Forth, not
\ checkable habu — it is the harness that RUNS habu on what you type.

require ../habu.fs

\ The REPL's own words are infrastructure, not habu to be checked — define them
\ with checking off so they don't land in the effect DB (WORDS stays clean).
CHECKING-ON? off

\ --- ANSI ---
: ESC ( -- )  27 emit ;
: +G ( -- )  ESC ." [32m" ;   : +R ( -- )  ESC ." [31m" ;
: +C ( -- )  ESC ." [36m" ;   : +D ( -- )  ESC ." [2m" ;   : -X ( -- )  ESC ." [0m" ;

\ --- effect lookup / browser ---
: .EFF ( a u -- )  EFFECT-OF ?dup if type else ." (none)" then ;
: .ENTRY ( nt -- f )                       \ one charted word: name + effect
   name>string  +C 2dup type -X  ."   " +D 2dup .EFF -X  cr  2drop  true ;
: WORDS  ( -- )  cr ['] .ENTRY EFFECTS traverse-wordlist ;
: EFFECT ( "name" -- )                     \ EFFECT SQUARE -> show one
   parse-name  +C 2dup type -X  ."  : " +D .EFF -X cr ;

: HELP ( -- )
   cr +C ." 🐍 habu REPL" -X ."  — type a checked definition to see its effect:" cr
   +D ."   : SQ ( i64 -- i64 ) DUP * ;" -X cr
   ." commands:  " +C ." HELP" -X ."    " +C ." WORDS" -X ."    " +C ." EFFECT <name>" -X
   ."    " +C ." bye" -X cr ;

\ --- per-line feedback ---
: ERR-MSG ( code -- a u )                  \ map common gforth throw codes
   dup -13 = if drop s" undefined word"        exit then
   dup -4  = if drop s" stack underflow"       exit then
   drop s" error" ;

: SHOW-OK ( -- )                           \ a checked def was accepted
   +G ."  ✓ " NM@ type -X
   +D ."   ( " NM@ EFFECT-OF ?dup if type else ." charted" then ."  )" -X cr ;

: REPL-LINE ( a u -- )
   CK-NONE CHECK-CODE !
   ['] EVALUATE catch ?dup if               \ runtime error in evaluated code
      +R ."  ✗ " ERR-MSG type -X cr
   else
      CHECK-CODE @ 0= if SHOW-OK then        \ checked def OK (errors self-report)
   then ;

create LBUF 512 chars allot
: REPL ( -- )
   HELP
   begin
      cr +D ." 🐍 habu> " -X
      LBUF 512 stdin read-line throw      ( u2 flag )
   while                                   ( u2 )
      LBUF swap REPL-LINE
   repeat drop
   cr +D ." bye" -X cr ;

CHECKING-ON? on                            \ user input in the REPL is checked habu
