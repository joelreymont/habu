\ stepper.f — the token stepper for the REPL: `step <code>` runs the rest of
\ the line ONE TOKEN at a time, echoing each token and printing the data stack
\ after it executes. No EVALUATE needed: while stepping, the REPL hook returns
\ one token per call, so the engine's own interpret loop is the evaluator.
\ Baked into bin/hbi after repl.f (uses its DATAB/REPLH-CELL/TTY?/EMITS/
\ RD-LINE); tools/hb-build.sh programs never see it.

$36A0 constant SINP-CELL        \ engine input cursor cell
$36A8 constant SINE-CELL        \ engine input end cell

create SBUF 1024 allot          \ captured rest-of-line
variable SLEN  variable SPOS  variable STEPPING

\ capture everything after `step` and consume it; the loop then drains us
: STEP ( -- )
   DATAB SINP-CELL + @ {: p :}
   DATAB SINE-CELL + @ p - SLEN !
   SLEN @ 1023 > IF 1023 SLEN ! THEN
   SLEN @ 0 ?do p i + c@ SBUF i + c! loop
   0 SPOS !  -1 STEPPING !
   DATAB SINE-CELL + @ DATAB SINP-CELL + ! ;

: S-SKIP ( -- )
   BEGIN SPOS @ SLEN @ < IF SBUF SPOS @ + c@ 33 < ELSE 0 THEN WHILE
      SPOS @ 1 + SPOS ! REPEAT ;

: S-SCAN ( -- a u )
   SPOS @ {: ts :}
   BEGIN SPOS @ SLEN @ < IF SBUF SPOS @ + c@ 32 > ELSE 0 THEN WHILE
      SPOS @ 1 + SPOS ! REPEAT
   SBUF ts +  SPOS @ ts - ;

\ a token that reads AHEAD (':' for a definition) can't be stepped one token at
\ a time — feeding ':' alone leaves it with no name to read. Detect it and feed
\ the rest of the line whole, running it normally.
: COLON? {: a u :}  u 1 = a c@ 58 = and ;
: REST-OF {: a u :}  a  SBUF SLEN @ +  a -  ;     \ ( a u -- a restlen ) token start .. end

\ between tokens: show the stack the last token left, then feed the next one
: NEXT-TOK ( -- n n )
   .s
   S-SKIP
   SPOS @ SLEN @ < 0= IF 0 STEPPING !  RD-LINE exit THEN
   S-SCAN
   2dup COLON? IF
      0 STEPPING !  REST-OF                       \ definition -> run whole, stop stepping
   ELSE
      s" step> " EMITS  over over EMITS  s"  " EMITS
   THEN ;

: SRD-LINE ( -- n n )  STEPPING @ IF NEXT-TOK ELSE RD-LINE THEN ;

: S-INSTALL ( -- )  TTY? IF ['] SRD-LINE DATAB REPLH-CELL + ! THEN ;
S-INSTALL
