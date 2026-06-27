\ stepper.f — the token stepper for the REPL: `step <code>` runs the rest of
\ the line ONE TOKEN at a time, echoing each token and printing the data stack
\ after it executes. No EVALUATE needed: while stepping, the REPL hook returns
\ one token per call, so the engine's own interpret loop is the evaluator.
\ Baked into bin/hb after layout.f, repl.f, and debug-watch.f (uses DATAB/REPLH-CELL/
\ TTY?/EMITS/RD-LINE/BPW-DUMP); tools/hb-build.f programs never see it.

create SBUF 1024 allot          \ captured rest-of-line
variable SLEN  variable SPOS  variable STEPPING

: SINP-FIELD ( -- ptr ptr u8 )
   DATAB INP-CELL + 0 ptr-field ;

: SINE-FIELD ( -- ptr ptr u8 )
   DATAB INE-CELL + 0 ptr-field ;

: SINP@ ( -- ptr u8 )
   SINP-FIELD @ ;

: SINE@ ( -- ptr u8 )
   SINE-FIELD @ ;

: SINP! ( ptr u8 -- )
   SINP-FIELD ! ;

: S-FALSE ( -- bool )
   0 0= 0= ;

: S-HAS? ( -- bool )
   SPOS @ SLEN @ < ;

: S-CUR ( -- u8 )
   SBUF SPOS @ + c@ ;

: S-WS? ( -- bool )
   S-HAS? IF S-CUR 33 < ELSE S-FALSE THEN ;

: S-NONWS? ( -- bool )
   S-HAS? IF S-CUR 32 > ELSE S-FALSE THEN ;

\ capture everything after `step` and consume it; the loop then drains us
: STEP ( -- )
   SINP@ {: p :}
   SINE@ p - SLEN !
   SLEN @ 1023 > IF 1023 SLEN ! THEN
   SLEN @ 0 ?do p i + c@ SBUF i + c! loop
   0 SPOS !  -1 STEPPING !
   SINE@ SINP! ;

: S-SKIP ( -- )
   BEGIN S-WS? WHILE
      SPOS @ 1 + SPOS ! REPEAT ;

: S-SCAN ( -- ptr u8 n )
   SPOS @ {: ts :}
   BEGIN S-NONWS? WHILE
      SPOS @ 1 + SPOS ! REPEAT
   SBUF ts +  SPOS @ ts - ;

\ a token that reads AHEAD (':' for a definition) can't be stepped one token at
\ a time — feeding ':' alone leaves it with no name to read. Detect it and feed
\ the rest of the line whole, running it normally.
: COLON? ( ptr u8 n -- bool ) {: a u :}  u 1 = a c@ 58 = and ;
: REST-OF ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}  a  SBUF SLEN @ +  a -  ;     \ token start .. end

\ between tokens: show the stack the last token left, then feed the next one
: S-NEXT-TOK ( -- ptr u8 n )
   .s
   BPW-DUMP
   S-SKIP
   SPOS @ SLEN @ < 0= IF 0 STEPPING !  RD-LINE exit THEN
   S-SCAN
   2dup COLON? IF
      0 STEPPING !  REST-OF                       \ definition -> run whole, stop stepping
   ELSE
      s" step> " EMITS  over over EMITS  s"  " EMITS
   THEN ;

: SRD-LINE ( -- ptr u8 n )  STEPPING @ IF S-NEXT-TOK ELSE RD-LINE THEN ;

: S-INSTALL ( -- )  TTY? IF ['] SRD-LINE DATAB REPLH-CELL + ! THEN ;
S-INSTALL
