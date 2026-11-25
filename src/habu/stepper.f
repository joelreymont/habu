\ stepper.f - the REPL token stepper: `step <code>` runs the rest of the line one
\ token at a time, echoing each token and the data stack after it. While
\ stepping the REPL's reader returns one token per call, so the engine's
\ interpret loop is the evaluator. Package DEBUG (debug.f); built in after
\ layout.f (tools/hb-build-core.f RUNTIME-FILES).

require src/habu/debug-watch.f
require src/lib/genio-core.f

package DEBUG

DYNAMIC-BUFFER SBUF u8          \ the rest of the line STEP took, sized to it
variable SLEN  variable SPOS  variable STEPPING

: SINP-FIELD ( -- ^^u8 )
   data-base INP-CELL + 0 ptr-field ;

: SINP@ ( -- ^u8 )
   SINP-FIELD @ ;

: SINP! ( ^u8 -- )
   SINP-FIELD ! ;

: S-FALSE ( -- bool )
   0 0= 0= ;

: S-HAS? ( -- bool )
   SPOS @ SLEN @ < ;

: S-CUR ( -- u8 )
   SPOS @ SBUF c@ ;

: S-WS? ( -- bool )
   S-HAS? IF S-CUR 33 < ELSE S-FALSE THEN ;

: S-NONWS? ( -- bool )
   S-HAS? IF S-CUR 32 > ELSE S-FALSE THEN ;

: S-SKIP ( -- )
   BEGIN S-WS? WHILE
      SPOS @ 1 + SPOS ! REPEAT ;

: S-SCAN ( -- n n )
   SPOS @ {: ts :}
   BEGIN S-NONWS? WHILE
      SPOS @ 1 + SPOS ! REPEAT
   ts  SPOS @ ts - ;

\ A token that reads ahead (':' for a definition) cannot be stepped alone: ':'
\ would have no name to read, so the rest of the line runs whole.
: COLON? ( n n -- bool )
   {: at u :}
   u 1 = at SBUF c@ 58 = and ;


: REST-OF ( n n -- n n )                            \ token start .. end
   {: at u :}
   at  SLEN @ at - ;


\ A word on the token may STEP and move SBUF: the REPL keeps a copy first.
: S-OUT ( n n -- str )
   {: at u :}
   at SBUF u >str ;


\ Stepping is over, so SBUF can go.
: S-LINE ( -- str )
   SBUF-RELEASE GENIO:READ-LINE ;


: BPW-DUMP ( -- )
   BPW-N@ 0 <= if exit then
   s" watch:" type cr
   BPW. ;

: S-NEXT-TOK ( -- str )
   .s
   BPW-DUMP
   S-SKIP
   SPOS @ SLEN @ < 0= IF 0 STEPPING !  S-LINE exit THEN
   S-SCAN
   2dup COLON? IF
      0 STEPPING !  REST-OF S-OUT                 \ definition -> run whole, stop stepping
   ELSE
      S-OUT  s" step> " type  dup type  s"  " type
   THEN ;

public

\ The REPL's next input: the stepped line's next token while stepping, else the
\ next line.
: REPL-READ ( -- str )  STEPPING @ IF S-NEXT-TOK ELSE S-LINE THEN ;

: STEP ( -- )
   SINP@ {: p :}
   REJECTION:INPUT$ dup STR:DATA swap STR:LEN + p - SLEN !
   SLEN @ SBUF-RESERVE
   SLEN @ 0 ?do p i + c@ i SBUF c! loop
   0 SPOS !  -1 STEPPING !
   REJECTION:INPUT$ dup STR:DATA swap STR:LEN + SINP! ;

;package
