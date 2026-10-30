\ stepper.f - the REPL token stepper: `step <code>` runs the rest of the line one
\ token at a time, echoing each token and the data stack after it. While
\ stepping the REPL hook returns one token per call, so the engine's interpret
\ loop is the evaluator. Package DEBUG (debug.f); baked after layout.f and
\ repl.f (native-runtime.f).

require src/habu/debug-watch.f

package DEBUG

DYNAMIC-BUFFER SBUF u8          \ the rest of the line STEP took, sized to it
DYNAMIC-BUFFER SOUT u8          \ the token or rest the REPL is reading: only S-OUT writes it
variable SLEN  variable SPOS  variable STEPPING

: SINP-FIELD ( -- ptr ptr u8 )
   data-base INP-CELL + 0 ptr-field ;

: SINE-FIELD ( -- ptr ptr u8 )
   data-base INE-CELL + 0 ptr-field ;

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


\ A word the REPL runs from what this hands out may STEP, which rewrites SBUF
\ and can move it, so the REPL reads a copy that only this writes.
: S-OUT ( n n -- ptr u8 n )
   {: at u :}
   u SOUT-RESERVE
   u 0 ?do at i + SBUF c@ i SOUT c! loop
   0 SOUT u ;


\ Stepping is over, and the last token it handed out is spent: both mappings go
\ before the next line, so an image saved from that line holds none.
: S-LINE ( -- ptr u8 n )
   SBUF-RELEASE SOUT-RELEASE RD-LINE ;


: BPW-DUMP ( -- )
   BPW-N@ 0 <= if exit then
   s" watch:" type cr
   BPW. ;

: S-NEXT-TOK ( -- ptr u8 n )
   .s
   BPW-DUMP
   S-SKIP
   SPOS @ SLEN @ < 0= IF 0 STEPPING !  S-LINE exit THEN
   S-SCAN
   2dup COLON? IF
      0 STEPPING !  REST-OF S-OUT                 \ definition -> run whole, stop stepping
   ELSE
      S-OUT  s" step> " type  over over type  s"  " type
   THEN ;

: SRD-LINE ( -- ptr u8 n )  STEPPING @ IF S-NEXT-TOK ELSE S-LINE THEN ;

public

: STEP ( -- )
   SINP@ {: p :}
   SINE@ p - SLEN !
   SLEN @ SBUF-RESERVE
   SLEN @ 0 ?do p i + c@ i SBUF c! loop
   0 SPOS !  -1 STEPPING !
   SINE@ SINP! ;

: S-INSTALL ( -- )
   [: SRD-LINE ;] is REPL-READ
   REPL-ENABLE ;
S-INSTALL

;package
