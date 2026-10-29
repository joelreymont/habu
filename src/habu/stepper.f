\ stepper.f - the REPL token stepper: `step <code>` runs the rest of the line one
\ token at a time, echoing each token and the data stack after it. While
\ stepping the REPL hook returns one token per call, so the engine's interpret
\ loop is the evaluator. Package DEBUG (debug.f); baked after layout.f and
\ repl.f (native-runtime.f).

require src/habu/debug-watch.f

package DEBUG

create SBUF 1024 allot          \ captured rest-of-line
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
   SBUF SPOS @ + c@ ;

: S-WS? ( -- bool )
   S-HAS? IF S-CUR 33 < ELSE S-FALSE THEN ;

: S-NONWS? ( -- bool )
   S-HAS? IF S-CUR 32 > ELSE S-FALSE THEN ;

: S-SKIP ( -- )
   BEGIN S-WS? WHILE
      SPOS @ 1 + SPOS ! REPEAT ;

: S-SCAN ( -- ptr u8 n )
   SPOS @ {: ts :}
   BEGIN S-NONWS? WHILE
      SPOS @ 1 + SPOS ! REPEAT
   SBUF ts +  SPOS @ ts - ;

\ A token that reads ahead (':' for a definition) cannot be stepped alone: ':'
\ would have no name to read, so the rest of the line runs whole.
: COLON? ( ptr u8 n -- bool ) {: a u :}  u 1 = a c@ 58 = and ;
: REST-OF ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}  a  SBUF SLEN @ +  a -  ;     \ token start .. end

: BPW-DUMP ( -- )
   BPW-N@ 0 <= if exit then
   s" watch:" type cr
   BPW. ;

: S-NEXT-TOK ( -- ptr u8 n )
   .s
   BPW-DUMP
   S-SKIP
   SPOS @ SLEN @ < 0= IF 0 STEPPING !  RD-LINE exit THEN
   S-SCAN
   2dup COLON? IF
      0 STEPPING !  REST-OF                       \ definition -> run whole, stop stepping
   ELSE
      s" step> " type  over over type  s"  " type
   THEN ;

: SRD-LINE ( -- ptr u8 n )  STEPPING @ IF S-NEXT-TOK ELSE RD-LINE THEN ;

public

: STEP ( -- )
   SINP@ {: p :}
   SINE@ p - SLEN !
   SLEN @ 1023 > IF 1023 SLEN ! THEN
   SLEN @ 0 ?do p i + c@ SBUF i + c! loop
   0 SPOS !  -1 STEPPING !
   SINE@ SINP! ;

: S-INSTALL ( -- )
   [: SRD-LINE ;] is REPL-READ
   REPL-ENABLE ;
S-INSTALL

;package
