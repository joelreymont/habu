\ fsm.f - state machines from a table. DEFINE reads a table of states, the
\ input each state senses, and the action and next state for each input, and
\ writes it as checked words under evaluate-closed, so a missing input, an
\ unknown next state or a state with neither row fails the load in the checker.
\ A row may name its own inputs ENUM, the one its SENSE answers, in place of
\ the header's.
\ STORAGE CLASS. PROCESS-WIDE: the text buffers and token cells serve one
\ DEFINE at a time, as only the main thread compiles; the buffers are released
\ before DEFINE returns or throws, so it keeps no state after.
\
\ DEFINE writes a word <NAME>/<state>/<input> for each line's ACTION and
\ <NAME>/<state> for each FINAL row's, `drop` for `-`; an ACTION of `-` on a
\ line writes no word. Each IN row becomes a word <NAME>/<state> that runs
\ SENSE and matches its answer: a `>state` NEXT constructs the state, then the
\ line word runs on the context under it; any other NEXT is a word that picks
\ one, after `dup` and the line word. <NAME>-STEP matches the state: an IN
\ row's arm answers false, a FINAL row's arm runs its word and answers true.
\ <NAME> calls <NAME>-STEP until it answers true.

require src/lib/error.f
require src/lib/string.f
require src/core/bytes.f

package FSM

private
-9660 constant E-FIRST
-9669 constant E-LAST
public
-9660 constant E-SYNTAX

private

\ ACT-TEXT takes the action words, then ROW-TEXT's row, step and run words;
\ RUN-TEXT holds the step word's arms: each word precedes every word calling it.
DYNAMIC-BUFFER ACT-TEXT u8
variable ACT-LEN
DYNAMIC-BUFFER ROW-TEXT u8
variable ROW-LEN
DYNAMIC-BUFFER RUN-TEXT u8
variable RUN-LEN

\ The header's tokens and the open row's state point into the source DEFINE
\ reads, which stays in place while it runs.
PTR-VARIABLE NAME-A   variable NAME-U
PTR-VARIABLE CTX-A    variable CTX-U
PTR-VARIABLE STATES-A variable STATES-U
PTR-VARIABLE INPUTS-A variable INPUTS-U
PTR-VARIABLE OPEN-A   variable OPEN-U

: NAME$ ( -- ptr u8 n ) NAME-A @ NAME-U @ ;
: CTX$ ( -- ptr u8 n ) CTX-A @ CTX-U @ ;
: STATES$ ( -- ptr u8 n ) STATES-A @ STATES-U @ ;
: INPUTS$ ( -- ptr u8 n ) INPUTS-A @ INPUTS-U @ ;
: OPEN$ ( -- ptr u8 n ) OPEN-A @ OPEN-U @ ;


: ACT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   ACT-LEN @ u + ACT-TEXT-RESERVE
   a ACT-LEN @ ACT-TEXT u BYTE-COPY
   ACT-LEN @ u + ACT-LEN ! ;


: ROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   ROW-LEN @ u + ROW-TEXT-RESERVE
   a ROW-LEN @ ROW-TEXT u BYTE-COPY
   ROW-LEN @ u + ROW-LEN ! ;


: RUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   RUN-LEN @ u + RUN-TEXT-RESERVE
   a RUN-LEN @ RUN-TEXT u BYTE-COPY
   RUN-LEN @ u + RUN-LEN ! ;


: RELEASE ( -- )
   ACT-TEXT-RELEASE 0 ACT-LEN !
   ROW-TEXT-RELEASE 0 ROW-LEN !
   RUN-TEXT-RELEASE 0 RUN-LEN ! ;

: TOKEN ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-SYNTAX throw then ;

: KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" IN" STR=
   a u s" FINAL" STR= or
   a u s" ;DEFINE" STR= or ;

\ `\` and `(` would be table tokens, and copied into a generated arm they
\ could comment the arm out, so no line holds either; `(` only opens a row's
\ inputs right after its SENSE.
: COMMENT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" \" STR=
   a u s" (" STR= or ;

\ A token a row needs: a keyword in its place cuts the row short.
: OPERAND ( -- ptr u8 n )
   TOKEN {: a:ptr u:n :}
   a u KEYWORD? a u COMMENT? or if E-SYNTAX throw then
   a u ;


: HEADER ( -- )
   TOKEN NAME-U ! NAME-A !
   TOKEN s" (" STR= 0= if E-SYNTAX throw then
   TOKEN {: a:ptr u:n :}
   a CTX-A ! 0 CTX-U !
   a u
   begin 2dup s" )" STR= 0= while
      + CTX-A @ - CTX-U !
      TOKEN
   repeat 2drop
   TOKEN STATES-U ! STATES-A !
   TOKEN INPUTS-U ! INPUTS-A ! ;

: ROW-HEAD ( ptr u8 n ptr u8 n ptr u8 n -- ) {: s:ptr su:n f:ptr fu:n i:ptr iu:n :}
   s" : " ROW+ NAME$ ROW+ s" /" ROW+ s su ROW+
   s"  ( " ROW+ CTX$ ROW+ s"  -- " ROW+ STATES$ ROW+
   s"  ) dup " ROW+ f fu ROW+ s"  MATCH " ROW+ i iu ROW+ s\" \n" ROW+ ;

\ The declared effect holds every ACTION to the context alone.
: ACT-BODY ( ptr u8 n -- ) {: a:ptr u:n :}
   s"  ( " ACT+ CTX$ ACT+ s"  -- ) " ACT+ a u ACT+ s\"  ;\n" ACT+ ;

: LINE-WORD+ ( ptr u8 n -- ) {: i:ptr iu:n :}
   NAME$ ROW+ s" /" ROW+ OPEN$ ROW+ s" /" ROW+ i iu ROW+ ;


: NEXT+ ( bool ptr u8 n ptr u8 n -- ) {: act:bool i:ptr iu:n x:ptr xu:n :}
   x c@ [char] > = 0= if
      act if s" dup " ROW+ i iu LINE-WORD+ s"  " ROW+ then
      x xu ROW+ exit
   then
   act 0= if s" drop " ROW+ then
   s" construct " ROW+ STATES$ ROW+ s"  " ROW+ x 1+ xu 1- ROW+
   act if s"  swap " ROW+ i iu LINE-WORD+ then ;


: LINE ( ptr u8 n -- ) {: i:ptr iu:n :}
   OPERAND {: a:ptr au:n :}
   OPERAND {: x:ptr xu:n :}
   a au s" -" STR= 0= {: act:bool :}
   act if
      s" : " ACT+ NAME$ ACT+ s" /" ACT+ OPEN$ ACT+ s" /" ACT+ i iu ACT+
      a au ACT-BODY
   then
   s"    " ROW+ i iu ROW+ s"  OF " ROW+
   act i iu x xu NEXT+ s\"  ENDOF\n" ROW+ ;

: ROW-END ( -- ) s\"    ;MATCH ;\n" ROW+ ;


: ROW-INPUTS ( -- ptr u8 n )
   OPERAND {: i:ptr iu:n :}
   i iu s" )" STR= if E-SYNTAX throw then
   TOKEN s" )" STR= 0= if E-SYNTAX throw then
   i iu ;


\ A row has at least one line, so the token after SENSE, or after
\ `( <inputs> )`, is its first.
: IN-ROW ( -- )
   OPERAND {: s:ptr su:n :}
   s OPEN-A ! su OPEN-U !
   OPERAND {: f:ptr fu:n :}
   TOKEN {: t:ptr tu:n :}
   t tu s" (" STR= if
      s su f fu ROW-INPUTS ROW-HEAD TOKEN
   else
      s su f fu INPUTS$ ROW-HEAD t tu
   then {: l:ptr lu:n :}
   l lu KEYWORD? l lu COMMENT? or if E-SYNTAX throw then
   l lu LINE
   s"    " RUN+ s su RUN+ s"  OF false swap " RUN+ NAME$ RUN+ s" /" RUN+ s su RUN+
   s\"  ENDOF\n" RUN+ ;


: FINAL-ROW ( -- )
   OPERAND {: s:ptr su:n :}
   OPERAND {: a:ptr au:n :}
   s" : " ACT+ NAME$ ACT+ s" /" ACT+ s su ACT+
   a au s" -" STR= if s" drop" else a au then ACT-BODY
   s"    " RUN+ s su RUN+ s"  OF true construct " RUN+ STATES$ RUN+
   s"  " RUN+ s su RUN+ s"  rot " RUN+ NAME$ RUN+ s" /" RUN+ s su RUN+
   s\"  ENDOF\n" RUN+ ;


\ One token after the header, in one of two states, an IN row's word open or
\ not: IN and FINAL close it, and any other token but `\` or `(` is a line
\ of the open row.
: STEP ( bool ptr u8 n -- bool ) {: open:bool a:ptr u:n :}
   a u s" IN" STR= if
      open if ROW-END then
      IN-ROW true exit
   then
   a u s" FINAL" STR= if
      open if ROW-END then
      FINAL-ROW false exit
   then
   open a u COMMENT? 0= and if a u LINE true exit then
   E-SYNTAX throw ;


: RUN-WORDS ( -- )
   s" : " ROW+ NAME$ ROW+ s" -STEP ( " ROW+ CTX$ ROW+ s"  " ROW+ STATES$ ROW+
   s"  -- " ROW+ STATES$ ROW+ s"  bool ) MATCH " ROW+ STATES$ ROW+ s\" \n" ROW+
   RUN-LEN @ 0 > if 0 RUN-TEXT RUN-LEN @ ROW+ then
   s\"    ;MATCH swap ;\n" ROW+
   s" : " ROW+ NAME$ ROW+ s"  ( " ROW+ CTX$ ROW+ s"  " ROW+ STATES$ ROW+
   s"  -- ) begin over swap " ROW+ NAME$ ROW+ s\" -STEP until 2drop ;\n" ROW+ ;


: GENERATE ( -- )
   HEADER
   false
   begin TOKEN 2dup s" ;DEFINE" STR= 0= while STEP repeat
   2drop if ROW-END then
   RUN-WORDS
   0 ROW-TEXT ROW-LEN @ ACT+
   0 ACT-TEXT ACT-LEN @ evaluate-closed ;

public

\ DEFINE <NAME> ( <ctx> ) <states> <inputs> IN … FINAL … ;DEFINE, in the
\ caller's open package and section.
: DEFINE ( -- )
   [: GENERATE ;] [: RELEASE ;] finally ;

;package

parses-through: FSM:DEFINE 0 ( ;DEFINE )
