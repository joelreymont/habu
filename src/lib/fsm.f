\ fsm.f - state machines from a table. DEFINE reads a table of states, the
\ input each state senses, and the action and next state for each input, and
\ writes it as checked words under evaluate-closed, so a missing input, an
\ unknown next state or a state with neither row fails the load in the checker.
\ The grammar and the generated words are in docs/forth.md "State machines".
\ STORAGE CLASS. PROCESS-WIDE: the text buffers and header cells serve one
\ DEFINE at a time, as only the main thread compiles; the buffers are released
\ before DEFINE returns or throws, so it keeps no state after.

require src/lib/error.f
require src/lib/string.f
require src/core/bytes.f

package FSM

private
-9660 constant E-FIRST
-9669 constant E-LAST
public
-9660 constant E-SYNTAX     \ a table that breaks DEFINE's grammar

private

\ ROW-TEXT gathers the whole generated source; RUN-TEXT the run word's state
\ arms, which follow every row word they call.
DYNAMIC-BUFFER ROW-TEXT u8
variable ROW-LEN
DYNAMIC-BUFFER RUN-TEXT u8
variable RUN-LEN

\ The header's tokens point into the source DEFINE reads, which stays in place
\ while it runs.
PTR-VARIABLE NAME-A   variable NAME-U
PTR-VARIABLE CTX-A    variable CTX-U
PTR-VARIABLE STATES-A variable STATES-U
PTR-VARIABLE INPUTS-A variable INPUTS-U

: NAME$ ( -- ptr u8 n ) NAME-A @ NAME-U @ ;
: CTX$ ( -- ptr u8 n ) CTX-A @ CTX-U @ ;
: STATES$ ( -- ptr u8 n ) STATES-A @ STATES-U @ ;
: INPUTS$ ( -- ptr u8 n ) INPUTS-A @ INPUTS-U @ ;


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
   ROW-TEXT-RELEASE 0 ROW-LEN !
   RUN-TEXT-RELEASE 0 RUN-LEN ! ;

: TOKEN ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-SYNTAX throw then ;

: KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" IN" STR=
   a u s" FINAL" STR= or
   a u s" ;DEFINE" STR= or ;

\ `\` and `(` would be table tokens, and copied into a generated arm they
\ could comment the arm out, so a row holds neither.
: COMMENT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" \" STR=
   a u s" (" STR= or ;

\ A token a row needs: a keyword in its place cuts the row short.
: OPERAND ( -- ptr u8 n )
   TOKEN {: a:ptr u:n :}
   a u KEYWORD? a u COMMENT? or if E-SYNTAX throw then
   a u ;


\ `( <ctx> )`: CTX$ spans the tokens between the parentheses.
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

\ `: <NAME>/<state> ( <ctx> -- <states> ) dup <SENSE> MATCH <inputs>`
: ROW-HEAD ( ptr u8 n ptr u8 n -- ) {: s:ptr su:n f:ptr fu:n :}
   s" : " ROW+ NAME$ ROW+ s" /" ROW+ s su ROW+
   s"  ( " ROW+ CTX$ ROW+ s"  -- " ROW+ STATES$ ROW+
   s"  ) dup " ROW+ f fu ROW+ s"  MATCH " ROW+ INPUTS$ ROW+ s\" \n" ROW+ ;

\ `[dup <ACTION>]`, nothing for `-`.
: ACTION+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" -" STR= if exit then
   s" dup " ROW+ a u ROW+ s"  " ROW+ ;


\ `>state` constructs the state, then the ACTION runs on the context under it;
\ any other NEXT is a word that picks one, after `[dup <ACTION>]`.
: NEXT+ ( ptr u8 n ptr u8 n -- ) {: a:ptr au:n x:ptr xu:n :}
   x c@ [char] > = 0= if a au ACTION+ x xu ROW+ exit then
   a au s" -" STR= if s" drop " ROW+ then
   s" construct " ROW+ STATES$ ROW+ s"  " ROW+ x 1+ xu 1- ROW+
   a au s" -" STR= if exit then
   s"  swap " ROW+ a au ROW+ ;


: LINE ( ptr u8 n -- ) {: i:ptr iu:n :}
   OPERAND {: a:ptr au:n :}
   OPERAND {: x:ptr xu:n :}
   s"    " ROW+ i iu ROW+ s"  OF " ROW+
   a au x xu NEXT+ s\"  ENDOF\n" ROW+ ;

: ROW-END ( -- ) s\"    ;MATCH ;\n" ROW+ ;


\ `IN <state> <SENSE>`: opens the state's row word and its arm in the run word.
: IN-ROW ( -- )
   OPERAND {: s:ptr su:n :}
   OPERAND s su 2swap ROW-HEAD
   s"    " RUN+ s su RUN+ s"  OF false over " RUN+ NAME$ RUN+ s" /" RUN+ s su RUN+
   s\"  ENDOF\n" RUN+ ;


\ `FINAL <state> <ACTION>`: the run word's arm that ends the run.
: FINAL-ROW ( -- )
   OPERAND {: s:ptr su:n :}
   OPERAND {: a:ptr au:n :}
   a au s" -" STR= {: none:bool :}
   s"    " RUN+ s su RUN+ s"  OF " RUN+
   none 0= if s" dup " RUN+ then
   s" true construct " RUN+ STATES$ RUN+ s"  " RUN+ s su RUN+
   none 0= if s"  rot " RUN+ a au RUN+ then
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


\ `: <NAME> ( <ctx> <states> -- ) begin MATCH <states> … ;MATCH swap until 2drop ;`
: RUN-WORD ( -- )
   s" : " ROW+ NAME$ ROW+ s"  ( " ROW+ CTX$ ROW+ s"  " ROW+ STATES$ ROW+
   s"  -- ) begin MATCH " ROW+ STATES$ ROW+ s\" \n" ROW+
   RUN-LEN @ 0 > if 0 RUN-TEXT RUN-LEN @ ROW+ then
   s\"    ;MATCH swap until 2drop ;\n" ROW+ ;


: GENERATE ( -- )
   HEADER
   false
   begin TOKEN 2dup s" ;DEFINE" STR= 0= while STEP repeat
   2drop if ROW-END then
   RUN-WORD
   0 ROW-TEXT ROW-LEN @ evaluate-closed ;

public

\ DEFINE <NAME> ( <ctx> ) <states> <inputs> IN … FINAL … ;DEFINE, in the
\ caller's open package and section; docs/forth.md "State machines".
: DEFINE ( -- )
   [: GENERATE ;] [: RELEASE ;] finally ;

;package

parses-through: FSM:DEFINE 0 ( ;DEFINE )
