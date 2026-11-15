\ fsm.f - state machines from a table. DEFINE reads a table of states, the
\ input each state senses, and the action and next state for each input, and
\ builds checked words from it, so a missing input, an unknown next state or a
\ state with neither row fails the load in the checker. A row may name its own
\ inputs ENUM, the one its SENSE answers, in place of the header's.
\ STORAGE CLASS. PROCESS-WIDE: the token, row and name buffers serve one
\ DEFINE at a time, as only the main thread compiles; they are released before
\ DEFINE returns or throws, so it keeps no state after.
\
\ DEFINE reads the whole table, failing on its grammar before it defines a
\ word, then builds each word with define-word: engine words
\ found here, keywords, the ENUMs as types and variants, and calls of the
\ table's words and its own, which bind where DEFINE is called. It defines a
\ word <NAME>/<state>/<input> for each line's ACTION and <NAME>/<state> for
\ each FINAL row's, `drop` for `-`; an ACTION of `-` on a line defines no word.
\ Each IN row becomes a word <NAME>/<state> that runs SENSE on a `dup` of the
\ context and matches its answer: a `>state` NEXT constructs the state, then
\ the line word runs on the context under it; any other NEXT is a word that
\ picks one, after `dup` and the line word. <NAME>-STEP matches the state: an
\ IN row's arm answers false, a FINAL row's arm runs its word and answers
\ true. <NAME> calls <NAME>-STEP until it answers true. Each word is defined
\ before every word calling it.

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

\ Every token DEFINE keeps points into the source it reads, which stays in
\ place while it runs. Reading one back makes it the token read last, so a
\ rejection while DEFINE builds from it is placed at it.
PTR-VARIABLE NAME-A   variable NAME-U
PTR-VARIABLE CTX-A    variable CTX-U
PTR-VARIABLE STATES-A variable STATES-U
PTR-VARIABLE INPUTS-A variable INPUTS-U

: AT ( ^u8 n -- ^u8 n ) 2dup SRCLOC:TOKEN! ;

: NAME$ ( -- ^u8 n ) NAME-A @ NAME-U @ AT ;
: CTX$ ( -- ^u8 n ) CTX-A @ CTX-U @ AT ;
: STATES$ ( -- ^u8 n ) STATES-A @ STATES-U @ AT ;
: INPUTS$ ( -- ^u8 n ) INPUTS-A @ INPUTS-U @ AT ;

\ The table's rows in order. A row's tokens start at its ROW-AT: an IN row's
\ state, SENSE and inputs, then ROW-LINES lines of input, ACTION and NEXT; a
\ FINAL row's state and ACTION, and no lines, as every IN row has one.
DYNAMIC-BUFFER TOK-A ^u8
DYNAMIC-BUFFER TOK-U n
variable TOKS
DYNAMIC-BUFFER ROW-AT n
DYNAMIC-BUFFER ROW-LINES n
variable ROWS

\ The name of the word being defined or called.
DYNAMIC-BUFFER WORD-TEXT u8
variable WORD-LEN

: TOK+ ( ^u8 n -- ) {: a:^ u:n :}
   TOKS @ {: k:n :}
   k 1+ TOK-A-RESERVE  k 1+ TOK-U-RESERVE
   a k TOK-A !  u k TOK-U !
   k 1+ TOKS ! ;

: TOK$ ( n -- ^u8 n ) {: k:n :} k TOK-A @ k TOK-U @ AT ;

\ Opens a row at the next token.
: ROW+ ( -- )
   ROWS @ {: r:n :}
   r 1+ ROW-AT-RESERVE  r 1+ ROW-LINES-RESERVE
   TOKS @ r ROW-AT !  0 r ROW-LINES !
   r 1+ ROWS ! ;

: LINES+ ( -- ) ROWS @ 1- ROW-LINES 1 swap +! ;

: FINAL? ( n -- bool ) ROW-LINES @ 0= ;

\ The token of line I of the IN row whose tokens start at K.
: LINE-TOK ( n n -- n ) {: k:n i:n :} k 3 + i 3 * + ;

: RELEASE ( -- )
   TOK-A-RELEASE TOK-U-RELEASE 0 TOKS !
   ROW-AT-RELEASE ROW-LINES-RELEASE 0 ROWS !
   WORD-TEXT-RELEASE 0 WORD-LEN ! ;

: TOKEN ( -- ^u8 n )
   parse-name dup 0= if 2drop E-SYNTAX throw then ;

: KEYWORD? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u s" IN" STR=
   a u s" FINAL" STR= or
   a u s" ;DEFINE" STR= or ;

\ DEFINE reads the table with parse-name, so `\` and `(` are tokens where a
\ reader sees a comment; no line holds either, and `(` only opens a row's
\ inputs right after its SENSE.
: COMMENT? ( ^u8 n -- bool ) {: a:^ u:n :}
   a u s" \" STR=
   a u s" (" STR= or ;

\ A token a row needs: a keyword in its place cuts the row short.
: OPERAND ( -- ^u8 n )
   TOKEN {: a:^ u:n :}
   a u KEYWORD? a u COMMENT? or if E-SYNTAX throw then
   a u ;


: HEADER ( -- )
   TOKEN NAME-U ! NAME-A !
   TOKEN s" (" STR= 0= if E-SYNTAX throw then
   TOKEN {: a:^ u:n :}
   a CTX-A ! 0 CTX-U !
   a u
   begin 2dup s" )" STR= 0= while
      + CTX-A @ - CTX-U !
      TOKEN
   repeat 2drop
   TOKEN STATES-U ! STATES-A !
   TOKEN INPUTS-U ! INPUTS-A ! ;

: LINE ( ^u8 n -- ) TOK+ OPERAND TOK+ OPERAND TOK+ LINES+ ;


: ROW-INPUTS ( -- ^u8 n )
   OPERAND {: i:^ iu:n :}
   i iu s" )" STR= if E-SYNTAX throw then
   TOKEN s" )" STR= 0= if E-SYNTAX throw then
   i iu ;


\ A row has at least one line, so the token after SENSE, or after
\ `( <inputs> )`, is its first.
: IN-ROW ( -- )
   ROW+
   OPERAND TOK+  OPERAND TOK+
   TOKEN {: t:^ tu:n :}
   t tu s" (" STR= if
      ROW-INPUTS TOK+ TOKEN
   else
      INPUTS$ TOK+ t tu
   then {: l:^ lu:n :}
   l lu KEYWORD? l lu COMMENT? or if E-SYNTAX throw then
   l lu LINE ;

: FINAL-ROW ( -- ) ROW+ OPERAND TOK+ OPERAND TOK+ ;


\ One token after the header, in one of two states, an IN row open or not:
\ IN and FINAL close it, and any other token but `\` or `(` is a line of the
\ open row.
: STEP ( bool ^u8 n -- bool ) {: open:bool a:^ u:n :}
   a u s" IN" STR= if IN-ROW true exit then
   a u s" FINAL" STR= if FINAL-ROW false exit then
   open a u COMMENT? 0= and if a u LINE true exit then
   E-SYNTAX throw ;

: READ-TABLE ( -- )
   HEADER
   false
   begin TOKEN 2dup s" ;DEFINE" STR= 0= while STEP repeat
   2drop drop ;


: WORD+ ( ^u8 n -- ) {: a:^ u:n :}
   WORD-LEN @ u + WORD-TEXT-RESERVE
   a WORD-LEN @ WORD-TEXT u BYTE-COPY
   WORD-LEN @ u + WORD-LEN ! ;

: WORD$ ( -- ^u8 n ) 0 WORD-TEXT WORD-LEN @ ;

\ <NAME>/<state>, of state token K.
: STATE-NAME ( n -- ^u8 n ) {: k:n :}
   0 WORD-LEN ! NAME$ WORD+ s" /" WORD+ k TOK$ WORD+ WORD$ ;

\ <NAME>/<state>/<input>, of state token K and line token L.
: LINE-NAME ( n n -- ^u8 n ) {: k:n l:n :}
   k STATE-NAME 2drop s" /" WORD+ l TOK$ WORD+ WORD$ ;

: STEP-NAME ( -- ^u8 n )
   0 WORD-LEN ! NAME$ WORD+ s" -STEP" WORD+ WORD$ ;

: CTX-TYPE ( -- EFFECT:type ) CTX$ EFFECT:READ-TYPE ;
: STATES-TYPE ( -- EFFECT:type ) STATES$ EFFECT:READ-TYPE ;

\ `construct <states> <state>`.
: NEW-STATE ( ^u8 n -- ) {: a:^ u:n :}
   s" construct" EFFECT:KEYWORD
   STATES$ STATES-TYPE EFFECT:TYPE-ITEM
   a u EFFECT:OPERAND ;

\ <word> ( CTX -- ) runs the ACTION, `drop` for `-`.
: ACT-WORD ( ^u8 n ^u8 n -- ) {: w:^ wu:n a:^ au:n :}
   w wu EFFECT:EMPTY CTX-TYPE EFFECT:IN define-word
   a au s" -" STR= if postpone drop else a au EFFECT:CALL then
   postpone ; ;

\ A line's word, unless its ACTION is `-`.
: LINE-ACT ( n n -- ) {: k:n l:n :}
   l 1+ TOK$ s" -" STR= if exit then
   k l LINE-NAME l 1+ TOK$ ACT-WORD ;

: ROW-ACTS ( n -- ) {: r:n :}
   r ROW-AT @ {: k:n :}
   r FINAL? if k STATE-NAME k 1+ TOK$ ACT-WORD exit then
   r ROW-LINES @ 0 ?do k  k i LINE-TOK  LINE-ACT loop ;


\ The NEXT of line L of state token K; ACT says the line has a word.
: NEXT-ITEMS ( bool n n -- ) {: act:bool k:n l:n :}
   l 2 + TOK$ {: x:^ xu:n :}
   x c@ [char] > = 0= if
      act if postpone dup  k l LINE-NAME EFFECT:CALL then
      x xu EFFECT:CALL exit
   then
   act 0= if postpone drop then
   x 1+ xu 1- NEW-STATE
   act if postpone swap  k l LINE-NAME EFFECT:CALL then ;

: LINE-ARM ( n n -- ) {: k:n l:n :}
   l TOK$ EFFECT:OPERAND  s" of" EFFECT:KEYWORD
   l 1+ TOK$ s" -" STR= 0= k l NEXT-ITEMS
   s" endof" EFFECT:KEYWORD ;

\ <NAME>/<state> ( CTX -- STATES ) dup SENSE match <inputs> … ;match
: ROW-WORD ( n -- ) {: r:n :}
   r ROW-AT @ {: k:n :}
   k STATE-NAME
   EFFECT:EMPTY CTX-TYPE EFFECT:IN STATES-TYPE EFFECT:OUT define-word
   postpone dup  k 1+ TOK$ EFFECT:CALL
   s" match" EFFECT:KEYWORD  k 2 + TOK$ 2dup EFFECT:READ-TYPE EFFECT:TYPE-ITEM
   r ROW-LINES @ 0 ?do k  k i LINE-TOK  LINE-ARM loop
   s" ;match" EFFECT:KEYWORD  postpone ; ;


\ An IN state's arm is `false swap <NAME>/<state>`, a FINAL state's
\ `true construct <states> <state> rot <NAME>/<state>`.
: STEP-ARM ( n -- ) {: r:n :}
   r ROW-AT @ {: k:n :}
   k TOK$ EFFECT:OPERAND  s" of" EFFECT:KEYWORD
   r FINAL? if
      postpone true  k TOK$ NEW-STATE  postpone rot
   else
      postpone false  postpone swap
   then
   k STATE-NAME EFFECT:CALL  s" endof" EFFECT:KEYWORD ;

: STEP-EFFECT ( -- EFFECT:type )
   EFFECT:EMPTY CTX-TYPE EFFECT:IN STATES-TYPE EFFECT:IN
   STATES-TYPE EFFECT:OUT s" bool" EFFECT:NAMED EFFECT:OUT ;

\ <NAME>-STEP ( CTX STATES -- STATES bool ) match <states> … ;match swap
: STEP-WORD ( -- )
   STEP-NAME STEP-EFFECT define-word
   s" match" EFFECT:KEYWORD  STATES$ STATES-TYPE EFFECT:TYPE-ITEM
   ROWS @ 0 ?do i STEP-ARM loop
   s" ;match" EFFECT:KEYWORD  postpone swap  postpone ; ;

\ <NAME> ( CTX STATES -- ) begin over swap <NAME>-STEP until 2drop
: RUN-WORD ( -- )
   NAME$ EFFECT:EMPTY CTX-TYPE EFFECT:IN STATES-TYPE EFFECT:IN define-word
   s" begin" EFFECT:KEYWORD  postpone over  postpone swap
   STEP-NAME EFFECT:CALL  s" until" EFFECT:KEYWORD
   postpone 2drop  postpone ; ;


\ The words are built with their table tokens read last, then the `;DEFINE`
\ READ-TABLE stopped at is read last again.
: GENERATE ( -- )
   READ-TABLE
   SRCLOC:TOKEN$ {: a:^ u:n :}
   [: ROWS @ 0 ?do i ROW-ACTS loop
      ROWS @ 0 ?do i FINAL? 0= if i ROW-WORD then loop
      STEP-WORD  RUN-WORD ;]
   [: a u SRCLOC:TOKEN! ;] finally ;

public

\ DEFINE <NAME> ( <ctx> ) <states> <inputs> IN … FINAL … ;DEFINE, in the
\ caller's open package and section.
: DEFINE ( -- )
   [: GENERATE ;] [: RELEASE ;] finally ;

;package

parses-through: FSM:DEFINE 0 ( ;DEFINE )
