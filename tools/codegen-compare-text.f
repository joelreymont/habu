\ codegen-compare-text.f - the line and token cursor the comparison's readers
\ share. One concern: walking text a line at a time and a word at a time.
\
\ TWO READERS, ONE CURSOR. The committed tables are read back structurally -
\ split into lines, each line split into words, each word turned into a row
\ field - and so is what nm and size say about the reference object. That is one
\ reader written twice until it is written once; this is the once. Both callers
\ get the same refusal on a malformed line, and a fix to the tokenizer reaches
\ both.
\
\ ONE CURSOR AT A TIME, AND SAYING SO. The state here is a single line cursor
\ and a single token cursor, because both callers walk one text to its end
\ before another is opened: the reference object is read at setup and a
\ committed table during a comparison. Two walks interleaved would share one
\ cursor and read each other's position, so nothing here is re-entrant and no
\ caller may nest one walk inside another.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/adt/option.f

package CODEGEN-TEXT

private

32 constant SPACE-BYTE
9 constant TAB-BYTE
10 constant NEWLINE-BYTE
48 constant DIGIT-0
57 constant DIGIT-9
97 constant LOWER-A
102 constant LOWER-F
65 constant UPPER-A
70 constant UPPER-F
16 constant HEX-BASE

PTR-VARIABLE TEXT-A
variable TEXT-U
variable TEXT-CURSOR
PTR-VARIABLE LINE-A
variable LINE-U
variable CURSOR

: LINE$ ( -- ptr u8 n )
   LINE-A @ LINE-U @ ;

: TRUE? ( -- bool )
   0 0= ;

: FALSE? ( -- bool )
   TRUE? 0= ;

\ A word is bounded by a space or a tab: size's output indents with tabs and
\ nm's with spaces, and a reader that knew only one of them would read a whole
\ indented line as one word.
: BLANK? ( n -- bool ) {: c:n :}
   c SPACE-BYTE = if TRUE? exit then
   c TAB-BYTE = ;

: SKIP-BLANKS ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start begin dup u < while
      dup a + c@ BLANK? 0= if exit then
      1+
   repeat ;

: TOKEN ( ptr u8 n n -- ptr u8 n n bool ) {: a:ptr u:n start:n :}
   a u start SKIP-BLANKS {: from:n :}
   from u >= if a 0 u FALSE? exit then
   from begin dup u < while
      dup a + c@ BLANK? if
         a from +  over from -  rot  TRUE? exit
      then
      1+
   repeat drop
   a from +  u from -  u  TRUE? ;

: HEX-DIGIT ( n -- n ) {: c:n :}
   c DIGIT-0 < if -1 exit then
   c DIGIT-9 <= if c DIGIT-0 - exit then
   c UPPER-A < if -1 exit then
   c UPPER-F <= if c UPPER-A - 10 + exit then
   c LOWER-A < if -1 exit then
   c LOWER-F <= if c LOWER-A - 10 + exit then
   -1 ;

public

\ ---- lines -------------------------------------------------------------------

\ Open a text for walking. Both cursors go back to the start, so a caller that
\ opens a text always reads it from its first line.
: TEXT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a TEXT-A !
   u TEXT-U !
   0 TEXT-CURSOR !
   a LINE-A !
   0 LINE-U !
   0 CURSOR ! ;

: NEXT-LINE ( -- ptr u8 n bool )
   TEXT-A @ TEXT-U @ NEWLINE-BYTE TEXT-CURSOR @ SPLIT-NEXT
   swap TEXT-CURSOR ! ;

\ ---- words within a line -----------------------------------------------------

: LINE! ( ptr u8 n -- )
   LINE-U ! LINE-A !
   0 CURSOR ! ;

: NEXT$ ( -- ptr u8 n bool )
   LINE$ CURSOR @ TOKEN
   swap CURSOR ! ;

\ The next word as a decimal number, or false. A word that is not a number is
\ not consumed differently from one that is: the cursor has moved past it either
\ way, which is what lets a caller read a row's fields in order and report the
\ first one that is not a number.
: NEXT-NUMBER ( -- n bool )
   NEXT$ 0= if 2drop 0 FALSE? exit then
   STR>NUMBER? MATCH option
     none OF 0 FALSE? ENDOF
     some OF TRUE? ENDOF
   ;MATCH ;

\ The next word as a hexadecimal number, which is how nm writes an address. An
\ empty word, or one carrying anything that is not a hexadecimal digit, is false.
: NEXT-HEX ( -- n bool )
   NEXT$ 0= if 2drop 0 FALSE? exit then
   {: a:ptr u:n :}
   u 0= if 0 FALSE? exit then
   0
   u 0 ?do
      i a + c@ HEX-DIGIT
      dup 0 < if 2drop 0 FALSE? unloop exit then
      swap HEX-BASE * +
   loop
   TRUE? ;

;package
