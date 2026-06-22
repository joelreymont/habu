\ token.f - checked whitespace token table for native lint tools.
\ Load after tools/lint/text.f.

$6000 constant TMAX
77 constant E-LINT-TOKEN-CAP

create TOFF TMAX cells allot
create TLEN TMAX cells allot
create TBOL TMAX cells allot

variable TN#
variable PARENS?
variable TI
variable TS
variable BOL

: SP? ( n -- bool )
   dup 32 = over 9 = or swap 10 = or ;

: T-OFF-FIELD ( n -- ptr ptr u8 )
   cells TOFF + 0 ptr-field ;

: T-OFF@ ( n -- ptr u8 )
   T-OFF-FIELD @ ;

: T-OFF! ( ptr u8 n -- ) {: a:ptr k :}
   a k T-OFF-FIELD ! ;

: T-LEN@ ( n -- n )
   cells TLEN + @ ;

: T-LEN! ( n n -- ) {: u k :}
   u k cells TLEN + ! ;

: T-FLAG ( n -- bool )
   0= if LINT-FALSE else LINT-TRUE then ;

: T-BOL@ ( n -- bool )
   cells TBOL + @ T-FLAG ;

: T-BOL! ( bool n -- ) {: f k :}
   f k cells TBOL + ! ;

: BOL? ( -- bool )
   BOL @ T-FLAG ;

: PARENS-ENABLED? ( -- bool )
   PARENS? @ T-FLAG ;

: TOKEN-ENSURE ( -- )
   TN# @ TMAX >= if E-LINT-TOKEN-CAP throw then ;

: TOKEN+ ( ptr u8 n bool -- ) {: a:ptr u bol :}
   TOKEN-ENSURE
   a TN# @ T-OFF!
   u TN# @ T-LEN!
   bol TN# @ T-BOL!
   TN# @ 1+ TN# ! ;

: TOK ( n -- ptr u8 n ) {: k :}
   k T-OFF@ k T-LEN@ ;

: TOK0? ( n -- bool )
   T-BOL@ ;

: TOK= ( n ptr u8 n -- bool ) {: k a:ptr u :}
   k TOK a u STR= ;

: TEOL? ( n -- bool )
   1+ dup TN# @ >= if drop LINT-TRUE else TOK0? then ;

: TOKEN-C@ ( ptr u8 -- n )
   TI @ + c@ ;

: TOKEN-ADV ( -- )
   TI @ 1+ TI ! ;

: TOKEN-SKIP-SPACE ( ptr u8 n -- ) {: a:ptr u :}
   begin TI @ u < while
      a TOKEN-C@ SP? if
         a TOKEN-C@ 10 = if LINT-TRUE BOL ! then
         TOKEN-ADV
      else
         exit
      then
   repeat ;

: TOKEN-SKIP-LINE ( ptr u8 n -- ) {: a:ptr u :}
   begin TI @ u < a TOKEN-C@ 10 <> and while
      TOKEN-ADV
   repeat ;

: TOKEN-PAREN-START? ( ptr u8 n -- bool ) {: a:ptr u :}
   PARENS-ENABLED? 0= if LINT-FALSE exit then
   a TOKEN-C@ 40 <> if LINT-FALSE exit then
   TI @ 1+ u >= if LINT-FALSE exit then
   a TI @ 1+ + c@ 32 = ;

: TOKEN-SKIP-PAREN ( ptr u8 n -- ) {: a:ptr u :}
   begin TI @ u < a TOKEN-C@ 41 <> and while
      TOKEN-ADV
   repeat
   TI @ u < if TOKEN-ADV then ;

: TOKEN-READ ( ptr u8 n -- ) {: a:ptr u :}
   TI @ TS !
   BOL? LINT-FALSE BOL !
   begin TI @ u < a TOKEN-C@ SP? 0= and while
      TOKEN-ADV
   repeat
   a TS @ + TI @ TS @ - rot TOKEN+ ;

: TOKENIZE ( ptr u8 n -- ) {: a:ptr u :}
   0 TN# !
   0 TI !
   LINT-TRUE BOL !
   begin TI @ u < while
      a u TOKEN-SKIP-SPACE
      TI @ u < if
         a TOKEN-C@ 92 = if a u TOKEN-SKIP-LINE
         else a u TOKEN-PAREN-START? if a u TOKEN-SKIP-PAREN
         else a u TOKEN-READ then then
      then
   repeat ;
