\ token.f - checked whitespace token table for native lint tools.
\ Load after tools/lint/text.f.

$8000 constant TMAX             \ largest linted source (src/core/checker.f) + headroom
77 constant E-LINT-TOKEN-CAP
s" E-LINT-TOKEN-CAP" E-LINT-TOKEN-CAP LINT-CODE-NAME+

create TOFF TMAX cells allot
create TLEN TMAX cells allot
create TBOL TMAX cells allot

variable TN#
variable PARENS?
variable LINT-TI
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
   k TOK a u LINT-STR= ;

: TEOL? ( n -- bool )
   1+ dup TN# @ >= if drop LINT-TRUE else TOK0? then ;

: TOKEN-C@ ( ptr u8 -- n )
   LINT-TI @ + c@ ;

: TOKEN-ADV ( -- )
   LINT-TI @ 1+ LINT-TI ! ;

: TOKEN-SKIP-SPACE ( ptr u8 n -- ) {: a:ptr u :}
   begin LINT-TI @ u < while
      a TOKEN-C@ SP? if
         a TOKEN-C@ 10 = if LINT-TRUE BOL ! then
         TOKEN-ADV
      else
         exit
      then
   repeat ;

: TOKEN-SKIP-LINE ( ptr u8 n -- ) {: a:ptr u :}
   begin LINT-TI @ u < a TOKEN-C@ 10 <> and while
      TOKEN-ADV
   repeat ;

: TOKEN-PAREN-START? ( ptr u8 n -- bool ) {: a:ptr u :}
   PARENS-ENABLED? 0= if LINT-FALSE exit then
   a TOKEN-C@ 40 <> if LINT-FALSE exit then
   LINT-TI @ 1+ u >= if LINT-FALSE exit then
   a LINT-TI @ 1+ + c@ 32 = ;

: TOKEN-SKIP-PAREN ( ptr u8 n -- ) {: a:ptr u :}
   begin LINT-TI @ u < a TOKEN-C@ 41 <> and while
      TOKEN-ADV
   repeat
   LINT-TI @ u < if TOKEN-ADV then ;

: TOKEN-READ ( ptr u8 n -- ) {: a:ptr u :}
   LINT-TI @ TS !
   BOL? LINT-FALSE BOL !
   begin LINT-TI @ u < a TOKEN-C@ SP? 0= and while
      TOKEN-ADV
   repeat
   a TS @ + LINT-TI @ TS @ - rot TOKEN+ ;

: TOKENIZE ( ptr u8 n -- ) {: a:ptr u :}
   0 TN# !
   0 LINT-TI !
   LINT-TRUE BOL !
   begin LINT-TI @ u < while
      a u TOKEN-SKIP-SPACE
      LINT-TI @ u < if
         a TOKEN-C@ 92 = if a u TOKEN-SKIP-LINE
         else a u TOKEN-PAREN-START? if a u TOKEN-SKIP-PAREN
         else a u TOKEN-READ then then
      then
   repeat ;
