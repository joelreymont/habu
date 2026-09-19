\ token.f - legacy token table projected from the shared source lexer.
require tools/lint/source-lex.f

$10000 constant TMAX            \ largest linted source (src/core/checker.f) + headroom
                                \ (item 9 grew checker.f past the old $8000 tokens)
77 constant E-LINT-TOKEN-CAP
s" E-LINT-TOKEN-CAP" E-LINT-TOKEN-CAP LINT-CODE-NAME+
-4821 constant E-LINT-TOKEN-SOURCE

\ Each token's start is an address into the linted source, so the starts are
\ declared pointer storage; the lengths and line flags stay plain cells.
TMAX TYPED-BUFFER TOFF ptr u8
create TLEN TMAX cells allot
create TBOL TMAX cells allot

variable TN#
variable PARENS?
variable TOKEN-LINE

: T-OFF@ ( n -- ptr u8 )
   TOFF @ ;

: T-OFF! ( ptr u8 n -- ) {: a:ptr k :}
   a k TOFF ! ;

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

: PARENS-ENABLED? ( -- bool )
   PARENS? @ T-FLAG ;

: TOKEN-ENSURE ( -- )   \ labeled capacity exit: a bare rc 77 is unattributable
   TN# @ TMAX >= if s" lint: token table full (TMAX); grow tools/lint/token.f" E-LINT-TOKEN-CAP die then ;

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

\ Primitive-name and REPL-path consumers expect an opener followed by its
\ payload with the closing quote. Keep that pair, but never split the payload:
\ its backslashes, semicolons and emitter-shaped text are inert.
: TOKEN-FROM-LEX ( n -- ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:COMMENT = PARENS-ENABLED? and if exit then
   k LINT-LEX:TOKEN
   k LINT-LEX:LINE@ TOKEN-LINE @ <> TOKEN+
   k LINT-LEX:LINE@ TOKEN-LINE !
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   k LINT-LEX:TOKEN LINT-NORMAL-STRING-OPENER?
   k LINT-LEX:TOKEN LINT-ESC-STRING-OPENER? or if
      k LINT-LEX:CONTENT 1+ LINT-FALSE TOKEN+
   then ;

: TOKENIZE ( ptr u8 n -- )
   0 TN# ! 0 TOKEN-LINE !
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-LINT-TOKEN-SOURCE throw then
   LINT-LEX:COUNT 0 ?do i TOKEN-FROM-LEX loop ;
