\ token.f - token table projected from the shared source lexer.
require tools/lint/text.f
require src/lib/source.f
require src/lib/source-lex.f
require src/core/exit-status.f

$10000 constant TMAX            \ largest linted source + headroom
-4821 s" the source lexer rejected the linted source" ERROR:CODE E-LINT-TOKEN-SOURCE

\ Each token's start is an address into the linted source.
TMAX TYPED-BUFFER TOFF ^u8
create TLEN TMAX cells allot

variable TN#
variable PARENS?

: T-OFF@ ( n -- ^u8 )
   TOFF @ ;

: T-OFF! ( ^u8 n -- ) {: a:^ k :}
   a k TOFF ! ;

: T-LEN@ ( n -- n )
   cells TLEN + @ ;

: T-LEN! ( n n -- ) {: u k :}
   u k cells TLEN + ! ;

: T-FLAG ( n -- bool )
   0= if LINT-FALSE else LINT-TRUE then ;

: PARENS-ENABLED? ( -- bool )
   PARENS? @ T-FLAG ;

: TOKEN-ENSURE ( -- )   \ labeled capacity exit: a bare rc 77 is unattributable
   TN# @ TMAX >= if s" lint: token table full (TMAX); grow tools/lint/token.f" EXIT-STATUS:FULL die then ;

: TOKEN+ ( ^u8 n -- ) {: a:^ u :}
   TOKEN-ENSURE
   a TN# @ T-OFF!
   u TN# @ T-LEN!
   TN# @ 1+ TN# ! ;

: TOK ( n -- ^u8 n ) {: k :}
   k T-OFF@ k T-LEN@ ;

: TOK= ( n ^u8 n -- bool ) {: k a:^ u :}
   k TOK a u LINT-STR= ;

\ Primitive-name and REPL-path consumers expect an opener followed by its
\ payload with the closing quote. Keep that pair, but never split the payload:
\ its backslashes, semicolons and emitter-shaped text are inert.
: TOKEN-FROM-LEX ( n -- ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:COMMENT = PARENS-ENABLED? and if exit then
   k LINT-LEX:TOKEN TOKEN+
   k LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   k LINT-LEX:TOKEN SOURCE:NORMAL-STRING-OPENER?
   k LINT-LEX:TOKEN SOURCE:ESC-STRING-OPENER? or if
      k LINT-LEX:CONTENT 1+ TOKEN+
   then ;

: TOKENIZE ( ^u8 n -- )
   0 TN# !
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? if E-LINT-TOKEN-SOURCE throw then
   LINT-LEX:COUNT 0 ?do i TOKEN-FROM-LEX loop ;
