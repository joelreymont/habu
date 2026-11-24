split-str
\ load-refs.f - the files a source names as loaded, read from its tokens.
\ REQUIRES loads into the image once (a file the image holds loads nothing),
\ INCLUDES reads again, LAUNCHES runs in a child (a literal a load helper
\ consumes). An import whose path is no literal the engine decodes is OPAQUE.
\ EACH and OPAQUE-EACH read src/lib/source-lex.f's tokens of the source lexed
\ last. Used by test/load-graph.f and test/gate-entry-guard.f.

require src/lib/string.f
require src/lib/source-lex.f
require src/habu/escape.f

package LOAD-REFS

public

0 constant REQUIRES
1 constant INCLUDES
2 constant LAUNCHES

private

\ EACH's visitor takes path, line, kind.
TYPED-VARIABLE VISIT [ ^u8 n n n -- ]

: IMPORT? ( str -- bool ) {: a:^ u:n :}
   a u s" require" STR:=CI
   a u s" include" STR:=CI or ;

\ Token idx is an `s"` or `s\"` literal, which leaves its string.
: STRING? ( n -- bool )
   {: idx:n :}
   idx LINT-LEX:LITERAL? 0= if false exit then
   idx LINT-LEX:TOKEN drop c@ dup 115 = swap 83 = or ;

: STRING-IMPORT? ( str -- bool ) {: a:^ u:n :}
   a u s" required" STR:=CI
   a u s" included" STR:=CI or ;

: IMPORT-KIND ( str -- n )
   {: a:^ u:n :}
   a u s" require" STR:=CI
   a u s" required" STR:=CI or if REQUIRES else INCLUDES then ;

\ ---- references ---------------------------------------------------------------

DYNAMIC-BUFFER DEC u8

\ A plain literal (two-byte opener) is its payload; an escaped one, whole since
\ the lexer stops at a bad escape, decodes into DEC until the next LITERAL.
: LITERAL ( n -- str ) {: idx:n :}
   idx LINT-LEX:CONTENT {: a:^ u:n :}
   idx LINT-LEX:TOKEN nip 2 = if a u exit then
   u 1+ DEC-RESERVE
   0 DEC {: dst:^ :}
   a u dst ESCAPE:DECODE drop  dst swap ;

\ Comments are lexer tokens, but they leave the data stack untouched.
: NEXT-CODE ( n -- n )
   1+ begin dup LINT-LEX:COUNT < while
      dup LINT-LEX:KIND@ LINT-LEX:COMMENT <> if exit then
      1+
   repeat ;

: PREV-CODE ( n -- n )
   1- begin dup 0 >= while
      dup LINT-LEX:KIND@ LINT-LEX:COMMENT <> if exit then
      1-
   repeat ;

\ These consumers take the path span directly. A later consumer is not proof
\ that an earlier string survived the words between them.
: DIRECT-LOAD? ( str -- bool ) {: a:^ u:n :}
   a u s" GE-SRC-FILE+" STR:=CI
   a u s" GE-ARG+" STR:=CI or
   a u s" ARG" STR:=CI or
   a u s" ARG+" STR:=CI or
   a u s" ARGS" STR:=CI or
   a u s" CHILD-RUN" STR:=CI or ;

: DIAGNOSTIC-LOAD? ( str -- bool ) {: a:^ u:n :}
   a u s" DIAGNOSTIC" STR:=CI
   a u s" DIAGNOSTIC-WB" STR:=CI or ;

: LITERAL-LOAD? ( n -- bool ) {: idx:n :}
   idx NEXT-CODE {: next:n :}
   next LINT-LEX:COUNT >= if false exit then
   next LINT-LEX:TOKEN DIRECT-LOAD? if true exit then
   next LINT-LEX:TOKEN s" >LEN" STR:=CI if
      next NEXT-CODE {: consumer:n :}
      consumer LINT-LEX:COUNT >= if false exit then
      consumer LINT-LEX:TOKEN s" PROC-ARGV+" STR:=CI exit
   then
   next STRING? if
      next NEXT-CODE {: consumer:n :}
      consumer LINT-LEX:COUNT >= if false exit then
      consumer LINT-LEX:TOKEN DIAGNOSTIC-LOAD? exit
   then
   false ;

: SCAN-WORD ( n -- )
   {: idx:n :}
   idx LINT-LEX:TOKEN IMPORT? 0= if exit then
   idx 1+ LINT-LEX:COUNT >= if exit then
   idx 1+ LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx 1+ LINT-LEX:TOKEN
   idx 1+ LINT-LEX:LINE@
   idx LINT-LEX:TOKEN IMPORT-KIND VISIT @ execute ;

: IMPORTED? ( n -- bool )
   NEXT-CODE {: next:n :}
   next LINT-LEX:COUNT >= if false exit then
   next LINT-LEX:TOKEN STRING-IMPORT? ;

\ Only a literal a load consumes is decoded.
: SCAN-STRING ( n -- )
   {: idx:n :}
   idx STRING? 0= if exit then
   idx IMPORTED? {: import:bool :}
   import idx LITERAL-LOAD? or 0= if exit then
   idx LITERAL
   idx LINT-LEX:LINE@
   import if idx NEXT-CODE LINT-LEX:TOKEN IMPORT-KIND else LAUNCHES then
   VISIT @ execute ;

: SCAN-TOKEN ( n -- ) {: idx:n :}
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx SCAN-WORD
   idx SCAN-STRING ;

\ `required` or `included` with no literal before it.
\ The word is matched by its spelling alone, so one that loads nothing, such as
\ a local named `required`, is opaque too: the finding fails closed and lets no
\ load pass unread.
: OPAQUE? ( n -- bool ) {: idx:n :}
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if false exit then
   idx LINT-LEX:TOKEN STRING-IMPORT? 0= if false exit then
   idx PREV-CODE {: prev:n :}
   prev 0 < if true exit then
   prev STRING? 0= ;

public

: EACH ( [ ^u8 n n n -- ] -- )
   VISIT !
   LINT-LEX:COUNT 0 ?do i SCAN-TOKEN loop ;

: OPAQUE-EACH ( [ n -- ] -- ) {: q :}
   LINT-LEX:COUNT 0 ?do
      i OPAQUE? if i LINT-LEX:LINE@ q execute then
   loop ;

;package
