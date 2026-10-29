\ load-refs.f - the files a source names as loaded, read from its tokens.
\ REQUIRES loads into the image once (a file the image holds loads nothing),
\ INCLUDES reads again, LAUNCHES runs in a child (a literal a load helper
\ consumes). An import whose path is no literal the engine decodes is OPAQUE.
\ EACH and OPAQUE-EACH read src/lib/source-lex.f's tokens of the source lexed
\ last. Used by test/gate-images.f and test/gate-entry-guard.f.

require src/lib/string.f
require src/lib/source-lex.f

package LOAD-REFS

public

0 constant REQUIRES
1 constant INCLUDES
2 constant LAUNCHES

private

\ EACH's visitor takes path, line, kind.
TYPED-VARIABLE VISIT [ ptr u8 n n n -- ]

: IMPORT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" require" STR=CI
   a u s" include" STR=CI or ;

\ Token idx is an `s"` or `s\"` literal, which leaves its string.
: STRING? ( n -- bool )
   {: idx:n :}
   idx LINT-LEX:LITERAL? 0= if false exit then
   idx LINT-LEX:TOKEN drop c@ dup 115 = swap 83 = or ;

: STRING-IMPORT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" required" STR=CI
   a u s" included" STR=CI or ;

: IMPORT-KIND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u s" require" STR=CI
   a u s" required" STR=CI or if REQUIRES else INCLUDES then ;

\ ---- escaped literals --------------------------------------------------------
\ LITERAL runs an escaped literal's text, opener through closing quote, under
\ evaluate-closed, so it holds what the engine's reader decodes. A bad escape
\ throws; zero or one cell left is PATH!'s underdepth, 70; more is
\ E-EVAL-RESIDUE. The decoded bytes stay in data space, and evaluation exits
\ while a task is live (src/habu/interpret.f), so EACH and OPAQUE-EACH run
\ before any task starts.

PTR-VARIABLE TEXT-A
variable TEXT-U
PTR-VARIABLE PATH-A
variable PATH-U

public

\ TEXT$ and PATH! are the hooks of LITERAL's closed text, which runs in the
\ caller's scope and so reaches them only qualified.
: TEXT$ ( -- ptr u8 n )
   TEXT-A @ TEXT-U @ ;

: PATH! ( ptr u8 n -- )
   PATH-U !
   PATH-A ! ;

private

\ ---- references ---------------------------------------------------------------

\ A plain literal (a two-byte opener) holds its payload; the token is the
\ opener, one delimiter, the payload and its closing quote.
: LITERAL ( n -- ptr u8 n bool ) {: idx:n :}
   idx LINT-LEX:CONTENT {: a:ptr u:n :}
   idx LINT-LEX:TOKEN {: o:ptr ou:n :}
   ou 2 = if a u true exit then
   o TEXT-A !
   ou u + 2 + TEXT-U !
   [: s" LOAD-REFS:TEXT$ evaluate LOAD-REFS:PATH!" evaluate-closed ;] catch
   0= if PATH-A @ PATH-U @ true exit then
   a u false ;

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
: DIRECT-LOAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" GE-SRC-FILE+" STR=CI
   a u s" GE-ARG+" STR=CI or
   a u s" ARG" STR=CI or
   a u s" ARG+" STR=CI or
   a u s" ARGS" STR=CI or
   a u s" CHILD-RUN" STR=CI or ;

: DIAGNOSTIC-LOAD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" DIAGNOSTIC" STR=CI
   a u s" DIAGNOSTIC-WB" STR=CI or ;

: LITERAL-LOAD? ( n -- bool ) {: idx:n :}
   idx NEXT-CODE {: next:n :}
   next LINT-LEX:COUNT >= if false exit then
   next LINT-LEX:TOKEN DIRECT-LOAD? if true exit then
   next LINT-LEX:TOKEN s" >LEN" STR=CI if
      next NEXT-CODE {: consumer:n :}
      consumer LINT-LEX:COUNT >= if false exit then
      consumer LINT-LEX:TOKEN s" PROC-ARGV+" STR=CI exit
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

\ Only a literal a load consumes is evaluated.
: SCAN-STRING ( n -- )
   {: idx:n :}
   idx STRING? 0= if exit then
   idx IMPORTED? {: import:bool :}
   import idx LITERAL-LOAD? or 0= if exit then
   idx LITERAL 0= if 2drop exit then
   idx LINT-LEX:LINE@
   import if idx NEXT-CODE LINT-LEX:TOKEN IMPORT-KIND else LAUNCHES then
   VISIT @ execute ;

: SCAN-TOKEN ( n -- ) {: idx:n :}
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx SCAN-WORD
   idx SCAN-STRING ;

\ `required` or `included` with no literal before it that the engine decodes.
\ The word is matched by its spelling alone, so one that loads nothing, such as
\ a local named `required`, is opaque too: the finding fails closed and lets no
\ load pass unread.
: OPAQUE? ( n -- bool ) {: idx:n :}
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if false exit then
   idx LINT-LEX:TOKEN STRING-IMPORT? 0= if false exit then
   idx PREV-CODE {: prev:n :}
   prev 0 < if true exit then
   prev STRING? 0= if true exit then
   prev LITERAL nip nip 0= ;

public

: EACH ( [ ptr u8 n n n -- ] -- )
   VISIT !
   LINT-LEX:COUNT 0 ?do i SCAN-TOKEN loop ;

: OPAQUE-EACH ( [ n -- ] -- ) {: q :}
   LINT-LEX:COUNT 0 ?do
      i OPAQUE? if i LINT-LEX:LINE@ q execute then
   loop ;

;package
