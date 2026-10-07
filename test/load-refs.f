\ load-refs.f - the files a source names as loaded, read from its tokens.
\
\ Three kinds, which EACH hands its visitor. REQUIRES loads the file into the
\ same image once: `require` and a path, or a string literal handed to
\ `required`; a file the image already holds, as it holds the engine's own,
\ loads nothing. INCLUDES reads the file again whatever the image holds:
\ `include` and a path, or a literal handed to `included`. LAUNCHES runs the
\ file in a child: a string literal a known load helper consumes.
\ test/gate-images.f reads all three into the gate's load graph, which finds
\ the keyed images a row needs and which test/gate-entry-guard.f walks to refuse
\ a row that runs another row's entry.
\
\ A string literal is the lexer's literal token (LINT-LEX:LITERAL?), never a
\ word spelled like its opener. An escaped literal (`s\"`) names the bytes the
\ engine decodes it to. An import whose path is no literal the engine decodes,
\ such as a computed string handed to `required` or a literal with a bad
\ escape, is OPAQUE: EACH skips it and OPAQUE-EACH names it.
\
\ Both read the tokens lib/source-lex.f holds for the source their caller
\ lexed last.

require lib/string.f
require lib/source-lex.f

package LOAD-REFS

public

0 constant REQUIRES
1 constant INCLUDES
2 constant LAUNCHES

private

\ The visitor EACH hands each reference to: path, line, kind.
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

\ The kind of an import word.
: IMPORT-KIND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u s" require" STR=CI
   a u s" required" STR=CI or if REQUIRES else INCLUDES then ;

\ ---- escaped literals --------------------------------------------------------
\ An escaped literal holds what the engine's own reader decodes it to: LITERAL
\ runs the literal's text, opener through closing quote, as a closed program
\ under plain `evaluate`, and PATH! takes the one string it must leave. A bad
\ escape throws; no cell or one is PATH!'s underdepth, 70; more is the closed
\ residue, E-EVAL-RESIDUE. The decoded bytes stay in data space, as an
\ interpret-mode `s\"` keeps them, and evaluation exits while a task is live
\ (docs/threads.md), so EACH and OPAQUE-EACH run before any task starts.

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

\ The bytes a string literal holds, and whether the engine decodes them. A plain
\ literal holds its payload. The opener is followed by one delimiter, then the
\ payload and its closing quote.
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

\ Hand every reference in the lexed source to the visitor, in source order.
: EACH ( [ ptr u8 n n n -- ] -- )
   VISIT !
   LINT-LEX:COUNT 0 ?do i SCAN-TOKEN loop ;

\ Hand the line of every opaque import to the visitor, in source order.
: OPAQUE-EACH ( [ n -- ] -- ) {: q :}
   LINT-LEX:COUNT 0 ?do
      i OPAQUE? if i LINT-LEX:LINE@ q execute then
   loop ;

;package
