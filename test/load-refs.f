\ load-refs.f - the files a source names as loaded, read from its tokens.
\
\ Two kinds. An IMPORT loads the file into the same image: `require` or
\ `include` and a path, or a string literal handed to `required` or `included`.
\ A LAUNCH runs the file in a child: a string literal a known load helper
\ consumes. test/gate-images.f reads both kinds into the gate's load graph,
\ which finds the keyed images a row needs and which test/gate-entry-guard.f
\ walks to refuse a row that runs another row's entry.
\
\ EACH reads the tokens tools/lint/source-lex.f holds for the source its caller
\ lexed last.

require lib/string.f
require tools/lint/source-lex.f

package LOAD-REFS

\ The visitor EACH hands each reference to: path, line, TRUE for an import.
TYPED-VARIABLE VISIT [ ptr u8 n n bool -- ]

: IMPORT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" require" STR=CI
   a u s" include" STR=CI or ;

: STRING? ( n -- bool ) {: idx:n :}
   idx LINT-LEX:TOKEN {: a:ptr u:n :}
   u 2 = if
      a c@ dup 115 = swap 83 = or a 1+ c@ 34 = and exit
   then
   u 3 <> if false exit then
   a c@ dup 115 = swap 83 = or
   a 1+ c@ 92 = and a 2 + c@ 34 = and ;

: STRING-IMPORT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" required" STR=CI
   a u s" included" STR=CI or ;

\ Comments are lexer tokens, but they leave the data stack untouched.
: NEXT-CODE ( n -- n )
   1+ begin dup LINT-LEX:COUNT < while
      dup LINT-LEX:KIND@ LINT-LEX:COMMENT <> if exit then
      1+
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

: SCAN-WORD ( n -- ) {: idx:n :}
   idx LINT-LEX:TOKEN IMPORT? 0= if exit then
   idx 1+ LINT-LEX:COUNT >= if exit then
   idx 1+ LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx 1+ LINT-LEX:TOKEN
   idx 1+ LINT-LEX:LINE@ true VISIT @ execute ;

: SCAN-STRING ( n -- ) {: idx:n :}
   idx STRING? 0= if exit then
   idx LINT-LEX:CONTENT {: a:ptr u:n :}
   idx LITERAL-LOAD? if a u idx LINT-LEX:LINE@ false VISIT @ execute then
   idx NEXT-CODE {: next:n :}
   next LINT-LEX:COUNT >= if exit then
   next LINT-LEX:TOKEN STRING-IMPORT? if
      a u idx LINT-LEX:LINE@ true VISIT @ execute
   then ;

: SCAN-TOKEN ( n -- ) {: idx:n :}
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if exit then
   idx SCAN-WORD
   idx SCAN-STRING ;

public

\ Hand every reference in the lexed source to the visitor, in source order.
: EACH ( [ ptr u8 n n bool -- ] -- )
   VISIT !
   LINT-LEX:COUNT 0 ?do i SCAN-TOKEN loop ;

;package
