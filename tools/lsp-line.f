\ lsp-line.f - the language server's line readers: the LF-ended lines of a
\ text, and the top-level members of a line that holds one JSON object, as each
\ of the checker's packets does.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the JSON reader storage belongs to the server's one
\ task.

require src/lib/string.f
require src/lib/adt/option.f
require src/lib/json.f

package LSP-LINE

private

10 constant LF

TYPED-VARIABLE JSON-ST JSON:cursor

\ The line of the text at offset O, without its LF, and the offset after it.
: LINE ( ptr u8 n n -- ptr u8 n n )
   {: a:ptr u:n o:n :}
   a o + u o - LF INDEX-OF MATCH option
      none OF a o + u o - u ENDOF
      some OF IDX>N dup >r a o + swap o r> + 1+ ENDOF
   ;MATCH ;

public

: EACH-LINE ( ptr u8 n [ ptr u8 n -- ] -- )
   {: a:ptr u:n q :}
   0 begin dup u < while
      a u rot LINE >r q execute r>
   repeat drop ;

: HAS-LINE? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n k:ptr ku:n :}
   0 begin dup u < while
      a u rot LINE >r k ku STR= if r> drop true exit then r>
   repeat drop false ;

\ The raw text of a top-level member of the line and its JSON token kind, or -1
\ when the line has no such member: a string's text between its quotes,
\ escapes and all, or a number's digits. The line is one JSON object.
: MEMBER ( ptr u8 n ptr u8 n -- ptr u8 n n )
   {: a:ptr u:n k:ptr ku:n :}
   JSON-ST a u JSON:READ-INIT
   JSON:READ-NEXT drop
   k ku JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE NULL$ -1 exit then
   JSON:READ-TOKEN >r JSON:READ-SPAN$ rot JSON:READ-CLOSE r> ;

: STRING-MEMBER ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   MEMBER JSON:T-STR = ;

\ A member that is an integer a cell holds; any other is as good as absent.
: INT-MEMBER ( ptr u8 n ptr u8 n -- n bool )
   MEMBER JSON:T-INT <> if 2drop 0 false exit then
   STR>NUMBER? MATCH option
      none OF 0 false ENDOF
      some OF true ENDOF
   ;MATCH ;

;package
