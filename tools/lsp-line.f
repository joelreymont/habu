\ lsp-line.f - the language server's line readers: the LF-ended lines of a
\ text, and the top-level members of a line that holds one JSON object, as each
\ of the checker's packets does.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the JSON reader storage belongs to the server's one
\ task.

require src/lib/string.f
require src/lib/string-option.f
require src/lib/adt/option.f
require src/lib/json.f
require src/lib/json/read.f

package LSP-LINE

private

10 constant LF

TYPED-VARIABLE JSON-ST JSON:cursor

\ The line of the text at offset O, without its LF, and the offset after it.
: LINE ( str n -- str n )
   {: s:str o:n :}
   s STR:DATA o + s STR:LEN o - >str LF STR:INDEX-OF MATCH option
      none OF s STR:DATA o + s STR:LEN o - >str s STR:LEN ENDOF
      some OF IDX>N dup >r s STR:DATA o + swap >str o r> + 1+ ENDOF
   ;MATCH ;

public

: EACH-LINE ( str [ str -- ] -- )
   {: s:str q :}
   0 begin dup s STR:LEN < while
      s swap LINE >r q execute r>
   repeat drop ;

: HAS-LINE? ( str str -- bool )
   {: s:str k:str :}
   0 begin dup s STR:LEN < while
      s swap LINE >r k STR:= if r> drop true exit then r>
   repeat drop false ;

\ The raw text of a top-level member of the line and its JSON token kind, or -1
\ when the line has no such member: a string's text between its quotes,
\ escapes and all, or a number's digits. The line is one JSON object.
: MEMBER ( str str -- str n )
   {: s:str k:str :}
   JSON-ST s JSON:READ-INIT
   JSON:READ-NEXT drop
   k JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE NULL$ -1 exit then
   JSON:READ-TOKEN >r JSON:READ-SPAN$ swap JSON:READ-CLOSE r> ;

: STRING-MEMBER ( str str -- str bool )
   MEMBER JSON:T-STR = ;

\ A member that is an integer a cell holds; any other is as good as absent.
: INT-MEMBER ( str str -- n bool )
   MEMBER JSON:T-INT <> if drop 0 false exit then
   STR:>NUMBER? MATCH option
      none OF 0 false ENDOF
      some OF true ENDOF
   ;MATCH ;

;package
