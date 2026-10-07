\ lsp-line.f - the language server's line readers: the LF-ended lines of a
\ text, and the top-level members of a line that holds one JSON object, as each
\ of the checker's packets does.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the JR storage belongs to the server's one
\ task.

require lib/string.f
require lib/adt/option.f
require lib/json-read.f

package LSP-LINE

private

10 constant LF

create JR-ST JR:STORAGE-BYTES allot      \ JR storage for every read of a member

\ The line of the text at offset O, without its LF, and the offset after it.
: LINE ( ptr u8 n n -- ptr u8 n n )
   {: a:ptr u:n o:n :}
   a o + u o - LF INDEX-OF MATCH option
      none OF a o + u o - u ENDOF
      some OF IDX>N dup >r a o + swap o r> + 1+ ENDOF
   ;MATCH ;

public

\ Hands each line of the text to Q.
: EACH-LINE ( ptr u8 n [ ptr u8 n -- ] -- )
   {: a:ptr u:n q :}
   0 begin dup u < while
      a u rot LINE >r q execute r>
   repeat drop ;

\ Whether the LF-ended lines of the text hold this one.
: HAS-LINE? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n k:ptr ku:n :}
   0 begin dup u < while
      a u rot LINE >r k ku STR= if r> drop true exit then r>
   repeat drop false ;

\ The raw text of a top-level member of the line and its JR token kind, or -1
\ when the line has no such member: a string's text between its quotes,
\ escapes and all, or a number's digits. The line is one JSON object.
: MEMBER ( ptr u8 n ptr u8 n -- ptr u8 n n )
   {: a:ptr u:n k:ptr ku:n :}
   JR-ST JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   k ku JR:FIND-KEY 0= if JR:CLOSE NULL$ -1 exit then
   JR:TOKEN >r JR:SPAN$ rot JR:CLOSE r> ;

: STRING-MEMBER ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   MEMBER JR:T-STR = ;

\ A member that is an integer a cell holds; any other is as good as absent.
: INT-MEMBER ( ptr u8 n ptr u8 n -- n bool )
   MEMBER JR:T-INT <> if 2drop 0 false exit then
   STR>NUMBER? MATCH option
      none OF 0 false ENDOF
      some OF true ENDOF
   ;MATCH ;

;package
