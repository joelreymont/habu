\ lsp-completion.f - the language server's completion: the spellings that
\ would bind at a cursor in an open document, as the checker offers them.
\
\ ANSWER writes the result of textDocument/completion from the check of the
\ document just run with its cursor (LSP-CHECK:RUN-AT): one item for each
\ spelling the checker offered there, in its order, read from that check's
\ candidate lines (CHECK:VERIFY-CANDIDATES$, as tools/check-verify-child.f
\ states them), and none when it offered none. The list is never incomplete:
\ the checker offers every spelling that would bind at the cursor's token and
\ begins, in any case, with the token's bytes before the cursor, so the client
\ filters it as the token grows. An item's label is the spelling. Its detail
\ shows the definition the check retained at the declaring token of a located
\ candidate, in the check's group for its file (LSP-DEFS:DEFS-GROUP-OF): the
\ one of the spelling, or of its tail when the spelling is qualified, else the
\ only one there (LSP-DEFS:GROUP-REC-NAMED), as hover does
\ (tools/lsp-hover.f) but on one line and without its statement
\ token, word, file and line: its declared effect, in parentheses, when it
\ declared one, then a comment naming the package it is declared in, unless it
\ is global. A candidate with no location, a local's or an engine word's, no
\ such definition, or a global one that declared no effect, gives no detail.
\ The server matches no names: every item is a candidate line.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/string.f
require lib/byte-buffer.f
require lib/adt/option.f
require lib/json-read.f
require lib/json-write.f
require tools/check-verify-core.f
require tools/lsp-defs.f
require tools/lsp-line.f

package LSP-COMPLETION
using JSON-WRITE
using LSP-DEFS

private

create JR-ST JR:STORAGE-BYTES allot      \ JR storage for decoding a line
DYNAMIC-BUFFER WORD-B u8                 \ the line's spelling, decoded,
variable WORD-U
DYNAMIC-BUFFER FILE-B u8                 \ its declaration's file, or none,
variable FILE-U
variable TARGET-V                        \ and the bytes its declaring token
variable TARGET-END-V                    \ starts and ends at there
create DETAIL-B BUF:HDR-BYTES allot      \ an item's detail
TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer,
variable ASKED                           \ the slot of the document asked about,
variable SHOWN-N                         \ and the items written so far

: WORD$ ( -- ptr u8 n )  0 WORD-B WORD-U @ ;
: FILE$ ( -- ptr u8 n )  0 FILE-B FILE-U @ ;
: D+ ( ptr u8 n -- )  BUF:N>BLEN DETAIL-B BUF:APPEND-SPAN ;
: DETAIL$ ( -- ptr u8 n )  DETAIL-B BUF:SPAN$ BUF:BLEN>N ;

\ The string the reader is at, decoded into WORD-B.
: STRING>WORD ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max WORD-B-RESERVE
   0 WORD-B raw JR:STR WORD-U ! ;

\ The string the reader is at, decoded into FILE-B.
: STRING>FILE ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max FILE-B-RESERVE
   0 FILE-B raw JR:STR FILE-U ! ;

\ The member whose key the reader is at, decoded; one an item needs nothing
\ of, passed over.
: MEMBER ( JR:reader -- JR:reader )
   s" word" JR:STR-EQ? if JR:NEXT drop STRING>WORD exit then
   s" file" JR:STR-EQ? if JR:NEXT drop STRING>FILE exit then
   s" target_start" JR:STR-EQ? if JR:NEXT drop JR:INT TARGET-V ! exit then
   s" target_end" JR:STR-EQ? if JR:NEXT drop JR:INT TARGET-END-V ! exit then
   JR:NEXT drop JR:SKIP-VALUE ;

\ The candidate line's members decoded, each once; a member it lacks is empty
\ or 0.
: DECODE ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 WORD-U !
   0 FILE-U !
   0 TARGET-V !
   0 TARGET-END-V !
   JR-ST JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while MEMBER repeat
   JR:CLOSE ;

\ The record of the line's spelling the check retained at the line's declaring
\ token, else the only one there, in its group for the declaration's file, if
\ the line names a file and one is there.
: TARGET-REC ( -- option<n> )
   FILE-U @ 0= if OPTION:NONE exit then
   ASKED @ FILE$ DEFS-GROUP-OF MATCH option
      some OF TARGET-V @ TARGET-END-V @ WORD$ GROUP-REC-NAMED ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

\ The item's detail from record R, as the header describes it: its declared
\ effect in parentheses, when it declared one, then its package's comment,
\ unless it is global; none when it has neither.
: DETAIL ( n -- )
   {: r:n :}
   r REC-EFFECT$ {: e:ptr eu:n :}
   r REC-PACKAGE$ {: p:ptr pu:n :}
   eu pu or 0= if exit then
   DETAIL-B BUF:CLEAR
   eu 0 > if s" ( " D+ e eu D+ s"  )" D+ then
   pu 0 > if
      eu 0 > if s"  " D+ then
      s" \ package " D+ p pu D+
   then
   OUT @ COMMA s" detail" DETAIL$ FIELD-S drop ;

\ The item for this candidate line.
: ITEM ( ptr u8 n -- )
   DECODE
   OUT @
   SHOWN-N @ 0 > if COMMA then
   OBJECT-START
   s" label" WORD$ FIELD-S drop
   TARGET-REC MATCH option
      some OF DETAIL ENDOF
      none OF ENDOF
   ;MATCH
   OUT @ OBJECT-END drop
   1 SHOWN-N +! ;

public

\ Readies the answer's storage, before any completion.
: PREPARE ( -- )
   DETAIL-B 1 BUF:N>BLEN BUF:INIT ;

\ Writes the result of textDocument/completion for the document in this slot,
\ whose check with a cursor has just run, the list the header describes, to
\ the writer.
: ANSWER ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: w slot:n :}
   w OUT !
   slot ASKED !
   0 SHOWN-N !
   w OBJECT-START
   s" isIncomplete" false FIELD-BOOL COMMA
   s" items" KEY ARRAY-START drop
   CHECK:VERIFY-CANDIDATES$ [: ITEM ;] LSP-LINE:EACH-LINE
   w ARRAY-END OBJECT-END ;

;using
;using
;package
