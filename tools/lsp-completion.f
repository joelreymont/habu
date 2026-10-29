\ lsp-completion.f - textDocument/completion: one item per spelling the checker
\ offered at the cursor (LSP-CHECK:RUN-AT; CHECK:VERIFY-CANDIDATES$ lines as
\ tools/check-verify-child.f states them), in its order, never incomplete: every
\ spelling that would bind and begins, case-folded, with the token's bytes before
\ the cursor, so the client filters it; the server matches no names.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/string.f
require src/lib/byte-buffer.f
require src/lib/adt/option.f
require src/lib/json-read.f
require src/lib/json-write.f
require tools/check-verify-core.f
require tools/lsp-defs.f
require tools/lsp-line.f

package LSP-COMPLETION
using JSON-WRITE
using LSP-DEFS

private

TYPED-VARIABLE JR-ST JR:cursor           \ JR storage for decoding a line
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

: STRING>WORD ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max WORD-B-RESERVE
   0 WORD-B raw JR:STR WORD-U ! ;

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
   JR-ST a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while MEMBER repeat
   JR:CLOSE ;

: TARGET-REC ( -- option<n> )
   FILE-U @ 0= if OPTION:NONE exit then
   ASKED @ FILE$ DEFS-GROUP-OF MATCH option
      some OF TARGET-V @ TARGET-END-V @ WORD$ GROUP-REC-NAMED ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

\ The detail is the record the check retained at a located candidate's declaring
\ token in its file's group (LSP-DEFS:DEFS-GROUP-OF), of the spelling or its
\ qualified tail, else the only one there (LSP-DEFS:GROUP-REC-NAMED), shown on one
\ line as hover does (tools/lsp-hover.f): its declared effect in parentheses, when
\ it declared one, then a comment naming its package, unless global. A candidate
\ with no location (a local, an engine word), no such record, or a global one with
\ no effect, gets none.
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
