\ lsp-completion.f - textDocument/completion: one item per spelling the checker
\ offered at the cursor (LSP-CHECK:RUN-AT; LSP-CHECK:VERIFY-CANDIDATES$ lines as
\ tools/check-verify-child.f states them), in its order, never incomplete: every
\ spelling that would bind and begins, case-folded, with the token's bytes before
\ the cursor, so the client filters it; the server matches no names.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/string.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/adt/option.f
require src/lib/json.f
require tools/check-verify-core.f
require tools/lsp-defs.f
require tools/lsp-line.f
require src/lib/json/read.f
require src/lib/json/write.f

package LSP-COMPLETION
using JSON
using LSP-DEFS

private

TYPED-VARIABLE JSON-ST JSON:cursor       \ JSON reader storage for decoding a line
TYPED-VARIABLE WORD-B ^VEC:vector<u8> SCRATCH-ONLY \ the line's spelling, decoded,
TYPED-VARIABLE FILE-B ^VEC:vector<u8> SCRATCH-ONLY \ its declaration's file, or none,
variable TARGET-V                        \ and the bytes its declaring token
variable TARGET-END-V                    \ starts and ends at there
TYPED-VARIABLE DETAIL-B ^VEC:vector<u8> SCRATCH-ONLY \ an item's detail
TYPED-VARIABLE OUT ^JSON:writer \ the answer's writer,
variable ASKED                           \ the slot of the document asked about,
variable SHOWN-N                         \ and the items written so far

: WORD$ ( -- str )  WORD-B @ VEC:BYTES ;
: FILE$ ( -- str )  FILE-B @ VEC:BYTES ;
: D+ ( str -- )  DETAIL-B @ VEC:APPEND ;
: DETAIL$ ( -- str )  DETAIL-B @ VEC:BYTES ;

\ The member whose key the reader is at, decoded; one an item needs nothing
\ of, passed over.
: MEMBER ( JSON:reader -- JSON:reader )
   s" word" READ-STR-EQ? if READ-NEXT drop WORD-B @ READ-STR drop exit then
   s" file" READ-STR-EQ? if READ-NEXT drop FILE-B @ READ-STR drop exit then
   s" target_start" READ-STR-EQ? if READ-NEXT drop READ-INT TARGET-V ! exit then
   s" target_end" READ-STR-EQ? if READ-NEXT drop READ-INT TARGET-END-V ! exit then
   READ-NEXT drop READ-SKIP-VALUE ;

\ The candidate line's members decoded, each once; a member it lacks is empty
\ or 0.
: DECODE ( str -- )
   {: s:str :}
   WORD-B @ 0 VEC:RESIZE
   FILE-B @ 0 VEC:RESIZE
   0 TARGET-V !
   0 TARGET-END-V !
   JSON-ST s READ-INIT
   READ-NEXT drop
   begin READ-NEXT T-KEY = while MEMBER repeat
   READ-CLOSE ;

: TARGET-REC ( -- option<n> )
   FILE-B @ VEC:COUNT 0= if OPTION:NONE exit then
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
   r REC-EFFECT$ {: e:str :}
   r REC-PACKAGE$ {: p:str :}
   e STR:LEN p STR:LEN or 0= if exit then
   DETAIL-B @ 0 VEC:RESIZE
   e STR:LEN 0 > if s" ( " D+ e D+ s"  )" D+ then
   p STR:LEN 0 > if
      e STR:LEN 0 > if s"  " D+ then
      s" \ package " D+ p D+

   then
   OUT @ WRITE-COMMA s" detail" DETAIL$ WRITE-FIELD-S drop ;

: ITEM ( str -- )
   DECODE
   OUT @
   SHOWN-N @ 0 > if WRITE-COMMA then
   WRITE-OBJECT-START
   s" label" WORD$ WRITE-FIELD-S drop
   TARGET-REC MATCH option
      some OF DETAIL ENDOF
      none OF ENDOF
   ;MATCH
   OUT @ WRITE-OBJECT-END drop
   1 SHOWN-N +! ;

public

\ Readies the answer's storage, before any completion.
: PREPARE ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW WORD-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW FILE-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW DETAIL-B ! ;

: ANSWER ( ^JSON:writer n -- ^JSON:writer )
   {: w slot:n :}
   w OUT !
   slot ASKED !
   0 SHOWN-N !
   w WRITE-OBJECT-START
   s" isIncomplete" false WRITE-FIELD-BOOL WRITE-COMMA
   s" items" WRITE-KEY WRITE-ARRAY-START drop
   LSP-CHECK:VERIFY-CANDIDATES$ [: ITEM ;] LSP-LINE:EACH-LINE
   w WRITE-ARRAY-END WRITE-OBJECT-END ;

;using
;using
;package
