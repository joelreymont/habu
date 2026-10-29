\ lsp-highlight.f - textDocument/documentHighlight: the token that declares a
\ word in an open document and every use of it there, from the last verified
\ check. STORAGE CLASS. CALLER-OWNED: no state; the writer is the caller's, the
\ uses and definitions are those tools/lsp-defs.f keeps.

require src/lib/json-write.f
require src/lib/adt/option.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-HIGHLIGHT
using JSON-WRITE
using LSP-DEFS

private

1 constant TEXT-KIND                     \ DocumentHighlightKind.Text

: NAMED ( ptr u8 n n -- option<n> )
   {: t:ptr tu:n u:n :}
   u USE-GROUP u USE-TARGET t tu u USE-WORD$ GROUP-REC-NAMED ;

\ LISTED: whether an item came before, for the comma; it holds after.
: ITEM ( ptr JSON-WRITE:writer bool n n -- ptr JSON-WRITE:writer bool )
   {: listed:bool from:n to:n :}
   listed if COMMA then
   OBJECT-START
   from LSP-TEXT:LINE-CHARACTER to from max LSP-TEXT:LINE-CHARACTER
   LSP-TEXT:RANGE-AT COMMA
   s" kind" TEXT-KIND FIELD-U
   OBJECT-END true ;

\ A use that names no record there is of no word.
: OF-WORD? ( ptr u8 n n n n n n -- bool )
   {: t:ptr tu:n u:n g:n ts:n te:n rec:n :}
   u USE-GROUP g <> if false exit then
   u USE-TARGET {: us:n ue:n :}
   us ts <> ue te <> or if false exit then
   t tu u NAMED MATCH option
      some OF rec = ENDOF
      none OF false ENDOF
   ;MATCH ;

\ The declaring token when it is in the document's own file, then each use with
\ the same declaring token naming the same record, in published order. All are
\ Text highlights: the checker states no read or write.
: HIGHLIGHTS ( ptr JSON-WRITE:writer n n n n n -- ptr JSON-WRITE:writer )
   {: slot:n g:n ts:n te:n rec:n :}
   slot LSP-DOCS:DOC-TEXT$ {: t:ptr tu:n :}
   ARRAY-START false
   g GROUP-OWN? if ts te ITEM then
   slot DEFS-USE-RANGE ?do
      t tu i g ts te rec OF-WORD? if i USE-BYTES ITEM then
   loop
   drop ARRAY-END ;

\ At a use (LSP-DEFS:DEFS-USE-AT) the record is the one its spelling names
\ (LSP-DEFS:GROUP-REC-NAMED); at a definition token in the document's own file,
\ the token's record when it declares no other (LSP-DEFS:GROUP-REC-SOLE: a
\ DEFTYPE's name declares both converters and selects neither). A record that
\ stands for several words (LSP-DEFS:REC-SEVERAL?: a file included into two
\ packages declares two words at one token) answers null rather than join the
\ uses of different words; so does no record.
: SELECTED-LIST ( ptr JSON-WRITE:writer n n n n option<n> -- ptr JSON-WRITE:writer )
   MATCH option
      some OF {: slot:n g:n ts:n te:n rec:n :}
         rec REC-SEVERAL? if NULL else slot g ts te rec HIGHLIGHTS then
      ENDOF
      none OF 2drop 2drop NULL ENDOF
   ;MATCH ;

: USE-LIST ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: slot:n u:n :}
   slot u USE-GROUP u USE-TARGET slot LSP-DOCS:DOC-TEXT$ u NAMED SELECTED-LIST ;

: REC-LIST ( ptr JSON-WRITE:writer n n n -- ptr JSON-WRITE:writer )
   {: slot:n g:n r:n :}
   r REC-BYTES {: ts:n te:n :}
   slot g ts te g ts te GROUP-REC-SOLE SELECTED-LIST ;

: HELD-LIST ( ptr JSON-WRITE:writer n n n -- ptr JSON-WRITE:writer )
   {: slot:n g:n at:n :}
   g at GROUP-REC-HOLDING MATCH option
      some OF {: r:n :} slot g r REC-LIST ENDOF
      none OF ARRAY-START ARRAY-END ENDOF
   ;MATCH ;

public

\ Only while the last completed check was verified and its positions are of the
\ text (LSP-DEFS:DEFS-VERIFIED?): a verified check reported every use it bound
\ to a located declaration, so the list is complete. A refused or deferred check
\ may have stopped reporting at a point it does not state, and an incomplete one
\ kept none: any other document answers null. A byte no use or definition token
\ holds answers an empty list.
: ANSWER ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: slot:n at:n :}
   slot DEFS-VERIFIED? 0= if NULL exit then
   slot at DEFS-USE-AT MATCH option
      some OF {: u:n :} slot u USE-LIST ENDOF
      none OF
         slot DEFS-OWN-GROUP MATCH option
            some OF {: g:n :} slot g at HELD-LIST ENDOF
            none OF ARRAY-START ARRAY-END ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

;using
;using
;package
