\ lsp-highlight.f - the language server's document highlight: the token that
\ declares a word in an open document and every use of that word there, as
\ the document's last check stated them once that check was verified.
\
\ ANSWER writes the result of textDocument/documentHighlight at a byte of an
\ open document's text, from the document's last completed check alone, and
\ only while that check's verdict was verified and its positions are of the
\ document's text (LSP-DEFS:DEFS-VERIFIED?): a verified check reported every
\ use in the document that it bound to a located declaration, so the list is
\ complete. A refused or deferred check may have stopped reporting uses at a
\ point it does not state, and a check that did not complete kept none, so
\ any other document answers null.
\
\ The byte selects a word. At a byte a use holds (LSP-DEFS:DEFS-USE-AT), it
\ is the record of the use's declaring token that the use's spelling names
\ (LSP-DEFS:GROUP-REC-NAMED), or none when no record there is named. At any
\ other byte that the token of a definition the check retained in the
\ document's own file holds, it is that token's one record
\ (LSP-DEFS:GROUP-REC-SOLE): a token that declares several words, as a
\ DEFTYPE's name declares both its converters, selects none of them, so it
\ answers null rather than join their uses. Any other byte selects nothing
\ and answers an empty list.
\
\ The list is the declaring token, when it is in the document's own file, then
\ each use of the check whose declaring token is the same and whose spelling
\ names the same record, or none as well, in the order the check published
\ them. Each is a Text highlight over its range in the document's text: the
\ checker states no read or write.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state; the writer is the
\ caller's, and the uses and definitions are those tools/lsp-defs.f keeps.

require lib/json-write.f
require lib/adt/option.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-HIGHLIGHT
using JSON-WRITE
using LSP-DEFS

private

1 constant TEXT-KIND                     \ DocumentHighlightKind.Text
-1 constant NO-REC                       \ the record of a use that names none

\ The record of its declaring token that use U of the text T names, NO-REC
\ for none.
: NAMED ( ptr u8 n n -- n )
   {: t:ptr tu:n u:n :}
   u USE-GROUP u USE-TARGET t tu u USE-WORD$ GROUP-REC-NAMED MATCH option
      some OF ENDOF
      none OF NO-REC ENDOF
   ;MATCH ;

\ A highlight from byte FROM to byte TO of the text positions count in,
\ listed next: a comma before it when LISTED says one came before it. LISTED
\ holds after it.
: ITEM ( ptr JSON-WRITE:writer bool n n -- ptr JSON-WRITE:writer bool )
   {: listed:bool from:n to:n :}
   listed if COMMA then
   OBJECT-START
   from LSP-TEXT:LINE-CHARACTER to from max LSP-TEXT:LINE-CHARACTER
   LSP-TEXT:RANGE-AT COMMA
   s" kind" TEXT-KIND FIELD-U
   OBJECT-END true ;

\ Whether use U of the text T is of the word whose declaring token in group G
\ starts and ends at TS and TE, and whose record there is REC.
: OF-WORD? ( ptr u8 n n n n n n -- bool )
   {: t:ptr tu:n u:n g:n ts:n te:n rec:n :}
   u USE-GROUP g <> if false exit then
   u USE-TARGET {: us:n ue:n :}
   us ts <> ue te <> or if false exit then
   t tu u NAMED rec = ;

\ The highlights of the word whose declaring token in group G starts and ends
\ at TS and TE, and whose record there is REC, in the document in this slot.
: HIGHLIGHTS ( ptr JSON-WRITE:writer n n n n n -- ptr JSON-WRITE:writer )
   {: slot:n g:n ts:n te:n rec:n :}
   slot LSP-DOCS:DOC-TEXT$ {: t:ptr tu:n :}
   ARRAY-START false
   g GROUP-OWN? if ts te ITEM then
   slot DEFS-USE-RANGE ?do
      t tu i g ts te rec OF-WORD? if i USE-BYTES ITEM then
   loop
   drop ARRAY-END ;

\ The highlights of the word use U selects in the document in this slot.
: USE-LIST ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: slot:n u:n :}
   slot u USE-GROUP u USE-TARGET slot LSP-DOCS:DOC-TEXT$ u NAMED HIGHLIGHTS ;

\ The highlights of the word record R of group G declares, in the document in
\ this slot; null when other records share R's token.
: REC-LIST ( ptr JSON-WRITE:writer n n n -- ptr JSON-WRITE:writer )
   {: slot:n g:n r:n :}
   r REC-BYTES {: ts:n te:n :}
   g ts te GROUP-REC-SOLE MATCH option
      some OF {: rec:n :} slot g ts te rec HIGHLIGHTS ENDOF
      none OF NULL ENDOF
   ;MATCH ;

\ The highlights of the word the definition token that holds this byte
\ declares, in group G, the own file of the document in this slot; an empty
\ list when no token does.
: HELD-LIST ( ptr JSON-WRITE:writer n n n -- ptr JSON-WRITE:writer )
   {: slot:n g:n at:n :}
   g at GROUP-REC-HOLDING MATCH option
      some OF {: r:n :} slot g r REC-LIST ENDOF
      none OF ARRAY-START ARRAY-END ENDOF
   ;MATCH ;

public

\ Writes the result of textDocument/documentHighlight at this byte of the text
\ of the document in this slot, which positions count in, the list the header
\ describes or null, to the writer.
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
