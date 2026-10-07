\ lsp-outline.f - the language server's document symbols: the outline of an
\ open document, the definitions its last check retained in it once that
\ check completed.
\
\ ANSWER writes the result of textDocument/documentSymbol for an open
\ document: one DocumentSymbol for each definition the document's last
\ completed check retained in the document's own file
\ (LSP-DEFS:DEFS-OWN-GROUP), in the order the check retained them, while that
\ check's positions are of the document's text; none of another open
\ document's or of a file the document requires. A document with no
\ definition, one whose last check did not complete and one that has none
\ answer an empty list.
\ - name: the word as the source wrote it.
\ - kind: the class's SymbolKind as workspace symbols give it
\   (LSP-SYMBOLS:KIND).
\ - range and selectionRange: both the range of the token that declared the
\   word, in the document's text. The check states where that token starts
\   and ends and nothing of the definition's extent, so the outline knows no
\   more, and no symbol has children.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/json-write.f
require lib/adt/option.f
require tools/lsp-defs.f
require tools/lsp-text.f
require tools/lsp-symbols.f

package LSP-OUTLINE
using JSON-WRITE
using LSP-DEFS

private

TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer

\ The DocumentSymbol of record R.
: SYMBOL ( n -- )
   {: r:n :}
   OUT @
   OBJECT-START
   s" name" r REC-WORD$ FIELD-S COMMA
   s" kind" r REC-CLASS LSP-SYMBOLS:KIND FIELD-U COMMA
   r REC-RANGE LSP-TEXT:RANGE-AT COMMA
   s" selectionRange" KEY r REC-RANGE LSP-TEXT:RANGE-OBJECT
   OBJECT-END drop ;

\ The symbols of group G, a comma before each but the first.
: SYMBOLS ( n -- )
   {: g:n :}
   g GROUP-FIRST begin dup 0 >= while
      dup g GROUP-FIRST <> if OUT @ COMMA drop then
      dup SYMBOL
      REC-NEXT
   repeat drop ;

public

\ Writes the result of textDocument/documentSymbol for the document in this
\ slot, the array the header describes, to the writer.
: ANSWER ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: w slot:n :}
   w OUT !
   w ARRAY-START drop
   slot DEFS-OWN-GROUP MATCH option
      some OF SYMBOLS ENDOF
      none OF ENDOF
   ;MATCH
   w ARRAY-END ;

;using
;using
;package
