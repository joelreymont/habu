\ lsp-outline.f - the language server's document symbols: the outline of an
\ open document, the definitions its last check retained in it once that
\ check completed.
\
\ ANSWER writes the result of textDocument/documentSymbol for an open
\ document: a flat list of SymbolInformation, one for each definition the
\ document's last completed check retained in the document's own file
\ (LSP-DEFS:DEFS-OWN-GROUP), in the order the check retained them, while that
\ check's positions are of the document's text; none of another open
\ document's or of a file the document requires. A document with no
\ definition, one whose last check did not complete and one that has none
\ answer an empty list. Each is the SymbolInformation workspace symbols give
\ (LSP-SYMBOLS:SYMBOL): its location is the token that declared the word, at
\ the URI the client opened the document by.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/json-write.f
require lib/adt/option.f
require tools/lsp-defs.f
require tools/lsp-symbols.f

package LSP-OUTLINE
using JSON-WRITE
using LSP-DEFS

private

TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer

\ The symbol for record R of group G, a comma before it unless it is the
\ group's first.
: LISTED ( n n -- )
   {: g:n r:n :}
   OUT @
   r g GROUP-FIRST <> if COMMA then
   g r LSP-SYMBOLS:SYMBOL drop ;

\ The symbols of group G.
: SYMBOLS ( n -- )
   {: g:n :}
   g GROUP-FIRST begin dup 0 >= while
      g over LISTED
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
