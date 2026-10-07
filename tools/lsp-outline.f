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
\ answer an empty list. Each is the SymbolInformation workspace symbols give,
\ named by its word as the source wrote it, without its package
\ (LSP-SYMBOLS:SYMBOL): its location is the token that declared the word, at
\ the URI the client opened the document by, whose text its range counts in.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state; the writer is the
\ caller's, and the definitions are those tools/lsp-defs.f keeps.

require lib/json-write.f
require lib/adt/option.f
require tools/lsp-defs.f
require tools/lsp-symbols.f

package LSP-OUTLINE
using JSON-WRITE
using LSP-DEFS

private

\ The symbol for record R of group G, a comma before it unless it is the
\ group's first.
: LISTED ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: w g:n r:n :}
   w r g GROUP-FIRST <> if COMMA then
   g r LSP-SYMBOLS:SYMBOL ;

\ The symbols of group G.
: SYMBOLS ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: w g:n :}
   g GROUP-FIRST begin dup 0 >= while
      w over g swap LISTED drop
      REC-NEXT
   repeat drop
   w ;

public

\ Writes the result of textDocument/documentSymbol for the document in this
\ slot, the array the header describes, to the writer.
: ANSWER ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: w slot:n :}
   w ARRAY-START
   slot DEFS-OWN-GROUP MATCH option
      some OF SYMBOLS ENDOF
      none OF ENDOF
   ;MATCH
   ARRAY-END ;

;using
;using
;package
