\ lsp-outline.f - textDocument/documentSymbol: a flat SymbolInformation list of
\ the definitions the document's last completed check retained in its own file
\ (LSP-DEFS:DEFS-OWN-GROUP), in retention order, while that check's positions are
\ of the document's text; otherwise an empty list. Symbols are LSP-SYMBOLS:SYMBOL's.
\ STORAGE CLASS. CALLER-OWNED: no state; the writer is the caller's, the
\ definitions tools/lsp-defs.f's.

require src/lib/json.f
require src/lib/adt/option.f
require tools/lsp-defs.f
require tools/lsp-symbols.f

package LSP-OUTLINE
using JSON
using LSP-DEFS

private

\ The symbol for record R of group G, a comma before it unless it is the
\ group's first.
: LISTED ( ptr JSON:writer n n -- ptr JSON:writer )
   {: w g:n r:n :}
   w r g GROUP-FIRST <> if WRITE-COMMA then
   g r LSP-SYMBOLS:SYMBOL ;

: SYMBOLS ( ptr JSON:writer n -- ptr JSON:writer )
   {: w g:n :}
   g GROUP-FIRST begin dup 0 >= while
      w over g swap LISTED drop
      REC-NEXT
   repeat drop
   w ;

public

: ANSWER ( ptr JSON:writer n -- ptr JSON:writer )
   {: w slot:n :}
   w WRITE-ARRAY-START
   slot DEFS-OWN-GROUP MATCH option
      some OF SYMBOLS ENDOF
      none OF ENDOF
   ;MATCH
   WRITE-ARRAY-END ;

;using
;using
;package
