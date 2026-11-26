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
require src/lib/json/write.f

package LSP-OUTLINE

private

\ The symbol for record R of group G, a comma before it unless it is the
\ group's first.
: LISTED ( ^JSON:writer n n -- ^JSON:writer )
   {: w g:n r:n :}
   w r g LSP-DEFS:GROUP-FIRST <> if JSON:WRITE-COMMA then
   g r LSP-SYMBOLS:SYMBOL ;

: SYMBOLS ( ^JSON:writer n -- ^JSON:writer )
   {: w g:n :}
   g LSP-DEFS:GROUP-FIRST begin dup 0 >= while
      w over g swap LISTED drop
      LSP-DEFS:REC-NEXT
   repeat drop
   w ;

public

: ANSWER ( ^JSON:writer n -- ^JSON:writer )
   {: w slot:n :}
   w JSON:WRITE-ARRAY-START
   slot LSP-DEFS:DEFS-OWN-GROUP MATCH option
      some OF SYMBOLS ENDOF
      none OF ENDOF
   ;MATCH
   JSON:WRITE-ARRAY-END ;

;package
