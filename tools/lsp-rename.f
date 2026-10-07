\ lsp-rename.f - the language server's rename: the bytes of a token a rename
\ of the declaration it selects edits.
\
\ PREPARED writes the result of textDocument/prepareRename at a byte of an
\ open document's text, once LSP-REFERENCES:PLACE has selected and placed the
\ declaration there, refusing nothing, and the use that selected it and its
\ declaring token spell its tail (LSP-REFERENCES:SPELT?). The result is the
\ Range of the tail of the token at the byte, the use that holds it or else
\ the declaring token: its bytes after a qualifier's colon. A rename edits the
\ tail alone, so the name the client asks for is a tail and a qualifier stays.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state; the writer is the
\ caller's, the selection LSP-REFERENCES's and the uses those tools/lsp-defs.f
\ keeps.

require lib/json-write.f
require lib/adt/option.f
require tools/lsp-docs.f
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-references.f

package LSP-RENAME
using LSP-DEFS

public

\ The Range of the tail of the token at this byte of the text of the open
\ document in this slot, which positions then count in: the use that holds
\ the byte, else the declaring token LSP-REFERENCES:PLACE placed.
: PREPARED ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: slot:n at:n :}
   slot LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   slot at DEFS-USE-AT MATCH option
      some OF USE-BYTES ENDOF
      none OF LSP-REFERENCES:TOKEN ENDOF
   ;MATCH {: from:n to:n :}
   from to LSP-REFERENCES:TAIL-FROM LSP-TEXT:LINE-CHARACTER
   to LSP-TEXT:LINE-CHARACTER
   LSP-TEXT:RANGE-VALUE ;

;using
;package
