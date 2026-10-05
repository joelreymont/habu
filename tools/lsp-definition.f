\ lsp-definition.f - the language server's go to definition: the declaration a
\ use in an open document binds to, as the document's last check bound it once
\ that check completed.
\
\ ANSWER writes the result of textDocument/definition at a byte of an open
\ document's text: the first use of the document's last check, when it
\ completed, whose bytes, from its start to before its end, hold the byte
\ (tools/lsp-defs.f), and the declaration it binds to as a list of one
\ Location. The Location is the first record of the use's group, its check's
\ group for the declaration's file (LSP-DEFS:USE-GROUP), whose token starts and
\ ends at the bytes the declaration's does: at the group's URI, over the
\ token's range in the text the check read.
\ A byte no use holds, a document whose last check did not complete or that has
\ none, and a declaration no such record holds answer an empty list.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/json-write.f
require lib/adt/option.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-DEFINITION
using JSON-WRITE
using LSP-DEFS

private

TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer

\ The Location of record R of group G.
: LOCATION ( n n -- )
   {: g:n r:n :}
   OUT @
   OBJECT-START
   s" uri" g GROUP-URI$ FIELD-RAW COMMA
   r REC-RANGE LSP-TEXT:RANGE-AT
   OBJECT-END drop ;

\ The Location of the declaration use U binds to: the first record of the
\ use's group whose token is its declaring one, if one is.
: TARGET ( n -- )
   {: u:n :}
   u USE-GROUP {: g:n :}
   g u USE-TARGET GROUP-REC-AT MATCH option
      some OF g swap LOCATION ENDOF
      none OF ENDOF
   ;MATCH ;

public

\ Writes the result of textDocument/definition at this byte of the text of
\ the document in this slot, the list the header describes, to the writer.
: ANSWER ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: w slot:n at:n :}
   w OUT !
   w ARRAY-START drop
   slot at DEFS-USE-AT MATCH option
      some OF TARGET ENDOF
      none OF ENDOF
   ;MATCH
   w ARRAY-END ;

;using
;using
;package
