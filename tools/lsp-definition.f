\ lsp-definition.f - textDocument/definition at a byte of an open document: the
\ first use of its last completed check holding the byte (tools/lsp-defs.f), and
\ the declaration it binds to as one Location: a source declaration's first record
\ in the use's group (LSP-DEFS:USE-GROUP), a primitive's name in src/habu/prims.f,
\ or a source-backed engine word's current declaration, by its origin span.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/json-write.f
require src/lib/adt/option.f
require src/habu/engine-origin.f
require tools/lsp-source-index.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-DEFINITION
using JSON-WRITE
using LSP-DEFS
using LSP-DOCS

private

TYPED-VARIABLE OUT ptr JSON-WRITE:writer

\ A Location at these positions in group G.
: LOCATION-AT ( n n n n n -- )
   {: g:n line:n col:n end-line:n end-col:n :}
   OUT @
   OBJECT-START
   s" uri" g GROUP-URI$ FIELD-RAW COMMA
   line col end-line end-col LSP-TEXT:RANGE-AT
   OBJECT-END drop ;

: LOCATION ( n n -- )
   {: g:n r:n :}
   g r REC-RANGE LOCATION-AT ;

: TEXT-OF! ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   path pathu DOC-HOLDING MATCH option
      some OF DOC-TEXT$ LSP-TEXT:TEXT! ENDOF
      none OF path pathu LSP-TEXT:FILE-TEXT! ENDOF
   ;MATCH ;

: BUILTIN-TARGET ( n n -- )
   {: u:n g:n :}
   u USE-TARGET {: old:n old-end:n :}
   g GROUP-PATH$ old ENGINE-ORIGIN:SYMBOL-AT
   {: sym:n found:bool :}
   found 0= if exit then
   sym ENGINE-ORIGIN:TARGET
   {: rel:ptr relu:n at:n end:n name:ptr nameu:n live:bool :}
   live at old = and old-end 0= old-end end = or and 0= if exit then
   g [: dup GROUP-PATH$ TEXT-OF! ;] catch {: rc:n :} drop
   rc 0<> if rc LSP-TEXT:FS-RC? 0= if rc throw then exit then
   sym g GROUP-PATH$
   LSP-TEXT:TEXT$ SOURCE-INDEX:TARGET-IN
   {: start:n stop:n located:bool :}
   located 0= if exit then
   start LSP-TEXT:LINE-CHARACTER
   stop LSP-TEXT:LINE-CHARACTER
   {: line:n col:n end-line:n end-col:n :}
   g line col end-line end-col LOCATION-AT ;

\ The Location of the declaration use U binds to: a built-in's current source
\ declaration, a primitive's validated source target, or a source record.
: TARGET ( n -- )
   {: u:n :}
   u USE-GROUP {: g:n :}
   u USE-VISIT -1 = if g 0 >= if u g BUILTIN-TARGET then exit then
   u USE-VISIT 0< if
      u USE-TARGET {: start:n end:n :}
      end start > if g u USE-TARGET-RANGE LOCATION-AT then
      exit
   then
   g u USE-TARGET GROUP-REC-AT MATCH option
      some OF g swap LOCATION ENDOF
      none OF ENDOF
   ;MATCH ;

public

\ An empty list for a byte no use holds, a document whose last check did not
\ complete or that has none, and a declaration no record holds; also for a
\ primitive whose source is missing or differs from the running table, or an
\ engine word whose current declaration is missing or ambiguous. An engine word's
\ declaration prefers an open document's text.
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
;using
;package
