\ lsp-symbols.f - the language server's workspace symbols: the definitions the
\ open documents' last completed checks retained.
\
\ ANSWER writes the result of workspace/symbol for a query: one
\ SymbolInformation for each definition the store keeps (tools/lsp-defs.f)
\ whose word holds the query, ASCII letters compared without case. A file
\ several open documents' checks reached is answered once, from the check the
\ store answers it from (LSP-DEFS:DEFS-EACH): the check of the document
\ holding the file while the store keeps one, else the latest of them. The
\ answer follows the store: check after check, oldest first, each check's
\ files in the order its lines first name them.
\ - name: the word as the source wrote it.
\ - kind: from the class the verifier states for the path that declared the
\   word (tools/check-verify-child.f): 14 (Constant) for a constant, 13
\   (Variable) for storage, 12 (Function) for a word or a re-export.
\ - location: the range of the token that declared the word, at the URI of the
\   open document that held its file when the check completed, else at the
\   file URI of its path.
\ - containerName: the package the checker recorded the word under; a global
\   has none.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/string.f
require lib/json-write.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-SYMBOLS
using JSON-WRITE
using LSP-DEFS

private

14 constant CONSTANT-KIND                \ SymbolKind.Constant
13 constant VARIABLE-KIND                \ SymbolKind.Variable
12 constant FUNCTION-KIND                \ SymbolKind.Function

TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer,
TYPED-VARIABLE QUERY-A ptr u8            \ its query
variable QUERY-U
variable SHOWN-N                         \ and the symbols it lists so far

\ Whether the needle occurs in the text, ASCII letters compared without case.
: CONTAINS-CI? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   u v - 1+ 0 ?do
      a i + v b v STR=CI if unloop true exit then
   loop
   false ;

\ The SymbolKind of a class.
: KIND ( n -- n )
   {: class:n :}
   class CLASS-CONSTANT = if CONSTANT-KIND exit then
   class CLASS-STORAGE = if VARIABLE-KIND exit then
   FUNCTION-KIND ;

\ The SymbolInformation for record R of group G.
: SYMBOL ( n n -- )
   {: g:n r:n :}
   OUT @
   SHOWN-N @ 0 > if COMMA then
   OBJECT-START
   s" name" r REC-WORD$ FIELD-S COMMA
   s" kind" r REC-CLASS KIND FIELD-U COMMA
   s" location" KEY OBJECT-START
      s" uri" g GROUP-URI$ FIELD-RAW COMMA
      r REC-RANGE LSP-TEXT:RANGE-AT
   OBJECT-END
   r REC-PACKAGE$ {: p:ptr pu:n :}
   pu 0 > if COMMA s" containerName" p pu FIELD-S then
   OBJECT-END drop
   1 SHOWN-N +! ;

\ The symbols of group G whose words hold the query.
: GROUP-SYMBOLS ( n -- )
   {: g:n :}
   g GROUP-FIRST begin dup 0 >= while
      dup REC-WORD$ QUERY-A @ QUERY-U @ CONTAINS-CI? if g over SYMBOL then
      REC-NEXT
   repeat drop ;

public

\ Writes the result of workspace/symbol for this query, the array the header
\ describes, to the writer.
: ANSWER ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: w q:ptr qu:n :}
   w OUT !
   q QUERY-A !
   qu QUERY-U !
   0 SHOWN-N !
   w ARRAY-START drop
   [: GROUP-SYMBOLS ;] DEFS-EACH
   w ARRAY-END ;

;using
;using
;package
