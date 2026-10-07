\ lsp-symbols.f - the language server's workspace symbols: the definitions the
\ open documents' last completed checks retained.
\
\ A definition's tail is the text after its word's first colon when that
\ colon is neither the word's first nor its last byte, as Habu spells a
\ qualified word PKG:WORD, else the whole word: the word a caller writes
\ after a package's colon, also for a word re-exported by EXPORT PKG:WORD.
\
\ ANSWER writes the result of workspace/symbol for a query: one
\ SymbolInformation for each definition the store keeps (tools/lsp-defs.f)
\ whose word holds the query, or, when the query holds a colon, that the
\ checker recorded public in the package before the query's first colon,
\ compared whole, and whose tail holds the text after it; ASCII letters
\ compared without case. A file several open documents' checks reached is
\ answered once, from the check the store answers it from
\ (LSP-DEFS:DEFS-EACH): the check of the document holding the file while the
\ store keeps one, else the latest of them. The answer follows the store:
\ check after check, oldest first, each check's files in the order its lines
\ first name them.
\ - name: for a definition the checker recorded public in a package, that
\   package, a colon and its tail, as a caller spells it qualified; for any
\   other, the word as the source wrote it.
\ - kind: from the class the verifier states for the path that declared the
\   word (tools/check-verify-child.f): 14 (Constant) for a constant, 13
\   (Variable) for storage, 12 (Function) for a word or a re-export.
\ - location: the range of the token that declared the word, at the URI of the
\   checked document for its own file, whose text the range counts in, and
\   for any other at the URI of the open document that held the file when the
\   check completed, else at the file URI of its path.
\ - containerName: the package the checker recorded the word under; a global
\   has none.
\ SYMBOL writes that SymbolInformation for one definition named by its word as
\ the source wrote it, whatever its package: document symbols
\ (tools/lsp-outline.f) answer with it.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/string.f
require lib/adt/option.f
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
TYPED-VARIABLE QUERY-A ptr u8            \ its query,
variable QUERY-U
variable COLON-AT                        \ where its first colon is, -1 for none,
variable SHOWN-N                         \ and the symbols it lists so far
DYNAMIC-BUFFER NAME-B u8                 \ a symbol's qualified name

\ Whether the needle occurs in the text, ASCII letters compared without case.
: CONTAINS-CI? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   u v - 1+ 0 ?do
      a i + v b v STR=CI if unloop true exit then
   loop
   false ;

\ Where the text's first colon is, -1 for none.
: COLON-IN ( ptr u8 n -- n )
   [char] : INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ The query's text before its first colon, then after it.
: PACKAGE$ ( -- ptr u8 n )  QUERY-A @ COLON-AT @ ;
: TAIL$ ( -- ptr u8 n )
   COLON-AT @ 1+ {: at:n :}
   QUERY-A @ at + QUERY-U @ at - ;

\ The tail of record R's word, as the header states it.
: REC-TAIL$ ( n -- ptr u8 n )
   REC-WORD$ {: w:ptr wu:n :}
   w wu COLON-IN {: at:n :}
   at 0 > at wu 1- < and if w at 1+ + wu at 1+ - exit then
   w wu ;

\ Whether the query lists record R: its word holds the query, or, when the
\ query holds a colon, the checker recorded it public in the package before
\ the first one, compared whole, and its tail holds the text after it.
: LISTED? ( n -- bool )
   {: r:n :}
   r REC-WORD$ QUERY-A @ QUERY-U @ CONTAINS-CI? if true exit then
   COLON-AT @ 0 < if false exit then
   r REC-PUBLIC? 0= if false exit then
   r REC-PACKAGE$ PACKAGE$ STR=CI 0= if false exit then
   r REC-TAIL$ TAIL$ CONTAINS-CI? ;

\ The name the header gives record R: its word, or PKG:TAIL built in NAME-B
\ with a byte past it reserved, as the store's BYTES, so that the tail's
\ place has an address however short the tail is.
: NAME$ ( n -- ptr u8 n )
   {: r:n :}
   r REC-PUBLIC? 0= if r REC-WORD$ exit then
   r REC-TAIL$ {: w:ptr wu:n :}
   r REC-PACKAGE$ {: p:ptr pu:n :}
   pu 1+ wu + {: nu:n :}
   nu 1+ NAME-B-RESERVE
   p 0 NAME-B pu BYTE-COPY
   [char] : pu NAME-B c!
   w pu 1+ NAME-B wu BYTE-COPY
   0 NAME-B nu ;

\ The SymbolKind of a class.
: KIND ( n -- n )
   {: class:n :}
   class CLASS-CONSTANT = if CONSTANT-KIND exit then
   class CLASS-STORAGE = if VARIABLE-KIND exit then
   FUNCTION-KIND ;

\ The SymbolInformation the header describes for record R of group G, by
\ this name.
: NAMED ( ptr JSON-WRITE:writer n n ptr u8 n -- ptr JSON-WRITE:writer )
   {: g:n r:n nm:ptr nu:n :}
   OBJECT-START
   s" name" nm nu FIELD-S COMMA
   s" kind" r REC-CLASS KIND FIELD-U COMMA
   s" location" KEY OBJECT-START
      s" uri" g GROUP-URI$ FIELD-RAW COMMA
      r REC-RANGE LSP-TEXT:RANGE-AT
   OBJECT-END
   r REC-PACKAGE$ {: p:ptr pu:n :}
   pu 0 > if COMMA s" containerName" p pu FIELD-S then
   OBJECT-END ;

public

\ That SymbolInformation for record R of group G, named by its word.
: SYMBOL ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: g:n r:n :}
   g r r REC-WORD$ NAMED ;

private

\ The symbol for record R of group G listed next.
: SHOWN ( n n -- )
   {: g:n r:n :}
   OUT @
   SHOWN-N @ 0 > if COMMA then
   g r r NAME$ NAMED drop
   1 SHOWN-N +! ;

\ The symbols of group G the query lists.
: GROUP-SYMBOLS ( n -- )
   {: g:n :}
   g GROUP-FIRST begin dup 0 >= while
      dup LISTED? if g over SHOWN then
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
   q qu COLON-IN COLON-AT !
   0 SHOWN-N !
   w ARRAY-START drop
   [: GROUP-SYMBOLS ;] DEFS-EACH
   w ARRAY-END ;

;using
;using
;package
