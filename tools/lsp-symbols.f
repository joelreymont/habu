\ lsp-symbols.f - workspace/symbol: the definitions the open documents' last
\ completed checks retained (tools/lsp-defs.f). Document symbols
\ (tools/lsp-outline.f) answer with SYMBOL.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/string.f
require src/lib/adt/option.f
require src/lib/json-write.f
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

: CONTAINS-CI? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   u v - 1+ 0 ?do
      a i + v b v STR=CI if unloop true exit then
   loop
   false ;

: COLON-IN ( ptr u8 n -- n )
   [char] : INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: PACKAGE$ ( -- ptr u8 n )  QUERY-A @ COLON-AT @ ;
: TAIL$ ( -- ptr u8 n )
   COLON-AT @ 1+ {: at:n :}
   QUERY-A @ at + QUERY-U @ at - ;

\ The text after the word's first colon when that colon is neither its first
\ nor its last byte, else the whole word: what a caller writes after a
\ package's colon, also for a word re-exported by EXPORT PKG:WORD.
: REC-TAIL$ ( n -- ptr u8 n )
   REC-WORD$ {: w:ptr wu:n :}
   w wu COLON-IN {: at:n :}
   at 0 > at wu 1- < and if w at 1+ + wu at 1+ - exit then
   w wu ;

\ Its word holds the query; or, for a query with a colon, the checker recorded
\ it public in the package before the first colon (compared whole) and its tail
\ holds the text after it. ASCII letters compare without case.
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

: KIND ( n -- n )
   {: class:n :}
   class CLASS-CONSTANT = if CONSTANT-KIND exit then
   class CLASS-STORAGE = if VARIABLE-KIND exit then
   FUNCTION-KIND ;

\ name: PKG:TAIL for a definition recorded public in a package, else the word as
\ written. kind: from the class the verifier states for the declaring path
\ (tools/check-verify-child.f): Constant, Variable for storage, Function for a
\ word or a re-export. location: the declaring token's range, at the checked
\ document's URI for its own file, else at the URI of the open document that
\ held the file when the check completed, else at its path's file URI.
\ containerName: the package the word was recorded under; a global has none.
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

\ Named by its word as the source wrote it, whatever its package.
: SYMBOL ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: g:n r:n :}
   g r r REC-WORD$ NAMED ;

private

: SHOWN ( n n -- )
   {: g:n r:n :}
   OUT @
   SHOWN-N @ 0 > if COMMA then
   g r r NAME$ NAMED drop
   1 SHOWN-N +! ;

: GROUP-SYMBOLS ( n -- )
   {: g:n :}
   g GROUP-FIRST begin dup 0 >= while
      dup LISTED? if g over SHOWN then
      REC-NEXT
   repeat drop ;

public

\ A file several checks reached is answered once, from the check the store
\ answers it from (LSP-DEFS:DEFS-EACH): the check of the document holding the
\ file while the store keeps one, else the latest. The order follows the store:
\ check after check, oldest first, each check's files in first-named order.
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
