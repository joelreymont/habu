split-str
\ lsp-symbols.f - workspace/symbol: the definitions the open documents' last
\ completed checks retained (tools/lsp-defs.f). Document symbols
\ (tools/lsp-outline.f) answer with SYMBOL.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/string.f
require src/lib/adt/option.f
require src/lib/json.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-SYMBOLS
using JSON
using LSP-DEFS

private

14 constant CONSTANT-KIND                \ SymbolKind.Constant
13 constant VARIABLE-KIND                \ SymbolKind.Variable
12 constant FUNCTION-KIND                \ SymbolKind.Function

TYPED-VARIABLE OUT ^JSON:writer \ the answer's writer,
TYPED-VARIABLE QUERY-A ^u8            \ its query,
variable QUERY-U
variable COLON-AT                        \ where its first colon is, -1 for none,
variable SHOWN-N                         \ and the symbols it lists so far
DYNAMIC-BUFFER NAME-B u8                 \ a symbol's qualified name

: CONTAINS-CI? ( str str -- bool )
   {: a:^ u:n b:^ v:n :}
   u v - 1+ 0 ?do
      a i + v b v STR=CI if unloop true exit then
   loop
   false ;

: COLON-IN ( str -- n )
   [char] : INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: PACKAGE$ ( -- str )  QUERY-A @ COLON-AT @ ;
: TAIL$ ( -- str )
   COLON-AT @ 1+ {: at:n :}
   QUERY-A @ at + QUERY-U @ at - ;

\ The text after the word's first colon when that colon is neither its first
\ nor its last byte, else the whole word: what a caller writes after a
\ package's colon, also for a word re-exported by EXPORT PKG:WORD.
: REC-TAIL$ ( n -- str )
   REC-WORD$ {: w:^ wu:n :}
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
: NAME$ ( n -- str )
   {: r:n :}
   r REC-PUBLIC? 0= if r REC-WORD$ exit then
   r REC-TAIL$ {: w:^ wu:n :}
   r REC-PACKAGE$ {: p:^ pu:n :}
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
: NAMED ( ^JSON:writer n n str -- ^JSON:writer )
   {: g:n r:n nm:^ nu:n :}
   WRITE-OBJECT-START
   s" name" nm nu WRITE-FIELD-S WRITE-COMMA
   s" kind" r REC-CLASS KIND WRITE-FIELD-U WRITE-COMMA
   s" location" WRITE-KEY WRITE-OBJECT-START
      s" uri" g GROUP-URI$ WRITE-FIELD-RAW WRITE-COMMA
      r REC-RANGE LSP-TEXT:RANGE-AT
   WRITE-OBJECT-END
   r REC-PACKAGE$ {: p:^ pu:n :}
   pu 0 > if WRITE-COMMA s" containerName" p pu WRITE-FIELD-S then
   WRITE-OBJECT-END ;

public

\ Named by its word as the source wrote it, whatever its package.
: SYMBOL ( ^JSON:writer n n -- ^JSON:writer )
   {: g:n r:n :}
   g r r REC-WORD$ NAMED ;

private

: SHOWN ( n n -- )
   {: g:n r:n :}
   OUT @
   SHOWN-N @ 0 > if WRITE-COMMA then
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
: ANSWER ( ^JSON:writer ^u8 n -- ^JSON:writer )
   {: w q:^ qu:n :}
   w OUT !
   q QUERY-A !
   qu QUERY-U !
   q qu COLON-IN COLON-AT !
   0 SHOWN-N !
   w WRITE-ARRAY-START drop
   [: GROUP-SYMBOLS ;] DEFS-EACH
   w WRITE-ARRAY-END ;

;using
;using
;package
