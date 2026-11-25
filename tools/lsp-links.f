split-str
\ lsp-links.f - textDocument/documentLink: one DocumentLink per load the
\ document's last completed check reported (LSP-DOCS:DOC-LOADS$), in order. Range:
\ the loader's operand (LSP-TEXT:RANGE); target: the open document holding the
\ file, else its URI (LSP-DEFS:DEP-URI$). A link is its line mapped: the server
\ reads, resolves and parses nothing for it.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f
require tools/lsp-line.f

package LSP-LINKS
using JSON

private

TYPED-VARIABLE JSON-ST JSON:cursor       \ JSON reader storage for decoding a line
TYPED-VARIABLE PATH-B ^VEC:vector<u8> SCRATCH-ONLY \ the line's file, decoded,
TYPED-VARIABLE HELD bool                 \ and whether its loader held it
TYPED-VARIABLE TIP-B ^VEC:vector<u8> SCRATCH-ONLY \ a held link's tooltip
TYPED-VARIABLE OUT ^JSON:writer \ the answer's writer,
variable SHOWN-N                         \ and the links written so far

: PATH$ ( -- str )  PATH-B @ VEC:BYTES ;
: T+ ( str -- )  TIP-B @ VEC:APPEND ;
: TIP$ ( -- str )  TIP-B @ VEC:BYTES ;

\ The member whose key the reader is at, decoded; one a link needs nothing
\ of, passed over.
: MEMBER ( JSON:reader -- JSON:reader )
   s" path" READ-STR-EQ? if READ-NEXT drop PATH-B @ READ-STR 2drop exit then
   s" outcome" READ-STR-EQ? if READ-NEXT drop s" held" READ-STR-EQ? HELD ! exit then
   READ-NEXT drop READ-SKIP-VALUE ;

: DECODE ( str -- )
   {: a:^ u:n :}
   PATH-B @ 0 VEC:RESIZE
   false HELD !
   JSON-ST a u READ-INIT
   READ-NEXT drop
   begin READ-NEXT T-KEY = while MEMBER repeat
   READ-CLOSE ;

\ A loader that held its file (the engine provided it, or a load before recorded
\ it) says so, naming the file as hover does: relative to the server's working
\ directory when inside it, else by its path.
: HELD-TIP ( -- str )
   TIP-B @ 0 VEC:RESIZE
   s" held " T+
   PATH$ SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE T+
   s" : this loader did not read it; the engine provides it or it was already recorded." T+
   TIP$ ;

: LINK ( str -- )
   {: a:^ u:n :}
   a u DECODE
   OUT @
   SHOWN-N @ 0 > if WRITE-COMMA then
   WRITE-OBJECT-START
   a u LSP-TEXT:RANGE WRITE-COMMA
   s" target" PATH$ LSP-DEFS:DEP-URI$ WRITE-FIELD-S
   HELD @ if WRITE-COMMA s" tooltip" HELD-TIP WRITE-FIELD-S then
   WRITE-OBJECT-END drop
   1 SHOWN-N +! ;

public

: PREPARE ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW PATH-B !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW TIP-B ! ;

: ANSWER ( ^JSON:writer n -- ^JSON:writer )
   {: w slot:n :}
   w OUT !
   0 SHOWN-N !
   slot LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   w WRITE-ARRAY-START drop
   slot LSP-DOCS:DOC-LOADS$ [: LINK ;] LSP-LINE:EACH-LINE
   w WRITE-ARRAY-END ;

;using
;package
