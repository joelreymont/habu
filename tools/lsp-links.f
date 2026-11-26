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
require src/lib/json/read.f
require src/lib/json/write.f

package LSP-LINKS

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
   s" path" JSON:READ-STR-EQ? if JSON:READ-NEXT drop PATH-B @ JSON:READ-STR drop exit then
   s" outcome" JSON:READ-STR-EQ? if JSON:READ-NEXT drop s" held" JSON:READ-STR-EQ? HELD ! exit then
   JSON:READ-NEXT drop JSON:READ-SKIP-VALUE ;

: DECODE ( str -- )
   {: s:str :}
   PATH-B @ 0 VEC:RESIZE
   false HELD !
   JSON-ST s JSON:READ-INIT
   JSON:READ-NEXT drop
   begin JSON:READ-NEXT JSON:T-KEY = while MEMBER repeat
   JSON:READ-CLOSE ;

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
   {: s:str :}
   s DECODE
   OUT @
   SHOWN-N @ 0 > if JSON:WRITE-COMMA then
   JSON:WRITE-OBJECT-START
   s LSP-TEXT:RANGE JSON:WRITE-COMMA
   s" target" PATH$ LSP-DEFS:DEP-URI$ JSON:WRITE-FIELD-S
   HELD @ if JSON:WRITE-COMMA s" tooltip" HELD-TIP JSON:WRITE-FIELD-S then
   JSON:WRITE-OBJECT-END drop
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
   w JSON:WRITE-ARRAY-START drop
   slot LSP-DOCS:DOC-LOADS$ [: LINK ;] LSP-LINE:EACH-LINE
   w JSON:WRITE-ARRAY-END ;

;package
