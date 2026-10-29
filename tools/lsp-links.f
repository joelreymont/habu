\ lsp-links.f - textDocument/documentLink: one DocumentLink per load the
\ document's last completed check reported (LSP-DOCS:DOC-LOADS$), in order. Range:
\ the loader's operand (LSP-TEXT:RANGE); target: the open document holding the
\ file, else its URI (LSP-DEFS:DEP-URI$). A link is its line mapped: the server
\ reads, resolves and parses nothing for it.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/byte-buffer.f
require src/lib/json-read.f
require src/lib/json-write.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f
require tools/lsp-line.f

package LSP-LINKS
using JSON-WRITE

private

TYPED-VARIABLE JR-ST JR:cursor           \ JR storage for decoding a line
DYNAMIC-BUFFER PATH-B u8                 \ the line's file, decoded,
variable PATH-U
TYPED-VARIABLE HELD bool                 \ and whether its loader held it
create TIP-B BUF:HDR-BYTES allot         \ a held link's tooltip
TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer,
variable SHOWN-N                         \ and the links written so far

: PATH$ ( -- ptr u8 n )  0 PATH-B PATH-U @ ;
: T+ ( ptr u8 n -- )  BUF:N>BLEN TIP-B BUF:APPEND-SPAN ;
: TIP$ ( -- ptr u8 n )  TIP-B BUF:SPAN$ BUF:BLEN>N ;

: STRING>PATH ( JR:reader -- JR:reader )
   JR:SPAN$ nip {: raw:n :}
   raw 1 max PATH-B-RESERVE
   0 PATH-B raw JR:STR PATH-U ! ;

\ The member whose key the reader is at, decoded; one a link needs nothing
\ of, passed over.
: MEMBER ( JR:reader -- JR:reader )
   s" path" JR:STR-EQ? if JR:NEXT drop STRING>PATH exit then
   s" outcome" JR:STR-EQ? if JR:NEXT drop s" held" JR:STR-EQ? HELD ! exit then
   JR:NEXT drop JR:SKIP-VALUE ;

: DECODE ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 PATH-U !
   false HELD !
   JR-ST a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while MEMBER repeat
   JR:CLOSE ;

\ A loader that held its file (the engine provided it, or a load before recorded
\ it) says so, naming the file as hover does: relative to the server's working
\ directory when inside it, else by its path.
: HELD-TIP ( -- ptr u8 n )
   TIP-B BUF:CLEAR
   s" held " T+
   PATH$ SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE T+
   s" : this loader did not read it; the engine provides it or it was already recorded." T+
   TIP$ ;

: LINK ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u DECODE
   OUT @
   SHOWN-N @ 0 > if COMMA then
   OBJECT-START
   a u LSP-TEXT:RANGE COMMA
   s" target" PATH$ LSP-DEFS:DEP-URI$ FIELD-S
   HELD @ if COMMA s" tooltip" HELD-TIP FIELD-S then
   OBJECT-END drop
   1 SHOWN-N +! ;

public

: PREPARE ( -- )
   TIP-B 1 BUF:N>BLEN BUF:INIT ;

: ANSWER ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: w slot:n :}
   w OUT !
   0 SHOWN-N !
   slot LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   w ARRAY-START drop
   slot LSP-DOCS:DOC-LOADS$ [: LINK ;] LSP-LINE:EACH-LINE
   w ARRAY-END ;

;using
;package
