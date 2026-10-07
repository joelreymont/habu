\ lsp-links.f - the language server's document links: the files the top-level
\ loaders of an open document reached, as its last check reported them once
\ that check completed.
\
\ ANSWER writes the result of textDocument/documentLink for an open document:
\ one DocumentLink for each load the document's last completed check reported
\ (LSP-DOCS:DOC-LOADS$, the lines of CHECK:VERIFY-LOADS$ as
\ tools/check-verify-child.f states them), in their order, and none when that
\ check reported none or did not complete. A link's range is its loader's
\ operand, the bytes its line states in the document's text, in LSP positions
\ (LSP-TEXT:RANGE). Its target is the file its line names, at the URI of the
\ open document that holds that file, else at the file's URI
\ (LSP-DEFS:DEP-URI$). A loader that read its file has no tooltip; one that
\ held it, the engine providing it or a load before having recorded it, says
\ so in its tooltip, naming the file as hover does: relative to the server's
\ working directory when it lies inside it, else by its path. A link is its
\ line mapped: the server reads, resolves and parses nothing for it.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/byte-buffer.f
require lib/json-read.f
require lib/json-write.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f
require tools/lsp-line.f

package LSP-LINKS
using JSON-WRITE

private

create JR-ST JR:STORAGE-BYTES allot      \ JR storage for decoding a line
DYNAMIC-BUFFER PATH-B u8                 \ the line's file, decoded,
variable PATH-U
TYPED-VARIABLE HELD bool                 \ and whether its loader held it
create TIP-B BUF:HDR-BYTES allot         \ a held link's tooltip
TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer,
variable SHOWN-N                         \ and the links written so far

: PATH$ ( -- ptr u8 n )  0 PATH-B PATH-U @ ;
: T+ ( ptr u8 n -- )  BUF:N>BLEN TIP-B BUF:APPEND-SPAN ;
: TIP$ ( -- ptr u8 n )  TIP-B BUF:SPAN$ BUF:BLEN>N ;

\ The string the reader is at, decoded into PATH-B.
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

\ The load line's file and outcome decoded.
: DECODE ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 PATH-U !
   false HELD !
   JR-ST JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   begin JR:NEXT JR:T-KEY = while MEMBER repeat
   JR:CLOSE ;

\ The tooltip of a link whose loader held its file.
: HELD-TIP ( -- ptr u8 n )
   TIP-B BUF:CLEAR
   s" held " T+
   PATH$ SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE T+
   s" : this loader did not read it; the engine provides it or it was already recorded." T+
   TIP$ ;

\ The link for this load line.
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

\ Readies the answer's storage, before any link.
: PREPARE ( -- )
   TIP-B 1 BUF:N>BLEN BUF:INIT ;

\ Writes the result of textDocument/documentLink for the document in this
\ slot, the array the header describes, to the writer.
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
