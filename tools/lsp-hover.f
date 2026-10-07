\ lsp-hover.f - the language server's hover: the declaration a token in an
\ open document declares or binds to, as the document's last check stated it
\ once that check completed.
\
\ ANSWER writes the result of textDocument/hover at a byte of an open
\ document's text. At a byte a use of the document's last completed check
\ holds (LSP-DEFS:DEFS-USE-AT), it shows the declaration the use binds to: of
\ the records at the token go to definition gives the Location of
\ (tools/lsp-definition.f), the one of the word as the use writes it, or of
\ its tail when the use qualifies it, else the only one
\ (LSP-DEFS:GROUP-REC-NAMED); else, when no definition line states that
\ declaration, or several share its token and none is of that word or its
\ tail, the word as the use writes it and the file the declaration is in. At
\ any other byte that the token of a definition the check retained in the
\ document's own file holds, while the check's positions are of the document's
\ text (LSP-DEFS:DEFS-OWN-GROUP), it shows the first such definition. Its range is
\ the use's or the token's. Any other byte, and a document whose last check
\ did not complete or that has none, answer null.
\
\ A definition shows as two lines of Habu: the statement token that declared
\ it, its word and, in parentheses, its declared effect when it declared one,
\ then a comment naming the package it is declared in, unless it is global,
\ and where it is: its file, relative to the server's working directory when
\ it lies inside it, else its path, and the 1-based line of its token. A use
\ with no record shows its word, then its declaration's file. As Markdown,
\ the lines are a fenced habu block; as plain text, just the lines.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the answer being written belongs to the
\ server's one task.

require lib/string.f
require lib/fmt.f
require lib/byte-buffer.f
require lib/json-write.f
require lib/adt/option.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-HOVER
using JSON-WRITE
using LSP-DEFS

private

10 constant LF

create VALUE-B BUF:HDR-BYTES allot       \ the contents' text, grown as it is written
TYPED-VARIABLE OUT ptr JSON-WRITE:writer \ the answer's writer,
variable TARGET-G                        \ and the group of the record it shows

: V+ ( ptr u8 n -- )  BUF:N>BLEN VALUE-B BUF:APPEND-SPAN ;
: NL ( -- )  LF VALUE-B BUF:APPEND-BYTE ;
: VALUE$ ( -- ptr u8 n )  VALUE-B BUF:SPAN$ BUF:BLEN>N ;

\ The file at this path, relative to the server's working directory when it
\ lies inside it.
: WHERE+ ( ptr u8 n -- )  SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE V+ ;

\ The fence that opens the lines as Markdown, when MD holds.
: OPEN ( bool -- )
   if s" ```habu" V+ NL then ;

\ The lines of record R of group G.
: RECORD-LINES ( n n -- )
   {: g:n r:n :}
   r REC-KIND$ V+ s"  " V+ r REC-WORD$ V+
   r REC-EFFECT$ {: e:ptr eu:n :}
   eu 0 > if s"  ( " V+ e eu V+ s"  )" V+ then
   NL s" \ " V+
   r REC-PACKAGE$ {: p:ptr pu:n :}
   pu 0 > if s" package " V+ p pu V+ s" , " V+ then
   g GROUP-PATH$ WHERE+ s" :" V+
   r REC-RANGE 2drop drop 1+ SB-RESET FMT:SB-INT SB$ V+ ;

\ The word as use U of the text T writes it: the bytes the use spans, held to
\ the text.
: USE-WORD$ ( ptr u8 n n -- ptr u8 n )
   {: t:ptr tu:n u:n :}
   u USE-BYTES {: from:n to:n :}
   from 0 max tu min {: a:n :}
   to a max tu min {: b:n :}
   t a + b a - ;

\ The lines of use U of the text T when TARGET-REC gives none: the word as the
\ use writes it, then its declaration's file.
: USE-LINES ( ptr u8 n n -- )
   {: t:ptr tu:n u:n :}
   t tu u USE-WORD$ V+
   NL s" \ " V+ u USE-GROUP GROUP-PATH$ WHERE+ ;

\ The hover object up to its range: its contents, the lines VALUE-B holds,
\ fenced as Markdown when MD holds, else plain text.
: CONTENTS ( bool -- ptr JSON-WRITE:writer )
   {: md:bool :}
   md if NL s" ```" V+ then
   OUT @
   OBJECT-START
   s" contents" KEY OBJECT-START
   md if s" kind" s" markdown" FIELD-S else s" kind" s" plaintext" FIELD-S then
   COMMA s" value" VALUE$ FIELD-S
   OBJECT-END COMMA ;

\ The record of the declaration use U of the text T binds to, by the word as
\ the use writes it, if the use's group holds one, with that group in
\ TARGET-G.
: TARGET-REC ( ptr u8 n n -- option<n> )
   {: t:ptr tu:n u:n :}
   u USE-GROUP dup TARGET-G !
   u USE-TARGET t tu u USE-WORD$ GROUP-REC-NAMED ;

\ The hover of use U of the text T, positions counting in it.
: USE-HOVER ( ptr u8 n n bool -- )
   {: t:ptr tu:n u:n md:bool :}
   md OPEN
   t tu u TARGET-REC MATCH option
      some OF TARGET-G @ swap RECORD-LINES ENDOF
      none OF t tu u USE-LINES ENDOF
   ;MATCH
   u USE-BYTES {: from:n to:n :}
   md CONTENTS
   from LSP-TEXT:LINE-CHARACTER to from max LSP-TEXT:LINE-CHARACTER
   LSP-TEXT:RANGE-AT OBJECT-END drop ;

\ The hover of record R of the group in TARGET-G.
: RECORD-HOVER ( n bool -- )
   {: r:n md:bool :}
   md OPEN
   TARGET-G @ r RECORD-LINES
   md CONTENTS r REC-RANGE LSP-TEXT:RANGE-AT OBJECT-END drop ;

\ The record of the document in this slot's own file whose token holds this
\ byte, its group in TARGET-G, if one does.
: HELD-REC ( n n -- option<n> )
   {: slot:n at:n :}
   slot DEFS-OWN-GROUP MATCH option
      some OF dup TARGET-G ! at GROUP-REC-HOLDING ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

\ The hover of the definition whose token holds this byte of the document in
\ this slot, else null.
: DEF-HOVER ( n n bool -- )
   {: slot:n at:n md:bool :}
   slot at HELD-REC MATCH option
      some OF md RECORD-HOVER ENDOF
      none OF OUT @ NULL drop ENDOF
   ;MATCH ;

public

\ Readies the answer's storage, before any hover.
: PREPARE ( -- )
   VALUE-B 1 BUF:N>BLEN BUF:INIT ;

\ Writes the result of textDocument/hover at this byte of the text of the
\ document in this slot, which positions count in, the hover the header
\ describes or null, as Markdown when MD holds, else plain text, to the
\ writer.
: ANSWER ( ptr JSON-WRITE:writer n n bool -- ptr JSON-WRITE:writer )
   {: w slot:n at:n md:bool :}
   w OUT !
   VALUE-B BUF:CLEAR
   slot at DEFS-USE-AT MATCH option
      some OF slot LSP-DOCS:DOC-TEXT$ rot md USE-HOVER ENDOF
      none OF slot at md DEF-HOVER ENDOF
   ;MATCH
   w ;

;using
;using
;package
