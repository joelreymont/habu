\ lsp-hover.f - textDocument/hover: the declaration a token in an open document
\ declares or binds to, as the document's last completed check stated it.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/byte-buffer.f
require src/lib/json-write.f
require src/lib/adt/option.f
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

: WHERE+ ( ptr u8 n -- )  SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE V+ ;

: OPEN ( bool -- )
   if s" ```habu" V+ NL then ;

\ Two lines of Habu: the declaring statement token, the word and its declared
\ effect in parentheses when it has one; then a comment naming its package
\ unless global, its file (relative to the working directory when inside it)
\ and the 1-based line of its token. Fenced as habu under Markdown.
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

: USE-LINES ( ptr u8 n n -- )
   {: t:ptr tu:n u:n :}
   t tu u USE-WORD$ V+
   NL s" \ " V+ u USE-GROUP GROUP-PATH$ WHERE+ ;

: CONTENTS ( bool -- ptr JSON-WRITE:writer )
   {: md:bool :}
   md if NL s" ```" V+ then
   OUT @
   OBJECT-START
   s" contents" KEY OBJECT-START
   md if s" kind" s" markdown" FIELD-S else s" kind" s" plaintext" FIELD-S then
   COMMA s" value" VALUE$ FIELD-S
   OBJECT-END COMMA ;

\ Leaves the record's group in TARGET-G.
: TARGET-REC ( ptr u8 n n -- option<n> )
   {: t:ptr tu:n u:n :}
   u USE-GROUP dup TARGET-G !
   u USE-TARGET t tu u USE-WORD$ GROUP-REC-NAMED ;

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

: RECORD-HOVER ( n bool -- )
   {: r:n md:bool :}
   md OPEN
   TARGET-G @ r RECORD-LINES
   md CONTENTS r REC-RANGE LSP-TEXT:RANGE-AT OBJECT-END drop ;

\ Leaves the record's group in TARGET-G.
: HELD-REC ( n n -- option<n> )
   {: slot:n at:n :}
   slot DEFS-OWN-GROUP MATCH option
      some OF dup TARGET-G ! at GROUP-REC-HOLDING ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

: DEF-HOVER ( n n bool -- )
   {: slot:n at:n md:bool :}
   slot at HELD-REC MATCH option
      some OF md RECORD-HOVER ENDOF
      none OF OUT @ NULL drop ENDOF
   ;MATCH ;

public

: PREPARE ( -- )
   VALUE-B 1 BUF:N>BLEN BUF:INIT ;

\ At a use the last completed check holds (LSP-DEFS:DEFS-USE-AT): the record go
\ to definition locates (tools/lsp-definition.f) of the word as the use writes
\ it, or of its tail when the use qualifies it, else the only one
\ (LSP-DEFS:GROUP-REC-NAMED); with no such record, the word and its
\ declaration's file. Else, at the token of a definition retained in the
\ document's own file while the check's positions are of this text
\ (LSP-DEFS:DEFS-OWN-GROUP), the first such definition. The range is the use's
\ or the token's. Any other byte, or no completed check, answers null.
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
