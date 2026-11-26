\ lsp-hover.f - textDocument/hover: the declaration a token in an open document
\ declares or binds to, as the document's last completed check stated it.
\ STORAGE CLASS. PROCESS-GLOBAL: the answer belongs to the server's one task.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/json.f
require src/lib/adt/option.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f
require src/lib/json/write.f

package LSP-HOVER

private

10 constant LF

TYPED-VARIABLE VALUE-B ^VEC:vector<u8> SCRATCH-ONLY \ the contents' text, grown as it is written
TYPED-VARIABLE OUT ^JSON:writer \ the answer's writer,
variable TARGET-G                        \ and the group of the record it shows

: V+ ( str -- )  VALUE-B @ VEC:APPEND ;
: NL ( -- )  LF VALUE-B @ VEC:PUSH c! ;
: VALUE$ ( -- str )  VALUE-B @ VEC:BYTES ;

: WHERE+ ( str -- )  SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE V+ ;

: OPEN ( bool -- )
   if s" ```habu" V+ NL then ;

\ Two lines of Habu: the declaring statement token, the word and its declared
\ effect in parentheses when it has one; then a comment naming its package
\ unless global, its file (relative to the working directory when inside it)
\ and the 1-based line of its token. Fenced as habu under Markdown.
: RECORD-LINES ( n n -- )
   {: g:n r:n :}
   r LSP-DEFS:REC-KIND$ V+ s"  " V+ r LSP-DEFS:REC-WORD$ V+
   r LSP-DEFS:REC-EFFECT$ {: e:str :}
   e STR:LEN 0 > if s"  ( " V+ e V+ s"  )" V+ then
   NL s" \ " V+
   r LSP-DEFS:REC-PACKAGE$ {: p:str :}
   p STR:LEN 0 > if s" package " V+ p V+ s" , " V+ then
   g LSP-DEFS:GROUP-PATH$ WHERE+ s" :" V+
   r LSP-DEFS:REC-RANGE 2drop drop 1+ SB-RESET FMT:SB-INT SB$ V+ ;

: USE-LINES ( str n -- )
   {: t:str u:n :}
   t u LSP-DEFS:USE-WORD$ V+
   NL s" \ " V+ u LSP-DEFS:USE-GROUP LSP-DEFS:GROUP-PATH$ WHERE+ ;

: CONTENTS ( bool -- ^JSON:writer )
   {: md:bool :}
   md if NL s" ```" V+ then
   OUT @
   JSON:WRITE-OBJECT-START
   s" contents" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   md if s" kind" s" markdown" JSON:WRITE-FIELD-S else s" kind" s" plaintext" JSON:WRITE-FIELD-S then
   JSON:WRITE-COMMA s" value" VALUE$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END JSON:WRITE-COMMA ;

\ Leaves the record's group in TARGET-G.
: TARGET-REC ( str n -- option<n> )
   {: t:str u:n :}
   u LSP-DEFS:USE-GROUP dup TARGET-G !
   u LSP-DEFS:USE-TARGET t u LSP-DEFS:USE-WORD$ LSP-DEFS:GROUP-REC-NAMED ;

: USE-HOVER ( str n bool -- )
   {: t:str u:n md:bool :}
   md OPEN
   t u TARGET-REC MATCH option
      some OF TARGET-G @ swap RECORD-LINES ENDOF
      none OF t u USE-LINES ENDOF
   ;MATCH
   u LSP-DEFS:USE-BYTES {: from:n to:n :}
   md CONTENTS
   from LSP-TEXT:LINE-CHARACTER to from max LSP-TEXT:LINE-CHARACTER
   LSP-TEXT:RANGE-AT JSON:WRITE-OBJECT-END drop ;

: RECORD-HOVER ( n bool -- )
   {: r:n md:bool :}
   md OPEN
   TARGET-G @ r RECORD-LINES
   md CONTENTS r LSP-DEFS:REC-RANGE LSP-TEXT:RANGE-AT JSON:WRITE-OBJECT-END drop ;

\ Leaves the record's group in TARGET-G.
: HELD-REC ( n n -- option<n> )
   {: slot:n at:n :}
   slot LSP-DEFS:DEFS-OWN-GROUP MATCH option
      some OF dup TARGET-G ! at LSP-DEFS:GROUP-REC-HOLDING ENDOF
      none OF OPTION:NONE ENDOF
   ;MATCH ;

: DEF-HOVER ( n n bool -- )
   {: slot:n at:n md:bool :}
   slot at HELD-REC MATCH option
      some OF md RECORD-HOVER ENDOF
      none OF OUT @ JSON:WRITE-NULL drop ENDOF
   ;MATCH ;

public

: PREPARE ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW VALUE-B ! ;

\ At a use the last completed check holds (LSP-DEFS:DEFS-USE-AT): the record go
\ to definition locates (tools/lsp-definition.f) of the word as the use writes
\ it, or of its tail when the use qualifies it, else the only one
\ (LSP-DEFS:GROUP-REC-NAMED); with no such record, the word and its
\ declaration's file. Else, at the token of a definition retained in the
\ document's own file while the check's positions are of this text
\ (LSP-DEFS:DEFS-OWN-GROUP), the first such definition. The range is the use's
\ or the token's. Any other byte, or no completed check, answers null.
: ANSWER ( ^JSON:writer n n bool -- ^JSON:writer )
   {: w slot:n at:n md:bool :}
   w OUT !
   VALUE-B @ 0 VEC:RESIZE
   slot at LSP-DEFS:DEFS-USE-AT MATCH option
      some OF slot LSP-DOCS:DOC-TEXT$ swap md USE-HOVER ENDOF
      none OF slot at md DEF-HOVER ENDOF
   ;MATCH
   w ;

;package
