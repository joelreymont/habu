\ lsp-rename.f - the language server's rename. A rename edits a token's tail alone,
\ its bytes after a qualifier's colon, so the new name the client asks for is a tail
\ and a qualifier stays. PREPARED and ANSWER reject nothing once LSP-REFERENCES has
\ selected and placed the declaration.
\ STORAGE CLASS. CALLER-OWNED: no state; the writer is the caller's, the selection
\ and locations LSP-REFERENCES's, the uses tools/lsp-defs.f's.

require src/lib/string.f
require src/lib/string-option.f
require src/lib/json.f
require src/lib/adt/option.f
require src/lib/source-lex.f
require tools/lsp-docs.f
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-references.f
require src/lib/json/write.f

package LSP-RENAME

private

34 constant QUOTE
92 constant BACKSLASH

\ Whether these bytes read as a qualified name, or with a second colon as
\ one no word has: their first colon is neither first nor last.
: QUALIFIES? ( str -- bool )
   {: s:str :}
   s [char] : STR:INDEX-OF MATCH option
      some OF IDX>N {: k:n :} k 0 >  k s STR:LEN 1- <  and ENDOF
      none OF false ENDOF
   ;MATCH ;

: ANY-BYTE? ( str [ n -- bool ] -- bool )
   {: s:str test :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ test execute if true unloop exit then
   loop
   false ;

\ Whether location K is the first of its URI's: the first one, or one whose
\ URI is not the one before it's.
: NEW-URI? ( n -- bool )
   {: k:n :}
   k 0= if true exit then
   k LSP-REFERENCES:LOC-URI$ k 1- LSP-REFERENCES:LOC-URI$ STR:= 0= ;

public

\ The Range of the tail of the token at this byte of the open document in this
\ slot, which positions then count in: the use that holds the byte, else the
\ declaring token LSP-REFERENCES:PLACE placed. The selecting use and the declaring
\ token spell its tail (LSP-REFERENCES:SPELT?).
: PREPARED ( ^JSON:writer n n -- ^JSON:writer )
   {: slot:n at:n :}
   slot LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   slot at LSP-DEFS:DEFS-USE-AT MATCH option
      some OF LSP-DEFS:USE-BYTES ENDOF
      none OF LSP-REFERENCES:TOKEN ENDOF
   ;MATCH {: from:n to:n :}
   from to LSP-REFERENCES:TAIL-FROM LSP-TEXT:LINE-CHARACTER
   to LSP-TEXT:LINE-CHARACTER
   LSP-TEXT:RANGE-VALUE ;

\ Why this new name cannot be written over a tail, else empty: it is empty; it
\ holds a byte that ends a token as parse-name reads one (LINT-LEX:ENGINE-DELIM?);
\ its first colon is neither first nor last, making the tail a qualified name; or it
\ holds a quote or a backslash, which a string literal naming the word (a use before
\ a call of a names: word) could not spell.
: WHY$ ( str -- str )
   {: s:str :}
   s STR:LEN 0= if s" the new name is empty" exit then
   s [: LINT-LEX:ENGINE-DELIM? ;] ANY-BYTE? if
      s" the new name holds a space or a control byte" exit
   then
   s QUALIFIES? if s" the new name holds a colon that qualifies it" exit then
   s [: QUOTE = ;] ANY-BYTE? if s" the new name holds a quote" exit then
   s [: BACKSLASH = ;] ANY-BYTE? if s" the new name holds a backslash" exit then
   s" " ;

\ Why a name WHY$ passes cannot be written over the tails the gather placed, else
\ empty: it holds a colon and a tail follows a qualifier, giving the token a name no
\ word has (CHECKER-QUALIFIED?).
: QUALIFIED-WHY$ ( str -- str )
   {: s:str :}
   s [: [char] : = ;] ANY-BYTE? 0= if s" " exit then
   LSP-REFERENCES:LOCATIONS 0 ?do
      i LSP-REFERENCES:LOC-QUALIFIED? if
         s" the new name holds a colon and a use is qualified" unloop exit
      then
   loop
   s" " ;

\ The result of textDocument/rename, the changes that write this name over
\ the tail of each location, to the writer. The declaration is the first
\ location, then the uses by file, P's first (LSP-REFERENCES:SETTLE), so
\ each URI's locations are one run.
: ANSWER ( ^JSON:writer str -- ^JSON:writer )
   {: name:str :}
   JSON:WRITE-OBJECT-START
   s" changes" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   LSP-REFERENCES:LOCATIONS 0 ?do
      i NEW-URI? if
         i 0 > if JSON:WRITE-ARRAY-END JSON:WRITE-COMMA then
         i LSP-REFERENCES:LOC-URI$ JSON:WRITE-KEY JSON:WRITE-ARRAY-START
      else
         JSON:WRITE-COMMA
      then
      JSON:WRITE-OBJECT-START
      i LSP-REFERENCES:LOC-TAIL LSP-TEXT:RANGE-AT JSON:WRITE-COMMA
      s" newText" name JSON:WRITE-FIELD-S
      JSON:WRITE-OBJECT-END
   loop
   JSON:WRITE-ARRAY-END
   JSON:WRITE-OBJECT-END
   JSON:WRITE-OBJECT-END ;

;package
