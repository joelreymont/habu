\ lsp-rename.f - the language server's rename. A rename edits a token's tail alone,
\ its bytes after a qualifier's colon, so the new name the client asks for is a tail
\ and a qualifier stays. PREPARED and ANSWER refuse nothing once LSP-REFERENCES has
\ selected and placed the declaration.
\ STORAGE CLASS. CALLER-OWNED: no state; the writer is the caller's, the selection
\ and locations LSP-REFERENCES's, the uses tools/lsp-defs.f's.

require src/lib/string.f
require src/lib/json-write.f
require src/lib/adt/option.f
require src/lib/source-lex.f
require tools/lsp-docs.f
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-references.f

package LSP-RENAME
using JSON-WRITE
using LSP-DEFS

private

34 constant QUOTE
92 constant BACKSLASH

\ Whether these bytes read as a qualified name, or with a second colon as
\ one no word has: their first colon is neither first nor last.
: QUALIFIES? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u [char] : INDEX-OF MATCH option
      some OF IDX>N {: k:n :} k 0 >  k u 1- <  and ENDOF
      none OF false ENDOF
   ;MATCH ;

: ANY-BYTE? ( ptr u8 n [ n -- bool ] -- bool )
   {: a:ptr u:n test :}
   u 0 ?do
      a i + c@ test execute if true unloop exit then
   loop
   false ;

\ Whether location K is the first of its URI's: the first one, or one whose
\ URI is not the one before it's.
: NEW-URI? ( n -- bool )
   {: k:n :}
   k 0= if true exit then
   k LSP-REFERENCES:LOC-URI$ k 1- LSP-REFERENCES:LOC-URI$ STR= 0= ;

public

\ The Range of the tail of the token at this byte of the open document in this
\ slot, which positions then count in: the use that holds the byte, else the
\ declaring token LSP-REFERENCES:PLACE placed. The selecting use and the declaring
\ token spell its tail (LSP-REFERENCES:SPELT?).
: PREPARED ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: slot:n at:n :}
   slot LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   slot at DEFS-USE-AT MATCH option
      some OF USE-BYTES ENDOF
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
: WHY$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   u 0= if s" the new name is empty" exit then
   a u [: LINT-LEX:ENGINE-DELIM? ;] ANY-BYTE? if
      s" the new name holds a space or a control byte" exit
   then
   a u QUALIFIES? if s" the new name holds a colon that qualifies it" exit then
   a u [: QUOTE = ;] ANY-BYTE? if s" the new name holds a quote" exit then
   a u [: BACKSLASH = ;] ANY-BYTE? if s" the new name holds a backslash" exit then
   s" " ;

\ Why a name WHY$ passes cannot be written over the tails the gather placed, else
\ empty: it holds a colon and a tail follows a qualifier, giving the token a name no
\ word has (CHECKER-QUALIFIED?).
: QUALIFIED-WHY$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a u [: [char] : = ;] ANY-BYTE? 0= if s" " exit then
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
: ANSWER ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: name:ptr nu:n :}
   OBJECT-START
   s" changes" KEY OBJECT-START
   LSP-REFERENCES:LOCATIONS 0 ?do
      i NEW-URI? if
         i 0 > if ARRAY-END COMMA then
         i LSP-REFERENCES:LOC-URI$ KEY ARRAY-START
      else
         COMMA
      then
      OBJECT-START
      i LSP-REFERENCES:LOC-TAIL LSP-TEXT:RANGE-AT COMMA
      s" newText" name nu FIELD-S
      OBJECT-END
   loop
   ARRAY-END
   OBJECT-END
   OBJECT-END ;

;using
;using
;package
