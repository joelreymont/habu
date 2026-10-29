\ lsp-tokens.f - textDocument/semanticTokens/full: a token over each word an
\ open document's last completed check bound in it, typed by what the check
\ states the word is. A word bound to no located declaration (an engine word)
\ gets none, and the client keeps its own coloring there.
\ STORAGE CLASS. PROCESS-GLOBAL: an answer's tokens gather in the module's
\ buffers, for the server's one task; tools/lsp-defs.f keeps the uses.

require src/lib/json-write.f
require src/lib/sort.f
require src/lib/adt/option.f
require tools/lsp-docs.f
require tools/lsp-defs.f
require tools/lsp-text.f

package LSP-TOKENS
using JSON-WRITE
using LSP-DEFS

private

0 constant T-FUNCTION                    \ the legend's token types, by index,
1 constant T-VARIABLE
1 constant M-READONLY                    \ and its modifiers, by bit

0 constant K-START                       \ a token's cells: the bytes it starts
1 constant K-END                         \ and ends at, and its record's class
2 constant K-CLASS
3 constant TOKEN-CELLS

DYNAMIC-BUFFER TOKENS n                  \ the tokens gathered for an answer,
variable TOKEN-N                         \ how many,
DYNAMIC-BUFFER ORDER n                   \ their indices, by where they start,
variable PREV-LINE                       \ and the line and character of the
variable PREV-CHAR                       \ one written last

: TOKEN@ ( n n -- n )  swap TOKEN-CELLS * + TOKENS @ ;
: TOKEN! ( n n n -- )  swap TOKEN-CELLS * + TOKENS ! ;

\ An export's record gets no token, nor a use bound to one: the verifier states
\ the export's own class, not the exported word's.
: GATHER ( n n n -- )
   {: from:n to:n class:n :}
   class CLASS-EXPORT = if exit then
   TOKEN-N @ {: k:n :}
   k 1+ TOKEN-CELLS * TOKENS-RESERVE
   k 1+ ORDER-RESERVE
   from k K-START TOKEN!
   to k K-END TOKEN!
   class k K-CLASS TOKEN!
   k k ORDER !
   1 TOKEN-N +! ;

\ Only a token that declares its record alone (LSP-DEFS:GROUP-REC-SOLE): a
\ DEFTYPE's name declares both its converters and gets none.
: DECLARED ( n -- )
   {: g:n :}
   g GROUP-FIRST begin dup 0 >= while
      {: r:n :}
      g r REC-BYTES GROUP-REC-SOLE MATCH option
         some OF drop r REC-BYTES r REC-CLASS GATHER ENDOF
         none OF ENDOF
      ;MATCH
      r REC-NEXT
   repeat drop ;

\ Typed by the record its spelling names (LSP-DEFS:GROUP-REC-NAMED), as hover
\ and highlight select it. A string literal before a call of a word `names:`
\ states is a use too.
: USED ( ptr u8 n n -- )
   {: t:ptr tu:n u:n :}
   u USE-GROUP 0 < if exit then
   u USE-GROUP u USE-TARGET t tu u USE-WORD$ GROUP-REC-NAMED MATCH option
      some OF {: r:n :} u USE-BYTES r REC-CLASS GATHER ENDOF
      none OF ENDOF
   ;MATCH ;

: EARLIER? ( n n -- bool )
   {: a:n b:n :}
   a K-START TOKEN@ b K-START TOKEN@ < ;

\ LSP encodes each token from the one before, in start order, and the check
\ publishes a literal's use after the call that follows it. No two start at one
\ byte: a definition's token declares its record alone, a use is its own token,
\ and an EXPORT's operand gives the use's token alone.
: SORTED ( -- )
   TOKEN-N @ 2 < if exit then
   0 ORDER TOKEN-N @ [: EARLIER? ;] SORT:SORT! ;

: TYPED ( n -- n n )
   {: class:n :}
   class CLASS-CONSTANT = if T-VARIABLE M-READONLY exit then
   class CLASS-STORAGE = if T-VARIABLE 0 exit then
   T-FUNCTION 0 ;

\ Lines from the previous token's line; characters from its start on the same
\ line, else from the line's start (UTF-16 units, LSP-TEXT). A token lies on one
\ line, since the check binds nothing in a literal its line leaves open, so its
\ length is its end's character less its start's.
: ENCODED ( ptr JSON-WRITE:writer n bool -- ptr JSON-WRITE:writer )
   {: k:n first:bool :}
   first 0= if COMMA then
   k K-START TOKEN@ {: from:n :}
   from LSP-TEXT:LINE-CHARACTER {: ln:n ch:n :}
   k K-END TOKEN@ LSP-TEXT:LINE-CHARACTER nip {: end:n :}
   ln PREV-LINE @ - U COMMA
   ln PREV-LINE @ = if ch PREV-CHAR @ - else ch then U COMMA
   end ch - U COMMA
   k K-CLASS TOKEN@ TYPED {: ty:n mods:n :}
   ty U COMMA mods U
   ln PREV-LINE !
   ch PREV-CHAR ! ;

public

: LEGEND ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   s" semanticTokensProvider" KEY OBJECT-START
      s" legend" KEY OBJECT-START
         s" tokenTypes" KEY ARRAY-START
            s" function" STRING COMMA
            s" variable" STRING
         ARRAY-END COMMA
         s" tokenModifiers" KEY ARRAY-START
            s" readonly" STRING
         ARRAY-END
      OBJECT-END COMMA
      s" full" true FIELD-BOOL
   OBJECT-END ;

\ From the last completed check whatever its verdict, while its positions are
\ of the text: a refused or deferred check stopped at a point, and what it
\ reported before is still what it bound. Otherwise, as while a check of the
\ document runs, the list is empty. Positions count in the text from now on.
: ANSWER ( ptr JSON-WRITE:writer n -- ptr JSON-WRITE:writer )
   {: w slot:n :}
   slot LSP-DOCS:DOC-TEXT$ {: t:ptr tu:n :}
   t tu LSP-TEXT:TEXT!
   0 TOKEN-N !
   slot DEFS-OWN-GROUP MATCH option
      some OF DECLARED ENDOF
      none OF ENDOF
   ;MATCH
   slot DEFS-USE-RANGE ?do t tu i USED loop
   SORTED
   0 PREV-LINE !
   0 PREV-CHAR !
   w OBJECT-START
   s" data" KEY ARRAY-START
   TOKEN-N @ 0 ?do i ORDER @ i 0= ENCODED loop
   ARRAY-END
   OBJECT-END ;

;using
;using
;package
