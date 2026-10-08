\ lsp-tokens.f - the language server's semantic tokens: a token over each
\ word an open document's last completed check bound in it, typed by what the
\ check states the word is.
\
\ LEGEND writes the semanticTokensProvider capability: the token types and
\ modifiers whose indices and bits ANSWER writes, and that a request for a
\ whole document's tokens is answered. ANSWER writes the result of
\ textDocument/semanticTokens/full for an open document from its last
\ completed check, whatever that check's verdict, while that check's
\ positions are of the document's text: a refused or deferred check stopped
\ reporting at a point, and what it reported before is still what it bound.
\ While no completed check's positions are of the text, as once a check of the
\ document starts until it completes, the list is empty.
\
\ A token is over the token that declares a definition the check retained in
\ the document's own file (LSP-DEFS:DEFS-OWN-GROUP), when that token declares
\ no other (LSP-DEFS:GROUP-REC-SOLE): a DEFTYPE's name declares both its
\ converters and gets none. A token is also over each use the check bound in
\ the document, a string literal before a call of a word `names:` states
\ among them, typed by the record of its declaring token that its spelling
\ names (LSP-DEFS:GROUP-REC-NAMED), as hover and highlight select it; a use
\ that names no record gets none. Its type is the record's class: function
\ for a word, variable for storage, variable and readonly for a constant. An
\ export's record gets none, and so does a use bound to one: the verifier
\ states the export's own class, not the exported word's. A word the check
\ binds to no located declaration, as it binds an engine word, has no use and
\ so no token, and the client keeps its own coloring wherever none is.
\
\ The tokens are listed in the order they start in the text, as LSP encodes
\ them, each from the one before: the check publishes a literal's use after
\ the call that follows it, so ANSWER sorts them. No two start at one byte:
\ a definition's token declares its record alone and a use is its own token;
\ an EXPORT's operand, which declares the export and is a use of the word it
\ exports, gives the use's token alone.
\ Positions count lines and UTF-16 units of the document's text (LSP-TEXT). A
\ token is one name, or a literal's bytes, on one line, since the check binds
\ nothing in a literal its line leaves open: its length is the character of
\ its end less that of its start.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the tokens of an answer are gathered into
\ the module's buffers, which belong to the server's one task; the writer is
\ the caller's, and the uses and definitions are those tools/lsp-defs.f keeps.

require lib/json-write.f
require lib/sort.f
require lib/adt/option.f
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

\ A token from byte FROM to byte TO of the text, of a record of this class,
\ gathered unless the class is an export's.
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

\ The token of each record of group G whose token declares it alone.
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

\ The token of use U of the text T, when its spelling names a record.
: USED ( ptr u8 n n -- )
   {: t:ptr tu:n u:n :}
   u USE-GROUP u USE-TARGET t tu u USE-WORD$ GROUP-REC-NAMED MATCH option
      some OF {: r:n :} u USE-BYTES r REC-CLASS GATHER ENDOF
      none OF ENDOF
   ;MATCH ;

\ Whether token A starts before token B.
: EARLIER? ( n n -- bool )
   {: a:n b:n :}
   a K-START TOKEN@ b K-START TOKEN@ < ;

\ ORDER sorted by where each token starts.
: SORTED ( -- )
   TOKEN-N @ 2 < if exit then
   0 ORDER TOKEN-N @ [: EARLIER? ;] SORT:SORT! ;

\ The type and modifiers of a token of a record of this class, not an
\ export's.
: TYPED ( n -- n n )
   {: class:n :}
   class CLASS-CONSTANT = if T-VARIABLE M-READONLY exit then
   class CLASS-STORAGE = if T-VARIABLE 0 exit then
   T-FUNCTION 0 ;

\ Token K, written next as LSP encodes it, a comma before it unless it is
\ the first: the lines from the line of the token written before, the
\ characters from that token's start when on its line, else from the line's
\ start, its length in characters, its type and its modifiers.
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

\ Writes the semanticTokensProvider member of the server's capabilities to
\ the writer: the legend, and whole documents' tokens answered.
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

\ Writes the result of textDocument/semanticTokens/full for the document in
\ this slot, the tokens the header describes, to the writer. Positions count
\ in the document's text from now on.
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
