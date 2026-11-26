\ statement.f - the statement's records: a rel is a statement, its WITH's
\ CTEs and its text, which is its pieces in order, then its open text; a
\ piece is one string of that text, held as given, not copied, or a child
\ rel, whose WITH and text it stands for; the open text is the bytes copied
\ in since the last piece, which the next piece turns into a text piece; a
\ cte is one WITH entry, its name and body, a rel that reads only the CTEs
\ before it in the list; a ctes is a list of CTEs in written order. A
\ recursive rel's WITH is WITH RECURSIVE.
\ Records are made one by one and never move.
\ STORAGE CLASS. CALLER-OWNED: every record lives in the allocator its
\ caller passes.

require src/lib/allocator.f
require src/lib/vector.f

package SPARQL
private

\ A piece's rel is NULL-PTR for a text piece. A next is NULL-PTR at the
\ end of its list; a rel's with is NULL-PTR for no CTE, its first and
\ last NULL-PTR for no piece, its open NULL-PTR for no open text, and al
\ is the allocator its pieces and open text come from.
MUTUAL piece cte rel
STRUCTURE piece DERIVE addr
   FIELD text str
   FIELD rel ^rel
   FIELD next ^piece
;STRUCTURE
STRUCTURE cte DERIVE addr
   FIELD name str
   FIELD materialized bool
   FIELD body ^rel
   FIELD next ^cte
;STRUCTURE
STRUCTURE rel DERIVE addr
   FIELD with ^cte
   FIELD recursive bool
   FIELD first ^piece
   FIELD last ^piece
   FIELD open ^VEC:vector<u8>
   FIELD al ^ALLOC:allocator
;STRUCTURE
;MUTUAL

\ first and last are NULL-PTR when the list is empty.
STRUCTURE ctes DERIVE addr
   FIELD first ^cte
   FIELD last ^cte
;STRUCTURE

\ A CTE that is the last of its list.
: NEW-CTE ( ^ALLOC:allocator str bool ^rel -- ^cte )
   {: al:^ name:str mat:bool body:^ :}
   al [type] cte ALLOC:NEW {: c:^ :}
   name c CTE-NAME ! mat c CTE-MATERIALIZED !
   body c CTE-BODY ! NULL-PTR c CTE-NEXT ! c ;

: NEW-CTES ( ^ALLOC:allocator -- ^ctes )
   {: al:^ :}
   al [type] ctes ALLOC:NEW {: list:^ :}
   NULL-PTR list CTES-FIRST ! NULL-PTR list CTES-LAST ! list ;

\ Appends a CTE of the name, materialized flag and body to the list.
: +CTE ( ^ALLOC:allocator ^ctes str bool ^rel -- )
   {: al:^ list:^ name:str mat:bool body:^ :}
   al name mat body NEW-CTE {: c:^ :}
   list CTES-LAST @ {: last:^ :}
   last NULL-PTR = if c list CTES-FIRST ! else c last CTE-NEXT ! then
   c list CTES-LAST ! ;

\ A rel with no CTE and no text, not recursive.
: NEW-REL ( ^ALLOC:allocator -- ^rel )
   {: al:^ :}
   al [type] rel ALLOC:NEW {: r:^ :}
   NULL-PTR r REL-WITH ! false r REL-RECURSIVE !
   NULL-PTR r REL-FIRST ! NULL-PTR r REL-LAST !
   NULL-PTR r REL-OPEN ! al r REL-AL ! r ;

: OPEN? ( ^rel -- bool )
   REL-OPEN @ dup NULL-PTR = if drop false exit then
   VEC:COUNT 0<> ;

: LINK-PIECE ( ^rel str ^rel -- )
   {: r:^ t:str c:^ :}
   r REL-AL @ [type] piece ALLOC:NEW {: p:^ :}
   t p PIECE-TEXT ! c p PIECE-REL ! NULL-PTR p PIECE-NEXT !
   r REL-LAST @ {: last:^ :}
   last NULL-PTR = if p r REL-FIRST ! else p last PIECE-NEXT ! then
   p r REL-LAST ! ;

\ Turns non-empty open text into a text piece of its bytes, which nothing
\ appends to again, so they hold.
: END-OPEN ( ^rel -- )
   {: r:^ :}
   r OPEN? 0= if exit then
   r r REL-OPEN @ VEC:BYTES NULL-PTR LINK-PIECE NULL-PTR r REL-OPEN ! ;

\ Appends a piece of the text and child rel to the rel's pieces, after
\ its open text.
: +PIECE ( ^rel str ^rel -- )
   {: r:^ t:str c:^ :}
   r END-OPEN r t c LINK-PIECE ;

\ The rel's open text, made from its al when it has none.
: OPEN ( ^rel -- ^VEC:vector<u8> )
   {: r:^ :}
   r REL-OPEN @ NULL-PTR = if
      r REL-AL @ 0 [type] u8 VEC:NEW r REL-OPEN !
   then r REL-OPEN @ ;

\ Copies the bytes onto the rel's open text.
: +SQL ( ^rel str -- ) {: r:^ s:str :} s r OPEN VEC:APPEND ;

\ Answers a new last byte of the rel's open text, zero.
: +BYTE ( ^rel -- ^u8 ) OPEN VEC:PUSH ;

\ Appends the text to the rel's. The piece holds the string, which must
\ outlive the write: a literal, arena bytes, or the bytes of a vector
\ nothing appends to afterwards, since an append may move its block.
: +TEXT ( ^rel str -- ) NULL-PTR +PIECE ;

\ Appends the child rel, its WITH and text, to the rel's pieces.
: +REL ( ^rel ^rel -- )
   {: r:^ c:^ :}
   r s" " c +PIECE ;

: EMPTY? ( ^rel -- bool )
   {: r:^ :} r REL-FIRST @ NULL-PTR = r OPEN? 0= and ;

\ Appends the text unless the rel is empty.
: +SEP ( ^rel str -- )
   {: r:^ t:str :}
   r EMPTY? 0= if r t +SQL then ;

: TEXT-REL ( ^ALLOC:allocator str -- ^rel )
   {: al:^ t:str :}
   al NEW-REL {: r:^ :} r t +TEXT r ;

\ A rel of the rel's text so far, its open text turned into a piece: text
\ appended later is not in it. A piece never changes, but for its next.
: VIEW ( ^rel -- ^rel )
   {: r:^ :}
   r END-OPEN r REL-AL @ NEW-REL {: v:^ :}
   r REL-FIRST @ v REL-FIRST ! r REL-LAST @ v REL-LAST ! v ;

;package
