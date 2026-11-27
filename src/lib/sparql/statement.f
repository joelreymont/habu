\ statement.f - the statement's records: a rel is a statement, its WITH's
\ CTEs and its text, which is its pieces in order, then its open text; a
\ piece is one string of that text, held as given, not copied, then a
\ child rel when it has one, whose WITH and text it stands for; the open
\ text is the bytes copied in since the last piece, which the next piece
\ takes as its string; a cte is one WITH entry, its name and body, a rel
\ that reads only the CTEs before it in the list; a ctes is a list of CTEs
\ in written order. A recursive rel's WITH is WITH RECURSIVE. A rel's
\ copies are the hidden bytes behind the text copied into it, open or since
\ turned into pieces: those of the expressions PostgreSQL copies in for the
\ derived tables' columns that text reads.
\ A statement's texts hold every rel's copied bytes: a rel's open text is
\ a range of a chunk, which grows in place while it ends at the chunk's
\ used end and fits, else goes on at the used end: copied there when no
\ longer than a text piece, else ended as one. Bytes once written never
\ move or change, so a leaf's text holds for the whole compile.
\ Records are made one by one and never move.
\ STORAGE CLASS. CALLER-OWNED: every record lives in the allocator its
\ caller passes.

require src/lib/allocator.f

package SPARQL
private

MUTUAL piece cte rel texts
STRUCTURE piece DERIVE addr
   FIELD text str
   FIELD rel ^rel  \ NULL-PTR for none
   FIELD next ^piece  \ NULL-PTR at the end of its list
;STRUCTURE
STRUCTURE cte DERIVE addr
   FIELD name str
   FIELD materialized bool
   FIELD body ^rel
   FIELD next ^cte
;STRUCTURE
\ A rel's open text is len bytes from start in a chunk of its texts, none
\ when len is 0.
STRUCTURE rel DERIVE addr
   FIELD with ^cte  \ NULL-PTR for no CTE
   FIELD recursive bool
   FIELD first ^piece  \ NULL-PTR for no piece, as last is
   FIELD last ^piece
   FIELD start ^u8
   FIELD len n
   FIELD texts ^texts
   FIELD copies n
;STRUCTURE
\ A texts' chunk is size bytes from base, used of them written.
STRUCTURE texts DERIVE addr
   FIELD al ^ALLOC:allocator  \ the chunks and every record of the statement
   FIELD base ^u8
   FIELD used n
   FIELD size n
   \ The rel whose open text ends at the used end, NULL-PTR for none.
   FIELD last ^rel
;STRUCTURE
;MUTUAL

\ first and last are NULL-PTR when the list is empty.
STRUCTURE ctes DERIVE addr
   FIELD first ^cte
   FIELD last ^cte
;STRUCTURE

\ The bytes of a chunk, unless one text needs more.
4096 constant CHUNK-BYTES

\ Texts with no chunk yet.
: NEW-TEXTS ( ^ALLOC:allocator -- ^texts )
   {: al:^ :}
   al [type] texts ALLOC:NEW {: ts:^ :}
   al ts TEXTS-AL ! NULL-PTR ts TEXTS-BASE ! 0 ts TEXTS-USED !
   0 ts TEXTS-SIZE ! NULL-PTR ts TEXTS-LAST ! ts ;

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

\ A rel of the texts with no CTE, no text and no copies, not recursive.
: NEW-REL ( ^texts -- ^rel )
   {: ts:^ :}
   ts TEXTS-AL @ [type] rel ALLOC:NEW {: r:^ :}
   NULL-PTR r REL-WITH ! false r REL-RECURSIVE !
   NULL-PTR r REL-FIRST ! NULL-PTR r REL-LAST !
   NULL-PTR r REL-START ! 0 r REL-LEN ! ts r REL-TEXTS !
   0 r REL-COPIES ! r ;

: OPEN? ( ^rel -- bool ) REL-LEN @ 0<> ;

: OPEN$ ( ^rel -- str )
   {: r:^ :} r REL-START @ r REL-LEN @ SPAN:MAKE SPAN:$ ;

: LINK-PIECE ( ^rel str ^rel -- )
   {: r:^ t:str c:^ :}
   r REL-TEXTS @ TEXTS-AL @ [type] piece ALLOC:NEW {: p:^ :}
   t p PIECE-TEXT ! c p PIECE-REL ! NULL-PTR p PIECE-NEXT !
   r REL-LAST @ {: last:^ :}
   last NULL-PTR = if p r REL-FIRST ! else p last PIECE-NEXT ! then
   p r REL-LAST ! ;

\ Leaves the rel with no open text, so its next byte starts a new range.
: CLEAR-OPEN ( ^rel -- )
   {: r:^ :}
   0 r REL-LEN ! r REL-TEXTS @ {: ts:^ :}
   ts TEXTS-LAST @ r = if NULL-PTR ts TEXTS-LAST ! then ;

\ Links one piece of the rel's open text and the child, NULL-PTR for
\ none, and leaves no open text; no piece when both are empty.
: END-OPEN ( ^rel ^rel -- )
   {: r:^ c:^ :}
   r OPEN? if r r OPEN$ c LINK-PIECE r CLEAR-OPEN exit then
   c NULL-PTR <> if r s" " c LINK-PIECE then ;

: USED-END ( ^texts -- ^u8 )
   {: ts:^ :} ts TEXTS-BASE @ ts TEXTS-USED @ + ;

: ROOM? ( ^texts n -- bool )
   {: ts:^ k:n :} ts TEXTS-USED @ k + ts TEXTS-SIZE @ <= ;

\ A chunk of n bytes or CHUNK-BYTES, whichever is more, the old one
\ left as written.
: NEW-CHUNK ( ^texts n -- )
   {: ts:^ k:n :}
   ts TEXTS-AL @ k CHUNK-BYTES max ALLOC:BYTES
   ts TEXTS-SIZE ! ts TEXTS-BASE ! 0 ts TEXTS-USED ! ;

\ Whether the rel's open text takes no more bytes than the text piece
\ it would end as.
: SHORT? ( ^rel -- bool ) REL-LEN @ [type] piece width>n <= ;

\ Copies the rel's open text to the used end, in a new chunk when it and
\ n more bytes do not fit, and goes on there; its old bytes stay unused.
: COPY-OPEN ( ^rel n -- )
   {: r:^ k:n :}
   r REL-TEXTS @ {: ts:^ :} r REL-LEN @ {: m:n :}
   ts m k + ROOM? 0= if ts m k + NEW-CHUNK then
   r REL-START @ ts USED-END m BYTE-COPY
   ts USED-END r REL-START ! m ts TEXTS-USED +! r ts TEXTS-LAST ! ;

\ Goes on with the rel's open text at the used end, room for n more bytes.
: MOVE-OPEN ( ^rel n -- )
   {: r:^ k:n :}
   r SHORT? 0= if r NULL-PTR END-OPEN then r k COPY-OPEN ;

\ Answers n new bytes, zero, at the end of the rel's open text.
: OPEN-BYTES ( ^rel n -- ^u8 )
   {: r:^ k:n :}
   r REL-TEXTS @ {: ts:^ :}
   ts TEXTS-LAST @ r = ts k ROOM? and 0= if r k MOVE-OPEN then
   ts USED-END k ts TEXTS-USED +! k r REL-LEN +! ;

\ Copies the bytes onto the end of the rel's open text; none leaves it
\ as it is.
: +SQL ( ^rel str -- )
   {: r:^ s:str :}
   s STR:LEN 0= if exit then
   s STR:DATA r s STR:LEN OPEN-BYTES s STR:LEN BYTE-COPY ;

\ Answers a new last byte of the rel's open text, zero.
: +BYTE ( ^rel -- ^u8 ) 1 OPEN-BYTES ;

\ Appends the text to the rel's. The piece holds the string, which must
\ outlive the write: a literal, arena bytes, or the bytes of a vector
\ nothing appends to afterwards, since an append may move its block.
: +TEXT ( ^rel str -- )
   {: r:^ t:str :}
   r NULL-PTR END-OPEN r t NULL-PTR LINK-PIECE ;

\ Appends the child rel, its WITH and text, to the rel's pieces.
: +REL ( ^rel ^rel -- ) END-OPEN ;

\ Whether the rel is only its open text: no piece and no WITH.
: LEAF? ( ^rel -- bool )
   {: r:^ :} r REL-FIRST @ NULL-PTR = r REL-WITH @ NULL-PTR = and ;

\ Whether the rel's open text takes more bytes than two pieces, the most
\ linking it as a text piece makes.
: LONG? ( ^rel -- bool ) REL-LEN @ [type] piece width>n 2 * > ;

\ Appends the child's text: a leaf's open text, with its copies, copied
\ unless LONG?, else as a text piece of its bytes, which never move; any
\ other rel as a piece, which nothing may append to afterwards.
: +SPLICE ( ^rel ^rel -- )
   {: r:^ c:^ :}
   c LEAF? 0= if r c +REL exit then
   c REL-COPIES @ r REL-COPIES +!
   c LONG? if r c OPEN$ +TEXT exit then
   c OPEN? if r c OPEN$ +SQL then ;

: EMPTY? ( ^rel -- bool )
   {: r:^ :} r REL-FIRST @ NULL-PTR = r OPEN? 0= and ;

\ Appends the text unless the rel is empty.
: +SEP ( ^rel str -- )
   {: r:^ t:str :}
   r EMPTY? 0= if r t +SQL then ;

: TEXT-REL ( ^texts str -- ^rel )
   {: ts:^ t:str :}
   ts NEW-REL {: r:^ :} r t +TEXT r ;

\ A rel of the rel's text so far and its copies, its open text ended as a
\ piece; text appended later is not in it: a piece changes only in its next.
: VIEW ( ^rel -- ^rel )
   {: r:^ :}
   r NULL-PTR END-OPEN r REL-TEXTS @ NEW-REL {: v:^ :}
   r REL-FIRST @ v REL-FIRST ! r REL-LAST @ v REL-LAST !
   r REL-COPIES @ v REL-COPIES ! v ;

;package
