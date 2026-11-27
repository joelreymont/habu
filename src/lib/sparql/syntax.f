\ Caller-owned syntax. Node zero is absent; lists are cons nodes so a
\ shared RDF term has no mutable sibling link. The caller owns the arena.
\ A rejection reads `L:C: expected X, found Y; FIX`, then `L | ` and line L
\ of the query as written, then a caret under column C. L and C count from 1,
\ C in bytes. A line over 80 bytes shows 40 bytes each side of C, `...` at
\ each cut end; the caret's padding is the visible width of what precedes it.
\ A fix removes the mistake it names: applied as written, the query parses
\ there or is rejected later for a mistake of its own. Later is, by a later
\ stage, anywhere; by the same stage or an earlier one (lexer, parser,
\ validator), past the found token the fix keeps, or at it when it ends a
\ construct (the end of the query, `}`, `)` or `]`) and the fix wrote only
\ before it, or at a closer the fix wrote when it wrote nothing else; not
\ before where it stood when the fix removes or replaces it. A
\ fix replaces, removes or moves the found token or tokens it names; writes
\ before it, or a variable into GROUP BY; writes the closer of a construct the
\ found token cannot continue, unless one of its own closers follows and the
\ found token starts no pattern, or moves a data block's } there when its own
\ } follows; or puts a condition or a row in parentheses, or the triples or
\ pattern before a UNION in braces. Found whole: a
\ malformed escape, blank node label or language tag, a nameless `?` or `$`,
\ a line break in a one-quote string and a prefixed name whose prefix ends
\ in `.`; an unclosed string, at its line's first `}`, `)` or `]` after its
\ quote or at its end.

require src/lib/arena.f
require src/lib/vector.f
require src/lib/string.f
require src/lib/entropy.f
require src/lib/siphash.f
require src/lib/reason.f
require src/lib/utf8-scalar.f
require src/lib/unicode/visible.f
require src/core/rejection.f
require src/lib/fmt.f
require src/lib/sparql/segs.f

package SPARQL

private

-10000 constant E-FIRST
-10099 constant E-LAST
-10001 s" rejected SPARQL query" ERROR:CODE E-QUERY

\ The reads, items, aliases and variables, a statement may have.
\ PostgreSQL's planner memory grows with them and no setting of its own
\ bounds it: a 4 KB query of nested OPTIONALs took 8 GB.
512 constant MOST

\ What a rejection past MOST expects, and its fix.
: MOST-TEXT ( -- str str )
   s" at most 512 patterns, sources they match and variables"
   s" split the query, or state predicates so that fewer sources match" ;

1 constant N-IRI
2 constant N-LITERAL
3 constant N-VAR
4 constant N-BNODE
5 constant N-UNDEF
6 constant N-CALL
7 constant N-UNARY
8 constant N-BINARY
9 constant N-LIST
10 constant N-TRIPLE
11 constant N-GROUP
12 constant N-OPTIONAL
13 constant N-UNION
14 constant N-MINUS
15 constant N-GRAPH
16 constant N-PREFIX
17 constant N-FILTER
18 constant N-BIND
19 constant N-VALUES
20 constant N-SELECT
21 constant N-ASK
24 constant N-PATH
25 constant N-ALIAS
27 constant N-ORDER
28 constant N-GROUPBY
29 constant N-HAVING
30 constant N-LIMIT
31 constant N-OFFSET
32 constant N-QUERY-INFO

1 constant F-DISTINCT
4 constant F-STAR
32 constant F-DESC

\ A term (IRI, literal, variable, blank node, UNDEF) has no parts; its
\ fields hold its head, datatype, language and number. Any other node's
\ a, b, c and d hold node indexes or 0, and value its other number.
\ A node is three cells: a packed cell, then a and b. The packed cell
\ holds, from bit 0, the decoded offset of the token the node was read
\ at (32 bits), its text's length (11) and start less that offset (6,
\ signed), a bit for a side text, a bit for extra cells, flags (7) and
\ kind (6). A text that is no slice of the decoded source, or does not
\ fit, is a side text. Cells a and b hold a kind's first two fields
\ below; once a later field or a side text is set, b holds the index of
\ the node's extra cells: the second field, the later ones in order,
\ then the side text's index.
\   IRI: c d value              literal: a value b
\   variable, blank node, prefix, LIMIT, OFFSET: value
\   call, triple: a b c         list: a b value d
\   unary, binary, path: a b    SELECT, ASK: a b d value
\   query info: a b c value     every other kind: a b c d value
STRUCTURE node DERIVE addr
   FIELD packed n
   FIELD a n
   FIELD b n
;STRUCTURE

\ Each packed field's first bit and width.
: OFFSET-BITS ( -- n n ) 0 32 ;
: LENGTH-BITS ( -- n n ) 32 11 ;
: DELTA-BITS ( -- n n ) 43 6 ;
: SIDE-BITS ( -- n n ) 49 1 ;
: EXTRA-BITS ( -- n n ) 50 1 ;
: FLAGS-BITS ( -- n n ) 51 7 ;
: KIND-BITS ( -- n n ) 58 6 ;

0 constant PART-A
1 constant PART-B
2 constant PART-C
3 constant PART-D
4 constant PART-VALUE

\ Where a kind keeps a field: nowhere, cell a, cell b or else the first
\ extra cell, or the next extra cells.
0 constant IN-NONE
1 constant IN-A
2 constant IN-B
3 constant IN-X1
4 constant IN-X2
5 constant IN-X3

\ Per kind, the place of each field in 4 bits from field a's, and from
\ bit 20 how many extra cells its fields take.
N-QUERY-INFO 1+ TYPED-BUFFER LAYOUTS n

: LAYOUT! ( n n n n n n -- )
   {: kind:n a:n b:n c:n d:n value:n :}
   a b max c max d max value max 1- 0 max 20 lshift
   value 16 lshift or d 12 lshift or c 8 lshift or
   b 4 lshift or a or kind LAYOUTS ! ;

: VALUE-ONLY ( n -- )
   IN-NONE IN-NONE IN-NONE IN-NONE IN-A LAYOUT! ;

: INSTALL-LAYOUTS ( -- )
   N-QUERY-INFO 1+ 1 ?do i IN-A IN-B IN-X1 IN-X2 IN-X3 LAYOUT! loop
   N-IRI IN-NONE IN-NONE IN-A IN-B IN-X1 LAYOUT!
   N-LITERAL IN-A IN-X1 IN-NONE IN-NONE IN-B LAYOUT!
   N-VAR VALUE-ONLY N-BNODE VALUE-ONLY N-PREFIX VALUE-ONLY
   N-LIMIT VALUE-ONLY N-OFFSET VALUE-ONLY
   N-CALL IN-A IN-B IN-X1 IN-NONE IN-NONE LAYOUT!
   N-TRIPLE IN-A IN-B IN-X1 IN-NONE IN-NONE LAYOUT!
   N-LIST IN-A IN-B IN-NONE IN-X2 IN-X1 LAYOUT!
   N-UNARY IN-A IN-B IN-NONE IN-NONE IN-NONE LAYOUT!
   N-BINARY IN-A IN-B IN-NONE IN-NONE IN-NONE LAYOUT!
   N-PATH IN-A IN-B IN-NONE IN-NONE IN-NONE LAYOUT!
   N-SELECT IN-A IN-B IN-NONE IN-X1 IN-X2 LAYOUT!
   N-ASK IN-A IN-B IN-NONE IN-X1 IN-X2 LAYOUT!
   N-QUERY-INFO IN-A IN-B IN-X1 IN-NONE IN-X2 LAYOUT! ;

INSTALL-LAYOUTS

STRUCTURE token DERIVE addr
   FIELD kind n
   FIELD text str
;STRUCTURE

\ Where an escape ends, in the decoded query and in the query as written.
STRUCTURE escape DERIVE addr
   FIELD decoded n
   FIELD original n
;STRUCTURE

STRUCTURE iri DERIVE addr
   FIELD scheme str
   FIELD authority str
   FIELD path str
   FIELD query str
   FIELD fragment str
   FIELD flags n
;STRUCTURE

\ Node indexes by key, open addressed with linear probing: slot i is a
\ four-byte node index, 0 for none. The slots double past half load,
\ each node hashed again; the old ones stay in the arena.
STRUCTURE interns DERIVE addr
   FIELD slots ^u8
   FIELD mask n
   FIELD count n
   FIELD k0 n
   FIELD k1 n
   FIELD arena ^ARENA:arena
   FIELD sip SIPHASH:state
;STRUCTURE

\ The parser reads the tokens before, at and after the cursor, which is the
\ current token's decoded offset; a token of kind 0 is the end.
\ Names holds the first variable, prefix and blank node label node of each
\ name. That node's value is a variable's number, from 1 in order of first
\ use, a prefix's IRI node, or the group a label was first used in. The
\ declared base is the BASE IRI's node, 0 for none; base and directory are
\ set with it. No name from position to word-end has a colon.
STRUCTURE syntax DERIVE addr
   FIELD arena ^ARENA:arena
   FIELD work ^ARENA:arena  \ a child of arena that frames cut back
   FIELD nodes ^segs<node>
   FIELD extras ^segs<n>  \ the nodes' extra cells
   FIELD side ^segs<str>  \ the side texts
   FIELD previous token
   FIELD current token
   FIELD next token
   FIELD source str
   FIELD original str
   FIELD escapes ^VEC:vector<escape>
   FIELD position n
   FIELD word-end n
   FIELD cursor n
   FIELD root n
   FIELD variables n  \ how many variables there are
   FIELD declared-base n
   FIELD base ^iri
   FIELD directory str
   FIELD serial n
   FIELD group n
   FIELD nesting n
   FIELD patterns n  \ the triple patterns read; MOST bounds them
   FIELD interns interns  \ the IRI and literal nodes by key
   FIELD names interns
;STRUCTURE

\ The offset in the query as written of decoded offset n: the last escape
\ ending at or before n fixes the distance between the two.
: WRITTEN ( ^syntax n -- n )
   {: state at:n :}
   state SYNTAX-ESCAPES @ {: shifts :}
   0 0 0 begin
      dup shifts VEC:COUNT < if
         shifts over VEC:AT ESCAPE-DECODED @ at <=
      else false then
   while
      {: index:n :} 2drop
      shifts index VEC:AT {: pair :}
      pair ESCAPE-DECODED @ pair ESCAPE-ORIGINAL @ index 1+
   repeat drop {: from:n to:n :}
   at from - to + ;

\ The end of the line from offset n, before its LF or CRLF.
: LINE-END ( str n -- n )
   {: line:str start:n :}
   line str> {: text size:n :}
   start begin
      dup size < if text over + c@ 10 <> else false then
   while 1+ repeat {: end:n :}
   end size < end start > and if
      text end 1- + c@ 13 = if end 1- exit then
   then end ;

\ The part of the line from start to end shown around offset n, cut at
\ character starts.
: WINDOW ( str n n n -- n n )
   {: line:str start:n end:n at:n :}
   line str> drop {: text :}
   end start - 80 <= if start end exit then
   at 40 - start max begin
      dup at < if text over + c@ $C0 and $80 = else false then
   while 1+ repeat {: from:n :}
   text from + end from - >str at 40 + end min from -
      UTF8:FIT from + from swap ;

\ Appends n spaces and a caret.
: CARET ( n -- )
   0 ?do s"  " REASON:APPEND loop s" ^" REASON:APPEND ;

\ Appends line L of the query as written, which starts at the first offset,
\ and a caret under the second.
: SHOW-LINE ( ^syntax n n n -- )
   {: state line:n start:n at:n :}
   state SYNTAX-ORIGINAL @ {: original:str :}
   original str> drop {: text :}
   original start LINE-END {: end:n :}
   original start end at WINDOW {: from:n to:n :}
   s\" \n" REASON:APPEND
   line FMT:INT$ dup str> nip 3 + {: lead:n :} REASON:APPEND
   s"  | " REASON:APPEND
   from start > if s" ..." REASON:APPEND then
   text from + to from - >str REASON:VISIBLE
   to end < if s" ..." REASON:APPEND then
   s\" \n" REASON:APPEND
   from start > if 3 else 0 then lead +
   text from + at from - >str UNICODE-VISIBLE:LEN + CARET ;

\ The text as far as a rejection shows it, and whether that cuts it.
: SHORTEN ( str -- str bool )
   {: whole:str :}
   whole str> {: text size:n :}
   whole REASON:QUOTE-MOST UTF8:FIT {: shown:n :}
   text shown >str shown size < ;

\ Appends what was found, quoted; empty is the end of the query.
: FOUND-TEXT ( str -- )
   {: found:str :}
   found str> nip 0= if
      s" the end of the query" REASON:APPEND exit
   then
   found SHORTEN REASON:QUOTE ;

\ Throws E-QUERY at decoded offset n with what was expected, what was found
\ (empty for the end of the query) and the fix.
: REJECTED ( ^syntax n str str str -- )
   {: state at:n x:str y:str f:str :}
   state at WRITTEN {: spot:n :}
   state SYNTAX-ORIGINAL @ str> drop {: text :}
   text text spot + 1 1 0 REJECTION:POSITION drop
      {: line:n col:n :}
   REASON:OPEN line col REASON:AT
   REASON:EXPECTED x REASON:APPEND
   REASON:FOUND y FOUND-TEXT
   REASON:FIX f REASON:APPEND
   state line spot col 1- - spot SHOW-LINE
   E-QUERY REASON:STOP ;

\ The text of the token after the parser's current one, empty past the
\ last.
: NEXT-TOKEN$ ( ^syntax -- str )
   SYNTAX-NEXT TOKEN-TEXT @ ;

\ Rejects at the lexer's position, found the character there.
: REJECT-BYTE ( ^syntax str str -- )
   {: state x:str f:str :}
   state SYNTAX-SOURCE @ {: source:str :}
   source str> {: text size:n :}
   state SYNTAX-POSITION @ {: at:n :}
   at size >= if 0 else source at UTF8:SUBPART-END at - then
      {: wide:n :}
   state at x text at + wide >str f REJECTED ;

\ Rejects at byte n, found the bytes from n to the lexer's position.
: REJECT-SPAN ( ^syntax n str str -- )
   {: state at:n x:str f:str :}
   state SYNTAX-SOURCE @ str> drop {: text :}
   state SYNTAX-POSITION @ at - {: wide:n :}
   state at x text at + wide >str f REJECTED ;

: NODE ( ^syntax n -- ^node )
   swap SYNTAX-NODES @ swap SEG-AT ;

: PACKED@ ( ^node n n -- n )
   {: node at:n width:n :}
   node NODE-PACKED @ at rshift 1 width lshift 1- and ;

: PACKED! ( ^node n n n -- )
   {: node value:n at:n width:n :}
   1 width lshift 1- at lshift {: mask:n :}
   node NODE-PACKED @ mask invert and
   value at lshift mask and or node NODE-PACKED ! ;

: SIDE? ( ^node -- bool ) SIDE-BITS PACKED@ 0<> ;
: EXTRA? ( ^node -- bool ) EXTRA-BITS PACKED@ 0<> ;

\ The text's start less the offset, sign extended.
: DELTA@ ( ^node -- n )
   DELTA-BITS PACKED@ dup 1 DELTA-BITS nip 1- lshift >= if
      1 DELTA-BITS nip lshift -
   then ;

: NODE-K ( ^syntax n -- n ) NODE KIND-BITS PACKED@ ;

: LAYOUT ( ^syntax n -- n ) NODE-K LAYOUTS @ ;

\ Where node n's kind keeps the field.
: PLACE-OF ( ^syntax n n -- n )
   {: state index:n part:n :}
   state index LAYOUT part 4 * rshift 15 and ;

\ How many extra cells node n's fields take.
: FIELD-CELLS ( ^syntax n -- n ) LAYOUT 20 rshift ;

\ Extra cell n of the node.
: EXTRA ( ^syntax ^node n -- ^n )
   {: state node slot:n :}
   state SYNTAX-EXTRAS @ node NODE-B @ slot + SEG-AT ;

: PART@ ( ^syntax n n -- n )
   {: state index:n part:n :}
   state index NODE {: node :}
   state index part PLACE-OF {: place:n :}
   place IN-NONE = if 0 exit then
   place IN-A = if node NODE-A @ exit then
   node EXTRA? if state node place IN-B - EXTRA @ exit then
   place IN-B = if node NODE-B @ else 0 then ;

\ Gives node n n new extra cells holding its fields' extra cells, or
\ else its cell b.
: MOVE-EXTRAS ( ^syntax n n -- )
   {: state index:n size:n :}
   state index NODE {: node :}
   state SYNTAX-EXTRAS @ {: cells:^ :}
   cells SEG-COUNT {: at:n :}
   cells at size + SEG-ENSURE
   node EXTRA? if
      state index FIELD-CELLS 0 ?do
         state node i EXTRA @ cells at i + SEG-AT !
      loop
   else node NODE-B @ cells at SEG-AT ! then
   at node NODE-B ! node 1 EXTRA-BITS PACKED! ;

\ A kind keeps only the fields its row in INSTALL-LAYOUTS names: a write
\ to any other field is lost, so code that sets a new field of a kind
\ names it in that row first.
: PART! ( ^syntax n n n -- )
   {: state index:n part:n value:n :}
   state index NODE {: node :}
   state index part PLACE-OF {: place:n :}
   place IN-NONE = if exit then
   place IN-A = if value node NODE-A ! exit then
   node EXTRA? 0= if
      place IN-B = if value node NODE-B ! exit then
      value 0= if exit then
      state index state index FIELD-CELLS MOVE-EXTRAS
   then value state node place IN-B - EXTRA ! ;

\ The node's side text's cell in the side array.
: SIDE ( ^syntax n -- ^str )
   {: state index:n :}
   state SYNTAX-SIDE @
   state state index NODE state index FIELD-CELLS EXTRA @
   SEG-AT ;

: TEXT$ ( ^syntax n -- str )
   {: state index:n :}
   state index NODE {: node :}
   node SIDE? if state index SIDE @ exit then
   state SYNTAX-SOURCE @ str> drop
   node OFFSET-BITS PACKED@ + node DELTA@ +
   node LENGTH-BITS PACKED@ >str ;

\ The text's start less the node's offset, and whether the node holds
\ the text as a slice of the decoded source.
: SLICE ( ^syntax ^node str -- n bool )
   {: state node text:str :}
   text str> {: start length:n :}
   length 0= if 0 true exit then
   state SYNTAX-SOURCE @ str> {: base size:n :}
   start base - {: at:n :}
   at node OFFSET-BITS PACKED@ - {: delta:n :}
   1 DELTA-BITS nip 1- lshift {: reach:n :}
   delta at 0 >= at length + size <= and
   delta reach negate >= and delta reach < and
   length 1 LENGTH-BITS nip lshift < and ;

: TEXT! ( ^syntax n str -- )
   {: state index:n text:str :}
   state index NODE {: node :}
   state SYNTAX-SIDE @ {: side:^ :}
   node SIDE? 0= if
      state node text SLICE if
         node swap DELTA-BITS PACKED!
         node text str> nip LENGTH-BITS PACKED! exit
      then drop
      state index FIELD-CELLS {: slot:n :}
      state index slot 1+ MOVE-EXTRAS
      side SEG-COUNT state node slot EXTRA !
      side SEG-PUSH drop node 1 SIDE-BITS PACKED!
   then text state index SIDE ! ;

: NODE-A@ ( ^syntax n -- n ) PART-A PART@ ;
: NODE-B@ ( ^syntax n -- n ) PART-B PART@ ;
: NODE-C@ ( ^syntax n -- n ) PART-C PART@ ;
: NODE-D@ ( ^syntax n -- n ) PART-D PART@ ;
: NODE-VALUE@ ( ^syntax n -- n ) PART-VALUE PART@ ;
: NODE-FLAGS@ ( ^syntax n -- n ) NODE FLAGS-BITS PACKED@ ;
: NODE-TOKEN@ ( ^syntax n -- n ) NODE OFFSET-BITS PACKED@ ;

: NODE-A! ( ^syntax n n -- ) PART-A swap PART! ;
: NODE-B! ( ^syntax n n -- ) PART-B swap PART! ;
: NODE-C! ( ^syntax n n -- ) PART-C swap PART! ;
: NODE-D! ( ^syntax n n -- ) PART-D swap PART! ;
: NODE-VALUE! ( ^syntax n n -- ) PART-VALUE swap PART! ;
: NODE-FLAGS! ( ^syntax n n -- )
   rot rot NODE swap FLAGS-BITS PACKED! ;

\ A slice text is held again from the new offset.
: NODE-TOKEN! ( ^syntax n n -- )
   {: state index:n at:n :}
   state index TEXT$ {: text:str :}
   state index NODE {: node :}
   node at OFFSET-BITS PACKED!
   node SIDE? 0= if state index text TEXT! then ;

\ The node's fields and text are placed again as the kind keeps them.
: NODE-KIND! ( ^syntax n n -- )
   {: state index:n kind:n :}
   state index NODE {: node :}
   state index TEXT$ {: text:str :}
   state index NODE-A@ {: a:n :}
   state index NODE-B@ {: b:n :}
   state index NODE-C@ {: c:n :}
   state index NODE-D@ {: d:n :}
   state index NODE-VALUE@ {: value:n :}
   node OFFSET-BITS PACKED@ {: at:n :}
   node FLAGS-BITS PACKED@ {: flags:n :}
   0 node NODE-PACKED ! 0 node NODE-A ! 0 node NODE-B !
   node kind KIND-BITS PACKED! node at OFFSET-BITS PACKED!
   node flags FLAGS-BITS PACKED!
   state index a NODE-A! state index b NODE-B!
   state index c NODE-C! state index d NODE-D!
   state index value NODE-VALUE! state index text TEXT! ;

: ADD-NODE ( ^syntax n -- n )
   {: state kind:n :}
   state SYNTAX-NODES @ SEG-COUNT {: index:n :}
   state SYNTAX-NODES @ SEG-PUSH {: node :}
   node kind KIND-BITS PACKED!
   node state SYNTAX-CURSOR @ OFFSET-BITS PACKED! index ;

\ Only query forms carry this node: trailing VALUES, aggregates and projected
\ names, read by validation and emission; the offset of a projection's `*`
\ token places its rejection.
: QUERY-INFO ( ^syntax n -- n ) NODE-VALUE@ ;

: NEW-QUERY ( ^syntax n -- n )
   {: state kind:n :}
   state kind ADD-NODE {: query:n :}
   state N-QUERY-INFO ADD-NODE {: info:n :}
   state query info NODE-VALUE! query ;

: QUERY-VALUES@ ( ^syntax n -- n ) over swap QUERY-INFO NODE-A@ ;
: QUERY-AGGREGATES ( ^syntax n -- n ) over swap QUERY-INFO NODE-B@ ;
: QUERY-NAMES ( ^syntax n -- n ) over swap QUERY-INFO NODE-C@ ;
: QUERY-STAR ( ^syntax n -- n ) over swap QUERY-INFO NODE-VALUE@ ;

: QUERY-VALUES! ( ^syntax n n -- )
   {: state query:n x:n :} state state query QUERY-INFO x NODE-A! ;
: QUERY-AGGREGATES! ( ^syntax n n -- )
   {: state query:n x:n :} state state query QUERY-INFO x NODE-B! ;
: QUERY-NAMES! ( ^syntax n n -- )
   {: state query:n x:n :} state state query QUERY-INFO x NODE-C! ;
: QUERY-STAR! ( ^syntax n n -- )
   {: state query:n x:n :}
   state state query QUERY-INFO x NODE-VALUE! ;

: TEXT-NODE ( ^syntax n str -- n )
   {: state kind:n text:str :}
   state kind ADD-NODE {: index:n :}
   state index text TEXT! index ;

\ Slot n of the slots at the address.
: ENTRY ( ^u8 n -- ^u8 )
   4 * + ;

\ The node index under the hash that the quotation finds equal, 0 for none.
: INTERNED ( ^interns n [ n -- bool ] -- n )
   {: table:^ hash:n same :}
   table INTERNS-SLOTS @ {: slots :}
   slots NULL-PTR = if 0 exit then
   table INTERNS-MASK @ {: mask:n :}
   hash mask and begin
      slots over ENTRY LE:U32@ dup 0<> if
         {: at:n index:n :}
         index same execute if index exit then
         at 1+ mask and true
      else drop false then
   while repeat drop 0 ;

: PLACE ( ^interns n n -- )
   {: table:^ hash:n index:n :}
   table INTERNS-SLOTS @ {: slots :}
   table INTERNS-MASK @ {: mask:n :}
   hash mask and begin
      slots over ENTRY LE:U32@ 0<>
   while 1+ mask and repeat
   index slots rot ENTRY LE:U32! ;

\ n slots in place of the table's, holding its nodes under the hashes
\ the quotation answers.
: RESIZE ( ^interns n [ n -- n ] -- )
   {: table:^ size:n hasher :}
   table INTERNS-SLOTS @ {: old :}
   table INTERNS-MASK @ 1+ {: before:n :}
   table INTERNS-ARENA @ size 4 * ARENA:ALLOC drop
      table INTERNS-SLOTS !
   size 1- table INTERNS-MASK !
   old NULL-PTR = if exit then
   before 0 ?do
      old i ENTRY LE:U32@ {: index:n :}
      index 0<> if table index hasher execute index PLACE then
   loop ;

\ Adds node index n under the hash the quotation answers for it; 16
\ slots first, doubled when one more node would fill past half.
: +INTERN ( ^interns n [ n -- n ] -- )
   {: table:^ index:n hasher :}
   table INTERNS-SLOTS @ NULL-PTR = if
      table 16 hasher RESIZE
   else
      table INTERNS-MASK @ 1+ {: size:n :}
      table INTERNS-COUNT @ 1+ 2 * size > if
         table size 2 * hasher RESIZE
      then
   then
   table index hasher execute index PLACE
   1 table INTERNS-COUNT +! ;

\ The SipHash state, keyed for the table and reset.
: INTERNS-HASH ( ^interns -- ^SIPHASH:state )
   {: table:^ :}
   table INTERNS-SIP {: sip :}
   sip table INTERNS-K0 @ table INTERNS-K1 @ SIPHASH:INIT sip ;

\ An empty table whose slots the arena holds.
: INTERNS-INIT ( ^interns ^ARENA:arena -- )
   {: table:^ arena :}
   arena table INTERNS-ARENA !
   ENTROPY:RANDOM table INTERNS-K0 !
   ENTROPY:RANDOM table INTERNS-K1 ! ;

: COPY$ ( ^syntax str -- str )
   {: state source:str :}
   source str> {: text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size
      ALLOC:BYTES {: out length:n :}
   text out size BYTE-COPY out length >str ;

: CONS ( ^syntax n n -- n )
   {: state item:n tail:n :}
   state N-LIST ADD-NODE {: index:n :}
   state index item NODE-A!
   state index tail NODE-B! index ;

: REVERSE ( ^syntax n -- n )
   {: state list:n :}
   0 list begin dup 0<> while
      {: result:n current:n :}
      state current NODE-B@ {: next:n :}
      state current result NODE-B!
      current next
   repeat drop ;

: LIST-LENGTH ( ^syntax n -- n )
   {: state list:n :}
   0 list begin dup 0<> while
      state swap NODE-B@ swap 1+ swap
   repeat drop ;

: UNARY-NODE ( ^syntax n n -- n )
   {: state kind:n operand:n :}
   state kind ADD-NODE {: index:n :}
   state index operand NODE-A! index ;

: BINARY-NODE ( ^syntax n n n -- n )
   {: state kind:n left:n right:n :}
   state kind left UNARY-NODE {: index:n :}
   state index right NODE-B! index ;

: SYNTAX-NEW ( str ^ARENA:arena -- ^syntax )
   {: text:str arena :}
   arena ARENA:ALLOCATOR [type] syntax ALLOC:NEW
      {: state :}
   arena state SYNTAX-ARENA !
   arena ARENA:NEW-IN state SYNTAX-WORK !
   state SYNTAX-INTERNS arena INTERNS-INIT
   state SYNTAX-NAMES arena INTERNS-INIT
   text state SYNTAX-SOURCE !
   text state SYNTAX-ORIGINAL !
   arena ARENA:ALLOCATOR 0 [type] escape VEC:NEW
      state SYNTAX-ESCAPES !
   arena ARENA:ALLOCATOR [type] n SEG-NEW state SYNTAX-EXTRAS !
   arena ARENA:ALLOCATOR [type] str SEG-NEW state SYNTAX-SIDE !
   arena ARENA:ALLOCATOR [type] node SEG-NEW {: nodes:^ :}
   nodes 1 SEG-ENSURE  nodes state SYNTAX-NODES ! state ;

;package
