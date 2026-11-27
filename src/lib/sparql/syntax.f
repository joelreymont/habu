\ Caller-owned syntax. Node zero is absent; lists are cons nodes so a
\ shared RDF term has no mutable sibling link. The caller owns the arena.
\ A rejection reads `L:C: expected X, found Y; FIX`, then `L | ` and line L
\ of the query as written, then a caret under column C. L and C count from 1,
\ C in bytes. A line over 80 bytes shows 40 bytes each side of C, `...` at
\ each cut end; the caret's padding is the visible width of what precedes it.

require src/lib/arena.f
require src/lib/vector.f
require src/lib/string.f
require src/lib/map.f
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

\ A term (IRI, literal, variable, blank node, UNDEF) has no parts. Any other
\ node's a, b, c and d hold node indexes or 0, and value its other number.
STRUCTURE node DERIVE addr
   FIELD kind n
   FIELD flags n
   FIELD text str
   FIELD a n
   FIELD b n
   FIELD c n
   FIELD d n
   FIELD value n
   FIELD token n
;STRUCTURE

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

\ Node indexes by key, open addressed with linear probing: entry i is a
\ node index, 0 for none, then its key's hash, which regrowth reuses. The
\ entries double past half load; the old ones stay in the arena.
STRUCTURE interns DERIVE addr
   FIELD slots ^n
   FIELD mask n
   FIELD count n
   FIELD k0 n
   FIELD k1 n
   FIELD arena ^ARENA:arena
   FIELD sip SIPHASH:state
;STRUCTURE

\ Prefixes, variables and blank node labels are MAP maps: a prefix to its
\ IRI node, a variable's name to its number, from 1 in order of first use,
\ a label to the group it was first used in. The declared base is the BASE
\ IRI's node, 0 for none; base and directory are set with it. No name from
\ position to word-end has a colon.
STRUCTURE syntax DERIVE addr
   FIELD arena ^ARENA:arena
   FIELD nodes ^segs<node>
   FIELD tokens ^segs<token>
   FIELD source str
   FIELD original str
   FIELD escapes ^VEC:vector<escape>
   FIELD position n
   FIELD word-end n
   FIELD cursor n
   FIELD root n
   FIELD prefixes ^n
   FIELD variables ^n
   FIELD declared-base n
   FIELD base ^iri
   FIELD directory str
   FIELD serial n
   FIELD group n
   FIELD labels ^n
   FIELD nesting n
   FIELD terms n  \ the IRI and literal nodes made
   FIELD interns interns  \ the IRI and literal nodes by key
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

\ Rejects at token n, found its text; past the last token, at the end.
: REJECT-TOKEN ( ^syntax n str str -- )
   {: state index:n x:str f:str :}
   state SYNTAX-SOURCE @ str> {: source length:n :}
   state SYNTAX-TOKENS @ {: tokens :}
   index tokens SEG-COUNT < if
      tokens index SEG-AT {: token :}
      token TOKEN-TEXT @ {: y:str :}
      state y str> drop source - x y f REJECTED
   then
   state length x s" " f REJECTED ;

\ Rejects at the parser's current token.
: REJECT ( ^syntax str str -- )
   {: state x:str f:str :}
   state state SYNTAX-CURSOR @ x f REJECT-TOKEN ;

\ Rejects at the lexer's position, found the character there.
: REJECT-BYTE ( ^syntax str str -- )
   {: state x:str f:str :}
   state SYNTAX-SOURCE @ {: source:str :}
   source str> {: text size:n :}
   state SYNTAX-POSITION @ {: at:n :}
   at size >= if 0 else source at UTF8:SUBPART-END at - then
      {: wide:n :}
   state at x text at + wide >str f REJECTED ;

: NEST ( ^syntax -- )
   dup SYNTAX-NESTING @ 256 >= if
      s" at most 256 nested groups or expressions"
         s" nest fewer groups or expressions" REJECT
   then
   1 swap SYNTAX-NESTING +! ;

: UNNEST ( ^syntax -- ) -1 swap SYNTAX-NESTING +! ;

: NODE ( ^syntax n -- ^node )
   swap SYNTAX-NODES @ swap SEG-AT ;

\ Rejects at the token node n was read at.
: REJECT-NODE ( ^syntax n str str -- )
   {: state index:n x:str f:str :}
   state state index NODE NODE-TOKEN @ x f REJECT-TOKEN ;

: TEXT$ ( ^syntax n -- str )
   NODE NODE-TEXT @ ;

: NODE-K ( ^syntax n -- n ) NODE NODE-KIND @ ;
: NODE-A@ ( ^syntax n -- n ) NODE NODE-A @ ;
: NODE-B@ ( ^syntax n -- n ) NODE NODE-B @ ;
: NODE-C@ ( ^syntax n -- n ) NODE NODE-C @ ;
: NODE-D@ ( ^syntax n -- n ) NODE NODE-D @ ;

: ADD-NODE ( ^syntax n -- n )
   {: state kind:n :}
   state SYNTAX-NODES @ SEG-COUNT {: index:n :}
   state SYNTAX-NODES @ SEG-PUSH {: node :}
   kind node NODE-KIND !
   state SYNTAX-CURSOR @ node NODE-TOKEN ! index ;

\ Only query forms carry this node: trailing VALUES, aggregates and projected
\ names, read by validation and emission; the index of a projection's `*`
\ token places its rejection.
: QUERY-INFO ( ^syntax n -- ^node )
   over swap NODE NODE-VALUE @ NODE ;

: NEW-QUERY ( ^syntax n -- n )
   {: state kind:n :}
   state kind ADD-NODE {: query:n :}
   state N-QUERY-INFO ADD-NODE state query NODE NODE-VALUE ! query ;

: QUERY-VALUES@ ( ^syntax n -- n ) QUERY-INFO NODE-A @ ;
: QUERY-AGGREGATES ( ^syntax n -- n ) QUERY-INFO NODE-B @ ;
: QUERY-NAMES ( ^syntax n -- n ) QUERY-INFO NODE-C @ ;
: QUERY-STAR ( ^syntax n -- n ) QUERY-INFO NODE-VALUE @ ;

: TEXT! ( ^syntax n str -- )
   {: state index:n text:str :}
   state index NODE {: node :}
   text node NODE-TEXT ! ;

: TEXT-NODE ( ^syntax n str -- n )
   {: state kind:n text:str :}
   state kind ADD-NODE {: index:n :}
   state index text TEXT! index ;

\ An IRI or literal node; its value is its number among the query's terms.
: TERM-NODE ( ^syntax n str -- n )
   {: state kind:n text:str :}
   state kind text TEXT-NODE {: index:n :}
   state SYNTAX-TERMS @ state index NODE NODE-VALUE !
   1 state SYNTAX-TERMS +! index ;

\ Entry n of the entries at the address; its hash is the next cell.
: ENTRY ( ^n n -- ^n )
   2 * cells + ;

\ The node index under the hash that the quotation finds equal, 0 for none.
: INTERNED ( ^interns n [ n -- bool ] -- n )
   {: table:^ hash:n same :}
   table INTERNS-SLOTS @ {: slots:^ :}
   slots NULL-PTR = if 0 exit then
   table INTERNS-MASK @ {: mask:n :}
   hash mask and begin
      slots over ENTRY @ dup 0<> if
         {: at:n index:n :}
         slots at ENTRY cell+ @ hash = if
            index same execute if index exit then
         then at 1+ mask and true
      else drop false then
   while repeat drop 0 ;

: PLACE ( ^interns n n -- )
   {: table:^ hash:n index:n :}
   table INTERNS-SLOTS @ {: slots:^ :}
   table INTERNS-MASK @ {: mask:n :}
   hash mask and begin
      slots over ENTRY @ 0<>
   while 1+ mask and repeat
   slots swap ENTRY {: entry:^ :}
   index entry ! hash entry cell+ ! ;

\ n entries in place of the table's, holding its nodes.
: RESIZE ( ^interns n -- )
   {: table:^ size:n :}
   table INTERNS-SLOTS @ {: old:^ :}
   table INTERNS-MASK @ 1+ {: before:n :}
   table INTERNS-ARENA @ size 2 * cells ARENA:ALLOC drop CELL-VIEW
      table INTERNS-SLOTS !
   size 1- table INTERNS-MASK !
   old NULL-PTR = if exit then
   before 0 ?do
      old i ENTRY {: entry:^ :}
      entry @ 0<> if table entry cell+ @ entry @ PLACE then
   loop ;

\ Adds node index n under the hash; 16 entries first, doubled when one
\ more node would fill past half.
: +INTERN ( ^interns n n -- )
   {: table:^ hash:n index:n :}
   table INTERNS-SLOTS @ NULL-PTR = if
      table 16 RESIZE
   else
      table INTERNS-MASK @ 1+ {: size:n :}
      table INTERNS-COUNT @ 1+ 2 * size > if table size 2 * RESIZE then
   then
   table hash index PLACE
   1 table INTERNS-COUNT +! ;

\ The SipHash state, keyed for the table and reset.
: INTERNS-HASH ( ^interns -- ^SIPHASH:state )
   {: table:^ :}
   table INTERNS-SIP {: sip :}
   sip table INTERNS-K0 @ table INTERNS-K1 @ SIPHASH:INIT sip ;

: COPY$ ( ^syntax str -- str )
   {: state source:str :}
   source str> {: text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size
      ALLOC:BYTES {: out length:n :}
   text out size BYTE-COPY out length >str ;

: CONS ( ^syntax n n -- n )
   {: state item:n tail:n :}
   state N-LIST ADD-NODE {: index:n :}
   item state index NODE NODE-A !
   tail state index NODE NODE-B ! index ;

: REVERSE ( ^syntax n -- n )
   {: state list:n :}
   0 list begin dup 0<> while
      {: result:n current:n :}
      state current NODE-B@ {: next:n :}
      result state current NODE NODE-B !
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
   operand state index NODE NODE-A ! index ;

: BINARY-NODE ( ^syntax n n n -- n )
   {: state kind:n left:n right:n :}
   state kind left UNARY-NODE {: index:n :}
   right state index NODE NODE-B ! index ;

: SYNTAX-NEW ( str ^ARENA:arena -- ^syntax )
   {: text:str arena :}
   arena ARENA:ALLOCATOR [type] syntax ALLOC:NEW
      {: state :}
   arena state SYNTAX-ARENA !
   state SYNTAX-INTERNS {: table:^ :}
   arena table INTERNS-ARENA !
   ENTROPY:RANDOM table INTERNS-K0 !
   ENTROPY:RANDOM table INTERNS-K1 !
   text state SYNTAX-SOURCE !
   text state SYNTAX-ORIGINAL !
   arena ARENA:ALLOCATOR 0 [type] escape VEC:NEW
      state SYNTAX-ESCAPES !
   arena ARENA:ALLOCATOR [type] node SEG-NEW {: nodes:^ :}
   nodes 1 SEG-ENSURE  nodes state SYNTAX-NODES !
   arena ARENA:ALLOCATOR [type] token SEG-NEW
      state SYNTAX-TOKENS ! state ;

;package
