\ Caller-owned syntax. Node zero is absent; lists are cons nodes so a
\ shared RDF term has no mutable sibling link. The caller owns the arena.

require src/lib/arena.f
require src/lib/vector.f
require src/lib/string.f
require src/lib/time.f
require src/lib/task.f

package SPARQL

public

-10001 s" malformed SPARQL query" ERROR:CODE E-SYNTAX
-10002 s" SPARQL SERVICE is unavailable" ERROR:CODE E-SERVICE
-10003 s" SPARQL deadline exceeded" ERROR:CODE E-DEADLINE
-10004 s" SPARQL nesting exceeds 256 levels" ERROR:CODE E-NESTING

private

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
16 constant N-SERVICE
17 constant N-FILTER
18 constant N-BIND
19 constant N-VALUES
20 constant N-SELECT
21 constant N-ASK
22 constant N-CONSTRUCT
23 constant N-DESCRIBE
24 constant N-PATH
25 constant N-ALIAS
26 constant N-DATASET
27 constant N-ORDER
28 constant N-GROUPBY
29 constant N-HAVING
30 constant N-LIMIT
31 constant N-OFFSET

1 constant F-DISTINCT
2 constant F-REDUCED
4 constant F-STAR
8 constant F-SILENT
16 constant F-NAMED
32 constant F-DESC

STRUCTURE node DERIVE addr
   FIELD kind n
   FIELD flags n
   FIELD text ptr u8
   FIELD size n
   FIELD a n
   FIELD b n
   FIELD c n
   FIELD d n
   FIELD value n
;STRUCTURE

STRUCTURE token DERIVE addr
   FIELD kind n
   FIELD text ptr u8
   FIELD size n
   FIELD offset n
;STRUCTURE

public

STRUCTURE syntax OPAQUE DERIVE addr
   FIELD arena ptr ARENA:arena
   FIELD nodes ptr VEC:vector<node>
   FIELD tokens ptr VEC:vector<token>
   FIELD source ptr u8
   FIELD length n
   FIELD position n
   FIELD cursor n
   FIELD root n
   FIELD prefixes n
   FIELD base ptr u8
   FIELD base-size n
   FIELD serial n
   FIELD group n
   FIELD labels n
   FIELD deadline n
   FIELD ticks n
   FIELD nesting n
;STRUCTURE

private

: CHECK-BUDGET ( ptr syntax -- )
   {: state :}
   state SYNTAX-TICKS @ 63 and 0= if
      TASK:PAUSE
      state SYNTAX-DEADLINE @ dup 0<> if
         TIME:MONO-NS <= if E-DEADLINE throw then
      else drop then
   then 1 state SYNTAX-TICKS +! ;

: NEST ( ptr syntax -- )
   dup CHECK-BUDGET
   dup SYNTAX-NESTING @ 256 >= if drop E-NESTING throw then
   1 swap SYNTAX-NESTING +! ;

: UNNEST ( ptr syntax -- ) -1 swap SYNTAX-NESTING +! ;

: NODE ( ptr syntax n -- ptr node )
   over CHECK-BUDGET
   swap SYNTAX-NODES @ swap VEC:AT ;

: TEXT$ ( ptr syntax n -- ptr u8 n )
   NODE dup NODE-TEXT @ swap NODE-SIZE @ ;

: NODE-K ( ptr syntax n -- n ) NODE NODE-KIND @ ;
: NODE-A@ ( ptr syntax n -- n ) NODE NODE-A @ ;
: NODE-B@ ( ptr syntax n -- n ) NODE NODE-B @ ;
: NODE-C@ ( ptr syntax n -- n ) NODE NODE-C @ ;
: NODE-D@ ( ptr syntax n -- n ) NODE NODE-D @ ;

: ADD-NODE ( ptr syntax n -- n )
   {: state kind:n :}
   state CHECK-BUDGET
   state SYNTAX-NODES @ VEC:COUNT {: index:n :}
   kind state SYNTAX-NODES @ VEC:PUSH NODE-KIND !
   index ;

: TEXT! ( ptr syntax n ptr u8 n -- )
   {: state index:n text size:n :}
   state index NODE {: node :}
   text node NODE-TEXT ! size node NODE-SIZE ! ;

: TEXT-NODE ( ptr syntax n ptr u8 n -- n )
   {: state kind:n text size:n :}
   state kind ADD-NODE {: index:n :}
   state index text size TEXT! index ;

: COPY$ ( ptr syntax ptr u8 n -- ptr u8 n )
   {: state text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size
      ALLOC:BYTES {: out length:n :}
   text out size BYTE-COPY out length ;

: CONS ( ptr syntax n n -- n )
   {: state item:n tail:n :}
   state N-LIST ADD-NODE {: index:n :}
   item state index NODE NODE-A !
   tail state index NODE NODE-B ! index ;

: REVERSE ( ptr syntax n -- n )
   {: state list:n :}
   0 list begin dup 0<> while
      {: result:n current:n :}
      state current NODE-B@ {: next:n :}
      result state current NODE NODE-B !
      current next
   repeat drop ;

: LIST-LENGTH ( ptr syntax n -- n )
   {: state list:n :}
   0 list begin dup 0<> while
      state swap NODE-B@ swap 1+ swap
   repeat drop ;

: UNARY-NODE ( ptr syntax n n -- n )
   {: state kind:n operand:n :}
   state kind ADD-NODE {: index:n :}
   operand state index NODE NODE-A ! index ;

: BINARY-NODE ( ptr syntax n n n -- n )
   {: state kind:n left:n right:n :}
   state kind left UNARY-NODE {: index:n :}
   right state index NODE NODE-B ! index ;

: SYNTAX-NEW ( ptr u8 n ptr u8 n ptr ARENA:arena
      -- ptr syntax )
   {: text size:n base base-size:n arena :}
   size 0 < base-size 0 < or if E-SYNTAX throw then
   arena ARENA:ALLOCATOR [type] syntax ALLOC:NEW
      {: state :}
   arena state SYNTAX-ARENA !
   text state SYNTAX-SOURCE ! size state SYNTAX-LENGTH !
   base state SYNTAX-BASE !
   base-size state SYNTAX-BASE-SIZE !
   arena ARENA:ALLOCATOR 1 [type] node VEC:NEW
      state SYNTAX-NODES !
   arena ARENA:ALLOCATOR 0 [type] token VEC:NEW
      state SYNTAX-TOKENS ! state ;

;package
