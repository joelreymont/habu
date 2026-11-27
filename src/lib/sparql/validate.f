\ Scope restrictions from SPARQL 1.1 sections 18.2.1 and 19.8. A scope is a
\ bit vector over variable numbers; expression-only uses never introduce
\ bindings. A walker follows a left-leaning chain of operators or UNIONs in a
\ loop and recurses only where the parser recursed, so the parser's nesting
\ bound bounds it.

require src/lib/sparql/pattern.f
require src/lib/sparql/syntax.f
require src/lib/sparql/terms.f
require src/lib/sparql/expression.f
require src/lib/sparql/iri.f
require src/lib/sparql/lex.f
require src/lib/fmt.f

package SPARQL
private

\ Bits is span cells; a variable's bit is its number.
STRUCTURE var-set DERIVE addr
   FIELD bits ^n
   FIELD span n
;STRUCTURE

defer SCOPE ( ^syntax n -- ^var-set )
defer QUERY-SCOPE ( ^syntax n -- ^var-set )

: VAR-COUNT ( ^syntax -- n ) SYNTAX-VARIABLES @ ;

\ A set lives in the work arena; ADD-SCOPE cuts a child's sets once unioned.
: NEW-SET ( ^syntax -- ^var-set )
   {: state :}
   state SYNTAX-WORK @ {: arena :}
   arena ARENA:ALLOCATOR [type] var-set ALLOC:NEW {: set :}
   state VAR-COUNT 64 / 1+ {: span:n :}
   arena span cells ARENA:ALLOC drop CELL-VIEW set VAR-SET-BITS !
   span set VAR-SET-SPAN ! set ;

\ The number of variable node n.
: NUMBER@ ( ^syntax n -- n ) NODE-VALUE@ ;

\ Cell n of the set.
: SCOPE-CELL ( ^var-set n -- ^n ) cells swap VAR-SET-BITS @ + ;

\ The cell and the mask of variable node n's bit.
: SCOPE-BIT ( ^syntax ^var-set n -- ^n n )
   {: state set name:n :}
   state name NUMBER@ {: number:n :}
   set number 64 / SCOPE-CELL 1 number 64 mod lshift ;

: MEMBER? ( ^syntax ^var-set n -- bool )
   SCOPE-BIT swap @ and 0<> ;

: SCOPE-ADD ( ^syntax ^var-set n -- )
   {: state set name:n :}
   state name NODE-K N-VAR <> if exit then
   state set name SCOPE-BIT {: at:^ mask:n :}
   at @ mask or at ! ;

\ Adds the second set's members to the first.
: SCOPE-UNION ( ^var-set ^var-set -- )
   {: into from :}
   into VAR-SET-SPAN @ 0 ?do
      into i SCOPE-CELL from i SCOPE-CELL @ over @ or swap !
   loop ;

\ `use a new variable instead of ?x` for variable node n.
: FRESH-FIX ( ^syntax n -- str )
   {: state name:n :}
   state s" use a new variable instead of ?"
      state state name TEXT$ FIX-NAME JOIN$ ;

: FRESH-VAR ( ^syntax ^var-set n -- )
   {: state set name:n :}
   state set name MEMBER? if
      state name s" a variable not already in scope"
         state name FRESH-FIX REJECT-NODE
   then
   state set name SCOPE-ADD ;

: UNIQUE-NAMES ( ^syntax n -- ^var-set )
   {: state list:n :}
   state NEW-SET {: names :}
   list begin dup 0<> while
      {: current:n :}
      state names state current NODE-A@ FRESH-VAR
      state current NODE-B@
   repeat drop names ;

STRUCTURE link DERIVE addr FIELD node n FIELD next ^link ;STRUCTURE

\ The left spine below node n through nodes of the two kinds: those nodes
\ as a list in the syntax's work arena, deepest first, and the node below
\ the deepest.
: SPINE ( ^syntax n n n -- ^link n )
   {: state top:n first:n second:n :}
   state SYNTAX-WORK @ ARENA:ALLOCATOR {: al:^ :}
   NULL-PTR top begin
      state over NODE-K dup first = swap second = or
   while
      {: list:^ node:n :}
      al [type] link ALLOC:NEW {: at:^ :}
      node at LINK-NODE !  list at LINK-NEXT !
      at state node NODE-A@
   repeat ;

: BLANK-LABEL ( ^syntax n n -- )
   {: state term:n bgp:n :}
   state term NODE-K N-BNODE <> if exit then
   state term NODE-FLAGS@ F-GENERATED and 0<> if exit then
   state N-BNODE state term TEXT$ FIRST-NAME {: first:n :}
   first 0= if
      state term bgp NODE-VALUE! state term +FIRST-NAME exit
   then
   state first NODE-VALUE@ bgp <> if
      state term s" a blank node label not used in another group"
         s" rename the label" REJECT-NODE
   then ;

: TRIPLE-LABELS ( ^syntax n n -- )
   {: state triple:n bgp:n :}
   state state triple NODE-A@ bgp BLANK-LABEL
   state state triple NODE-C@ bgp BLANK-LABEL ;

\ Runs the action on each leaf of expression n, left to right; its answer is
\ walked next unless 0, then a call's ORDER BY conditions.
: WALK ( R ^syntax n [ R ^syntax n -- R n ] -- R )
   {: state expression:n action :}
   state SYNTAX-WORK @ {: work:^ :}
   work ARENA:MARK {: mark:n :}
   state expression N-BINARY N-ALIAS SPINE {: spine:^ leaf:n :}
   state leaf NODE-K N-LIST = if
      leaf begin dup 0<> while
         {: list:n :}
         state state list NODE-A@ action recurse
         state list NODE-B@
      repeat drop
   else
      state leaf action execute {: operand:n :}
      operand 0<> if
         state operand action recurse
         state leaf NODE-K N-CALL = if
            state state leaf NODE-C@ action recurse
         then
      then
   then
   spine begin dup NULL-PTR <> while
      {: at:^ :} at LINK-NODE @ {: node:n :}
      state node NODE-K N-BINARY = if
         state state node NODE-B@ action recurse
      then at LINK-NEXT @
   repeat drop
   work mark ARENA:CUT ;

: AGGREGATE-CALL? ( ^syntax n -- bool )
   {: state leaf:n :}
   state leaf NODE-K N-CALL <> if false exit then
   state leaf AGGREGATE? ;

\ A leaf's operand; 0 for EXISTS, whose group is a scope of its own.
: OPERAND ( ^syntax n -- n )
   {: state leaf:n :}
   state leaf NODE-K {: kind:n :}
   kind N-CALL = if
      state leaf EXISTS-CALL? if 0 exit then
   then
   kind N-CALL = kind N-UNARY = or kind N-ORDER = or if
      state leaf NODE-A@
   else 0 then ;

: EXISTS-SCOPE ( ^syntax n -- )
   {: state leaf:n :}
   state leaf NODE-K N-CALL <> if exit then
   state leaf EXISTS-CALL? if
      state state leaf NODE-A@ SCOPE drop
   then ;

: PLAIN-LEAF ( ^syntax n -- n )
   {: state leaf:n :}
   state leaf AGGREGATE-CALL? if
      state leaf s" an expression without an aggregate"
         s" use aggregates only in SELECT, HAVING and ORDER BY, unnested"
         REJECT-NODE
   then
   state leaf EXISTS-SCOPE state leaf OPERAND ;

: OUTPUT-LEAF ( ^syntax n -- n )
   {: state leaf:n :}
   state leaf AGGREGATE-CALL? if
      state state leaf NODE-A@ [: PLAIN-LEAF ;] WALK
      state state leaf NODE-C@ [: PLAIN-LEAF ;] WALK 0 exit
   then
   state leaf PLAIN-LEAF ;

\ Checks an expression that may hold aggregates where the flag is true.
: EXPRESSION-CHECK ( ^syntax n bool -- )
   if [: OUTPUT-LEAF ;] WALK else [: PLAIN-LEAF ;] WALK then ;

: TRIPLE-SCOPE ( ^syntax ^var-set n -- )
   {: state names triple:n :}
   state names state triple NODE-A@ SCOPE-ADD
   state names state triple NODE-B@ SCOPE-ADD
   state names state triple NODE-C@ SCOPE-ADD ;

\ The objects of list n as those of triples whose subject and predicate
\ are already in.
: OBJECTS-SCOPE ( ^syntax ^var-set n n -- )
   {: state names list:n bgp:n :}
   list begin dup 0<> while
      {: cell:n :} state cell NODE-A@ {: object:n :}
      state object bgp BLANK-LABEL
      state names object SCOPE-ADD
      state cell NODE-B@
   repeat drop ;

\ A triple, an N-OBJECTS or an N-COLLECTION as its triples; a member's
\ blank node, rdf:first, rdf:rest and rdf:nil add no label or name.
: TRIPLES-SCOPE ( ^syntax ^var-set n n -- )
   {: state names pattern:n bgp:n :}
   state pattern NODE-K N-COLLECTION = if
      state names state pattern NODE-A@ bgp OBJECTS-SCOPE exit
   then
   state pattern bgp TRIPLE-LABELS
   state names pattern TRIPLE-SCOPE
   state pattern NODE-K N-OBJECTS <> if exit then
   state names state pattern NODE-D@ bgp OBJECTS-SCOPE ;

: BIND-SCOPE ( ^syntax n ^var-set -- )
   {: state binding:n names :}
   state state binding NODE-A@ false EXPRESSION-CHECK
   state names state binding NODE-B@ FRESH-VAR ;

\ Adds pattern n's scope to the set and frees the sets that made it.
: ADD-SCOPE ( ^syntax ^var-set n -- )
   {: state names pattern:n :}
   state SYNTAX-WORK @ {: work:^ :}
   work ARENA:MARK {: mark:n :}
   names state pattern SCOPE SCOPE-UNION
   work mark ARENA:CUT ;

: GROUP-SCOPE ( ^syntax n -- ^var-set )
   {: state group:n :}
   state NEW-SET {: names :}
   0 state group NODE-A@ begin dup 0<> while
      {: bgp:n list:n :}
      state list NODE-A@ {: pattern:n :}
      state pattern NODE-K {: kind:n :}
      kind N-TRIPLE = kind N-OBJECTS = or kind N-COLLECTION = or if
         bgp 0= if pattern else bgp then {: current:n :}
         state names pattern current TRIPLES-SCOPE current
      else
         kind N-BIND = if
            state pattern names BIND-SCOPE 0
         else
            state names pattern ADD-SCOPE
            kind N-FILTER = if bgp else 0 then
         then
      then state list NODE-B@
   repeat 2drop names ;

: UNION-SCOPE ( ^syntax n -- ^var-set )
   {: state pattern:n :}
   state NEW-SET {: names :}
   state SYNTAX-WORK @ {: work:^ :}
   work ARENA:MARK {: mark:n :}
   state pattern N-UNION N-UNION SPINE {: spine:^ leaf:n :}
   state names leaf ADD-SCOPE
   spine begin dup NULL-PTR <> while
      {: at:^ :}
      state names state at LINK-NODE @ NODE-B@ ADD-SCOPE
      at LINK-NEXT @
   repeat drop
   work mark ARENA:CUT names ;

: GRAPH-SCOPE ( ^syntax n -- ^var-set )
   {: state pattern:n :}
   state state pattern NODE-B@ SCOPE {: names :}
   state names state pattern NODE-A@ SCOPE-ADD names ;

: PATTERN-SCOPE ( ^syntax n -- ^var-set )
   {: state pattern:n :}
   state pattern NODE-K case
      N-GROUP of state pattern GROUP-SCOPE endof
      N-OPTIONAL of state state pattern NODE-A@ SCOPE endof
      N-UNION of state pattern UNION-SCOPE endof
      N-GRAPH of state pattern GRAPH-SCOPE endof
      N-MINUS of
         state state pattern NODE-A@ SCOPE drop state NEW-SET
      endof
      N-FILTER of
         state state pattern NODE-A@ false EXPRESSION-CHECK
         state NEW-SET
      endof
      N-BIND of
         state NEW-SET {: names :}
         state pattern names BIND-SCOPE names
      endof
      N-VALUES of state state pattern NODE-A@ UNIQUE-NAMES endof
      \ N-SELECT, a subquery: a group adds its own triples and the parser
      \ makes no other pattern.
      state pattern QUERY-SCOPE swap
   endcase ;

\ Each aggregate call is reached once: the parser shares no nodes and the
\ walk stops at an aggregate.
: AGGREGATE-LEAF ( n ^syntax n -- n n )
   {: found:n state leaf:n :}
   state leaf AGGREGATE-CALL? if state leaf found CONS 0 exit then
   found state leaf OPERAND ;

: COLLECT-AGGREGATES ( n ^syntax n -- n )
   [: AGGREGATE-LEAF ;] WALK ;

: FIND-AGGREGATES ( ^syntax n -- n )
   {: state query:n :}
   0 state state query NODE-A@ COLLECT-AGGREGATES
   state query NODE-D@ begin dup 0<> while
      {: found:n list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K dup N-HAVING = swap N-ORDER = or if
         found state state modifier NODE-A@ COLLECT-AGGREGATES
      else found then state list NODE-B@
   repeat drop ;

\ Adds GROUP BY's names to grouped and its aliases to names.
: GROUPED-NAMES ( ^syntax n ^var-set ^var-set -- )
   {: state expressions:n names grouped :}
   expressions begin dup 0<> while
      {: list:n :}
      state list NODE-A@ {: expression:n :}
      state expression false EXPRESSION-CHECK
      state expression NODE-K N-ALIAS = if
         state expression NODE-B@ {: name:n :}
         state names name FRESH-VAR
         state grouped name SCOPE-ADD
      else
         state grouped expression SCOPE-ADD
      then state list NODE-B@
   repeat drop ;

\ `add ?x to GROUP BY or wrap it in SAMPLE(?x)` for variable node n.
: GROUP-FIX ( ^syntax n -- str )
   {: state name:n :}
   state state name TEXT$ FIX-NAME {: var:str :}
   state s" add ?" var JOIN$
   state swap s"  to GROUP BY or wrap it in SAMPLE(?" JOIN$
   state swap var JOIN$
   state swap s" )" JOIN$ ;

: GROUPED-LEAF ( ^syntax n ^var-set -- n )
   {: state leaf:n grouped :}
   state leaf NODE-K N-VAR = if
      state grouped leaf MEMBER? 0= if
         state leaf s" a variable in GROUP BY or in an aggregate"
            state leaf GROUP-FIX REJECT-NODE
      then
   then
   state leaf AGGREGATE-CALL? if 0 else state leaf OPERAND then ;

: CHECK-GROUPED ( ^syntax n ^var-set -- )
   {: state expression:n grouped :}
   state expression [: grouped GROUPED-LEAF ;] WALK ;

: GROUP-BY-NODE ( ^syntax n -- n )
   {: state query:n :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K N-GROUPBY = if modifier exit then
      state list NODE-B@
   repeat drop 0 ;

: TAIL-NAMES ( ^syntax n -- ^var-set )
   {: state query:n :}
   state query QUERY-VALUES@ dup 0<> if state swap SCOPE
   else drop state NEW-SET then ;

: BIT-COUNT ( n -- n )
   0 swap begin dup 0<> while dup 1- and swap 1+ swap repeat drop ;

: MEMBER-COUNT ( ^var-set -- n )
   {: set :}
   0 set VAR-SET-SPAN @ 0 ?do set i SCOPE-CELL @ BIT-COUNT + loop ;

\ The variables `*` selects from the scope, in order of first use. Each has
\ a node after the query's own.
: STAR-NAMES ( ^syntax n ^var-set -- n )
   {: state query:n scope :}
   state NEW-SET {: rest :}
   rest scope SCOPE-UNION
   0 query scope MEMBER-COUNT begin dup 0<> while
      {: list:n node:n count:n :}
      state node NODE-K N-VAR = if
         state rest node MEMBER?
      else false then if
         state rest node SCOPE-BIT {: at:^ mask:n :}
         at @ mask xor at !
         state node list CONS node 1+ count 1-
      else list node 1+ count then
   repeat 2drop state swap REVERSE ;

\ The set's variables as a fix writes them, `?a ?b`, in order of first use.
\ A first pass sizes the text, so its bytes are written once.
: VARS-FIX ( ^syntax n ^var-set -- str )
   {: state query:n set :}
   state query set STAR-NAMES {: names:n :}
   names 0= if s" " exit then
   0 names begin dup 0<> while
      {: sum:n list:n :}
      sum  state  state state list NODE-A@ TEXT$  FIX-NAME STR:LEN + 2 +
      state list NODE-B@
   repeat drop 1- {: size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size ALLOC:BYTES drop {: out :}
   0 names begin dup 0<> while
      {: at:n list:n :}
      at 0<> if 32 out at + c! at 1+ else at then {: from:n :}
      [char] ? out from + c!
      state  state state list NODE-A@ TEXT$  FIX-NAME str> {: text width:n :}
      text out from 1+ + width BYTE-COPY
      from 1+ width +  state list NODE-B@
   repeat 2drop  out size >str ;

\ Whether the query uses the variable of the name.
: USED? ( ^syntax str -- bool )
   {: state name:str :}
   state N-VAR name FIRST-NAME 0<> ;

\ The first of the names nK, nK+1 ... the query does not use, n for n0.
: UNUSED-NAME ( ^syntax n -- str )
   {: state k:n :}
   k 0= if s" n" else state s" n" k FMT:INT$ JOIN$ then {: name:str :}
   state name USED? if state k 1+ RECURSE exit then name ;

\ The fix of `*` in a grouped query: the variables it may select, else a
\ count under a name the query does not use.
: STAR-FIX ( ^syntax n ^var-set -- str )
   {: state query:n grouped :}
   grouped MEMBER-COUNT 0= if
      state s" replace * with (COUNT(*) AS ?" state 0 UNUSED-NAME JOIN$
      state swap s" )" JOIN$ exit
   then state s" replace * with " state query grouped VARS-FIX JOIN$ ;

\ The set of the projected variables, which become the query's names in
\ projection order. Scope gains the aliases; grouped, a set of the caller's
\ own, gains each projected name, as an expression may use the names
\ projected before it.
: PROJECT-SCOPE ( ^syntax n ^var-set ^var-set bool -- ^var-set )
   {: state query:n scope grouped aggregate:bool :}
   state query NODE-FLAGS@ F-STAR and 0<> if
      aggregate if
         state state query QUERY-STAR
            s" SELECT with named variables in a grouped query"
            state query grouped STAR-FIX REJECT-TOKEN
      then
      state query state query scope STAR-NAMES QUERY-NAMES! scope exit
   then
   state NEW-SET {: projected :}
   0 state query NODE-A@ begin dup 0<> while
      {: names:n list:n :}
      state list NODE-A@ {: expression:n :}
      state expression true EXPRESSION-CHECK
      aggregate if state expression grouped CHECK-GROUPED then
      state expression NODE-K N-ALIAS = if
         state expression NODE-B@ {: name:n :}
         state scope name FRESH-VAR
         state projected name FRESH-VAR
         state grouped name SCOPE-ADD
         state name names CONS
      else
         state grouped expression SCOPE-ADD
         state projected expression MEMBER? if names else
            state projected expression SCOPE-ADD
            state expression names CONS
         then
      then state list NODE-B@
   repeat drop {: names:n :}
   state query state names REVERSE QUERY-NAMES! projected ;

\ What ORDER BY may name in a grouped query.
: ORDER-SCOPE ( ^syntax n ^var-set ^var-set -- ^var-set )
   {: state query:n grouped projected :}
   state NEW-SET {: names :}
   names grouped SCOPE-UNION
   names projected SCOPE-UNION
   names state query TAIL-NAMES SCOPE-UNION names ;

: QUERY-MODIFIERS ( ^syntax n ^var-set ^var-set bool -- )
   {: state query:n grouped projected aggregate:bool :}
   aggregate if state query grouped projected ORDER-SCOPE
   else grouped then {: ordered :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K dup N-ORDER = swap N-HAVING = or if
         state modifier NODE-A@ {: expression:n :}
         state expression true EXPRESSION-CHECK
         aggregate if
            state expression
            state modifier NODE-K N-ORDER = if ordered else grouped then
            CHECK-GROUPED
         then
      then state list NODE-B@
   repeat drop ;

: CHECK-QUERY ( ^syntax n -- ^var-set )
   {: state query:n :}
   state state query NODE-B@ SCOPE {: names :}
   state query GROUP-BY-NODE {: grouping:n :}
   state query FIND-AGGREGATES {: aggregates:n :}
   state query aggregates QUERY-AGGREGATES!
   aggregates 0<> grouping 0<> or {: aggregate:bool :}
   state NEW-SET {: grouped :}
   grouping 0<> if
      state state grouping NODE-A@ names grouped GROUPED-NAMES
   then
   names state query TAIL-NAMES SCOPE-UNION
   state query NODE-K N-SELECT = if
      state NEW-SET {: extended :}
      extended grouped SCOPE-UNION
      extended state query TAIL-NAMES SCOPE-UNION
      state query names extended aggregate PROJECT-SCOPE
   else names then {: projected :}
   state query grouped projected aggregate QUERY-MODIFIERS
   projected ;

: INSTALL-VALIDATION ( -- )
   ['] PATTERN-SCOPE is SCOPE
   ['] CHECK-QUERY is QUERY-SCOPE ;

INSTALL-VALIDATION

;package
