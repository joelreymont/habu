\ emit.f - a parsed query compiled to one PostgreSQL statement on the native
\ columns the store describes. Its parameters are the query's constants and
\ names; no query text enters the statement. The description's texts, a
\ slot's `term`, a source's `graph` and a key set's keys, are string
\ literals: the host wrote them, store.f checked them, and as parameters
\ they would grow with the description past the 65,535 one call takes.
\
\ A relation is SQL text usable as a FROM item and, for each variable in
\ scope, a binding: always bound or maybe, and its form, a native column of
\ one shape or a JSONB term, GENERIC or KNOWN (a term the compiler knows).
\
\ A group's triple pattern matches its candidate sources: those whose
\ slots hold the pattern's constants, a column's shape by its `holds`, and
\ whose graph is the active graph's. Shape inference keeps a candidate
\ only while each of its always-bound variables meets a live candidate of
\ every other joined item binding it always (one shape, or a part and its
\ root; a shape and a term it holds; two terms that are one), to a
\ fixpoint; a joined item left with none makes the group empty, with no
\ SQL. A pattern's candidates group by the description source they were
\ read from: one group is that source's FROM item, several a UNION ALL of
\ one branch per group, and where a group's candidates read several
\ columns at a slot, a variable binds their COALESCE and a constant is
\ held by one of them. A pattern of one such group whose subject slot is
\ a unique column reads the row an earlier pattern of its subject reads
\ from that source's FROM item by that column. A nested group, and a GRAPH
\ of a named graph's IRI, with no OPTIONAL, MINUS, BIND or FILTER directly
\ in it adds its items to its parent's, join being associative; any other
\ is a derived table. A chain of UNIONs is one UNION ALL derived table of
\ its groups that are not
\ statically empty. A union's column is native where every branch binding
\ it has a shape of one root, the root's where they differ, NULL where a
\ branch does not bind it. A subquery is a derived table of its
\ projection; its variables are a frame of their own.
\
\ A group folds its items in order (SPARQL 1.1 section 18.2.2.6) into one
\ FROM of CROSS JOINs: a joined item meets the bindings so far in the
\ WHERE, an OPTIONAL is a LEFT JOIN whose ON holds the meeting and the
\ OPTIONAL's FILTERs, a MINUS is a correlated NOT EXISTS. A BIND's variable
\ is its expression, written at each read; one that calls RAND, UUID or
\ STRUUID, or that PostgreSQL would plan as more than MOST-COPIED bytes, is
\ a column of a derived table of the bindings so far. A VALUES is a joined
\ derived table of its data block's terms. The joined items up to the next
\ OPTIONAL, MINUS or BIND fold in connected order, next the first written
\ of those that share a variable with the bindings so far and have a given
\ end, a derived table having one, else share one, else have a given end,
\ else of all, as PostgreSQL plans together only join_collapse_limit FROM
\ items, in written order. A triple read from several description sources
\ folds after the other joined items up to the next OPTIONAL, MINUS or
\ BIND. Unless its subject or object is given, each of its variables
\ those items bind always has keys, one set for all the triples folded
\ there: the distinct values bound to it by the items connected to it,
\ those binding it always and, to a fixpoint, those binding always a
\ variable a connected item binds always. No union inside an EXISTS has
\ keys. The first of its keys that a branch binds natively, of its root
\ or through its shape's `in` for a term key, drives the union: those
\ branches read once per key, `tK AS a CROSS JOIN LATERAL (<their UNION
\ ALL>)`, each with its binding `=` a.k, and the other branches join
\ beside them in a UNION ALL. A condition reading the keys in each branch
\ would make PostgreSQL hash them once per branch, held until the
\ statement ends. The union's other keys add no condition: the group's
\ meeting keeps its answers. A key that drives a union is a CTE at the
\ statement's WITH, materialized, since a WITH inside the union's derived
\ table would stop PostgreSQL flattening the union; connected items with
\ several such keys join in one CTE, which those keys read.
\ Two always bindings of a variable meet as equal, a native one the
\ anchor: native `=` within a shape and between a part and its root,
\ `col = in(term)` against a term, JSONB `=` between terms. A maybe
\ binding meets also where either is NULL, and the variable is then the
\ first bound of the two. A variable's slot on a column that may hold
\ NULL is tested `IS NOT NULL` where its relation's WHERE is written,
\ unless a strict `=` of that WHERE already reads the column, since
\ PostgreSQL would count the column's null fraction twice. The group's
\ FILTERs read the folded bindings. An
\ expression has two SQL forms, its term, JSONB, and its test, an SQL
\ boolean; a FILTER and HAVING hold its test, all else its term. Each
\ conjunct of such a test that is `=` of a natively bound variable and an
\ IRI or a plain or language-tagged string, either way round, IN a list of
\ only those, or sameTerm of it and any constant also adds the native
\ equality it implies, which an index can serve; the test stays and
\ decides.
\
\ [NOT] EXISTS substitutes the solution it reads (SPARQL 1.1 section
\ 18.6): each binding of a variable that solution binds meets its binding
\ once, where it is made: a slot, GRAPH's variable, BIND's, a VALUES or
\ subquery column, a walk's end. A variable it binds always is no slot's variable. A
\ FILTER's and a BIND's expression read their scope through the
\ substitution, and a nested EXISTS substitutes that scope, the innermost
\ solution first.
\
\ A triple whose verb is a path expands before shape inference: `^p`
\ swaps subject and object, `p/q` is two triples joined on a fresh
\ variable, `p|q` a UNION of the two; `!(...)` matches the sources whose
\ predicate is none of its IRIs, its inverse members with subject and
\ object swapped, a UNION when it has both. `p+`, `p*` and `p?` are walks,
\ each folded after the joined items up to the next OPTIONAL, MINUS or
\ BIND: a WITH RECURSIVE CTE of p's steps from its seeds, UNION
\ (distinct) so a cycle ends. The seeds are a constant end or one the
\ substitution binds always, its value, else the distinct values of
\ an end the solutions so far bind always, which are then written once as
\ a CTE both read, else every subject of p's steps for `+` and every
\ subject and object in the active graph for `*` and `?`; from the
\ object end the walk steps backwards. `*` and `?` add each seed as its
\ own end. A walk's steps, when its operand has a walk in it, are a CTE
\ at the statement's WITH, which reads no scope, so a nested path writes
\ each walk once; the active graph's subjects and objects are one CTE
\ there, which the seeds of every walk from every node read.
\
\ A query that GROUP BY or an aggregate groups reads its groups, a derived
\ table grouped on its keys' anchors, native where native, whose columns
\ hold the aggregates and whose HAVING holds its conditions. SUM, AVG,
\ MIN and MAX without DISTINCT of a variable bound always to a column of
\ a numeric shape holding xsd:integer and xsd:decimal, and MIN and MAX of
\ one of a text shape holding xsd:string, aggregate the column: SUM and
\ AVG are sparql.number of its sum and of that over the count, in SPARQL's
\ datatype, an integer 0 for no solution, and MIN and MAX the shape's
\ `out` of its min or max, text compared COLLATE "C". Any other goes
\ through sparql.aggregate over array_agg of its operand's terms,
\ DISTINCT and GROUP_CONCAT always. Where the
\ grouped solutions are one union's of two or more branches and no key or
\ aggregate calls EXISTS, each branch groups on its own as the union's
\ columns present it. Where a GROUP BY variable binds each branch always
\ to a term no other branch binds it to, each group lies in one branch:
\ the branches' groups are the groups, and a WHERE over them holds the
\ conditions. Else, where every aggregate is COUNT, MIN, MAX or SAMPLE
\ without DISTINCT and every GROUP BY key binds a variable, the branches'
\ partial groups regroup: COUNT sums its counts, 0 for none; MIN and MAX
\ take the first of their partials in ORDER BY's order, which an unbound
\ leads as in the whole, or where the union's column is native the min
\ or max of their native partials, and SAMPLE any bound one; a branch
\ with no solution keeps no partial. SUM and AVG add doubles one by one, so
\ regrouping changes their answer, and DISTINCT meets across branches:
\ those group the whole union. A trailing VALUES joins the solutions that
\ leaves. SELECT's expressions bind as
\ BIND's do, over the solutions, each a maybe binding but an always bound
\ aggregate's: COUNT's, a native SUM's, and with GROUP BY, whose every
\ group has a solution, MIN's, MAX's or SAMPLE's of a variable the
\ grouped solutions bind always. DISTINCT groups the projected columns. ORDER BY's keys
\ number the solutions, and the number orders the slice, the running byte
\ total and the document. The projection is one derived table, which the
\ document or a subquery reads.
\ STORAGE CLASS. CALLER-OWNED: everything lives in the syntax's arena.

require src/lib/sparql/parse.f
require src/lib/sparql/store.f
require src/lib/json/write.f
require src/lib/fmt.f
require src/lib/map.f
require src/lib/sparql/syntax.f
require src/lib/vector.f
require src/lib/json/value.f
require src/lib/sparql/iri.f
require src/lib/map-core.f
require src/lib/sparql/pattern.f
require src/lib/sparql/expression.f
require src/lib/sparql/validate.f

package SPARQL
private

-1 constant GENERIC
-2 constant KNOWN

\ The reads, items, aliases and variables, a statement may have.
\ PostgreSQL's planner memory grows with them and no setting of its own
\ bounds it: a 4 KB query of nested OPTIONALs took 8 GB.
512 constant MOST

\ The bytes a read of a BIND's variable may copy. A BIND read twice by the
\ next doubles them; as LATERAL items PostgreSQL pulls up, a chain of 100
\ each read once took its planner 2.9 GB.
4096 constant MOST-COPIED

\ The parameters one PostgreSQL call takes.
65535 constant PARAMETERS

\ A parameter of the statement: a term's JSON text, or a string's own.
STRUCTURE argument DERIVE addr FIELD text str FIELD term bool ;STRUCTURE

\ A variable's name, or `_:` and a blank node's label or `#` and number,
\ and its frame: 0 for the query's own, a subquery's serial for its own.
STRUCTURE name DERIVE addr FIELD text str FIELD frame n ;STRUCTURE

\ shape is a shape's index for a native column of it, else GENERIC or
\ KNOWN; term is a KNOWN term's text; copies is the bytes of the expressions
\ PostgreSQL copies for the derived tables' columns in sql; nullable, that
\ sql is a column that may hold NULL which no condition of its relation's
\ WHERE drops yet.
STRUCTURE binding DERIVE addr
   FIELD var n
   FIELD shape n
   FIELD sql str
   FIELD term str
   FIELD always bool
   FIELD copies n
   FIELD nullable bool
;STRUCTURE

\ A row the FROM reads once under the alias: of the FROM item `from`, by
\ a unique column slot, for the subject node.
STRUCTURE subject-row DERIVE addr
   FIELD node n
   FIELD from str
   FIELD slot slot
   FIELD alias str
;STRUCTURE

\ branches are a union's when its solutions are the relation's, else empty.
STRUCTURE relation DERIVE addr
   FIELD from ^VEC:vector<u8>
   FIELD where ^VEC:vector<u8>
   FIELD bindings ^VEC:vector<binding>
   FIELD rows ^VEC:vector<subject-row>
   FIELD empty bool
   FIELD branches ^VEC:vector<relation>
;STRUCTURE

\ One way an item matches: a source's index, or -1 for a derived table,
\ and the binding each variable there gets.
STRUCTURE choice DERIVE addr
   FIELD source n
   FIELD alive bool
   FIELD kinds ^VEC:vector<binding>
;STRUCTURE

\ An item of a group: a triple's or a BIND's node, or zero and a derived
\ table with its columns.
STRUCTURE item DERIVE addr
   FIELD node n
   \ Zero for a joined item, else N-OPTIONAL, N-MINUS, N-BIND or N-PATH.
   FIELD mode n
   FIELD table str
   FIELD columns ^VEC:vector<binding>
   FIELD filters ^VEC:vector<n>  \ an OPTIONAL's
   FIELD choices ^VEC:vector<choice>
   FIELD graph str  \ the active graph's term
   FIELD union bool  \ whether UNION? holds of it once pruned
   FIELD branches ^VEC:vector<relation>  \ a union's, else empty
;STRUCTURE

\ An aggregate call node and its SQL: the aggregate inside the grouped
\ query, its column above it; always when no group leaves it unbound.
\ native is the shape whose column the call reads natively over the
\ grouped solutions, else -1.
STRUCTURE aggregate DERIVE addr
   FIELD node n
   FIELD sql str
   FIELD always bool
   FIELD native n
;STRUCTURE

\ What a term's text says: whether the term is an IRI and, if not, its
\ datatype IRI, decoded.
STRUCTURE known-term DERIVE addr FIELD uri bool FIELD datatype str ;STRUCTURE

\ A constant node's text, empty before it is written, and its place in
\ the plan's known terms plus 1, 0 before it is looked up.
STRUCTURE node-term DERIVE addr FIELD text str FIELD place n ;STRUCTURE

\ An active graph's every subject and object: the statement's CTE name
\ of them, empty when no source is in the graph, and the form of its k.
STRUCTURE node-set DERIVE addr
   FIELD graph str
   FIELD name str
   FIELD kind n
;STRUCTURE

\ terms and strings map a text to its parameter's number. graph is the
\ active graph's text, empty for the default graph; frame is the
\ variables' frame. An expression is written to out and reads the
\ bindings in scope and the aggregates. outer is the substitution, empty
\ outside EXISTS. ctes is the statement's CTEs, comma-separated, each
\ reading only those before it. MOST bounds reads.
\ places is a MAP map from a text to its place in known; nodes is by node
\ index; sets holds the node CTEs written, one per active graph.
STRUCTURE plan DERIVE addr
   FIELD syntax ^syntax
   FIELD store ^store
   FIELD arguments ^VEC:vector<argument>
   FIELD terms ^n
   FIELD strings ^n
   FIELD variables ^VEC:vector<name>
   FIELD reads n
   FIELD serial n
   FIELD frame n
   FIELD graph str
   FIELD out ^VEC:vector<u8>
   FIELD scope ^VEC:vector<binding>
   FIELD outer ^VEC:vector<binding>
   FIELD ctes ^VEC:vector<u8>
   FIELD aggregates ^VEC:vector<aggregate>
   FIELD known ^VEC:vector<known-term>
   FIELD places ^n
   FIELD nodes ^VEC:vector<node-term>
   FIELD sets ^VEC:vector<node-set>
;STRUCTURE

defer SQL-EXPRESSION ( ^plan n -- )
\ The expression as an SQL boolean, its effective boolean value.
defer SQL-TEST ( ^plan n -- )
\ A group's `{ SELECT ... }`, the node, as a joined item.
defer SUBQUERY-ITEM ( ^plan ^VEC:vector<item> n -- )
\ A group's relation before its FILTERs, and the FILTER nodes.
defer GROUP-SQL ( ^plan n -- ^relation ^VEC:vector<n> )
\ An EXISTS or NOT EXISTS call as an SQL boolean.
defer EXISTS-SQL ( ^plan n -- )

: TREE ( ^plan -- ^syntax ) PLAN-SYNTAX @ ;

: AL ( ^plan -- ^ALLOC:allocator ) TREE SYNTAX-ARENA @ ARENA:ALLOCATOR ;

: NEW-SQL ( ^plan -- ^VEC:vector<u8> ) AL 0 [type] u8 VEC:NEW ;

: +SQL ( ^VEC:vector<u8> str -- ) {: v:^ s:str :} s v VEC:APPEND ;

: +INT ( ^VEC:vector<u8> n -- ) FMT:INT$ +SQL ;

\ Appends the separator unless the text is empty.
: +SEP ( ^VEC:vector<u8> str -- )
   {: v:^ a:str :}
   v VEC:COUNT 0<> if v a +SQL then ;

: SQL+ ( ^plan str -- ) {: plan a:str :} plan PLAN-OUT @ a +SQL ;

: WRITER ( ^plan -- ^JSON:writer )
   TREE SYNTAX-ARENA @ NEW-WRITER ;

: TYPE$ ( ^syntax n -- str )
   NODE-K N-IRI = if s" uri" else s" literal" then ;

: WRITE-TERM ( ^JSON:writer ^syntax n -- ^JSON:writer )
   {: writer state term:n :}
   writer state term TYPE$ state term TEXT$ TERM-START
   state term NODE-B@ dup 0<> if
      {: language:n :}
      s" xml:lang" state language TEXT$ +TERM-MEMBER
   else drop state term NODE-A@ dup 0<> if
      state swap TEXT$ {: dtype:str :}
      dtype s" http://www.w3.org/2001/XMLSchema#string" STR:= 0= if
         s" datatype" dtype +TERM-MEMBER
      then
   else drop then then JSON:WRITE-OBJECT-END ;

\ The literal's datatype IRI, decoded.
: DATATYPE$ ( JSON:value ^ARENA:arena -- str )
   {: v arena :}
   v s" xml:lang" HAS? if
      s" http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" exit
   then
   v s" datatype" JSON:FIND MATCH option
      none OF s" http://www.w3.org/2001/XMLSchema#string" ENDOF
      some OF arena DECODED$ ENDOF
   ;MATCH ;

: READ-KNOWN ( str ^known-term ^ARENA:arena -- )
   {: text:str term:^ arena :}
   text JSON:PARSE {: v :}
   v s" type" JSON:GET s" uri" JSON:STRING= {: uri:bool :}
   uri term KNOWN-TERM-URI !
   uri if exit then
   v arena DATATYPE$ term KNOWN-TERM-DATATYPE ! ;

\ The text's place in known, added on its first use; the text lives as
\ long as the plan.
: TERM-PLACE ( ^plan str -- n )
   {: plan text:str :}
   plan TREE SYNTAX-ARENA @ {: arena :}
   plan PLAN-PLACES arena MAP:ROOM
   plan PLAN-PLACES @ {: places:^ :}
   places text MAP:GET MATCH option
      none OF ENDOF
      some OF exit ENDOF
   ;MATCH
   plan PLAN-KNOWN @ {: known:^ :}
   known VEC:COUNT places text MAP:SET
   text known VEC:PUSH arena READ-KNOWN
   known VEC:COUNT 1- ;

: TERM@ ( ^plan n -- ^known-term )
   {: plan k:n :} plan PLAN-KNOWN @ k VEC:AT ;

: NODE-TERM@ ( ^plan n -- ^node-term )
   {: plan node:n :}
   plan PLAN-NODES @ {: nodes:^ :}
   nodes node 1+ VEC:ENSURE
   nodes node VEC:AT ;

\ The IRI or literal node's text.
: TERM-JSON ( ^plan n -- str )
   {: plan node:n :}
   plan node NODE-TERM@ {: row:^ :}
   row NODE-TERM-TEXT @ {: text:str :}
   text STR:LEN 0<> if text exit then
   plan WRITER plan TREE node WRITE-TERM JSON:WRITTEN$
   dup row NODE-TERM-TEXT ! ;

\ The IRI or literal node's term.
: NODE-KNOWN ( ^plan n -- ^known-term )
   {: plan node:n :}
   plan node NODE-TERM@ NODE-TERM-PLACE {: place:^ :}
   place @ 0= if plan plan node TERM-JSON TERM-PLACE 1+ place ! then
   plan place @ 1- TERM@ ;

\ Rejects the whole query: what it may have at most, and the fix.
: TOO-MANY ( ^plan str str -- )
   {: plan most:str fix:str :}
   plan TREE dup SYNTAX-TOKENS @ VEC:COUNT most fix REJECT-TOKEN ;

\ The text's parameter `$k`, added on its first use as a term or as a
\ string. The text must live as long as the plan.
: ARGUMENT ( ^plan str bool -- str )
   {: plan text:str term:bool :}
   term if plan PLAN-TERMS else plan PLAN-STRINGS then {: mp:^ :}
   mp plan TREE SYNTAX-ARENA @ MAP:ROOM
   mp @ text MAP:GET MATCH option
      none OF
         plan PLAN-ARGUMENTS @ VEC:COUNT PARAMETERS = if
            plan s" at most 65535 constants" s" split the query" TOO-MANY
         then
         plan PLAN-ARGUMENTS @ VEC:PUSH {: a:^ :}
         text a ARGUMENT-TEXT ! term a ARGUMENT-TERM !
         plan PLAN-ARGUMENTS @ VEC:COUNT dup mp @ text MAP:SET
      ENDOF
      some OF ENDOF
   ;MATCH {: k:n :}
   plan NEW-SQL {: v:^ :}
   v s" $" +SQL v k +INT v VEC:BYTES ;

\ The JSON term's parameter, JSONB.
: TERM-ARG ( ^plan str -- str ) true ARGUMENT ;

\ Appends the text inside an E'' literal, each `\` and `'` doubled: `\'`
\ is rejected under backslash_quote off and unsafe in a multibyte encoding.
: +ESCAPED ( ^VEC:vector<u8> str -- )
   {: v:^ t:str :}
   t STR:LEN 0 ?do
      t STR:DATA i + c@ {: b:n :}
      b [char] \ = b [char] ' = or if b v VEC:PUSH c! then
      b v VEC:PUSH c!
   loop ;

\ The description's JSON term as a literal, JSONB.
: TERM-LITERAL ( ^plan str -- str )
   {: plan t:str :}
   plan NEW-SQL {: v:^ :}
   v s" E'" +SQL v t +ESCAPED v s" '::jsonb" +SQL v VEC:BYTES ;

\ The string's parameter, text.
: TEXT-ARG ( ^plan str -- str ) false ARGUMENT ;

: STRING+ ( ^plan str -- )
   {: plan a:str :} plan plan a TEXT-ARG SQL+ ;

: NUMBER-ARG ( ^plan n -- str )
   {: plan value:n :}
   plan plan TREE s" " value FMT:INT$ JOIN$ TEXT-ARG ;

: VARIABLE? ( ^syntax n -- bool )
   NODE-K dup N-VAR = swap N-BNODE = or ;

\ Whether the node is a path of the operator.
: OPERATOR? ( ^syntax n str -- bool )
   {: state node:n op:str :}
   state node NODE-K N-PATH <> if false exit then
   state node TEXT$ op STR:= ;

: NEGATED? ( ^syntax n -- bool ) s" !" OPERATOR? ;

: VAR-NAME ( ^plan n -- str )
   {: plan node:n :}
   plan TREE {: state :}
   state node NODE-K N-VAR = if state node TEXT$ exit then
   state node NODE NODE-FLAGS @ F-GENERATED and 0<> if
      state s" _:#" state node NODE NODE-VALUE @ FMT:INT$ JOIN$ exit
   then state s" _:" state node TEXT$ JOIN$ ;

\ Counts one more read.
: +READ ( ^plan -- )
   {: plan :}
   1 plan PLAN-READS +!
   plan PLAN-READS @ MOST > if
      plan s" at most 512 patterns, sources they match and variables"
         s" split the query, or state predicates so that fewer sources match"
         TOO-MANY
   then ;

: NAMED? ( ^name str n -- bool )
   {: v:^ name:str frame:n :}
   v NAME-FRAME @ frame = v NAME-TEXT @ name STR:= and ;

\ The variable of the name in the frame.
: VAR-ID ( ^plan str n -- n )
   {: plan name:str frame:n :}
   plan PLAN-VARIABLES @ {: vars:^ :}
   vars VEC:COUNT 0 ?do
      vars i VEC:AT name frame NAMED? if i unloop exit then
   loop
   plan +READ
   vars VEC:PUSH {: v:^ :}
   name v NAME-TEXT ! frame v NAME-FRAME !
   vars VEC:COUNT 1- ;

: VAR ( ^plan n -- n )
   {: plan node:n :}
   plan plan node VAR-NAME plan PLAN-FRAME @ VAR-ID ;

\ `vK`, variable K's column in a derived table.
: COLUMN$ ( ^plan n -- str )
   {: plan var:n :}
   plan NEW-SQL {: v:^ :} v s" v" +SQL v var +INT v VEC:BYTES ;

: COLUMN-OF ( ^plan str n -- str )
   {: plan alias:str var:n :}
   plan NEW-SQL {: v:^ :}
   v alias +SQL v s" ." +SQL v plan var COLUMN$ +SQL v VEC:BYTES ;

: NEXT-ALIAS ( ^plan -- str )
   {: plan :}
   plan +READ
   1 plan PLAN-SERIAL +!
   plan NEW-SQL {: v:^ :}
   v s" t" +SQL v plan PLAN-SERIAL @ +INT v VEC:BYTES ;

: NEW-BINDINGS ( ^plan -- ^VEC:vector<binding> )
   AL 0 [type] binding VEC:NEW ;

: INIT-RELATION ( ^plan ^relation -- )
   {: plan r:^ :}
   plan NEW-SQL r RELATION-FROM !
   plan NEW-SQL r RELATION-WHERE !
   plan NEW-BINDINGS r RELATION-BINDINGS !
   plan AL 0 [type] subject-row VEC:NEW r RELATION-ROWS !
   plan AL 0 [type] relation VEC:NEW r RELATION-BRANCHES ! ;

: NEW-RELATION ( ^plan -- ^relation )
   {: plan :}
   plan AL [type] relation ALLOC:NEW {: r:^ :}
   plan r INIT-RELATION r ;

: BIND ( ^VEC:vector<binding> n n str str -- )
   {: list:^ var:n shape:n sql:str term:str :}
   list VEC:PUSH {: b:^ :}
   var b BINDING-VAR ! shape b BINDING-SHAPE !
   sql b BINDING-SQL ! term b BINDING-TERM !
   true b BINDING-ALWAYS ! 0 b BINDING-COPIES !
   false b BINDING-NULLABLE ! ;

\ The index of variable n's first binding in the list, or -1.
: FIND-VAR ( ^VEC:vector<binding> n -- n )
   {: list:^ var:n :}
   list VEC:COUNT 0 ?do
      list i VEC:AT BINDING-VAR @ var = if i unloop exit then
   loop -1 ;

: SHAPE@ ( ^plan n -- ^shape )
   {: plan k:n :} plan PLAN-STORE @ STORE-SHAPES @ k VEC:AT ;

: SOURCE@ ( ^plan n -- ^source )
   {: plan k:n :} plan PLAN-STORE @ STORE-SOURCES @ k VEC:AT ;

\ Appends `f(a)`.
: +CALL ( ^VEC:vector<u8> str str -- )
   {: v:^ f:str a:str :}
   v f +SQL v s" (" +SQL v a +SQL v s" )" +SQL ;

\ The binding's value as a JSONB term.
: TERM-SQL ( ^plan ^binding -- str )
   {: plan b:^ :}
   b BINDING-SHAPE @ {: shape:n :}
   shape 0< if b BINDING-SQL @ exit then
   plan NEW-SQL {: v:^ :}
   v plan shape SHAPE@ SHAPE-OUT @ b BINDING-SQL @ +CALL v VEC:BYTES ;

\ The shape's parent, or the shape itself when it is within none.
: ROOT ( ^plan n -- n ) SHAPE@ SHAPE-ROOT @ ;

\ Whether x and y are shapes of one root.
: ONE-ROOT? ( ^plan n n -- bool )
   {: plan x:n y:n :}
   x 0< y 0< or if false exit then
   plan x ROOT plan y ROOT = ;

\ The form of x and y, shapes of one root: their shape, else the root.
: UNITED ( ^plan n n -- n )
   {: plan x:n y:n :}
   x y = if x else plan x ROOT then ;

: LISTED$? ( ^VEC:vector<datatype> str -- bool )
   {: list:^ w:str :}
   list VEC:COUNT 0 ?do
      list i VEC:AT DATATYPE-IRI @ w STR:= if true unloop exit then
   loop false ;

\ Whether shape k holds the term.
: HOLDS? ( ^plan n ^known-term -- bool )
   {: plan k:n term:^ :}
   plan k SHAPE@ SHAPE-HOLDS @ MATCH holds
      any OF true ENDOF
      uris OF term KNOWN-TERM-URI @ ENDOF
      literals OF {: list:^ :}
         term KNOWN-TERM-URI @ if false
         else list term KNOWN-TERM-DATATYPE @ LISTED$? then
      ENDOF
   ;MATCH ;

\ Whether shape k holds the KNOWN binding's term.
: HOLDS-KNOWN? ( ^plan n ^binding -- bool )
   {: plan k:n b:^ :}
   plan k plan plan b BINDING-TERM @ TERM-PLACE TERM@ HOLDS? ;

\ Whether two bindings of one variable can hold one term: two terms that
\ are one, a shape and a term it holds, a shape and itself or its root;
\ parts of one root hold disjoint terms.
: COMPATIBLE? ( ^plan ^binding ^binding -- bool )
   {: plan a:^ b:^ :}
   a BINDING-SHAPE @ {: x:n :} b BINDING-SHAPE @ {: y:n :}
   x GENERIC = y GENERIC = or if true exit then
   x KNOWN = y KNOWN = and if
      a BINDING-TERM @ b BINDING-TERM @ STR:= exit
   then
   x KNOWN = if plan y a HOLDS-KNOWN? exit then
   y KNOWN = if plan x b HOLDS-KNOWN? exit then
   x y = if true exit then
   plan x y ONE-ROOT? 0= if false exit then
   plan x ROOT {: r:n :} x r = y r = or ;

\ A binding's rank as an anchor: native, then KNOWN, then GENERIC.
: RANK ( ^binding -- n )
   BINDING-SHAPE @ {: shape:n :}
   shape -1 > if 2 exit then
   shape KNOWN = if 1 else 0 then ;

: LAST ( ^VEC:vector<binding> -- ^binding ) dup VEC:COUNT 1- VEC:AT ;

: COPY-BINDING ( ^binding ^VEC:vector<binding> -- )
   {: b:^ list:^ :}
   list b BINDING-VAR @ b BINDING-SHAPE @ b BINDING-SQL @
   b BINDING-TERM @ BIND
   b BINDING-COPIES @ list LAST BINDING-COPIES !
   b BINDING-ALWAYS @ list LAST BINDING-ALWAYS ! ;

: COPY-SCOPE ( ^plan ^VEC:vector<binding> -- ^VEC:vector<binding> )
   {: plan scope:^ :}
   plan NEW-BINDINGS {: copy:^ :}
   scope VEC:COUNT 0 ?do scope i VEC:AT copy COPY-BINDING loop copy ;

\ The condition that bindings a and o of one variable hold one term, the
\ higher-ranked on the left: empty when they are one, `false` when no term
\ fits both. Two KNOWN bindings that can meet are one term.
: SAME-SQL ( ^plan ^binding ^binding -- str )
   {: plan a:^ o:^ :}
   plan a o COMPATIBLE? 0= if s" false" exit then
   a BINDING-SQL @ o BINDING-SQL @ STR:= if s" " exit then
   a BINDING-SHAPE @ KNOWN = o BINDING-SHAPE @ KNOWN = and if
      s" " exit
   then
   a RANK o RANK < if o a else a o then {: x:^ y:^ :}
   x BINDING-SHAPE @ {: shape:n :}
   plan NEW-SQL {: v:^ :}
   v x BINDING-SQL @ +SQL v s"  = " +SQL
   shape -1 > y BINDING-SHAPE @ 0< and if
      v plan shape SHAPE@ SHAPE-IN @ y BINDING-SQL @ +CALL
   else v y BINDING-SQL @ +SQL then v VEC:BYTES ;

: CONSTANT-VALUE ( ^plan n -- ^binding )
   {: plan node:n :}
   plan node TERM-JSON {: j:str :}
   plan NEW-BINDINGS {: tmp:^ :}
   tmp -1 KNOWN plan j TERM-ARG j BIND tmp LAST ;

: +AND ( ^VEC:vector<u8> str -- )
   {: where:^ a:str :}
   where s"  AND " +SEP where a +SQL ;

: +NULL-TEST ( ^VEC:vector<u8> str -- )
   {: where:^ sql:str :}
   where sql +AND where s"  IS NOT NULL" +SQL ;

: +NULL-OR ( ^VEC:vector<u8> ^binding -- )
   {: v:^ b:^ :}
   b BINDING-ALWAYS @ if exit then
   v b BINDING-SQL @ +SQL v s"  IS NULL OR " +SQL ;

\ Appends to where that s and o meet: one term, or a maybe one NULL.
\ Answers whether that is a strict `=`, which drops a NULL of either.
: +MEETS ( ^plan ^binding ^binding ^VEC:vector<u8> -- bool )
   {: plan s:^ o:^ where:^ :}
   plan s o SAME-SQL {: eq:str :}
   eq STR:LEN 0= if false exit then
   s BINDING-ALWAYS @ o BINDING-ALWAYS @ and if
      where eq +AND true exit
   then
   where s"  AND " +SEP where s" (" +SQL
   where s +NULL-OR where o +NULL-OR where eq +SQL where s" )" +SQL
   false ;

: TAKE ( ^binding ^binding -- )
   {: s:^ o:^ :}
   o BINDING-SHAPE @ s BINDING-SHAPE !
   o BINDING-SQL @ s BINDING-SQL !
   o BINDING-TERM @ s BINDING-TERM !
   o BINDING-COPIES @ s BINDING-COPIES ! ;

\ s becomes the first bound of maybe bindings s and o: native where their
\ shapes share a root, of the root where they differ, else a term.
: COALESCED ( ^plan ^binding ^binding -- )
   {: plan s:^ o:^ :}
   s BINDING-SHAPE @ {: x:n :} o BINDING-SHAPE @ {: y:n :}
   plan NEW-SQL {: v:^ :}
   v s" COALESCE(" +SQL
   plan x y ONE-ROOT? if
      v s BINDING-SQL @ +SQL v s" ," +SQL v o BINDING-SQL @ +SQL
      plan x y UNITED s BINDING-SHAPE !
   else
      v plan s TERM-SQL +SQL v s" ," +SQL v plan o TERM-SQL +SQL
      GENERIC s BINDING-SHAPE !
   then
   v s" )" +SQL v VEC:BYTES s BINDING-SQL !
   o BINDING-COPIES @ s BINDING-COPIES +! ;

\ s becomes its variable's value once o joins it; under an OPTIONAL o's
\ side may be missing.
: MERGE ( ^plan ^binding ^binding bool -- )
   {: plan s:^ o:^ optional:bool :}
   o BINDING-ALWAYS @ optional 0= and {: sure:bool :}
   s BINDING-ALWAYS @ if
      sure o RANK s RANK > and if s o TAKE then exit
   then
   sure if s o TAKE true s BINDING-ALWAYS ! exit then
   plan s o COALESCED ;

\ Joins binding o into the scope, its condition to where. A first o's
\ copy is nullable as o is; a strict `=` leaves the scope's binding not
\ nullable, else a nullable o's null test goes to where at once, unless
\ o reads the scope binding's column, which that binding's flag covers.
\ An OPTIONAL's ON drops no row of the scope, so it changes no flag.
: MEET ( ^plan ^VEC:vector<binding> ^binding ^VEC:vector<u8> bool -- )
   {: plan scope:^ o:^ where:^ optional:bool :}
   scope o BINDING-VAR @ FIND-VAR {: k:n :}
   k 0< if
      o scope COPY-BINDING
      optional if false scope LAST BINDING-ALWAYS ! exit then
      o BINDING-NULLABLE @ scope LAST BINDING-NULLABLE ! exit
   then
   scope k VEC:AT {: s:^ :}
   plan s o where +MEETS {: strict:bool :}
   s BINDING-SQL @ o BINDING-SQL @ STR:= {: same:bool :}
   o BINDING-NULLABLE @ same 0= and {: open:bool :}
   plan s o optional MERGE
   optional if exit then
   strict if false s BINDING-NULLABLE ! exit then
   open if where o BINDING-SQL @ +NULL-TEST then ;

: MEET-ALL ( ^plan ^VEC:vector<binding> ^VEC:vector<binding>
      ^VEC:vector<u8> bool -- )
   {: plan scope:^ list:^ where:^ optional:bool :}
   list VEC:COUNT 0 ?do
      plan scope list i VEC:AT where optional MEET
   loop ;

\ The occurrences joined into one binding per variable, their conditions
\ to where.
: JOINS ( ^plan ^VEC:vector<binding> ^VEC:vector<u8>
      -- ^VEC:vector<binding> )
   {: plan list:^ where:^ :}
   plan NEW-BINDINGS {: scope:^ :}
   plan scope list where false MEET-ALL scope ;

: SLOT@ ( ^source n -- slot )
   {: src:^ k:n :}
   k 0= if src SOURCE-S @ exit then
   k 1 = if src SOURCE-P @ exit then
   src SOURCE-O @ ;

\ The triple's subject, predicate or object.
: PART ( ^syntax n n -- n )
   {: state triple:n k:n :}
   k 0= if state triple NODE-A@ exit then
   k 1 = if state triple NODE-B@ exit then
   state triple NODE-C@ ;

\ The column's shape and no term, or KNOWN and the term's text.
: SLOT-KIND ( slot -- n str )
   MATCH slot
      column OF {: c:str shape:n k u:bool nn:bool :} shape s" " ENDOF
      term OF {: t:str :} KNOWN t ENDOF
   ;MATCH ;

\ The slot under the alias: its column, or its term's reference.
: SLOT-SQL ( ^plan slot str -- str )
   {: plan sl alias:str :}
   sl MATCH slot
      column OF {: c:str shape:n k u:bool nn:bool :}
         plan NEW-SQL {: v:^ :}
         v alias +SQL v s" ." +SQL v c +SQL v VEC:BYTES
      ENDOF
      term OF {: t:str :} plan t TERM-LITERAL ENDOF
   ;MATCH ;

: TERM-SAME? ( ^plan n str -- bool )
   {: plan node:n t:str :} plan node TERM-JSON t STR:= ;

\ Whether a member of the list of IRI nodes is the term.
: LISTED? ( ^plan n str -- bool )
   {: plan list:n t:str :}
   list begin dup 0<> while
      {: each:n :}
      plan plan TREE each NODE-A@ t TERM-SAME? if true exit then
      plan TREE each NODE-B@
   repeat drop false ;

\ Whether the slot holds a predicate outside the list: any column, or a
\ term not in it.
: OUTSIDE? ( ^plan n slot -- bool )
   {: plan list:n sl :}
   sl MATCH slot
      column OF 2drop 2drop drop true ENDOF
      term OF {: t:str :} plan list t LISTED? 0= ENDOF
   ;MATCH ;

\ The IRIs of !(...)'s members of one direction, `^p` giving p.
: MEMBERS ( ^syntax n bool -- n )
   {: state list:n inverse:bool :}
   0 list begin dup 0<> while
      {: each:n :}
      state each NODE-A@ {: m:n :}
      state m s" ^" OPERATOR? inverse xor 0= if
         inverse if state m NODE-A@ else m then
         state swap rot CONS
      then
      state each NODE-B@
   repeat drop ;

\ The key set the key names.
: KEY-SET@ ( ^plan ^key -- ^key-set )
   {: plan k:^ :}
   plan PLAN-STORE @ STORE-KEYS @ k KEY-SET @ VEC:AT ;

\ Whether the node is an IRI the key's set lists.
: LISTED-KEY? ( ^plan n ^key -- bool )
   {: plan node:n k:^ :}
   plan TREE {: state :}
   state node NODE-K N-IRI <> if false exit then
   plan k KEY-SET@ KEY-SET-PLACES @ state node TEXT$ MAP:HAS? ;

\ Whether a constant of the pattern fits the slot: a column whose shape
\ holds it and whose key set, if it has a key, lists it; or the slot's
\ own term; a predicate !(...) of IRIs, a slot outside them.
: FITS? ( ^plan n slot -- bool )
   {: plan node:n sl :}
   plan TREE node NEGATED? if
      plan plan TREE node NODE-A@ sl OUTSIDE? exit
   then
   sl MATCH slot
      column OF {: c:str shape:n k u:bool nn:bool :}
         plan shape plan node NODE-KNOWN HOLDS?
         k MATCH nullable
            none OF ENDOF
            some OF {: key:^ :} plan node key LISTED-KEY? and ENDOF
         ;MATCH
      ENDOF
      term OF {: t:str :} plan node t TERM-SAME? ENDOF
   ;MATCH ;

: GRAPH-FITS? ( ^plan ^source -- bool )
   {: plan src:^ :} src SOURCE-GRAPH @ plan PLAN-GRAPH @ STR:= ;

\ Whether every occurrence of b's variable in the list can meet b.
: MEETS? ( ^plan ^VEC:vector<binding> ^binding -- bool )
   {: plan list:^ b:^ :}
   list VEC:COUNT 0 ?do
      list i VEC:AT {: o:^ :}
      o BINDING-VAR @ b BINDING-VAR @ = if
         plan o b COMPATIBLE? 0= if false unloop exit then
      then
   loop true ;

\ Whether a variable repeated in the list meets itself.
: CONSISTENT? ( ^plan ^VEC:vector<binding> -- bool )
   {: plan list:^ :}
   list VEC:COUNT 0 ?do
      plan list list i VEC:AT MEETS? 0= if false unloop exit then
   loop true ;

\ Whether the substitution binds b's variable always to a term b cannot
\ hold.
: CLASHES? ( ^plan ^binding -- bool )
   {: plan b:^ :}
   plan PLAN-OUTER @ {: outer:^ :}
   outer b BINDING-VAR @ FIND-VAR {: k:n :}
   k 0< if false exit then
   outer k VEC:AT {: o:^ :}
   o BINDING-ALWAYS @ plan o b COMPATIBLE? 0= and ;

: SUBSTITUTABLE? ( ^plan ^VEC:vector<binding> -- bool )
   {: plan list:^ :}
   list VEC:COUNT 0 ?do
      plan list i VEC:AT CLASHES? if false unloop exit then
   loop true ;

: NEW-CHOICE ( ^plan ^item n -- ^choice )
   {: plan it:^ k:n :}
   it ITEM-CHOICES @ VEC:PUSH {: c:^ :}
   k c CHOICE-SOURCE ! true c CHOICE-ALIVE !
   plan NEW-BINDINGS c CHOICE-KINDS ! c ;

\ Adds source k as the triple's choice when it fits.
: TRIPLE-CHOICE ( ^plan ^item n -- )
   {: plan it:^ k:n :}
   plan k SOURCE@ {: src:^ :}
   plan src GRAPH-FITS? 0= if exit then
   plan TREE {: state :}
   it ITEM-NODE @ {: triple:n :}
   3 0 do
      state triple i PART {: node:n :}
      state node VARIABLE? 0= if
         plan node src i SLOT@ FITS? 0= if unloop exit then
      then
   loop
   plan it k NEW-CHOICE CHOICE-KINDS @ {: kinds:^ :}
   3 0 do
      state triple i PART {: node:n :}
      state node VARIABLE? if
         src i SLOT@ SLOT-KIND {: shape:n t:str :}
         kinds plan node VAR shape s" " t BIND
      then
   loop
   plan kinds CONSISTENT? plan kinds SUBSTITUTABLE? and
   it ITEM-CHOICES @ dup VEC:COUNT 1- VEC:AT CHOICE-ALIVE ! ;

: NEW-ITEM ( ^plan ^VEC:vector<item> n -- ^item )
   {: plan items:^ node:n :}
   plan +READ
   items VEC:PUSH {: it:^ :}
   node it ITEM-NODE ! 0 it ITEM-MODE !
   plan PLAN-GRAPH @ it ITEM-GRAPH !
   plan AL 0 [type] relation VEC:NEW it ITEM-BRANCHES !
   plan AL 0 [type] choice VEC:NEW it ITEM-CHOICES ! it ;

: TRY-EACH ( ^plan ^item ^VEC:vector<n> -- )
   {: plan it:^ list:^ :}
   list VEC:COUNT 0 ?do plan it list i VEC:AT @ TRIPLE-CHOICE loop ;

\ Whether the merge of a and b takes a's k-th record before b's m-th.
: FIRST? ( ^VEC:vector<n> n ^VEC:vector<n> n -- bool )
   {: a:^ k:n b:^ m:n :}
   m b VEC:COUNT = if true exit then
   k a VEC:COUNT = if false exit then
   a k VEC:AT @ b m VEC:AT @ < ;

\ Tries the records of a and b, two lists in record order, merged.
: TRY-MERGED ( ^plan ^item ^VEC:vector<n> ^VEC:vector<n> -- )
   {: plan it:^ a:^ b:^ :}
   0 a VEC:COUNT b VEC:COUNT + 0 ?do
      {: k:n :}
      a k b i k - FIRST? if
         plan it a k VEC:AT @ TRIPLE-CHOICE k 1+
      else
         plan it b i k - VEC:AT @ TRIPLE-CHOICE k
      then
   loop drop ;

\ Tries the records whose `p` can be the IRI: its term's and the columns'.
: TRY-IRI ( ^plan ^item n -- )
   {: plan it:^ node:n :}
   plan PLAN-STORE @ {: st:^ :}
   st STORE-COLUMNS @ {: columns:^ :}
   st STORE-PLACES @ plan node TERM-JSON MAP:GET MATCH option
      none OF plan it columns TRY-EACH ENDOF
      some OF {: k:n :}
         st STORE-PREDICATES @ k VEC:AT PREDICATE-RECORDS @ {: named:^ :}
         plan it named columns TRY-MERGED
      ENDOF
   ;MATCH ;

: TRIPLE-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ triple:n :}
   plan items triple NEW-ITEM {: it:^ :}
   plan TREE {: state :}
   state triple NODE-B@ {: p:n :}
   state p NODE-K N-IRI = if plan it p TRY-IRI exit then
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 0 ?do
      plan it i TRIPLE-CHOICE
   loop ;

\ A joined derived table with its columns, the item's one choice.
: CHILD-ITEM ( ^plan ^VEC:vector<item> str ^VEC:vector<binding> bool -- )
   {: plan items:^ t:str cols:^ empty:bool :}
   plan items 0 NEW-ITEM {: it:^ :}
   t it ITEM-TABLE ! cols it ITEM-COLUMNS !
   plan it -1 NEW-CHOICE {: c:^ :}
   empty 0= c CHOICE-ALIVE !
   cols c CHOICE-KINDS ! ;

\ Whether the item's choices bind the variable always.
: BINDS? ( ^item n -- bool )
   {: it:^ var:n :}
   it ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0= if false exit then
   cs 0 VEC:AT CHOICE-KINDS @ {: kinds:^ :}
   kinds var FIND-VAR {: k:n :}
   k 0< if false exit then
   kinds k VEC:AT BINDING-ALWAYS @ ;

: LIVE-COUNT ( ^item -- n )
   ITEM-CHOICES @ {: cs:^ :}
   0 cs VEC:COUNT 0 ?do
      cs i VEC:AT CHOICE-ALIVE @ if 1+ then
   loop ;

: INDEXES ( ^plan -- ^VEC:vector<n> ) AL 0 [type] n VEC:NEW ;

: INDEXED? ( ^VEC:vector<n> n -- bool )
   {: list:^ k:n :}
   list VEC:COUNT 0 ?do
      list i VEC:AT @ k = if true unloop exit then
   loop false ;

\ A binding as COMPATIBLE? tells bindings apart. answers holds, by a class
\ up to it, whether the two are compatible: 0 unasked, 1 no, 2 yes.
STRUCTURE class DERIVE addr
   FIELD binding ^binding
   FIELD answers ^VEC:vector<n>
;STRUCTURE

\ A variable and its sites.
STRUCTURE domain DERIVE addr
   FIELD var n
   FIELD sites ^VEC:vector<n>
   FIELD queued bool
;STRUCTURE

\ A place where an item binds a variable always, the kind'th binding of
\ each of its choices: its class, by choice, and each class's number of
\ live choices. first is the item's first site; an item's sites are
\ consecutive.
STRUCTURE site DERIVE addr
   FIELD item ^item
   FIELD kind n
   FIELD first n
   FIELD domain n
   FIELD classes ^VEC:vector<n>
   FIELD live ^VEC:vector<n>
;STRUCTURE

\ The queue's domains from head on are unswept.
STRUCTURE pruning DERIVE addr
   FIELD plan ^plan
   FIELD classes ^VEC:vector<class>
   FIELD sites ^VEC:vector<site>
   FIELD domains ^VEC:vector<domain>
   FIELD queue ^VEC:vector<n>
   FIELD head n
;STRUCTURE

: CLASS@ ( ^pruning n -- ^class )
   {: w:^ k:n :} w PRUNING-CLASSES @ k VEC:AT ;

: SITE@ ( ^pruning n -- ^site )
   {: w:^ k:n :} w PRUNING-SITES @ k VEC:AT ;

: DOMAIN@ ( ^pruning n -- ^domain )
   {: w:^ k:n :} w PRUNING-DOMAINS @ k VEC:AT ;

: NEW-PRUNING ( ^plan -- ^pruning )
   {: plan :}
   plan AL {: al:^ :}
   al [type] pruning ALLOC:NEW {: w:^ :}
   plan w PRUNING-PLAN !
   al 0 [type] class VEC:NEW w PRUNING-CLASSES !
   al 0 [type] site VEC:NEW w PRUNING-SITES !
   al 0 [type] domain VEC:NEW w PRUNING-DOMAINS !
   plan INDEXES w PRUNING-QUEUE ! w ;

: OF-CLASS? ( ^binding ^class -- bool )
   {: b:^ c:^ :}
   c CLASS-BINDING @ {: o:^ :}
   o BINDING-SHAPE @ b BINDING-SHAPE @ <> if false exit then
   o BINDING-TERM @ b BINDING-TERM @ STR:= ;

\ The binding's class, new on its first use.
: CLASS-OF ( ^pruning ^binding -- n )
   {: w:^ b:^ :}
   w PRUNING-CLASSES @ {: cs:^ :}
   cs VEC:COUNT {: k:n :}
   k 0 ?do b cs i VEC:AT OF-CLASS? if i unloop exit then loop
   cs VEC:PUSH {: c:^ :}
   b c CLASS-BINDING !
   w PRUNING-PLAN @ AL cs VEC:COUNT [type] n VEC:NEW c CLASS-ANSWERS ! k ;

\ The index of the variable's domain, new on its first use.
: DOMAIN-OF ( ^pruning n -- n )
   {: w:^ var:n :}
   w PRUNING-DOMAINS @ {: ds:^ :}
   ds VEC:COUNT {: k:n :}
   k 0 ?do
      ds i VEC:AT DOMAIN-VAR @ var = if i unloop exit then
   loop
   ds VEC:PUSH {: d:^ :}
   var d DOMAIN-VAR !
   w PRUNING-PLAN @ INDEXES d DOMAIN-SITES ! k ;

: +SITE ( ^pruning ^item n n n -- )
   {: w:^ it:^ kind:n var:n first:n :}
   w PRUNING-SITES @ {: sites:^ :}
   w var DOMAIN-OF {: d:n :}
   sites VEC:COUNT w d DOMAIN@ DOMAIN-SITES @ VEC:PUSH !
   sites VEC:PUSH {: s:^ :}
   it s SITE-ITEM ! kind s SITE-KIND ! first s SITE-FIRST !
   d s SITE-DOMAIN !
   w PRUNING-PLAN @ INDEXES s SITE-CLASSES ! ;

: +SITES ( ^pruning ^item -- )
   {: w:^ it:^ :}
   it ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0= if exit then
   w PRUNING-SITES @ VEC:COUNT {: first:n :}
   cs 0 VEC:AT CHOICE-KINDS @ {: kinds:^ :}
   kinds VEC:COUNT 0 ?do
      kinds i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if w it i b BINDING-VAR @ first +SITE then
   loop ;

: SHARED? ( ^pruning ^site -- bool )
   {: w:^ s:^ :}
   w s SITE-DOMAIN @ DOMAIN@ DOMAIN-SITES @ VEC:COUNT 1 > ;

: CLASSIFY ( ^pruning ^site -- )
   {: w:^ s:^ :}
   s SITE-ITEM @ ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      w cs i VEC:AT CHOICE-KINDS @ s SITE-KIND @ VEC:AT CLASS-OF
      s SITE-CLASSES @ VEC:PUSH !
   loop ;

: COUNT-LIVE ( ^pruning ^site -- )
   {: w:^ s:^ :}
   w PRUNING-PLAN @ AL w PRUNING-CLASSES @ VEC:COUNT [type] n VEC:NEW
      s SITE-LIVE !
   s SITE-ITEM @ ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT CHOICE-ALIVE @ if
         1 s SITE-LIVE @ s SITE-CLASSES @ i VEC:AT @ VEC:AT +!
      then
   loop ;

: EACH-SHARED ( ^pruning [ ^pruning ^site -- ] -- )
   {: w:^ q :}
   w PRUNING-SITES @ VEC:COUNT 0 ?do
      w w i SITE@ SHARED? if w w i SITE@ q execute then
   loop ;

: ENQUEUE ( ^pruning n -- )
   {: w:^ k:n :}
   w k DOMAIN@ {: d:^ :}
   d DOMAIN-QUEUED @ if exit then
   true d DOMAIN-QUEUED ! k w PRUNING-QUEUE @ VEC:PUSH ! ;

\ Whether classes x and y are compatible, x not before y.
: ASK ( ^pruning n n -- bool )
   {: w:^ x:n y:n :}
   w x CLASS@ {: c:^ :}
   c CLASS-ANSWERS @ y VEC:AT {: kept:^ :}
   kept @ 0= if
      w PRUNING-PLAN @ c CLASS-BINDING @ w y CLASS@ CLASS-BINDING @
         COMPATIBLE? if 2 else 1 then kept !
   then
   kept @ 2 = ;

\ Whether class k is compatible with a live class of the site.
: SITE-SUPPORTS? ( ^pruning ^site n -- bool )
   {: w:^ s:^ k:n :}
   s SITE-LIVE @ {: live:^ :}
   live VEC:COUNT 0 ?do
      live i VEC:AT @ 0<> if
         w i k max i k min ASK if true unloop exit then
      then
   loop false ;

: SUPPORTED? ( ^pruning ^domain n -- bool )
   {: w:^ d:^ k:n :}
   d DOMAIN-SITES @ {: list:^ :}
   list VEC:COUNT 0 ?do
      w w list i VEC:AT @ SITE@ k SITE-SUPPORTS? 0= if
         false unloop exit
      then
   loop true ;

\ Counts choice ch out of the site.
: LOSE ( ^pruning ^site n -- )
   {: w:^ s:^ ch:n :}
   s SITE-LIVE @ s SITE-CLASSES @ ch VEC:AT @ VEC:AT {: live:^ :}
   -1 live +!
   live @ 0= if w s SITE-DOMAIN @ ENQUEUE then ;

\ Kills choice ch of the site's item.
: KILL ( ^pruning ^site n -- )
   {: w:^ s:^ ch:n :}
   s SITE-ITEM @ {: it:^ :}
   false it ITEM-CHOICES @ ch VEC:AT CHOICE-ALIVE !
   w PRUNING-SITES @ VEC:COUNT s SITE-FIRST @ ?do
      w i SITE@ {: o:^ :}
      o SITE-ITEM @ it <> if unloop exit then
      w o SHARED? if w o ch LOSE then
   loop ;

\ Kills the site's live choices of class k.
: KILL-AT ( ^pruning ^site n -- )
   {: w:^ s:^ k:n :}
   s SITE-LIVE @ k VEC:AT @ 0= if exit then
   s SITE-ITEM @ ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT CHOICE-ALIVE @ s SITE-CLASSES @ i VEC:AT @ k = and if
         w s i KILL
      then
   loop ;

: KILL-CLASS ( ^pruning ^domain n -- )
   {: w:^ d:^ k:n :}
   d DOMAIN-SITES @ {: list:^ :}
   list VEC:COUNT 0 ?do
      w w list i VEC:AT @ SITE@ k KILL-AT
   loop ;

\ Kills the choices giving the variable an unsupported class.
: SWEEP ( ^pruning ^domain -- )
   {: w:^ d:^ :}
   false d DOMAIN-QUEUED !
   w PRUNING-CLASSES @ VEC:COUNT 0 ?do
      w d i SUPPORTED? 0= if w d i KILL-CLASS then
   loop ;

: DRAIN ( ^pruning -- )
   {: w:^ :}
   w PRUNING-QUEUE @ {: queue:^ :}
   begin w PRUNING-HEAD @ queue VEC:COUNT < while
      w w queue w PRUNING-HEAD @ VEC:AT @ DOMAIN@ SWEEP
      1 w PRUNING-HEAD +!
   repeat ;

\ Kills each choice another item cannot support, until none changes.
: PRUNE ( ^plan ^VEC:vector<item> -- )
   {: plan items:^ :}
   items VEC:COUNT 2 < if exit then
   plan NEW-PRUNING {: w:^ :}
   items VEC:COUNT 0 ?do w items i VEC:AT +SITES loop
   w [: CLASSIFY ;] EACH-SHARED
   w [: COUNT-LIVE ;] EACH-SHARED
   w [: SITE-DOMAIN @ ENQUEUE ;] EACH-SHARED
   w DRAIN ;

\ The description source the choice's record was read from.
: ORIGIN ( ^plan ^choice -- n ) CHOICE-SOURCE @ SOURCE@ SOURCE-ORIGIN @ ;

\ The description sources of the item's live choices, each once.
: ORIGINS ( ^plan ^item -- ^VEC:vector<n> )
   {: plan it:^ :}
   plan INDEXES {: out:^ :}
   it ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if
         plan c ORIGIN {: o:n :}
         out o INDEXED? 0= if o out VEC:PUSH ! then
      then
   loop out ;

\ The records of the item's live choices read from description source
\ origin.
: LIVE-OF ( ^plan ^item n -- ^VEC:vector<n> )
   {: plan it:^ origin:n :}
   plan INDEXES {: out:^ :}
   it ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if
         plan c ORIGIN origin = if c CHOICE-SOURCE @ out VEC:PUSH ! then
      then
   loop out ;

\ Whether a joined item has no live choice.
: DEAD? ( ^VEC:vector<item> -- bool )
   {: items:^ :}
   items VEC:COUNT 0 ?do
      items i VEC:AT {: it:^ :}
      it ITEM-MODE @ 0= it LIVE-COUNT 0= and if true unloop exit then
   loop false ;

\ Binds a slot's occurrence of var into occs and meets it in where with
\ the substitution's binding of var; one bound always keeps it out of occs.
\ null: the slot's column may hold NULL. The occurrence is nullable unless
\ the meeting is a strict `=` or it reads the substitution's column; one
\ kept out of occs has its null test written to where at once.
: OCCUR ( ^plan ^VEC:vector<binding> n n str str bool ^VEC:vector<u8> -- )
   {: plan occs:^ var:n shape:n sql:str t:str null:bool where:^ :}
   plan PLAN-OUTER @ {: outer:^ :}
   outer var FIND-VAR {: k:n :}
   k 0< if
      occs var shape sql t BIND null occs LAST BINDING-NULLABLE ! exit
   then
   outer k VEC:AT {: o:^ :}
   o BINDING-ALWAYS @ if plan NEW-BINDINGS else occs then {: list:^ :}
   list var shape sql t BIND
   plan list LAST o where +MEETS 0= null and
   sql o BINDING-SQL @ STR:= 0= and {: open:bool :}
   o BINDING-ALWAYS @ 0= if open list LAST BINDING-NULLABLE ! exit then
   open if where sql +NULL-TEST then ;

\ Appends to where that b meets the substitution's binding of its variable.
: +OUTER ( ^plan ^binding ^VEC:vector<u8> -- )
   {: plan b:^ where:^ :}
   plan PLAN-OUTER @ {: outer:^ :}
   outer b BINDING-VAR @ FIND-VAR {: k:n :}
   k 0< if exit then
   plan b outer k VEC:AT where +MEETS drop ;

\ The column slot's key, none for a term slot.
: SLOT-KEY ( slot -- nullable<key> )
   MATCH slot
      column OF {: c:str shape:n k u:bool nn:bool :} k ENDOF
      term OF drop NULLABLE:NONE ENDOF
   ;MATCH ;

\ Appends to where that the key column holds one of the IRI's keys,
\ `<alias>.<col> = ANY(E'<keys>'::<type>)`.
: +KEY-MATCH ( ^plan n ^key str ^VEC:vector<u8> -- )
   {: plan node:n k:^ alias:str where:^ :}
   plan k KEY-SET@ {: set:^ :}
   where s"  AND " +SEP where alias +SQL where s" ." +SQL
   where k KEY-COL @ +SQL where s"  = ANY(E'" +SQL
   where set plan TREE node TEXT$ KEYS$ +ESCAPED
   where s" '::" +SQL where set KEY-SET-TYPE @ +SQL where s" )" +SQL ;

\ Appends to where that the column slot holds the constant, through its
\ key when it has one; for a predicate !(...), a predicate other than its
\ IRIs. A member outside the shape has a NULL `in`, which `IS NOT TRUE`
\ reads as not matched.
: +CONSTANT ( ^plan n slot str ^VEC:vector<u8> -- )
   {: plan node:n sl alias:str where:^ :}
   plan TREE node NEGATED? 0= if
      sl SLOT-KEY MATCH nullable
         none OF ENDOF
         some OF {: k:^ :}
            plan node k alias where +KEY-MATCH exit
         ENDOF
      ;MATCH
   then
   sl SLOT-KIND {: shape:n t:str :}
   plan shape SHAPE@ SHAPE-IN @ {: f:str :}
   plan sl alias SLOT-SQL {: col:str :}
   where s"  AND " +SEP where col +SQL
   plan TREE node NEGATED? 0= if
      where s"  = " +SQL
      where f plan plan node TERM-JSON TERM-ARG +CALL exit
   then
   where s"  IS NOT NULL" +SQL
   plan TREE node NODE-A@ {: list:n :}
   list 0= if exit then
   where s"  AND (" +SQL where col +SQL where s"  IN (" +SQL
   list begin dup 0<> while
      {: each:n :}
      where f plan plan plan TREE each NODE-A@ TERM-JSON TERM-ARG
         +CALL
      plan TREE each NODE-B@ dup 0<> if where s" ," +SQL then
   repeat drop where s" )) IS NOT TRUE" +SQL ;

\ A variable's occurrence of the slot under alias to occs; a constant a
\ column holds, to where.
: +SLOT ( ^plan n slot str ^VEC:vector<binding> ^VEC:vector<u8> -- )
   {: plan node:n sl alias:str occs:^ where:^ :}
   sl SLOT-KIND {: shape:n t:str :}
   plan TREE node VARIABLE? if
      plan sl alias SLOT-SQL {: sql:str :}
      shape -1 >  sl SLOT-NOTNULL? 0=  and {: null:bool :}
      plan occs plan node VAR shape sql t null where OCCUR exit
   then
   shape -1 > if plan node sl alias where +CONSTANT then ;

: SLOT-COL ( slot -- str )
   MATCH slot
      column OF {: c:str shape:n k u:bool nn:bool :} c ENDOF
      term OF drop s" " ENDOF
   ;MATCH ;

\ Whether two slots of one FROM item are one: a column of one name and
\ shape, or a term of one description source.
: SAME-SLOT? ( slot slot -- bool )
   {: a b :}
   a SLOT-COL b SLOT-COL STR:=
   a SLOT-KIND drop b SLOT-KIND drop = and ;

: RECORD-SLOT ( ^plan n n -- slot )
   {: plan r:n k:n :} plan r SOURCE@ k SLOT@ ;

\ Whether a record of the list has record r's slot k.
: SLOT-LISTED? ( ^plan ^VEC:vector<n> n n -- bool )
   {: plan list:^ r:n k:n :}
   plan r k RECORD-SLOT {: sl :}
   list VEC:COUNT 0 ?do
      plan list i VEC:AT @ k RECORD-SLOT sl SAME-SLOT? if
         true unloop exit
      then
   loop false ;

\ Of the records, the first of each different slot k.
: DISTINCT-AT ( ^plan ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: plan recs:^ k:n :}
   plan INDEXES {: out:^ :}
   recs VEC:COUNT 0 ?do
      recs i VEC:AT @ {: r:n :}
      plan out r k SLOT-LISTED? 0= if r out VEC:PUSH ! then
   loop out ;

: SLOT-SHAPE ( ^plan ^VEC:vector<n> n n -- n )
   {: plan recs:^ j:n k:n :}
   plan recs j VEC:AT @ k RECORD-SLOT SLOT-KIND drop ;

\ The form of the records' columns at slot k: their one shape, the root
\ of shapes of one root that differ, else GENERIC.
: COLUMNS-FORM ( ^plan ^VEC:vector<n> n -- n )
   {: plan recs:^ k:n :}
   plan recs 0 k SLOT-SHAPE
   recs VEC:COUNT 1 ?do
      {: form:n :} plan recs i k SLOT-SHAPE {: y:n :}
      plan form y ONE-ROOT? 0= if GENERIC unloop exit then
      plan form y UNITED
   loop ;

\ The records' columns at slot k under the alias as one value of their
\ form: COALESCE of the columns, or of their terms for GENERIC.
: COALESCE-COLUMNS ( ^plan ^VEC:vector<n> n str -- n str )
   {: plan recs:^ k:n alias:str :}
   plan recs k COLUMNS-FORM {: form:n :}
   plan NEW-SQL {: v:^ :}
   v s" COALESCE(" +SQL
   recs VEC:COUNT 0 ?do
      i 0<> if v s" ," +SQL then
      plan recs i VEC:AT @ k RECORD-SLOT {: sl :}
      plan sl alias SLOT-SQL {: col:str :}
      form 0< if
         v plan plan recs i k SLOT-SHAPE SHAPE@ SHAPE-OUT @ col +CALL
      else v col +SQL then
   loop v s" )" +SQL form v VEC:BYTES ;

\ What one column slot needs for the node: not null for a variable, the
\ constant held otherwise.
: COLUMN-COND ( ^plan n slot str -- str )
   {: plan node:n sl alias:str :}
   plan NEW-SQL {: v:^ :}
   plan TREE node VARIABLE? if
      v plan sl alias SLOT-SQL +SQL v s"  IS NOT NULL" +SQL v VEC:BYTES exit
   then
   plan node sl alias v +CONSTANT
   plan NEW-SQL {: w:^ :}
   w s" (" +SQL w v VEC:BYTES +SQL w s" )" +SQL w VEC:BYTES ;

\ Appends to where `(c1 OR ...)`, each the need of one of the records'
\ columns at slot k.
: +ANY-COLUMN ( ^plan n ^VEC:vector<n> n str ^VEC:vector<u8> -- )
   {: plan node:n recs:^ k:n alias:str where:^ :}
   where s"  AND " +SEP where s" (" +SQL
   recs VEC:COUNT 0 ?do
      i 0<> if where s"  OR " +SQL then
      plan recs i VEC:AT @ k RECORD-SLOT {: sl :}
      where plan node sl alias COLUMN-COND +SQL
   loop where s" )" +SQL ;

\ Slot k over several columns: a variable binds their COALESCE, and one
\ of them is not null or holds the constant.
: +COLUMNS ( ^plan n ^VEC:vector<n> n str ^VEC:vector<binding>
      ^VEC:vector<u8> -- )
   {: plan node:n recs:^ k:n alias:str occs:^ where:^ :}
   plan TREE node VARIABLE? if
      plan recs k alias COALESCE-COLUMNS {: form:n sql:str :}
      plan occs plan node VAR form sql s" " false where OCCUR
   then
   plan node recs k alias where +ANY-COLUMN ;

\ The slots from the first of the records, of one description source,
\ for the triple under the alias: one where they read one column or term
\ there, else their several columns.
: SLOTS-SQL ( ^plan n ^VEC:vector<n> str ^VEC:vector<binding>
      ^VEC:vector<u8> n -- )
   {: plan triple:n recs:^ alias:str occs:^ where:^ first:n :}
   plan TREE {: state :}
   3 first do
      state triple i PART {: node:n :}
      plan recs i DISTINCT-AT {: cols:^ :}
      cols VEC:COUNT 1 = if
         plan node plan cols 0 VEC:AT @ i RECORD-SLOT alias occs where
            +SLOT
      else plan node cols i alias occs where +COLUMNS then
   loop ;

: +FROM ( ^VEC:vector<u8> ^source str -- )
   {: from:^ src:^ alias:str :}
   from s"  CROSS JOIN " +SEP from src SOURCE-FROM @ +SQL
   from s"  AS " +SQL from alias +SQL ;

: SLOT-UNIQUE? ( slot -- bool )
   MATCH slot
      column OF {: c:str shape:n k u:bool nn:bool :} u ENDOF
      term OF drop false ENDOF
   ;MATCH ;

\ Whether the records read one subject slot, a unique column.
: UNIQUE-SUBJECT? ( ^plan ^VEC:vector<n> -- bool )
   {: plan recs:^ :}
   plan recs 0 DISTINCT-AT VEC:COUNT 1 <> if false exit then
   plan recs 0 VEC:AT @ 0 RECORD-SLOT SLOT-UNIQUE? ;

\ Whether two nodes are one variable, or constants that are one term.
: SAME-NODE? ( ^plan n n -- bool )
   {: plan a:n b:n :}
   plan TREE a VARIABLE? {: var:bool :}
   plan TREE b VARIABLE? var xor if false exit then
   var if plan a VAR plan b VAR = exit then
   plan a TERM-JSON plan b TERM-JSON STR:= ;

\ Whether the row is the source's by its subject slot for the node.
: ROW-OF? ( ^plan ^subject-row n ^source -- bool )
   {: plan row:^ node:n src:^ :}
   row SUBJECT-ROW-FROM @ src SOURCE-FROM @ STR:= 0= if false exit then
   row SUBJECT-ROW-SLOT @ src SOURCE-S @ SAME-SLOT? 0= if
      false exit
   then
   plan row SUBJECT-ROW-NODE @ node SAME-NODE? ;

\ The group's row the records read for the subject node, else -1.
: FIND-ROW ( ^plan n ^VEC:vector<n> ^relation -- n )
   {: plan node:n recs:^ group:^ :}
   plan recs UNIQUE-SUBJECT? 0= if -1 exit then
   plan recs 0 VEC:AT @ SOURCE@ {: src:^ :}
   group RELATION-ROWS @ {: rows:^ :}
   rows VEC:COUNT 0 ?do
      plan rows i VEC:AT node src ROW-OF? if i unloop exit then
   loop -1 ;

\ Keeps the row the records read under the alias for the subject node
\ when their subject slot is unique.
: KEEP-ROW ( ^plan n ^VEC:vector<n> str ^relation -- )
   {: plan node:n recs:^ alias:str group:^ :}
   plan recs UNIQUE-SUBJECT? 0= if exit then
   plan recs 0 VEC:AT @ SOURCE@ {: src:^ :}
   group RELATION-ROWS @ VEC:PUSH {: row:^ :}
   node row SUBJECT-ROW-NODE !
   src SOURCE-FROM @ row SUBJECT-ROW-FROM !
   src SOURCE-S @ row SUBJECT-ROW-SLOT !
   alias row SUBJECT-ROW-ALIAS ! ;

\ A pattern whose live choices are records of one description source:
\ it reads its subject's row where the group has it, else the source
\ joins the group directly.
: SINGLE-SQL ( ^plan n ^VEC:vector<n> ^VEC:vector<binding> ^relation -- )
   {: plan triple:n recs:^ occs:^ group:^ :}
   group RELATION-WHERE @ {: where:^ :}
   plan TREE triple 0 PART {: node:n :}
   plan node recs group FIND-ROW {: k:n :}
   k -1 > if
      group RELATION-ROWS @ k VEC:AT SUBJECT-ROW-ALIAS @ {: kept:str :}
      plan triple recs kept occs where 1 SLOTS-SQL exit
   then
   plan NEXT-ALIAS {: alias:str :}
   group RELATION-FROM @ plan recs 0 VEC:AT @ SOURCE@ alias +FROM
   plan node recs alias group KEEP-ROW
   plan triple recs alias occs where 0 SLOTS-SQL ;

\ One branch of a union: the triple over the records, of one description
\ source, as a relation of its own.
: SOURCE-BRANCH ( ^plan n ^VEC:vector<n> ^relation -- )
   {: plan triple:n recs:^ r:^ :}
   plan r INIT-RELATION
   r RELATION-FROM @ plan recs 0 VEC:AT @ SOURCE@ s" u" +FROM
   plan NEW-BINDINGS {: occs:^ :}
   plan triple recs s" u" occs r RELATION-WHERE @ 0 SLOTS-SQL
   plan occs r RELATION-WHERE @ JOINS r RELATION-BINDINGS ! ;

\ Record k alone.
: RECORD ( ^plan n -- ^VEC:vector<n> )
   {: plan k:n :}
   plan INDEXES {: out:^ :} k out VEC:PUSH ! out ;

\ The shape of the variable's binding in the first relation binding it.
: FIRST-SHAPE ( ^VEC:vector<relation> n -- n )
   {: branches:^ var:n :}
   branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k -1 > if bs k VEC:AT BINDING-SHAPE @ unloop exit then
   loop GENERIC ;

\ A variable's form across the branches binding it: their one shape, the
\ root of shapes of one root that differ, else GENERIC.
: FORM ( ^plan ^VEC:vector<relation> n -- n )
   {: plan branches:^ var:n :}
   branches var FIRST-SHAPE
   dup 0< if drop GENERIC exit then
   branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k -1 > if
         {: form:n :} bs k VEC:AT BINDING-SHAPE @ {: y:n :}
         plan form y ONE-ROOT? 0= if GENERIC unloop exit then
         plan form y UNITED
      then
   loop ;

\ Whether every branch binds the variable always.
: EVERYWHERE? ( ^VEC:vector<relation> n -- bool )
   {: branches:^ var:n :}
   branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k 0< if false unloop exit then
      bs k VEC:AT BINDING-ALWAYS @ 0= if false unloop exit then
   loop true ;

\ The bytes a column of the binding copies.
: COPIED ( ^binding -- n ) dup BINDING-COPIES @ swap BINDING-SQL @ STR:LEN + ;

\ The most a column of a branch's binding of the variable copies.
: MOST-OF ( ^VEC:vector<relation> n -- n )
   {: branches:^ var:n :}
   0 branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k -1 > if bs k VEC:AT COPIED max then
   loop ;

\ The union's columns: each variable a branch binds, in its form.
: COLUMNS ( ^plan ^VEC:vector<relation> -- ^VEC:vector<binding> )
   {: plan branches:^ :}
   plan NEW-BINDINGS {: cols:^ :}
   branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs VEC:COUNT 0 ?do
         bs i VEC:AT BINDING-VAR @ {: var:n :}
         cols var FIND-VAR 0< if
            cols var plan branches var FORM s" " s" " BIND
            branches var EVERYWHERE? cols LAST BINDING-ALWAYS !
            branches var MOST-OF cols LAST BINDING-COPIES !
         then
      loop
   loop cols ;

\ Writes to r's WHERE the null test of each binding still nullable.
: +NULL-TESTS ( ^relation -- )
   {: r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs VEC:COUNT 0 ?do
      bs i VEC:AT {: b:^ :}
      b BINDING-NULLABLE @ if
         r RELATION-WHERE @ b BINDING-SQL @ +NULL-TEST
         false b BINDING-NULLABLE !
      then
   loop ;

: +FROM-WHERE ( ^VEC:vector<u8> ^relation -- )
   {: v:^ r:^ :}
   r +NULL-TESTS
   r RELATION-FROM @ {: from:^ :}
   r RELATION-WHERE @ {: where:^ :}
   from VEC:COUNT 0<> if v s"  FROM " +SQL v from VEC:BYTES +SQL then
   where VEC:COUNT 0<> if v s"  WHERE " +SQL v where VEC:BYTES +SQL then ;

\ Appends the binding's value in form shape: native as it is, else a term.
: +VALUE ( ^plan ^VEC:vector<u8> ^binding n -- )
   {: plan v:^ b:^ shape:n :}
   shape 0< if v plan b TERM-SQL +SQL else v b BINDING-SQL @ +SQL then ;

\ Appends ` x AS vK`, the binding as a column of form shape.
: +COLUMN ( ^plan ^VEC:vector<u8> ^binding n -- )
   {: plan v:^ b:^ shape:n :}
   v s"  " +SQL plan v b shape +VALUE
   v s"  AS " +SQL v plan b BINDING-VAR @ COLUMN$ +SQL ;

\ Appends NULL typed as form shape.
: +NULL ( ^plan ^VEC:vector<u8> n -- )
   {: plan v:^ shape:n :}
   shape 0< if v s" NULL::jsonb" +SQL
   else v plan shape SHAPE@ SHAPE-IN @ s" NULL::jsonb" +CALL then ;

\ Appends `SELECT columns FROM ... WHERE ...`, the relation's solutions.
: +SELECT ( ^plan ^VEC:vector<u8> ^relation -- )
   {: plan v:^ r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   v s" SELECT" +SQL
   bs VEC:COUNT 0 ?do
      i 0<> if v s" ," +SQL then
      bs i VEC:AT {: b:^ :}
      plan v b b BINDING-SHAPE @ +COLUMN
   loop v r +FROM-WHERE ;

\ Binds var to what the union's column of form shape holds in branch b:
\ b's value in that form, NULL where b binds none.
: +PRESENTED ( ^plan ^VEC:vector<binding> ^relation n n -- )
   {: plan list:^ b:^ var:n shape:n :}
   b RELATION-BINDINGS @ {: bs:^ :}
   bs var FIND-VAR {: k:n :}
   plan NEW-SQL {: v:^ :}
   k 0< if plan v shape +NULL else plan v bs k VEC:AT shape +VALUE then
   list var shape v VEC:BYTES s" " BIND
   k 0< if false list LAST BINDING-ALWAYS ! exit then
   bs k VEC:AT list LAST {: from:^ to:^ :}
   from BINDING-ALWAYS @ to BINDING-ALWAYS !
   from BINDING-COPIES @ to BINDING-COPIES ! ;

\ Branch b as the union of columns cols presents it. u shares b's WHERE,
\ so b's null tests are written first.
: PRESENTED ( ^plan ^relation ^VEC:vector<binding> -- ^relation )
   {: plan b:^ cols:^ :}
   b +NULL-TESTS
   plan NEW-RELATION {: u:^ :}
   b RELATION-FROM @ u RELATION-FROM !
   b RELATION-WHERE @ u RELATION-WHERE !
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: c:^ :}
      plan u RELATION-BINDINGS @ b c BINDING-VAR @ c BINDING-SHAPE @
         +PRESENTED
   loop u ;

: +BRANCH ( ^plan ^VEC:vector<u8> ^relation ^VEC:vector<binding> -- )
   {: plan v:^ r:^ cols:^ :}
   plan v plan r cols PRESENTED +SELECT ;

\ The branches as one UNION ALL derived table, and its columns.
: UNION-TABLE ( ^plan ^VEC:vector<relation> -- str ^VEC:vector<binding> )
   {: plan branches:^ :}
   plan branches COLUMNS {: cols:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (" +SQL
   branches VEC:COUNT 0 ?do
      i 0<> if v s"  UNION ALL " +SQL then
      plan v branches i VEC:AT cols +BRANCH
   loop v s" )" +SQL v VEC:BYTES cols ;

\ The relation as a derived table, and its columns.
: SUBSELECT ( ^plan ^relation -- str ^VEC:vector<binding> )
   {: plan r:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (" +SQL plan v r +SELECT v s" )" +SQL
   v VEC:BYTES r RELATION-BINDINGS @ ;

\ Appends the derived table under a new alias to from and answers its
\ columns as bindings there.
: +TABLE ( ^plan str ^VEC:vector<binding> ^VEC:vector<u8>
      -- ^VEC:vector<binding> )
   {: plan t:str cols:^ from:^ :}
   plan NEXT-ALIAS {: alias:str :}
   from t +SQL from s"  AS " +SQL from alias +SQL
   plan NEW-BINDINGS {: occs:^ :}
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: c:^ :}
      c BINDING-VAR @ {: var:n :}
      occs var c BINDING-SHAPE @ plan alias var COLUMN-OF
         c BINDING-TERM @ BIND
      c BINDING-ALWAYS @ occs LAST BINDING-ALWAYS !
      c COPIED occs LAST BINDING-COPIES !
   loop occs ;

\ The derived table's rows whose columns meet the substitution.
: SUBSTITUTED ( ^plan str ^VEC:vector<binding> -- str )
   {: plan t:str cols:^ :}
   plan PLAN-OUTER @ VEC:COUNT 0= if t exit then
   plan NEW-SQL {: from:^ :} plan NEW-SQL {: where:^ :}
   plan t cols from +TABLE {: occs:^ :}
   occs VEC:COUNT 0 ?do plan occs i VEC:AT where +OUTER loop
   where VEC:COUNT 0= if t exit then
   plan NEW-SQL {: v:^ :}
   v s" (SELECT * FROM " +SQL v from VEC:BYTES +SQL
   v s"  WHERE " +SQL v where VEC:BYTES +SQL v s" )" +SQL v VEC:BYTES ;

\ Whether binding b of a key's variable can read the key's values: b
\ native, the key a term or of b's root.
: PROBES? ( ^plan ^binding ^binding -- bool )
   {: plan key:^ b:^ :}
   b BINDING-SHAPE @ {: t:n :} key BINDING-SHAPE @ {: form:n :}
   t 0< if false exit then
   form GENERIC = plan form t ONE-ROOT? or ;

\ The index of r's binding of the key's variable where it PROBES?,
\ else -1.
: PROBED ( ^plan ^binding ^relation -- n )
   {: plan key:^ r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs key BINDING-VAR @ FIND-VAR {: k:n :}
   k 0< if -1 exit then
   plan key bs k VEC:AT PROBES? if k else -1 then ;

: PROBED? ( ^plan ^binding ^VEC:vector<relation> -- bool )
   {: plan key:^ branches:^ :}
   branches VEC:COUNT 0 ?do
      plan key branches i VEC:AT PROBED -1 >
      if true unloop exit then
   loop false ;

\ The index of the first key some branch PROBED, the union's driver;
\ -1 when none.
: DRIVER ( ^plan ^VEC:vector<binding> ^VEC:vector<relation> -- n )
   {: plan keys:^ branches:^ :}
   keys VEC:COUNT 0 ?do
      plan keys i VEC:AT branches PROBED? if i unloop exit then
   loop -1 ;

\ Appends to where that binding b of a key's variable is the key of
\ row: `b = row.k`, through b's shape's `in` for a term key. That strict
\ `=` leaves b not nullable.
: +MEET-KEY ( ^plan ^binding ^binding str ^VEC:vector<u8> -- )
   {: plan key:^ b:^ row:str where:^ :}
   plan NEW-SQL {: k:^ :} k row +SQL k s" .k" +SQL
   where s"  AND " +SEP where b BINDING-SQL @ +SQL where s"  = " +SQL
   key BINDING-SHAPE @ GENERIC <> if where k VEC:BYTES +SQL else
      where plan b BINDING-SHAPE @ SHAPE@ SHAPE-IN @ k VEC:BYTES +CALL
   then false b BINDING-NULLABLE ! ;

\ Meets the key of row in each branch that PROBED it; answers the
\ indexes of those branches, then of the others.
: +KEYS ( ^plan ^binding str ^VEC:vector<relation>
      -- ^VEC:vector<n> ^VEC:vector<n> )
   {: plan key:^ row:str branches:^ :}
   plan INDEXES {: probed:^ :} plan INDEXES {: rest:^ :}
   branches VEC:COUNT 0 ?do
      branches i VEC:AT {: r:^ :}
      plan key r PROBED {: k:n :}
      k 0< if i rest VEC:PUSH ! else
         plan key r RELATION-BINDINGS @ k VEC:AT row
            r RELATION-WHERE @ +MEET-KEY
         i probed VEC:PUSH !
      then
   loop probed rest ;

\ Appends the listed branches as the union of columns cols presents
\ them.
: +UNION ( ^plan ^VEC:vector<u8> ^VEC:vector<relation> ^VEC:vector<n>
      ^VEC:vector<binding> -- )
   {: plan v:^ branches:^ list:^ cols:^ :}
   list VEC:COUNT 0 ?do
      i 0<> if v s"  UNION ALL " +SQL then
      plan v branches list i VEC:AT @ VEC:AT cols +BRANCH
   loop ;

\ `tK AS row CROSS JOIN LATERAL (<the listed branches>)`: the branches
\ read once per key of the key's CTE tK.
: PROBE-SQL ( ^plan ^binding str ^VEC:vector<relation> ^VEC:vector<n>
      ^VEC:vector<binding> -- str )
   {: plan key:^ row:str branches:^ list:^ cols:^ :}
   plan NEW-SQL {: v:^ :}
   v key BINDING-SQL @ +SQL v s"  AS " +SQL v row +SQL
   v s"  CROSS JOIN LATERAL (" +SQL
   plan v branches list cols +UNION v s" )" +SQL v VEC:BYTES ;

\ `(SELECT x.* FROM probe AS x UNION ALL <the listed branches>)`.
: BESIDE ( ^plan str ^VEC:vector<relation> ^VEC:vector<n>
      ^VEC:vector<binding> -- str )
   {: plan probe:str branches:^ list:^ cols:^ :}
   plan NEXT-ALIAS {: x:str :}
   plan NEW-SQL {: v:^ :}
   v s" (SELECT " +SQL v x +SQL v s" .* FROM " +SQL v probe +SQL
   v s"  AS " +SQL v x +SQL v s"  UNION ALL " +SQL
   plan v branches list cols +UNION v s" )" +SQL v VEC:BYTES ;

\ The branches driven by the key as FROM text whose last item takes the
\ alias +TABLE appends, and its columns.
: DRIVEN-TABLE ( ^plan ^binding ^VEC:vector<relation>
      -- str ^VEC:vector<binding> )
   {: plan key:^ branches:^ :}
   plan branches COLUMNS {: cols:^ :}
   plan NEXT-ALIAS {: row:str :}
   plan key row branches +KEYS {: probed:^ rest:^ :}
   plan key row branches probed cols PROBE-SQL {: t:str :}
   rest VEC:COUNT 0= if t cols exit then
   plan t branches rest cols BESIDE cols ;

\ Sets the item's branches, one per description source it reads.
: BRANCHES ( ^plan ^item -- )
   {: plan it:^ :}
   plan it ORIGINS {: origins:^ :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   origins VEC:COUNT 0 ?do
      plan it origins i VEC:AT @ LIVE-OF {: recs:^ :}
      branches VEC:PUSH {: b:^ :}
      plan it ITEM-NODE @ recs b SOURCE-BRANCH
   loop branches it ITEM-BRANCHES ! ;

\ A pattern of several description sources: a UNION ALL of a branch per
\ source, driven by the first key some branch binds natively.
: UNION-SQL ( ^plan ^item ^VEC:vector<binding> ^relation
      -- ^VEC:vector<binding> )
   {: plan it:^ keys:^ group:^ :}
   it ITEM-BRANCHES @ {: branches:^ :}
   group RELATION-FROM @ {: from:^ :}
   from s"  CROSS JOIN " +SEP
   plan keys branches DRIVER {: d:n :}
   d 0< if plan plan branches UNION-TABLE from +TABLE exit then
   plan plan keys d VEC:AT branches DRIVEN-TABLE from +TABLE ;

\ Appends the joined item, a derived table or a triple of one
\ description source, to the group's FROM and answers its bindings.
: ITEM-SQL ( ^plan ^item ^relation -- ^VEC:vector<binding> )
   {: plan it:^ group:^ :}
   it ITEM-NODE @ 0<> if
      plan it plan it ORIGINS 0 VEC:AT @ LIVE-OF {: recs:^ :}
      plan NEW-BINDINGS {: occs:^ :}
      plan it ITEM-NODE @ recs occs group SINGLE-SQL occs exit
   then
   group RELATION-FROM @ {: from:^ :}
   from s"  CROSS JOIN " +SEP
   plan it ITEM-TABLE @ it ITEM-COLUMNS @ from +TABLE ;

\ The scope through the substitution: a variable it binds is its value
\ there where bound. MEET's conditions are dropped: each slot meets it.
: SEEN ( ^plan ^VEC:vector<binding> -- ^VEC:vector<binding> )
   {: plan scope:^ :}
   plan plan PLAN-OUTER @ COPY-SCOPE {: seen:^ :}
   plan seen scope plan NEW-SQL false MEET-ALL seen ;

\ Whether the node is an infix expression of the operator.
: INFIX? ( ^syntax n str -- bool )
   {: state node:n op:str :}
   state node NODE-K N-BINARY <> if false exit then
   state node TEXT$ op STR:= ;

: CONSTANT? ( ^syntax n -- bool )
   NODE-K dup N-IRI = swap N-LITERAL = or ;

\ Whether the node is a constant that `=` finds equal to no other term.
: UNTYPED? ( ^plan n -- bool )
   {: plan node:n :}
   plan TREE node CONSTANT? 0= if false exit then
   plan node TERM-JSON JSON:PARSE s" datatype" HAS? 0= ;

\ Whether the list of expressions is not empty and each is UNTYPED?.
: ALL-UNTYPED? ( ^plan n -- bool )
   {: plan list:n :}
   list 0= if false exit then
   list begin dup 0<> while
      {: each:n :}
      plan plan TREE each NODE-A@ UNTYPED? 0= if false exit then
      plan TREE each NODE-B@
   repeat drop true ;

\ The index of the expression's binding, a variable's in the plan's
\ scope, when native; else -1.
: NATIVE-VAR ( ^plan n -- n )
   {: plan e:n :}
   plan TREE e NODE-K N-VAR <> if -1 exit then
   plan PLAN-SCOPE @ {: bs:^ :}
   bs plan e VAR FIND-VAR {: k:n :}
   k 0< if -1 exit then
   bs k VEC:AT BINDING-SHAPE @ 0< if -1 else k then ;

\ The condition that native binding k of the plan's scope is constant c.
: FIXED-SQL ( ^plan n n -- str )
   {: plan k:n c:n :}
   plan plan PLAN-SCOPE @ k VEC:AT plan c CONSTANT-VALUE SAME-SQL ;

\ Appends that variable e is constant c, when they are such and e native.
: +FIXED ( ^plan n n ^VEC:vector<u8> -- )
   {: plan e:n c:n where:^ :}
   plan TREE c CONSTANT? 0= if exit then
   plan e NATIVE-VAR {: k:n :}
   k 0< if exit then
   where plan k c FIXED-SQL +AND ;

: +FIXED-EITHER ( ^plan n n ^VEC:vector<u8> -- )
   {: plan a:n b:n where:^ :}
   plan a b where +FIXED plan b a where +FIXED ;

: +EQUAL-FIXED ( ^plan n ^VEC:vector<u8> -- )
   {: plan e:n where:^ :}
   plan TREE e NODE-A@ {: a:n :} plan TREE e NODE-B@ {: b:n :}
   plan a UNTYPED? plan b UNTYPED? or 0= if exit then
   plan a b where +FIXED-EITHER ;

: +SAME-FIXED ( ^plan n ^VEC:vector<u8> -- )
   {: plan call:n where:^ :}
   plan TREE {: state :}
   state call NODE-A@ {: list:n :}
   plan state list NODE-A@ state state list NODE-B@ NODE-A@
      where +FIXED-EITHER ;

: +IN-FIXED ( ^plan n ^VEC:vector<u8> -- )
   {: plan e:n where:^ :}
   plan TREE {: state :}
   state e NODE-B@ {: list:n :}
   plan list ALL-UNTYPED? 0= if exit then
   plan state e NODE-A@ NATIVE-VAR {: k:n :}
   k 0< if exit then
   where s"  AND " +SEP where s" (" +SQL
   list begin dup 0<> while
      {: each:n :}
      each list <> if where s"  OR " +SQL then
      where plan k state each NODE-A@ FIXED-SQL +SQL
      state each NODE-B@
   repeat drop where s" )" +SQL ;

\ Appends the native equalities the test's conjuncts imply.
: +IMPLIED ( ^plan n ^VEC:vector<u8> -- )
   {: plan e:n where:^ :}
   plan TREE {: state :}
   state e s" &&" INFIX? if
      plan state e NODE-A@ where RECURSE
      plan state e NODE-B@ where RECURSE exit
   then
   state e s" =" INFIX? if plan e where +EQUAL-FIXED exit then
   state e s" IN" INFIX? if plan e where +IN-FIXED exit then
   state e NODE-K N-CALL <> if exit then
   state e s" SAMETERM" CALL-IS? if plan e where +SAME-FIXED then ;

\ Appends the expression's test over the scope to the conditions, then
\ the equalities it implies. The outer scope and output are kept: an
\ EXISTS's group holds FILTERs.
: +TEST ( ^plan ^VEC:vector<binding> n ^VEC:vector<u8> -- )
   {: plan scope:^ e:n where:^ :}
   plan PLAN-SCOPE @ {: outer:^ :}
   plan PLAN-OUT @ {: out:^ :}
   plan scope SEEN plan PLAN-SCOPE ! where plan PLAN-OUT !
   where s"  AND " +SEP
   plan e SQL-TEST plan e where +IMPLIED
   outer plan PLAN-SCOPE ! out plan PLAN-OUT ! ;

: +FILTER ( ^plan ^VEC:vector<binding> n ^VEC:vector<u8> -- )
   {: plan scope:^ filter:n where:^ :}
   plan scope plan TREE filter NODE-A@ where +TEST ;

\ The group with its FILTERs on all its bindings.
: CLOSED ( ^plan n -- ^relation )
   {: plan group:n :}
   plan group GROUP-SQL {: r:^ filters:^ :}
   filters VEC:COUNT 0 ?do
      plan r RELATION-BINDINGS @ filters i VEC:AT @ r RELATION-WHERE @
         +FILTER
   loop r ;

\ A nested group: its relation as a derived table.
: GROUP-ITEM ( ^plan ^VEC:vector<item> ^relation -- )
   {: plan items:^ r:^ :}
   r RELATION-EMPTY @ if
      plan items s" " r RELATION-BINDINGS @ true CHILD-ITEM exit
   then plan items plan r SUBSELECT false CHILD-ITEM ;

\ Adds a relation that is not statically empty to the branches.
: +RELATION ( ^plan ^VEC:vector<relation> ^relation -- )
   {: plan branches:^ r:^ :}
   branches VEC:PUSH {: c:^ :}
   plan c INIT-RELATION
   r RELATION-FROM @ c RELATION-FROM !
   r RELATION-WHERE @ c RELATION-WHERE !
   r RELATION-BINDINGS @ c RELATION-BINDINGS !
   false c RELATION-EMPTY ! ;

\ Adds the group to the branches unless it is statically empty.
: +GROUP ( ^plan ^VEC:vector<relation> n -- )
   {: plan branches:^ group:n :}
   plan group CLOSED {: r:^ :}
   r RELATION-EMPTY @ if exit then
   plan branches r +RELATION ;

\ The branches as one UNION ALL derived table; none is statically empty.
: UNION-CHILD ( ^plan ^VEC:vector<item> ^VEC:vector<relation> -- )
   {: plan items:^ branches:^ :}
   branches VEC:COUNT 0= if
      plan items s" " plan NEW-BINDINGS true CHILD-ITEM exit
   then plan items plan branches UNION-TABLE false CHILD-ITEM
   branches items dup VEC:COUNT 1- VEC:AT ITEM-BRANCHES ! ;

\ A chain of UNIONs: one UNION ALL derived table of its groups.
: UNION-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan TREE {: state :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   state node N-UNION N-UNION SPINE {: spine:n leaf:n :}
   plan branches leaf +GROUP
   spine begin dup 0<> while
      {: list:n :}
      plan branches state state list NODE-A@ NODE-B@ +GROUP
      state list NODE-B@
   repeat drop
   plan items branches UNION-CHILD ;

\ Whether GRAPH g reads source k's named graph: any for a variable g, an
\ IRI's own.
: READS? ( ^plan n n -- bool )
   {: plan g:n k:n :}
   plan TREE g VARIABLE? if true exit then
   plan g plan k SOURCE@ SOURCE-GRAPH @ TERM-SAME? ;

\ The GRAPH's group with source k's graph active, a variable g joined with
\ that graph, to the branches unless statically empty.
: +GRAPH ( ^plan ^VEC:vector<relation> n n -- )
   {: plan branches:^ node:n k:n :}
   plan TREE {: state :}
   plan k SOURCE@ SOURCE-GRAPH @ {: t:str :}
   plan PLAN-GRAPH @ {: outer:str :}
   t plan PLAN-GRAPH !
   plan state node NODE-B@ CLOSED {: r:^ :}
   outer plan PLAN-GRAPH !
   r RELATION-EMPTY @ if exit then
   state node NODE-A@ {: g:n :}
   state g VARIABLE? if
      plan NEW-BINDINGS {: own:^ :}
      own plan g VAR KNOWN plan t TERM-LITERAL t BIND
      plan own LAST r RELATION-WHERE @ +OUTER
      plan r RELATION-BINDINGS @ own r RELATION-WHERE @ false MEET-ALL
   then plan branches r +RELATION ;

\ GRAPH g { P } (SPARQL 1.1 section 18.5): P with each named graph that g
\ can be active, a variable g bound to it. One branch is a derived table
\ that keeps its columns' forms.
: GRAPH-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan TREE node NODE-A@ {: g:n :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   plan PLAN-STORE @ STORE-GRAPHS @ {: firsts:^ :}
   firsts VEC:COUNT 0 ?do
      firsts i VEC:AT @ {: k:n :}
      plan g k READS? if plan branches node k +GRAPH then
   loop
   branches VEC:COUNT 1 = if
      plan items branches 0 VEC:AT GROUP-ITEM exit
   then plan items branches UNION-CHILD ;

\ An OPTIONAL's or MINUS's derived table; a statically empty one is no item.
: SIDE-ITEM ( ^plan ^VEC:vector<item> ^relation n -- ^item )
   {: plan items:^ r:^ mode:n :}
   plan items 0 NEW-ITEM {: it:^ :}
   mode it ITEM-MODE !
   plan r SUBSELECT it ITEM-COLUMNS ! it ITEM-TABLE ! it ;

\ OPTIONAL { P }: P before its FILTERs, which join the LEFT JOIN's ON.
: OPTIONAL-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan plan TREE node NODE-A@ GROUP-SQL {: r:^ filters:^ :}
   r RELATION-EMPTY @ if exit then
   plan items r N-OPTIONAL SIDE-ITEM ITEM-FILTERS filters swap ! ;

: MINUS-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan plan TREE node NODE-A@ CLOSED {: r:^ :}
   r RELATION-EMPTY @ if exit then
   plan items r N-MINUS SIDE-ITEM drop ;

\ BIND: an item whose place in the fold is its Extend.
: BIND-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   N-BIND plan items node NEW-ITEM ITEM-MODE ! ;

\ Whether a row of the data block holds UNDEF in column k.
: UNDEF-AT? ( ^syntax n n -- bool )
   {: state rows:n k:n :}
   rows begin dup 0<> while
      {: list:n :} state list NODE-A@
      k 0 ?do state swap NODE-B@ loop {: cell:n :}
      state state cell NODE-A@ NODE-K N-UNDEF = if true exit then
      state list NODE-B@
   repeat drop false ;

\ The data block's columns: each variable a term, always bound unless an
\ UNDEF is in its column.
: DATA-COLUMNS ( ^plan n -- ^VEC:vector<binding> )
   {: plan node:n :}
   plan TREE {: state :}
   plan NEW-BINDINGS {: cols:^ :}
   0 state node NODE-A@ begin dup 0<> while
      {: k:n list:n :}
      cols plan state list NODE-A@ VAR GENERIC s" " s" " BIND
      state state node NODE-B@ k UNDEF-AT? 0= cols LAST BINDING-ALWAYS !
      k 1+ state list NODE-B@
   repeat 2drop cols ;

\ Appends the row `(t,...)`, each term a parameter, UNDEF NULL.
: +DATA-ROW ( ^plan ^VEC:vector<u8> n -- )
   {: plan v:^ row:n :}
   plan TREE {: state :}
   v s" (" +SQL
   row begin dup 0<> while
      {: list:n :} state list NODE-A@ {: term:n :}
      list row <> if v s" ," +SQL then
      state term NODE-K N-UNDEF = if v s" NULL::jsonb" +SQL
      else v plan plan term TERM-JSON TERM-ARG +SQL then
      state list NODE-B@
   repeat drop v s" )" +SQL ;

\ The data block as a derived table, `(SELECT * FROM (VALUES ...) AS
\ d(vK,...))`; with no variables, one empty solution per row.
: DATA-TABLE ( ^plan n ^VEC:vector<binding> -- str )
   {: plan node:n cols:^ :}
   plan TREE {: state :}
   state node NODE-B@ {: rows:n :}
   plan NEW-SQL {: v:^ :}
   cols VEC:COUNT 0= if
      v s" (SELECT FROM generate_series(1," +SQL
      v state rows LIST-LENGTH +INT v s" ))" +SQL v VEC:BYTES exit
   then
   v s" (SELECT * FROM (VALUES " +SQL
   rows begin dup 0<> while
      {: list:n :}
      list rows <> if v s" ," +SQL then
      plan v state list NODE-A@ +DATA-ROW
      state list NODE-B@
   repeat drop
   v s" ) AS d(" +SQL
   cols VEC:COUNT 0 ?do
      i 0<> if v s" ," +SQL then
      v plan cols i VEC:AT BINDING-VAR @ COLUMN$ +SQL
   loop v s" ))" +SQL v VEC:BYTES ;

\ VALUES: its data block as a joined derived table; one with no rows is
\ statically empty.
: VALUES-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan node DATA-COLUMNS {: cols:^ :}
   plan TREE node NODE-B@ 0= if
      plan items s" " cols true CHILD-ITEM exit
   then
   plan plan node cols DATA-TABLE cols SUBSTITUTED {: t:str :}
   plan items t cols false CHILD-ITEM ;

\ A triple node s p o, placed at node at's token.
: TRIPLE-NODE ( ^plan n n n n -- n )
   {: plan at:n s:n p:n o:n :}
   plan TREE {: state :}
   state N-TRIPLE s p BINARY-NODE {: t:n :}
   o state t NODE NODE-C !
   state at NODE NODE-TOKEN @ state t NODE NODE-TOKEN ! t ;

: GROUP-NODE ( ^plan n -- n )
   {: plan triple:n :}
   plan TREE {: state :}
   state N-GROUP ADD-NODE {: g:n :}
   state triple 0 CONS state g NODE NODE-A ! g ;

\ The two triples' groups as a UNION item.
: ALTERNATIVE ( ^plan ^VEC:vector<item> n n -- )
   {: plan items:^ left:n right:n :}
   plan TREE N-UNION plan left GROUP-NODE plan right GROUP-NODE
      BINARY-NODE {: u:n :}
   plan items u UNION-ITEM ;

\ s !(...) o: the forward members' triple, the inverse members' with s
\ and o swapped, or a UNION of the two (SPARQL 1.1 section 18.2.2.9).
: NEGATED-ITEM ( ^plan ^VEC:vector<item> n n n n -- )
   {: plan items:^ at:n s:n path:n o:n :}
   plan TREE {: state :}
   state path NODE-A@ {: list:n :}
   state list false MEMBERS {: ahead:n :}
   state list true MEMBERS {: back:n :}
   state N-PATH s" !" ahead 0 OPERATOR-NODE {: fwd:n :}
   state N-PATH s" !" back 0 OPERATOR-NODE {: inv:n :}
   back 0= if plan items plan at s fwd o TRIPLE-NODE TRIPLE-ITEM exit then
   ahead 0= if plan items plan at o inv s TRIPLE-NODE TRIPLE-ITEM exit then
   plan items plan at s fwd o TRIPLE-NODE plan at o inv s TRIPLE-NODE
   ALTERNATIVE ;

\ A `+`, `*` or `?` triple, a walk the group joins after its joined items.
: WALK-ITEM ( ^plan ^VEC:vector<item> n -- )
   NEW-ITEM N-PATH swap ITEM-MODE ! ;

\ s path o expanded into items: `^` swaps s and o, `/` joins two triples
\ on a fresh variable, `|` is a UNION of the two.
: EXPAND ( ^plan ^VEC:vector<item> n n n n -- )
   {: plan items:^ at:n s:n path:n o:n :}
   plan TREE {: state :}
   state path NODE-K N-PATH <> if
      plan items plan at s path o TRIPLE-NODE TRIPLE-ITEM exit
   then
   state path NODE-A@ {: x:n :}
   state path NODE-B@ {: y:n :}
   state path s" ^" OPERATOR? if plan items at o x s RECURSE exit then
   state path s" /" OPERATOR? if
      state ANONYMOUS {: f:n :}
      plan items at s x f RECURSE
      plan items at f y o RECURSE exit
   then
   state path s" |" OPERATOR? if
      plan items plan at s x o TRIPLE-NODE plan at s y o TRIPLE-NODE
      ALTERNATIVE exit
   then
   state path NEGATED? if plan items at s path o NEGATED-ITEM exit then
   plan items plan at s path o TRIPLE-NODE WALK-ITEM ;

: PATTERN-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ triple:n :}
   plan TREE {: state :}
   state triple NODE-B@ {: verb:n :}
   state verb NODE-K N-PATH <> if plan items triple TRIPLE-ITEM exit then
   plan items triple state triple NODE-A@ verb state triple NODE-C@
   EXPAND ;

\ Whether the group's patterns are all joined.
: JOINED? ( ^syntax n -- bool )
   {: state group:n :}
   state group NODE-A@ begin dup 0<> while
      {: list:n :}
      state state list NODE-A@ NODE-K {: k:n :}
      k N-OPTIONAL = k N-MINUS = or k N-BIND = or k N-FILTER = or
      if false exit then
      state list NODE-B@
   repeat drop true ;

\ The index of the source whose named graph a GRAPH <iri> of a joined
\ group makes active, else -1.
: JOINED-GRAPH ( ^plan n -- n )
   {: plan node:n :}
   plan TREE {: state :}
   state node NODE-A@ {: g:n :}
   state g VARIABLE? state state node NODE-B@ JOINED? 0= or
   if -1 exit then
   plan PLAN-STORE @ STORE-GRAPHS @ {: firsts:^ :}
   firsts VEC:COUNT 0 ?do
      firsts i VEC:AT @ {: k:n :}
      plan g k READS? if k unloop exit then
   loop -1 ;

: ITEMS ( ^plan n ^VEC:vector<item> ^VEC:vector<n> -- )
   {: plan group:n items:^ filters:^ :}
   plan TREE {: state :}
   plan PLAN-GRAPH @ {: outer:str :}
   state group NODE-A@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: node:n :}
      state node NODE-K case
         N-TRIPLE of plan items node PATTERN-ITEM endof
         N-GROUP of
            state node JOINED? if plan node items filters RECURSE
            else plan items plan node CLOSED GROUP-ITEM then
         endof
         N-UNION of plan items node UNION-ITEM endof
         N-OPTIONAL of plan items node OPTIONAL-ITEM endof
         N-MINUS of plan items node MINUS-ITEM endof
         N-GRAPH of
            plan node JOINED-GRAPH {: k:n :}
            k 0< if plan items node GRAPH-ITEM else
               plan k SOURCE@ SOURCE-GRAPH @ plan PLAN-GRAPH !
               plan state node NODE-B@ items filters RECURSE
               outer plan PLAN-GRAPH !
            then
         endof
         N-BIND of plan items node BIND-ITEM endof
         N-VALUES of plan items node VALUES-ITEM endof
         N-SELECT of plan items node SUBQUERY-ITEM endof
         \ N-FILTER: a group holds no other pattern.
         node filters VEC:PUSH !
      endcase
      state list NODE-B@
   repeat drop ;

: JOIN-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   plan r RELATION-BINDINGS @ plan it r ITEM-SQL r RELATION-WHERE @
      false MEET-ALL ;

\ LeftJoin: P's derived table LEFT JOINs the FROM, ON P meeting the scope
\ and P's FILTERs on the joined bindings. An empty FROM is one empty row.
: OPTIONAL-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   r RELATION-BINDINGS @ {: scope:^ :}
   r RELATION-FROM @ {: from:^ :}
   from VEC:COUNT 0= if
      plan s" (SELECT)" plan NEW-BINDINGS from +TABLE drop
   then
   from s"  LEFT JOIN " +SQL
   plan it ITEM-TABLE @ it ITEM-COLUMNS @ from +TABLE {: occs:^ :}
   plan NEW-SQL {: on:^ :}
   plan scope occs on true MEET-ALL
   it ITEM-FILTERS @ {: filters:^ :}
   filters VEC:COUNT 0 ?do
      plan scope filters i VEC:AT @ on +FILTER
   loop
   from s"  ON " +SQL
   on VEC:COUNT 0= if from s" true" +SQL else from on VEC:BYTES +SQL then ;

\ How the scope shares a variable with the list: 0 not at all, 1 where
\ both sides are bound, 2 with both sides always bound.
: SHARING ( ^VEC:vector<binding> ^VEC:vector<binding> -- n )
   {: scope:^ list:^ :}
   0 list VEC:COUNT 0 ?do
      list i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR {: k:n :}
      k -1 > if
         scope k VEC:AT BINDING-ALWAYS @ o BINDING-ALWAYS @ and if
            drop 2 unloop exit
         then drop 1
      then
   loop ;

: +NOT-NULL ( ^VEC:vector<u8> ^binding -- )
   {: v:^ b:^ :}
   b BINDING-ALWAYS @ if exit then
   v s"  AND " +SEP v b BINDING-SQL @ +SQL v s"  IS NOT NULL" +SQL ;

\ Appends to where that some variable of occs the scope binds is bound on
\ both sides.
: +BOUND-BOTH ( ^plan ^VEC:vector<binding> ^VEC:vector<binding>
      ^VEC:vector<u8> -- )
   {: plan scope:^ occs:^ where:^ :}
   plan NEW-SQL {: any:^ :}
   occs VEC:COUNT 0 ?do
      occs i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR {: k:n :}
      k -1 > if
         plan NEW-SQL {: both:^ :}
         both scope k VEC:AT +NOT-NULL both o +NOT-NULL
         any s"  OR " +SEP any s" (" +SQL
         any both VEC:BYTES +SQL any s" )" +SQL
      then
   loop
   where s"  AND " +SEP where s" (" +SQL
   where any VEC:BYTES +SQL where s" )" +SQL ;

\ Minus: NOT EXISTS a row of P that meets the scope with a shared
\ variable bound on both sides; nothing when no variable can be.
: MINUS-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   r RELATION-BINDINGS @ {: scope:^ :}
   it ITEM-COLUMNS @ {: cols:^ :}
   scope cols SHARING {: sharing:n :}
   sharing 0= if exit then
   plan NEW-SQL {: v:^ :}
   v s" NOT EXISTS (SELECT 1 FROM " +SQL
   plan it ITEM-TABLE @ cols v +TABLE {: occs:^ :}
   plan NEW-SQL {: cond:^ :}
   plan plan scope COPY-SCOPE occs cond false MEET-ALL
   sharing 1 = if plan scope occs cond +BOUND-BOTH then
   cond VEC:COUNT 0<> if v s"  WHERE " +SQL v cond VEC:BYTES +SQL then
   v s" )" +SQL
   r RELATION-WHERE @ v VEC:BYTES +AND ;

\ The expression's SQL over the relation's bindings through the substitution.
: EXPRESSION-SQL ( ^plan ^relation n -- str )
   {: plan r:^ e:n :}
   plan PLAN-SCOPE @ {: outer:^ :}
   plan PLAN-OUT @ {: out:^ :}
   plan NEW-SQL {: v:^ :}
   plan r RELATION-BINDINGS @ SEEN plan PLAN-SCOPE ! v plan PLAN-OUT !
   plan e SQL-EXPRESSION
   outer plan PLAN-SCOPE ! out plan PLAN-OUT ! v VEC:BYTES ;

\ The index of the call among the plan's aggregates, or -1.
: FIND-AGGREGATE ( ^plan n -- n )
   {: plan call:n :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      as i VEC:AT AGGREGATE-NODE @ call = if i unloop exit then
   loop -1 ;

\ Whether the expression is an aggregate that is always bound.
: SURE? ( ^plan n -- bool )
   {: plan e:n :}
   plan e FIND-AGGREGATE {: k:n :}
   k 0< if false exit then
   plan PLAN-AGGREGATES @ k VEC:AT AGGREGATE-ALWAYS @ ;

\ The longest of the bindings' SQL that starts the text, when over len
\ bytes: its length and the bytes it copies.
: LONGEST ( n n ^VEC:vector<binding> str -- n n )
   {: bs:^ x:str :}
   bs VEC:COUNT 0 ?do
      bs i VEC:AT {: b:^ :}
      b BINDING-SQL @ {: sql:str :}
      over sql STR:LEN < if
         x sql STR:STARTS-WITH? if
            2drop sql STR:LEN b BINDING-COPIES @
         then
      then
   loop ;

\ The bytes the expression copies through the bindings it reads.
: COPIES-OF ( ^plan ^relation str -- n )
   {: plan r:^ x:str :}
   0 0 begin dup x STR:LEN < while
      {: sum:n at:n :}
      x str> at - swap at + swap >str {: rest:str :}
      0 0 r RELATION-BINDINGS @ rest LONGEST
         plan PLAN-OUTER @ rest LONGEST {: len:n copies:n :}
      sum copies + at len 1 max +
   repeat drop ;

\ Binds the variable to the expression itself.
: +COPIED ( ^plan ^relation n str n -- )
   {: plan r:^ var:n x:str copies:n :}
   plan NEW-SQL {: v:^ :}
   v s" (" +SQL v x +SQL v s" )" +SQL
   r RELATION-BINDINGS @ {: bs:^ :}
   bs var GENERIC v VEC:BYTES s" " BIND
   copies bs LAST BINDING-COPIES ! ;

\ The children of node n that may hold a call: two walked on their own
\ and the one the walk goes on to, 0 for none. A list or a chain goes
\ on along its long side.
: CHILDREN ( ^syntax n -- n n n )
   {: state node:n :}
   state node NODE-K {: kind:n :}
   state node NODE-A@ state node NODE-B@ {: a:n b:n :}
   kind N-LIST = if a 0 b exit then
   kind N-BINARY = kind N-UNION = or if b 0 a exit then
   kind N-SELECT = if a state node NODE-D@ b exit then
   kind N-GRAPH = if 0 0 b exit then
   kind N-TRIPLE = kind N-VALUES = or if 0 0 0 exit then
   0 0 a ;

\ Whether node n or a node below it, an EXISTS's group's included, calls
\ RAND, UUID or STRUUID, which give a new value at each call.
: VOLATILE? ( ^syntax n -- bool )
   {: state top:n :}
   top begin dup 0<> while
      {: node:n :}
      state node NODE-K N-CALL = if
         state node s" RAND UUID STRUUID" CALL-IN? if true exit then
      then
      state node CHILDREN {: one:n two:n next:n :}
      state one RECURSE state two RECURSE or if true exit then
      next
   repeat drop false ;

\ Makes r a derived table of its bindings and the expression as the
\ variable. PostgreSQL evaluates a materialized CTE once; a LATERAL item or
\ a derived table with OFFSET 0 it may evaluate once for several solutions
\ or again for each row one joins.
: +EVALUATED ( ^plan ^relation n str -- )
   {: plan r:^ var:n x:str :}
   r RELATION-BINDINGS @ var GENERIC x s" " BIND
   plan r SUBSELECT {: t:str cols:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (WITH c AS MATERIALIZED " +SQL v t +SQL
   v s"  SELECT * FROM c)" +SQL
   plan r INIT-RELATION
   plan v VEC:BYTES cols r RELATION-FROM @ +TABLE {: bs:^ :}
   bs VEC:COUNT 0 ?do 0 bs i VEC:AT BINDING-COPIES ! loop
   bs r RELATION-BINDINGS ! ;

\ Binds ?x of `(e AS ?x)` or BIND(e AS ?x) in r.
: +ALIAS ( ^plan ^relation n -- )
   {: plan r:^ e:n :}
   plan TREE {: state :}
   plan state e NODE-B@ VAR {: var:n :}
   state e NODE-A@ {: value:n :}
   plan r value EXPRESSION-SQL {: x:str :}
   state value VOLATILE? x STR:LEN MOST-COPIED > or if
      plan r var x +EVALUATED
   else
      plan r x COPIES-OF {: copies:n :}
      x STR:LEN copies + MOST-COPIED >
      if plan r var x +EVALUATED else plan r var x copies +COPIED then
   then
   r RELATION-BINDINGS @ LAST {: b:^ :}
   plan value SURE? b BINDING-ALWAYS !
   plan b r RELATION-WHERE @ +OUTER ;

: FOLD ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   it ITEM-MODE @ {: mode:n :}
   mode N-OPTIONAL = if plan it r OPTIONAL-STEP exit then
   mode N-MINUS = if plan it r MINUS-STEP exit then
   mode N-BIND = if plan r it ITEM-NODE @ +ALIAS exit then
   plan it r JOIN-STEP ;

\ A walk's triple, the end it starts from and the end it reaches; start
\ and end are the forms of its steps' two ends, kind its seeds' form;
\ pairs is the steps' table, with once a CTE. Its CTEs are named name and
\ a letter.
STRUCTURE walk DERIVE addr
   FIELD node n
   FIELD seed n
   FIELD far n
   FIELD inverse bool
   FIELD rest bool
   FIELD op str
   FIELD operand n
   FIELD name str
   FIELD pairs str
   FIELD once bool
   FIELD start n
   FIELD end n
   FIELD kind n
   FIELD ctes ^VEC:vector<u8>
;STRUCTURE

\ Whether the node is a variable r binds always.
: BOUND? ( ^plan ^relation n -- bool )
   {: plan r:^ node:n :}
   plan TREE node VARIABLE? 0= if false exit then
   r RELATION-BINDINGS @ plan node VAR FIND-VAR {: k:n :}
   k 0< if false exit then
   r RELATION-BINDINGS @ k VEC:AT BINDING-ALWAYS @ ;

: SEED! ( ^walk n n bool bool -- )
   {: w:^ seed:n far:n inverse:bool rest:bool :}
   seed w WALK-SEED ! far w WALK-FAR !
   inverse w WALK-INVERSE ! rest w WALK-REST ! ;

\ The index of the substitution's binding of the end when it binds it
\ always, else -1.
: OUTER-ALWAYS ( ^plan n -- n )
   {: plan node:n :}
   plan TREE node VARIABLE? 0= if -1 exit then
   plan PLAN-OUTER @ {: outer:^ :}
   outer plan node VAR FIND-VAR {: k:n :}
   k 0< if -1 exit then
   outer k VEC:AT BINDING-ALWAYS @ if k else -1 then ;

\ Whether the end's value is given: a constant, or a variable the
\ substitution binds always.
: GIVEN? ( ^plan n -- bool )
   {: plan node:n :}
   plan TREE node VARIABLE? 0= if true exit then
   plan node OUTER-ALWAYS -1 > ;

\ The walk of the triple: from a given end, else from an end r binds
\ always, the subject first; else from every node.
: NEW-WALK ( ^plan n ^relation -- ^walk )
   {: plan triple:n r:^ :}
   plan TREE {: state :}
   plan AL [type] walk ALLOC:NEW {: w:^ :}
   triple w WALK-NODE !
   state triple NODE-B@ {: path:n :}
   state path TEXT$ w WALK-OP !
   state path NODE-A@ w WALK-OPERAND !
   plan NEXT-ALIAS w WALK-NAME !
   plan NEW-SQL w WALK-CTES !
   state triple NODE-A@ {: s:n :} state triple NODE-C@ {: o:n :}
   plan s GIVEN? if w s o false false SEED! w exit then
   plan o GIVEN? if w o s true false SEED! w exit then
   plan r s BOUND? if w s o false true SEED! w exit then
   plan r o BOUND? if w o s true true SEED! w exit then
   w s o false false SEED! w ;

: PLUS? ( ^walk -- bool ) WALK-OP @ s" +" STR:= ;

: CTE-NAME ( ^plan ^walk str -- str )
   {: plan w:^ s:str :}
   plan NEW-SQL {: v:^ :}
   v w WALK-NAME @ +SQL v s +SQL v VEC:BYTES ;

\ The variable's native shape in the list, else GENERIC.
: SHAPE-OF ( ^VEC:vector<binding> n -- n )
   {: bs:^ var:n :}
   bs var FIND-VAR {: k:n :}
   k 0< if GENERIC exit then
   bs k VEC:AT BINDING-SHAPE @ dup 0< if drop GENERIC then ;

\ A value of the shape at sql, outside any scope.
: VALUE ( ^plan n str -- ^binding )
   {: plan shape:n s:str :}
   plan NEW-BINDINGS {: tmp:^ :}
   tmp -1 shape s s" " BIND tmp LAST ;

\ A given end's value: the constant, or the substitution's binding.
: GIVEN ( ^plan n -- ^binding )
   {: plan node:n :}
   plan node OUTER-ALWAYS {: k:n :}
   k 0< if plan node CONSTANT-VALUE exit then
   plan PLAN-OUTER @ k VEC:AT ;

\ Appends ` AND ` and the condition unless it is empty.
: +CONDITION ( ^VEC:vector<u8> str -- )
   {: v:^ a:str :}
   a STR:LEN 0= if exit then v a +AND ;

\ Appends the variable's value in the list in the form, ` AS ` and name.
: +PAIR ( ^plan ^VEC:vector<u8> ^VEC:vector<binding> n n str -- )
   {: plan v:^ bs:^ var:n form:n name:str :}
   bs var FIND-VAR {: k:n :}
   k 0< if v s" NULL::jsonb" +SQL else plan v bs k VEC:AT form +VALUE then
   v s"  AS " +SQL v name +SQL ;

\ Whether the path has a walk in it.
: WALKED? ( ^syntax n -- bool )
   {: state path:n :}
   path begin {: node:n :}
      state node NODE-K N-PATH <> if false exit then
      state node TEXT$ {: op:str :}
      op s" +" STR:= op s" *" STR:= or op s" ?" STR:= or if true exit then
      op s" !" STR:= if false exit then
      state node NODE-B@ dup 0<> if
         state swap RECURSE if true exit then
      else drop then
      state node NODE-A@
   again ;

\ The steps' table: the query, or, when the operand has a walk in it, CTE
\ nameP of it. A lookup cannot reach into that walk, so each step would
\ walk it again, and its text, written for each use, would grow
\ exponentially with nesting.
: PAIRS! ( ^plan ^walk str -- )
   {: plan w:^ t:str :}
   plan TREE w WALK-OPERAND @ WALKED? w WALK-ONCE !
   w WALK-ONCE @ 0= if t w WALK-PAIRS ! exit then
   plan w s" p" CTE-NAME {: n:str :}
   plan PLAN-CTES @ {: v:^ :}
   v s" ," +SEP v n +SQL v s"  AS MATERIALIZED " +SQL v t +SQL
   n w WALK-PAIRS ! ;

\ The walk's steps: the operand from fresh a to fresh b, b to a walking
\ from the object, as `(SELECT a, b ...)`, and the forms of a and b.
: STEPS ( ^plan ^walk -- )
   {: plan w:^ :}
   plan TREE {: state :}
   state ANONYMOUS {: a:n :} state ANONYMOUS {: b:n :}
   w WALK-INVERSE @ if b a else a b then {: x:n y:n :}
   plan plan w WALK-NODE @ x w WALK-OPERAND @ y TRIPLE-NODE GROUP-NODE
      {: g:n :}
   plan g CLOSED {: rp:^ :}
   rp RELATION-EMPTY @ if
      GENERIC w WALK-START ! GENERIC w WALK-END !
      plan w s" (SELECT NULL::jsonb AS a,NULL::jsonb AS b WHERE false)"
         PAIRS! exit
   then
   rp RELATION-BINDINGS @ {: bs:^ :}
   plan a VAR {: av:n :} plan b VAR {: bv:n :}
   bs av SHAPE-OF w WALK-START ! bs bv SHAPE-OF w WALK-END !
   plan NEW-SQL {: v:^ :}
   v s" (SELECT " +SQL plan v bs av w WALK-START @ s" a," +PAIR
   plan v bs bv w WALK-END @ s" b" +PAIR
   v rp +FROM-WHERE v s" )" +SQL plan w v VEC:BYTES PAIRS! ;

\ r's solutions become CTE nameR, which r then reads.
: REST ( ^plan ^walk ^relation -- )
   {: plan w:^ r:^ :}
   plan r SUBSELECT {: t:str cols:^ :}
   plan w s" r" CTE-NAME {: n:str :}
   w WALK-CTES @ {: ctes:^ :}
   ctes s" ," +SEP ctes n +SQL ctes s"  AS " +SQL ctes t +SQL
   plan r INIT-RELATION
   plan n cols r RELATION-FROM @ +TABLE r RELATION-BINDINGS ! ;

\ The given end's value, native where it has a shape.
: GIVEN-SEEDS ( ^plan ^walk ^VEC:vector<u8> -- )
   {: plan w:^ v:^ :}
   plan w WALK-SEED @ GIVEN {: b:^ :}
   b BINDING-SHAPE @ dup 0< if drop GENERIC then {: kind:n :}
   kind w WALK-KIND !
   plan v b kind +VALUE v s"  AS k" +SQL ;

: PAIR-SEEDS ( ^plan ^walk ^VEC:vector<u8> -- )
   {: plan w:^ v:^ :}
   w WALK-START @ w WALK-KIND !
   v s" p.a AS k" +SQL
   v s"  FROM " +SQL v w WALK-PAIRS @ +SQL v s"  AS p" +SQL ;

\ Triples n q x and x q n of fresh variables at the walk's node, and n's
\ variable: a node of a source is n in either.
: NODE-TRIPLES ( ^plan ^walk -- n n n )
   {: plan w:^ :}
   plan TREE {: state :}
   state ANONYMOUS {: n:n :}
   state ANONYMOUS {: q:n :}
   state ANONYMOUS {: x:n :}
   plan w WALK-NODE @ n q x TRIPLE-NODE
   plan w WALK-NODE @ x q n TRIPLE-NODE plan n VAR ;

\ Every subject and object in the active graph: a UNION ALL's branches
\ binding a fresh variable to each, and that variable.
: NODE-BRANCHES ( ^plan ^walk -- ^VEC:vector<relation> n )
   {: plan w:^ :}
   plan w NODE-TRIPLES {: down:n up:n var:n :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 0 ?do
      plan i SOURCE@ {: src:^ :}
      plan src GRAPH-FITS? if
         plan down plan i RECORD branches VEC:PUSH SOURCE-BRANCH
         plan up plan i RECORD branches VEC:PUSH SOURCE-BRANCH
      then
   loop branches var ;

\ Appends to the statement's WITH the CTE of the active graph's nodes,
\ k their column; answers its name and k's form, empty when no source
\ is in the graph.
: NODE-CTE ( ^plan ^walk -- str n )
   {: plan w:^ :}
   plan w NODE-BRANCHES {: branches:^ var:n :}
   branches VEC:COUNT 0= if s" " GENERIC exit then
   plan branches UNION-TABLE {: t:str cols:^ :}
   plan NEXT-ALIAS {: name:str :}
   plan PLAN-CTES @ {: v:^ :}
   v s" ," +SEP v name +SQL v s"  AS (SELECT " +SQL
   v plan s" n" var COLUMN-OF +SQL v s"  AS k FROM " +SQL
   v t +SQL v s"  AS n)" +SQL
   name cols var SHAPE-OF ;

\ The active graph's node CTE and k's form, written by the first walk
\ that seeds from every node there and read by the later ones.
: NODES ( ^plan ^walk -- str n )
   {: plan w:^ :}
   plan PLAN-SETS @ {: sets:^ :}
   sets VEC:COUNT 0 ?do
      sets i VEC:AT {: set:^ :}
      set NODE-SET-GRAPH @ plan PLAN-GRAPH @ STR:= if
         set NODE-SET-NAME @ set NODE-SET-KIND @ unloop exit
      then
   loop
   plan w NODE-CTE {: name:str kind:n :}
   sets VEC:PUSH {: set:^ :}
   plan PLAN-GRAPH @ set NODE-SET-GRAPH !
   name set NODE-SET-NAME ! kind set NODE-SET-KIND !
   name kind ;

\ Appends `EXISTS (SELECT 1 ...)`, after ` OR ` unless v is empty: the
\ triple over record rec with its variable's slot meeting k inside the
\ record's own condition, where an index on the slot's column serves
\ it. Nothing when they cannot meet.
: +EXISTS ( ^plan n n n ^binding ^VEC:vector<u8> -- )
   {: plan triple:n rec:n var:n k:^ v:^ :}
   plan NEW-RELATION {: r:^ :}
   plan triple plan rec RECORD r SOURCE-BRANCH
   r RELATION-BINDINGS @ {: bs:^ :}
   bs bs var FIND-VAR VEC:AT {: b:^ :}
   plan b k COMPATIBLE? 0= if exit then
   r RELATION-WHERE @ plan b k SAME-SQL +CONDITION
   v s"  OR " +SEP v s" EXISTS (SELECT 1" +SQL
   v r +FROM-WHERE v s" )" +SQL ;

\ The condition that k is a node of the active graph, `false` when no
\ source can hold it.
: NODE-SQL ( ^plan ^walk ^binding -- str )
   {: plan w:^ k:^ :}
   plan w NODE-TRIPLES {: down:n up:n var:n :}
   plan NEW-SQL {: v:^ :}
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 0 ?do
      plan i SOURCE@ {: src:^ :}
      plan src GRAPH-FITS? if
         plan down i var k v +EXISTS
         plan up i var k v +EXISTS
      then
   loop
   v VEC:COUNT 0= if s" false" exit then
   v VEC:BYTES ;

\ The values r binds the seed to; for `*` and `?` only the nodes of the
\ active graph, as a seed answers itself by the zero-length path.
: REST-SEEDS ( ^plan ^walk ^relation ^VEC:vector<u8> -- )
   {: plan w:^ r:^ v:^ :}
   plan w WALK-SEED @ VAR {: var:n :}
   r RELATION-BINDINGS @ var SHAPE-OF w WALK-KIND !
   plan w s" r" CTE-NAME {: n:str :}
   plan n var COLUMN-OF {: k:str :}
   v k +SQL v s"  AS k" +SQL
   v s"  FROM " +SQL v n +SQL
   w PLUS? if exit then
   plan w WALK-KIND @ k VALUE {: b:^ :}
   v s"  WHERE (" +SQL v plan w b NODE-SQL +SQL v s" )" +SQL ;

: NODE-SEEDS ( ^plan ^walk ^VEC:vector<u8> -- )
   {: plan w:^ v:^ :}
   plan w NODES {: t:str kind:n :}
   kind w WALK-KIND !
   t STR:LEN 0= if v s" NULL::jsonb AS k WHERE false" +SQL exit then
   v s" k FROM " +SQL v t +SQL ;

\ Appends CTE nameS, the distinct seeds k the walk starts from.
: SEEDS ( ^plan ^walk ^relation -- )
   {: plan w:^ r:^ :}
   w WALK-CTES @ {: v:^ :}
   v s" ," +SEP v plan w s" s" CTE-NAME +SQL
   v s"  AS (SELECT DISTINCT " +SQL
   plan w WALK-SEED @ GIVEN? if plan w v GIVEN-SEEDS
   else w WALK-REST @ if plan w r v REST-SEEDS
   else w PLUS? if plan w v PAIR-SEEDS
   else plan w v NODE-SEEDS then then then
   v s" )" +SQL ;

\ Appends the join to the walk's pairs p on the condition, `true` when
\ empty: ` JOIN nameP AS p ON` it, else ` CROSS JOIN LATERAL (SELECT *
\ FROM` the query ` AS p WHERE` it ` OFFSET 0) AS p`. The fence keeps
\ PostgreSQL from flattening the lookup into a hash join that scans every
\ pair on each step: a 1,000-node `p+` took 1.1 s that way.
: +STEP ( ^VEC:vector<u8> ^walk str -- )
   {: v:^ w:^ c:str :}
   w WALK-ONCE @ {: once:bool :}
   once if v s"  JOIN " +SQL
   else v s"  CROSS JOIN LATERAL (SELECT * FROM " +SQL then
   v w WALK-PAIRS @ +SQL v s"  AS p" +SQL
   once if v s"  ON " +SQL else v s"  WHERE " +SQL then
   c STR:LEN 0= if v s" true" +SQL else v c +SQL then
   once 0= if v s"  OFFSET 0) AS p" +SQL then ;

\ Appends ` AS n`, the step end p.b in the steps' start form.
: +KEY ( ^plan ^VEC:vector<u8> ^walk -- )
   {: plan v:^ w:^ :}
   w WALK-START @ {: start:n :}
   v s" ," +SQL
   start 0< if
      plan v plan w WALK-END @ s" p.b" VALUE start +VALUE
   else v plan start SHAPE@ SHAPE-IN @ s" p.b" +CALL then
   v s"  AS n" +SQL ;

\ Appends CTE nameW(k, e[, n]): each seed k and the ends e of its
\ steps, for `?` one step, else UNION (distinct) each further step, so a
\ cycle ends. When one of the steps' start and end forms is a term, n is
\ e in the start form, converted once per end so that a step compares
\ columns: converting for each pair scanned put the host's term parser
\ in the inner loop.
: WALK-CTE ( ^plan ^walk -- )
   {: plan w:^ :}
   w WALK-OP @ s" ?" STR:= 0= {: more:bool :}
   w WALK-START @ {: start:n :} w WALK-END @ {: end:n :}
   more start 0< end 0< xor and {: key:bool :}
   w WALK-CTES @ {: v:^ :}
   plan w s" s" CTE-NAME {: s:str :}
   plan w s" w" CTE-NAME {: n:str :}
   v s" ," +SQL v n +SQL
   v s"  AS (SELECT s.k AS k,p.b AS e" +SQL
   key if plan v w +KEY then
   v s"  FROM " +SQL v s +SQL v s"  AS s" +SQL
   v w plan plan start s" p.a" VALUE plan w WALK-KIND @ s" s.k" VALUE
      SAME-SQL +STEP
   more if
      v s"  UNION SELECT w.k,p.b" +SQL key if plan v w +KEY then
      v s"  FROM " +SQL v n +SQL v s"  AS w" +SQL
      key if plan start s" w.n" VALUE else plan end s" w.e" VALUE then
         {: at:^ :}
      v w plan plan start s" p.a" VALUE at SAME-SQL +STEP
   then v s" )" +SQL ;

\ An end of the walk whose value in a part is b: a constant meets b; a
\ variable's first end is a column of the form, a later one meets it.
: +END ( ^plan n ^binding n ^VEC:vector<binding> ^VEC:vector<binding>
      ^VEC:vector<u8> ^VEC:vector<u8> -- )
   {: plan node:n b:^ form:n seen:^ cols:^ list:^ cond:^ :}
   plan TREE node VARIABLE? 0= if
      cond plan b plan node CONSTANT-VALUE SAME-SQL +CONDITION exit
   then
   plan node VAR {: var:n :}
   seen var FIND-VAR {: k:n :}
   k -1 > if cond plan seen k VEC:AT b SAME-SQL +CONDITION exit then
   b seen COPY-BINDING var seen LAST BINDING-VAR !
   cols var form s" " s" " BIND
   list s" ," +SEP plan list b form +VALUE
   list s"  AS " +SQL list plan var COLUMN$ +SQL ;

\ Appends `SELECT ... FROM t WHERE ...`, a part of the walk's answer
\ whose start is k and end e, the end in the form; answers its columns.
: +PART ( ^plan ^walk str ^binding ^binding n ^VEC:vector<u8>
      -- ^VEC:vector<binding> )
   {: plan w:^ t:str k:^ e:^ form:n v:^ :}
   plan NEW-BINDINGS {: seen:^ :} plan NEW-BINDINGS {: cols:^ :}
   plan NEW-SQL {: list:^ :} plan NEW-SQL {: cond:^ :}
   plan w WALK-SEED @ GIVEN? 0= if
      plan w WALK-SEED @ k w WALK-KIND @ seen cols list cond +END
   then
   plan w WALK-FAR @ e form seen cols list cond +END
   v s" SELECT " +SQL v list VEC:BYTES +SQL v s"  FROM " +SQL v t +SQL
   cond VEC:COUNT 0<> if v s"  WHERE " +SQL v cond VEC:BYTES +SQL then cols ;

\ The walk's answer as a derived table and its columns: its rows, and
\ for `*` and `?` each seed as its own end, the zero-length path. An end
\ is native where the seeds and the steps' ends share a shape.
: ANSWER ( ^plan ^walk -- str ^VEC:vector<binding> )
   {: plan w:^ :}
   w WALK-KIND @ {: kind:n :} w WALK-END @ {: end:n :}
   w PLUS? kind end = or if end else GENERIC then {: form:n :}
   plan NEW-SQL {: v:^ :}
   v s" (" +SQL
   w WALK-REST @ 0= if
      v s" WITH RECURSIVE " +SQL v w WALK-CTES @ VEC:BYTES +SQL v s"  " +SQL
   then
   plan w plan w s" w" CTE-NAME plan kind s" k" VALUE plan end s" e" VALUE
      form v +PART {: cols:^ :}
   w PLUS? 0= if
      v s"  UNION " +SQL
      plan w plan w s" s" CTE-NAME plan kind s" k" VALUE
         plan kind s" k" VALUE form v +PART drop
   then
   v s" )" +SQL v VEC:BYTES cols ;

\ r, reading nameR joined with the walk, becomes one derived table under
\ the walk's CTEs.
: WRAP ( ^plan ^walk ^relation -- )
   {: plan w:^ r:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (WITH RECURSIVE " +SQL v w WALK-CTES @ VEC:BYTES +SQL v s"  " +SQL
   plan v r +SELECT v s" )" +SQL
   r RELATION-BINDINGS @ {: cols:^ :}
   plan r INIT-RELATION
   plan v VEC:BYTES cols r RELATION-FROM @ +TABLE r RELATION-BINDINGS ! ;

\ The walk's answer, its ends meeting the substitution, joined to r,
\ which a seed r binds first becomes.
: WALK-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   plan it ITEM-NODE @ r NEW-WALK {: w:^ :}
   plan w STEPS
   w WALK-REST @ if plan w r REST then
   plan w r SEEDS
   plan w WALK-CTE
   plan w ANSWER {: t:str cols:^ :}
   plan t cols SUBSTITUTED {: a:str :}
   r RELATION-FROM @ {: from:^ :}
   from s"  CROSS JOIN " +SEP
   plan r RELATION-BINDINGS @ plan a cols from +TABLE
      r RELATION-WHERE @ false MEET-ALL
   w WALK-REST @ if plan w r WRAP then ;

\ Folds the walks among items from..to, each under its item's graph.
: WALKS ( ^plan ^VEC:vector<item> n n ^relation -- )
   {: plan items:^ from:n to:n r:^ :}
   plan PLAN-GRAPH @ {: outer:str :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-MODE @ N-PATH = if
         it ITEM-GRAPH @ plan PLAN-GRAPH !
         plan it r WALK-STEP
         outer plan PLAN-GRAPH !
      then
   loop ;

\ Whether the item is a joined triple whose live choices come from
\ several description sources.
: UNION? ( ^plan ^item -- bool )
   {: plan it:^ :}
   it ITEM-MODE @ 0<> it ITEM-NODE @ 0= or if false exit then
   plan it ORIGINS VEC:COUNT 1 > ;

: DRIVER? ( ^item -- bool )
   {: it:^ :}
   it ITEM-MODE @ 0= it ITEM-UNION @ 0= and ;

\ The variable that stands for var's component, its own link.
: HEAD ( ^VEC:vector<n> n -- n )
   {: links:^ var:n :}
   var begin links over VEC:AT @ {: up:n :} up over <> while
      drop up
   repeat ;

\ The head of the variables the item binds always, -1 when there are none.
: COMPONENT ( ^VEC:vector<n> ^item -- n )
   {: links:^ it:^ :}
   it ITEM-CHOICES @ 0 VEC:AT CHOICE-KINDS @ {: kinds:^ :}
   kinds VEC:COUNT 0 ?do
      kinds i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if
         links b BINDING-VAR @ HEAD unloop exit
      then
   loop -1 ;

\ Puts the variables the item binds always in one component.
: LINK ( ^VEC:vector<n> ^item -- )
   {: links:^ it:^ :}
   links it COMPONENT {: head:n :}
   it ITEM-CHOICES @ 0 VEC:AT CHOICE-KINDS @ {: kinds:^ :}
   kinds VEC:COUNT 0 ?do
      kinds i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if
         head links links b BINDING-VAR @ HEAD VEC:AT !
      then
   loop ;

\ A link per variable: two that a driver among items from..to binds
\ always have one head.
: COMPONENTS ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<n> )
   {: plan items:^ from:n to:n :}
   plan INDEXES {: links:^ :}
   plan PLAN-VARIABLES @ VEC:COUNT 0 ?do i links VEC:PUSH ! loop
   to from ?do
      items i VEC:AT {: it:^ :}
      it DRIVER? if links it LINK then
   loop links ;

\ The drivers of the component among items from..to, folded in order.
: DRIVERS ( ^plan ^VEC:vector<item> n n ^VEC:vector<n> n -- ^relation )
   {: plan items:^ from:n to:n links:^ head:n :}
   plan NEW-RELATION {: d:^ :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it DRIVER? if
         links it COMPONENT head = if plan it d JOIN-STEP then
      then
   loop d ;

\ Leaves r the bindings of the listed variables it binds always, its
\ null tests written first, since it drops bindings.
: KEEP-READ ( ^plan ^VEC:vector<n> ^relation -- )
   {: plan vars:^ r:^ :}
   r +NULL-TESTS
   r RELATION-BINDINGS @ {: bs:^ :}
   plan NEW-BINDINGS {: cols:^ :}
   vars VEC:COUNT 0 ?do
      bs vars i VEC:AT @ FIND-VAR {: k:n :}
      k -1 > if
         bs k VEC:AT {: b:^ :}
         b BINDING-ALWAYS @ if b cols COPY-BINDING then
      then
   loop cols r RELATION-BINDINGS ! ;

\ Appends to the plan's ctes r's bindings as a CTE, which r then reads.
: HOIST ( ^plan ^relation -- )
   {: plan r:^ :}
   r RELATION-BINDINGS @ {: cols:^ :}
   plan NEXT-ALIAS {: name:str :}
   plan PLAN-CTES @ {: ctes:^ :}
   ctes s" ," +SEP ctes name +SQL
   ctes s"  AS MATERIALIZED (" +SQL plan ctes r +SELECT ctes s" )" +SQL
   plan r INIT-RELATION
   r RELATION-FROM @ name +SQL
   cols VEC:COUNT 0 ?do
      cols i VEC:AT BINDING-VAR @ {: var:n :}
      r RELATION-BINDINGS @ var cols var SHAPE-OF
         plan name var COLUMN-OF s" " BIND
   loop ;

\ Appends to the plan's ctes the variable's key CTE, the distinct values
\ r binds it to, and adds it to keys as a binding whose sql names the CTE.
: +KEY-CTE ( ^plan ^relation n ^VEC:vector<binding> -- )
   {: plan r:^ var:n keys:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs var SHAPE-OF {: form:n :}
   plan NEXT-ALIAS {: name:str :}
   plan PLAN-CTES @ {: ctes:^ :}
   ctes s" ," +SEP ctes name +SQL
   ctes s"  AS MATERIALIZED (SELECT DISTINCT " +SQL
   plan ctes bs bs var FIND-VAR VEC:AT form +VALUE
   ctes s"  AS k" +SQL ctes r +FROM-WHERE ctes s" )" +SQL
   keys var form name s" " BIND ;

\ Adds the keys of the listed variables that the component's drivers r
\ bind always.
: +KEYS-OF ( ^plan ^VEC:vector<n> ^relation ^VEC:vector<binding> -- )
   {: plan vars:^ r:^ keys:^ :}
   plan vars r KEEP-READ
   r RELATION-BINDINGS @ VEC:COUNT 1 > if plan r HOIST then
   r RELATION-BINDINGS @ {: bs:^ :}
   bs VEC:COUNT 0 ?do plan r bs i VEC:AT BINDING-VAR @ keys +KEY-CTE loop ;

\ Whether the union item takes keys: not when its subject or object is
\ given, which PostgreSQL pushes into each branch itself, nor inside an
\ EXISTS, whose substitution a CTE at the statement cannot read.
: KEYED? ( ^plan ^item -- bool )
   {: plan it:^ :}
   plan PLAN-OUTER @ VEC:COUNT 0<> if false exit then
   plan TREE {: state :}
   it ITEM-NODE @ {: triple:n :}
   plan state triple NODE-A@ GIVEN?
   plan state triple NODE-C@ GIVEN? or 0= ;

\ Adds to the list the item's variables not yet there.
: +READ-VARS ( ^plan ^item ^VEC:vector<n> -- )
   {: plan it:^ vars:^ :}
   plan TREE {: state :}
   3 0 do
      state it ITEM-NODE @ i PART {: node:n :}
      plan node GIVEN? 0= if
         plan node VAR {: var:n :}
         vars var INDEXED? 0= if var vars VEC:PUSH ! then
      then
   loop ;

\ The variables that the unions among items from..to that take keys read.
: READ-VARS ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<n> )
   {: plan items:^ from:n to:n :}
   plan INDEXES {: vars:^ :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-UNION @ if
         plan it KEYED? if plan it vars +READ-VARS then
      then
   loop vars ;

\ The keys r's bindings would give, each in its form, naming no CTE.
: FORMS ( ^plan ^relation -- ^VEC:vector<binding> )
   {: plan r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan NEW-BINDINGS {: keys:^ :}
   bs VEC:COUNT 0 ?do
      bs i VEC:AT BINDING-VAR @ {: var:n :}
      keys var bs var SHAPE-OF s" " s" " BIND
   loop keys ;

\ The index in keys of the item's DRIVER when it is a keyed union, else
\ -1.
: DRIVES ( ^plan ^VEC:vector<binding> ^item -- n )
   {: plan keys:^ it:^ :}
   it ITEM-UNION @ 0= if -1 exit then
   plan it KEYED? 0= if -1 exit then
   plan keys it ITEM-BRANCHES @ DRIVER ;

\ The variables of keys that hits holds, in keys' order.
: IN-ORDER ( ^plan ^VEC:vector<binding> ^VEC:vector<n>
      -- ^VEC:vector<n> )
   {: plan keys:^ hits:^ :}
   plan INDEXES {: vars:^ :}
   keys VEC:COUNT 0 ?do
      keys i VEC:AT BINDING-VAR @ {: var:n :}
      hits var INDEXED? if var vars VEC:PUSH ! then
   loop vars ;

\ Adds to driven the index of each union among items from..to not there
\ that one of keys DRIVES; answers those keys' variables.
: DRIVING ( ^plan ^VEC:vector<item> n n ^VEC:vector<binding>
      ^VEC:vector<n> -- ^VEC:vector<n> )
   {: plan items:^ from:n to:n keys:^ driven:^ :}
   plan INDEXES {: hits:^ :}
   to from ?do
      driven i INDEXED? 0= if
         plan keys items i VEC:AT DRIVES {: d:n :}
         d -1 > if
            i driven VEC:PUSH !
            keys d VEC:AT BINDING-VAR @ hits VEC:PUSH !
         then
      then
   loop plan keys hits IN-ORDER ;

\ The keys of the unions among items from..to: of each component in
\ turn, those that drive a union no earlier component's keys drive.
: RUN-KEYS ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<binding> )
   {: plan items:^ from:n to:n :}
   plan NEW-BINDINGS {: keys:^ :}
   plan items from to READ-VARS {: vars:^ :}
   vars VEC:COUNT 0= if keys exit then
   plan items from to COMPONENTS {: links:^ :}
   plan INDEXES {: done:^ :} plan INDEXES {: driven:^ :}
   vars VEC:COUNT 0 ?do
      links vars i VEC:AT @ HEAD {: head:n :}
      done head INDEXED? 0= if
         head done VEC:PUSH !
         plan items from to links head DRIVERS {: d:^ :}
         plan vars d KEEP-READ
         plan items from to plan d FORMS driven DRIVING
         {: used:^ :} plan used d keys +KEYS-OF
      then
   loop keys ;

\ Joins the union item into r, driven by the run's keys if it takes any.
: UNION-STEP ( ^plan ^item ^VEC:vector<binding> ^relation -- )
   {: plan it:^ run:^ r:^ :}
   plan it KEYED? if run else plan NEW-BINDINGS then {: keys:^ :}
   plan r RELATION-BINDINGS @ plan it keys r UNION-SQL
   r RELATION-WHERE @ false MEET-ALL ;

\ Folds the unions among items from..to, then the walks.
: LATE ( ^plan ^VEC:vector<item> n n ^relation -- )
   {: plan items:^ from:n to:n r:^ :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-UNION @ if plan it BRANCHES then
   loop
   plan items from to RUN-KEYS {: keys:^ :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-UNION @ if plan it keys r UNION-STEP then
   loop
   plan items from to r WALKS ;

\ Marks each variable the list binds always.
: +ALWAYS ( ^VEC:vector<binding> ^VEC:vector<bool> -- )
   {: list:^ bound:^ :}
   list VEC:COUNT 0 ?do
      list i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if
         true bound b BINDING-VAR @ VEC:AT !
      then
   loop ;

\ The variables marked that each solution has before r's next item.
: BOUND-NOW ( ^plan ^relation -- ^VEC:vector<bool> )
   {: plan r:^ :}
   plan AL plan PLAN-VARIABLES @ VEC:COUNT [type] bool VEC:NEW {: bound:^ :}
   plan PLAN-OUTER @ bound +ALWAYS
   r RELATION-BINDINGS @ bound +ALWAYS bound ;

: GIVEN-END? ( ^plan ^item -- bool )
   {: plan it:^ :}
   it ITEM-NODE @ {: triple:n :}
   triple 0= if true exit then
   plan TREE {: state :}
   plan state triple NODE-A@ GIVEN? if true exit then
   plan state triple NODE-C@ GIVEN? ;

\ For each item: 1 when it has a given end, 0 when not, -1 for a walk.
: ENDS ( ^plan ^VEC:vector<item> -- ^VEC:vector<n> )
   {: plan items:^ :}
   plan INDEXES {: ends:^ :}
   items VEC:COUNT 0 ?do
      items i VEC:AT {: it:^ :}
      it ITEM-MODE @ N-PATH = if -1
      else plan it GIVEN-END? if 1 else 0 then then
      ends VEC:PUSH !
   loop ends ;

: ITEM-KINDS ( ^item -- ^VEC:vector<binding> )
   ITEM-CHOICES @ 0 VEC:AT CHOICE-KINDS @ ;

: SHARES? ( ^item ^VEC:vector<bool> -- bool )
   {: it:^ bound:^ :}
   it ITEM-KINDS {: kinds:^ :}
   kinds VEC:COUNT 0 ?do
      bound kinds i VEC:AT BINDING-VAR @ VEC:AT @ if
         true unloop exit
      then
   loop false ;

\ The first item of the highest rank, -1 in ends out of the running.
: NEXT-ITEM
      ( ^VEC:vector<item> ^VEC:vector<n> ^VEC:vector<bool> -- n )
   {: items:^ ends:^ bound:^ :}
   -1 -1 items VEC:COUNT 0 ?do
      ends i VEC:AT @ {: end:n :}
      end -1 > if
         items i VEC:AT bound SHARES? if end 2 + else end then
         {: best:n at:n rank:n :}
         rank best > if rank i else best at then
      then
   loop nip ;

: COPY-ITEM ( ^item ^item -- )
   {: from:^ to:^ :}
   from ITEM-NODE @ to ITEM-NODE !
   from ITEM-MODE @ to ITEM-MODE !
   from ITEM-TABLE @ to ITEM-TABLE !
   from ITEM-COLUMNS @ to ITEM-COLUMNS !
   from ITEM-FILTERS @ to ITEM-FILTERS !
   from ITEM-CHOICES @ to ITEM-CHOICES !
   from ITEM-GRAPH @ to ITEM-GRAPH !
   from ITEM-UNION @ to ITEM-UNION !
   from ITEM-BRANCHES @ to ITEM-BRANCHES ! ;

\ Copies of items from..to.
: COPY-RUN ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<item> )
   {: plan items:^ from:n to:n :}
   plan AL 0 [type] item VEC:NEW {: run:^ :}
   to from ?do items i VEC:AT run VEC:PUSH COPY-ITEM loop run ;

\ Puts the joined items among from..to, which r's next items are, in
\ connected order; a walk keeps its place.
: CONNECT ( ^plan ^VEC:vector<item> n n ^relation -- )
   {: plan items:^ from:n to:n r:^ :}
   plan items from to COPY-RUN {: run:^ :}
   plan run ENDS {: ends:^ :}
   plan r BOUND-NOW {: bound:^ :}
   to from ?do
      items i VEC:AT {: place:^ :}
      place ITEM-MODE @ 0= if
         run ends bound NEXT-ITEM {: k:n :}
         run k VEC:AT {: it:^ :}
         it place COPY-ITEM
         it ITEM-KINDS bound +ALWAYS
         -1 ends k VEC:AT !
      then
   loop ;

\ Folds items from..to, a maximal run of joined items and walks.
: RUN ( ^plan ^VEC:vector<item> n n ^relation -- )
   {: plan items:^ from:n to:n r:^ :}
   plan items from to r CONNECT
   to from ?do
      items i VEC:AT {: it:^ :}
      it DRIVER? if plan it r FOLD then
   loop
   plan items from to r LATE ;

\ The items folded run by run, a run's unions and then its walks after
\ its other joined items: joins commute, and a union's keys read those
\ other items; a left join, a minus and an Extend read the solutions so
\ far.
: FOLD-ALL ( ^plan ^VEC:vector<item> ^relation -- )
   {: plan items:^ r:^ :}
   0 items VEC:COUNT 0 ?do
      items i VEC:AT {: it:^ :}
      plan it UNION? it ITEM-UNION !
      it ITEM-MODE @ {: mode:n :}
      mode 0<> mode N-PATH <> and if
         plan items rot i r RUN plan it r FOLD i 1+
      then
   loop
   plan items rot items VEC:COUNT r RUN ;

\ r's solutions are a union's when r folded that one item alone and no
\ FILTER.
: KEEP-BRANCHES ( ^VEC:vector<item> ^VEC:vector<n> ^relation -- )
   {: items:^ filters:^ r:^ :}
   items VEC:COUNT 1 <> filters VEC:COUNT 0<> or if exit then
   items 0 VEC:AT ITEM-BRANCHES @ r RELATION-BRANCHES ! ;

\ The group's items folded in order, and its FILTERs.
: GROUP ( ^plan n -- ^relation ^VEC:vector<n> )
   {: plan group:n :}
   plan AL 0 [type] item VEC:NEW {: items:^ :}
   plan AL 0 [type] n VEC:NEW {: filters:^ :}
   plan group items filters ITEMS
   plan items PRUNE
   plan NEW-RELATION {: r:^ :}
   items DEAD? r RELATION-EMPTY !
   r RELATION-EMPTY @ 0= if
      plan items r FOLD-ALL items filters r KEEP-BRANCHES
   then r filters ;

\ [NOT] EXISTS (SELECT 1 ...): the call's group with the scope as its
\ substitution.
: EXISTS+ ( ^plan n -- )
   {: plan call:n :}
   plan PLAN-OUT @ {: out:^ :}
   plan PLAN-OUTER @ {: outer:^ :}
   plan PLAN-SCOPE @ plan PLAN-OUTER !
   plan TREE call s" NOT EXISTS" CALL-IS? {: negated:bool :}
   plan plan TREE call NODE-A@ CLOSED {: r:^ :}
   outer plan PLAN-OUTER !
   r RELATION-EMPTY @ if
      negated if out s" true" +SQL else out s" false" +SQL then exit
   then
   negated if out s" NOT " +SQL then
   out s" EXISTS (SELECT 1" +SQL out r +FROM-WHERE out s" )" +SQL ;

\ The query's LIMIT and OFFSET.
: SLICE-SQL ( ^plan n ^VEC:vector<u8> -- )
   {: plan query:n v:^ :}
   plan TREE {: state :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: m:n :}
      state m NODE-K {: kind:n :}
      kind N-LIMIT = kind N-OFFSET = or if
         kind N-LIMIT = if v s"  LIMIT " +SQL else v s"  OFFSET " +SQL then
         v plan state m NODE NODE-VALUE @ NUMBER-ARG +SQL
         v s" ::bigint" +SQL
      then state list NODE-B@
   repeat drop ;

\ `ROW(...)` of the relation's anchors.
: ROW-SQL ( ^plan ^relation -- str )
   {: plan r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan NEW-SQL {: v:^ :}
   v s" ROW(" +SQL
   bs VEC:COUNT 0 ?do
      i 0<> if v s" ," +SQL then v bs i VEC:AT BINDING-SQL @ +SQL
   loop v s" )" +SQL v VEC:BYTES ;

\ COUNT's operand: `*`, for DISTINCT * the row of every variable, a bound
\ variable's anchor, native when native, else the expression's term.
: COUNTED ( ^plan ^relation n -- str )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   state call NODE NODE-FLAGS @ {: flags:n :}
   flags F-STAR and 0<> if
      flags F-DISTINCT and 0= if s" *" exit then
      plan r ROW-SQL exit
   then
   state state call NODE-A@ NODE-A@ {: e:n :}
   r RELATION-BINDINGS @ {: bs:^ :}
   state e NODE-K N-VAR = if
      bs plan e VAR FIND-VAR {: k:n :}
      k -1 > if bs k VEC:AT BINDING-SQL @ exit then
   then plan r e EXPRESSION-SQL ;

\ GROUP_CONCAT's SEPARATOR literal node, or zero for its default.
: SEPARATOR-SQL ( ^plan n -- str )
   {: plan sep:n :}
   sep 0= if s" NULL::text" exit then
   plan plan TREE sep TEXT$ TEXT-ARG ;

\ Whether one of the datatypes is d.
: HELD$? ( ^VEC:vector<datatype> str -- bool )
   {: list:^ d:str :}
   list VEC:COUNT 0 ?do
      list i VEC:AT DATATYPE-IRI @ d STR:= if true unloop exit then
   loop false ;

\ Whether the datatypes are iri and other alone.
: ONLY$? ( ^VEC:vector<datatype> str str -- bool )
   {: list:^ iri:str other:str :}
   list VEC:COUNT 0 ?do
      list i VEC:AT DATATYPE-IRI @ {: d:str :}
      d iri STR:= d other STR:= or 0= if false unloop exit then
   loop list iri HELD$? list other HELD$? and ;

\ Whether shape k has type t and holds iri and other literals alone.
: CLASS? ( ^plan n str str str -- bool )
   {: plan k:n t:str iri:str other:str :}
   plan k SHAPE@ {: y:^ :}
   y SHAPE-TYPE @ t STR:=CI 0= if false exit then
   y SHAPE-HOLDS @ MATCH holds
      any OF false ENDOF
      uris OF false ENDOF
      literals OF iri other ONLY$? ENDOF
   ;MATCH ;

\ Whether shape k holds xsd:integer and xsd:decimal, its values numeric.
\ A float or double column stays generic: its native sum raises on
\ overflow, where sparql.aggregate answers unbound, and its min and max do
\ not tell -0 from 0, which ORDER BY's order does.
: NUMBERS? ( ^plan n -- bool )
   s" numeric" s" http://www.w3.org/2001/XMLSchema#integer"
   s" http://www.w3.org/2001/XMLSchema#decimal" CLASS? ;

\ Whether shape k holds strings, its values text.
: STRINGS? ( ^plan n -- bool )
   s" text" s" http://www.w3.org/2001/XMLSchema#string"
   s" http://www.w3.org/2001/XMLSchema#string" CLASS? ;

\ The binding in r of the call's operand, a variable r binds.
: OPERAND-BINDING ( ^plan ^relation n -- ^binding )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs bs plan state state call NODE-A@ NODE-A@ VAR FIND-VAR VEC:AT ;

\ The shape the call reads natively over r, else -1: SUM, AVG, MIN or
\ MAX without DISTINCT of a variable r binds always to a column of a
\ NUMBERS? shape, or MIN or MAX of a STRINGS? one.
: NATIVE-SHAPE ( ^plan ^relation n -- n )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   state call NODE NODE-FLAGS @ F-DISTINCT and 0<> if -1 exit then
   state call s" SUM AVG MIN MAX" CALL-IN? 0= if -1 exit then
   plan r state state call NODE-A@ NODE-A@ BOUND? 0= if -1 exit then
   plan r call OPERAND-BINDING BINDING-SHAPE @ {: k:n :}
   k 0< if -1 exit then
   plan k NUMBERS? if k exit then
   state call s" MIN MAX" CALL-IN? plan k STRINGS? and if k exit then
   -1 ;

\ SUM over column c: its datatype decimal where a value has a fractional
\ digit, else integer, and 0 for none.
: SUM$ ( ^plan str -- str )
   {: plan c:str :}
   plan NEW-SQL {: v:^ :}
   v s" sparql.number(coalesce(" +SQL v s" sum" c +CALL
   v s" ,0),CASE WHEN max(" +SQL v s" min_scale" c +CALL
   v s" )>0 THEN 'decimal' ELSE 'integer' END)" +SQL v VEC:BYTES ;

\ AVG over column c as sparql.aggregate answers it: the sum's canonical
\ value over the count, unbound where the sum's text is past the 16,383
\ bytes sparql.numeric reads, an integer 0 for no value.
: AVG$ ( ^plan str -- str )
   {: plan c:str :}
   plan NEW-SQL {: n:^ :} n s" count" c +CALL
   n VEC:BYTES {: cnt:str :}
   plan NEW-SQL {: t:^ :} t s" sum" c +CALL
   t VEC:BYTES {: total:str :}
   plan NEW-SQL {: v:^ :}
   v s" sparql.number(CASE WHEN " +SQL v cnt +SQL
   v s" =0 THEN 0 WHEN octet_length(" +SQL
   v s" sparql.format_decimal" total +CALL
   v s" )<=16383 THEN " +SQL v s" trim_scale" total +CALL
   v s" /" +SQL v cnt +SQL v s"  END,CASE WHEN " +SQL v cnt +SQL
   v s" =0 THEN 'integer' ELSE 'decimal' END)" +SQL v VEC:BYTES ;

\ The call's min or max of column c of shape k, text by code point.
: EXTREME$ ( ^plan n n str -- str )
   {: plan call:n k:n c:str :}
   plan NEW-SQL {: v:^ :}
   plan TREE call s" MIN" CALL-IS? {: least:bool :}
   least if v s" min(" +SQL else v s" max(" +SQL then
   v c +SQL
   plan k STRINGS? if v s\"  COLLATE \"C\"" +SQL then
   v s" )" +SQL v VEC:BYTES ;

\ The call over column c of shape k, as NATIVE-SHAPE found it.
: NATIVE$ ( ^plan n n str -- str )
   {: plan call:n k:n c:str :}
   plan TREE {: state :}
   state call s" SUM" CALL-IS? if plan c SUM$ exit then
   state call s" AVG" CALL-IS? if plan c AVG$ exit then
   plan call k c EXTREME$ {: e:str :}
   plan NEW-SQL {: v:^ :}
   v plan k SHAPE@ SHAPE-OUT @ e +CALL v VEC:BYTES ;

\ The aggregate call over the relation: COUNT is count, a NATIVE-SHAPE
\ one native; any other goes through sparql.aggregate over array_agg of
\ its operand's terms.
: AGGREGATE$ ( ^plan ^relation n -- str )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   plan r call NATIVE-SHAPE {: k:n :}
   k -1 > if
      plan call k plan r call OPERAND-BINDING BINDING-SQL @ NATIVE$ exit
   then
   state call NODE NODE-FLAGS @ F-DISTINCT and 0<> {: unique:bool :}
   plan NEW-SQL {: v:^ :}
   state call s" COUNT" CALL-IS? if
      v s" sparql.number(count(" +SQL
      unique if v s" DISTINCT " +SQL then
      v plan r call COUNTED +SQL v s" ),'integer')" +SQL v VEC:BYTES exit
   then
   v s" sparql.aggregate(" +SQL v plan state call TEXT$ TEXT-ARG +SQL
   v s" ,array_agg(" +SQL
   v plan r state state call NODE-A@ NODE-A@ EXPRESSION-SQL +SQL
   unique if v s" ),true," +SQL else v s" ),false," +SQL then
   v plan state call NODE-B@ SEPARATOR-SQL +SQL v s" )" +SQL v VEC:BYTES ;

\ Whether every group of r's solutions binds the aggregate call.
: BOUND-CALL? ( ^plan n ^relation n -- bool )
   {: plan query:n r:^ call:n :}
   plan TREE {: state :}
   state call s" COUNT" CALL-IS? if true exit then
   state call s" SUM" CALL-IS? plan r call NATIVE-SHAPE -1 > and if
      true exit
   then
   state query GROUP-BY-NODE 0= if false exit then
   state call s" MIN MAX SAMPLE" CALL-IN? 0= if false exit then
   plan r state state call NODE-A@ NODE-A@ BOUND? ;

\ The query's aggregates over the relation, into the plan.
: AGGREGATES ( ^plan n ^relation -- )
   {: plan query:n r:^ :}
   plan TREE {: state :}
   plan AL 0 [type] aggregate VEC:NEW {: as:^ :}
   state query QUERY-AGGREGATES begin dup 0<> while
      {: list:n :} state list NODE-A@ {: call:n :}
      plan r call AGGREGATE$ {: sql:str :}
      as VEC:PUSH {: a:^ :}
      call a AGGREGATE-NODE ! sql a AGGREGATE-SQL !
      plan query r call BOUND-CALL? a AGGREGATE-ALWAYS !
      plan r call NATIVE-SHAPE a AGGREGATE-NATIVE !
      state list NODE-B@
   repeat drop as plan PLAN-AGGREGATES ! ;

\ Appends the test of each expression of the list over the scope to v.
: CONDITIONS ( ^plan n ^relation ^VEC:vector<u8> -- )
   {: plan list:n r:^ v:^ :}
   plan TREE {: state :}
   list begin dup 0<> while
      {: each:n :}
      plan r RELATION-BINDINGS @ state each NODE-A@ v +TEST
      state each NODE-B@
   repeat drop ;

\ Appends HAVING's conditions over the scope to v.
: HAVING-SQL ( ^plan n ^relation ^VEC:vector<u8> -- )
   {: plan query:n r:^ v:^ :}
   plan TREE {: state :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: m:n :}
      state m NODE-K N-HAVING = if plan state m NODE-A@ r v CONDITIONS then
      state list NODE-B@
   repeat drop ;

\ A GROUP BY key: a bound variable binds its anchor in g and `(e AS ?v)`
\ binds ?v to e's term; another expression's term goes to keys.
: GROUP-KEY ( ^plan ^relation ^relation n ^VEC:vector<u8> -- )
   {: plan r:^ g:^ e:n keys:^ :}
   plan TREE {: state :}
   g RELATION-BINDINGS @ {: gs:^ :}
   state e NODE-K N-ALIAS = if
      plan state e NODE-B@ VAR {: var:n :}
      gs var GENERIC plan r state e NODE-A@ EXPRESSION-SQL s" " BIND
      false gs LAST BINDING-ALWAYS ! exit
   then
   state e NODE-K N-VAR <> if
      keys s" ," +SEP keys plan r e EXPRESSION-SQL +SQL exit
   then
   r RELATION-BINDINGS @ {: bs:^ :}
   bs plan e VAR FIND-VAR {: k:n :}
   k 0< gs plan e VAR FIND-VAR -1 > or if exit then
   bs k VEC:AT gs COPY-BINDING ;

: GROUP-KEYS ( ^plan n ^relation ^relation ^VEC:vector<u8> -- )
   {: plan exprs:n r:^ g:^ keys:^ :}
   plan TREE {: state :}
   exprs begin dup 0<> while
      {: list:n :} plan r g state list NODE-A@ keys GROUP-KEY
      state list NODE-B@
   repeat drop ;

\ Points g's bindings and the plan's aggregates at their columns under
\ the alias.
: RENAME ( ^plan str ^relation -- )
   {: plan alias:str g:^ :}
   g RELATION-BINDINGS @ {: gs:^ :}
   gs VEC:COUNT 0 ?do
      gs i VEC:AT {: b:^ :}
      plan alias b BINDING-VAR @ COLUMN-OF b BINDING-SQL !
   loop
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      plan NEW-SQL {: c:^ :}
      c alias +SQL c s" .a" +SQL c i +INT
      c VEC:BYTES as i VEC:AT AGGREGATE-SQL !
   loop ;

\ Appends `SELECT` g's columns and the aggregates `FROM` r, grouped by
\ keys and kept by having.
: +GROUPED-SELECT ( ^plan ^VEC:vector<u8> ^relation ^relation
      ^VEC:vector<u8> ^VEC:vector<u8> -- )
   {: plan v:^ r:^ g:^ keys:^ having:^ :}
   g RELATION-BINDINGS @ {: gs:^ :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   v s" SELECT" +SQL
   gs VEC:COUNT 0 ?do
      i 0<> if v s" ," +SQL then
      gs i VEC:AT {: b:^ :} plan v b b BINDING-SHAPE @ +COLUMN
   loop
   as VEC:COUNT 0 ?do
      gs VEC:COUNT 0<> i 0<> or if v s" ," +SQL then
      v s"  " +SQL v as i VEC:AT AGGREGATE-SQL @ +SQL
      v s"  AS a" +SQL v i +INT
   loop
   v r +FROM-WHERE
   keys VEC:COUNT 0<> if v s"  GROUP BY " +SQL v keys VEC:BYTES +SQL then
   having VEC:COUNT 0<> if v s"  HAVING " +SQL v having VEC:BYTES +SQL then ;

\ Closes the derived table in v under a new alias, whose columns then
\ bind g's variables and hold the aggregates.
: CLOSE-GROUPS ( ^plan ^VEC:vector<u8> ^relation -- )
   {: plan v:^ g:^ :}
   plan NEXT-ALIAS {: alias:str :}
   v s" ) AS " +SQL v alias +SQL
   plan alias g RENAME ;

\ g's FROM item: the grouped SELECT of g's keys and the aggregates over r.
: GROUPS-FROM ( ^plan ^relation ^relation ^VEC:vector<u8>
      ^VEC:vector<u8> -- )
   {: plan r:^ g:^ keys:^ having:^ :}
   g RELATION-FROM @ {: v:^ :}
   v s" (" +SQL plan v r g keys having +GROUPED-SELECT
   plan v g CLOSE-GROUPS ;

: +KEY-COLUMNS ( ^VEC:vector<u8> ^relation -- )
   {: keys:^ g:^ :}
   g RELATION-BINDINGS @ {: gs:^ :}
   gs VEC:COUNT 0 ?do
      keys s" ," +SEP keys gs i VEC:AT BINDING-SQL @ +SQL
   loop ;

\ GROUP BY's keys over r: g's bindings, the other keys' terms to keys.
: KEYS-OF ( ^plan n ^relation ^relation ^VEC:vector<u8> -- )
   {: plan query:n r:^ g:^ keys:^ :}
   plan TREE {: state :}
   state query GROUP-BY-NODE {: grouping:n :}
   grouping 0<> if plan state grouping NODE-A@ r g keys GROUP-KEYS then ;

\ g's FROM item over r, with HAVING. A GROUP BY that leaves no key still
\ answers no group for no solution.
: GROUPS-OF ( ^plan n ^relation ^relation ^VEC:vector<u8> -- )
   {: plan query:n r:^ g:^ keys:^ :}
   plan NEW-SQL {: having:^ :}
   plan query g having HAVING-SQL
   keys g +KEY-COLUMNS
   plan TREE query GROUP-BY-NODE 0<> keys VEC:COUNT 0= and if
      having s"  AND " +SEP having s" count(*)>0" +SQL
   then
   plan r g keys having GROUPS-FROM ;

\ The aggregate call over u; when partial, COUNT's bare count and the
\ native min or max of a call native in shape k.
: PART$ ( ^plan ^relation n bool n -- str )
   {: plan u:^ call:n partial:bool k:n :}
   partial k -1 > and if
      plan call k plan u call OPERAND-BINDING BINDING-SQL @ EXTREME$ exit
   then
   partial plan TREE call s" COUNT" CALL-IS? and 0= if
      plan u call AGGREGATE$ exit
   then
   plan NEW-SQL {: v:^ :}
   v s" count(" +SQL v plan u call COUNTED +SQL v s" )" +SQL v VEC:BYTES ;

\ Points the plan's aggregates at their SQL over u.
: PARTS ( ^plan ^relation bool -- )
   {: plan u:^ partial:bool :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      as i VEC:AT {: a:^ :}
      plan u a AGGREGATE-NODE @ partial a AGGREGATE-NATIVE @ PART$
      a AGGREGATE-SQL !
   loop ;

\ Appends branch b's groups, b as the union of r's columns presents it;
\ without a key, a branch with no solution has none.
: +BRANCH-GROUPS ( ^plan n ^VEC:vector<u8> ^relation ^relation bool -- )
   {: plan query:n v:^ b:^ r:^ partial:bool :}
   plan b r RELATION-BINDINGS @ PRESENTED {: u:^ :}
   plan NEW-RELATION {: gb:^ :}
   plan NEW-SQL {: keys:^ :} plan NEW-SQL {: having:^ :}
   plan query u gb keys KEYS-OF
   plan u partial PARTS
   keys gb +KEY-COLUMNS
   keys VEC:COUNT 0= if having s" count(*)>0" +SQL then
   plan v u gb keys having +GROUPED-SELECT ;

\ Appends `(` and the groups of r's branches, a UNION ALL.
: +BRANCHES ( ^plan n ^VEC:vector<u8> ^relation bool -- )
   {: plan query:n v:^ r:^ partial:bool :}
   r RELATION-BRANCHES @ {: bs:^ :}
   v s" (" +SQL
   bs VEC:COUNT 0 ?do
      i 0<> if v s"  UNION ALL " +SQL then
      plan query v bs i VEC:AT r partial +BRANCH-GROUPS
   loop ;

\ The aggregate call over the partials in column c: the sum of COUNT's
\ counts, the min or max of the native partials of a call native in
\ shape k, else the aggregate of the partial terms.
: COMBINED$ ( ^plan n str n -- str )
   {: plan call:n c:str k:n :}
   plan TREE {: state :}
   k -1 > if plan call k c NATIVE$ exit then
   plan NEW-SQL {: v:^ :}
   state call s" COUNT" CALL-IS? if
      v s" sparql.number(coalesce(sum(" +SQL v c +SQL
      v s" ),0),'integer')" +SQL v VEC:BYTES exit
   then
   v s" sparql.aggregate(" +SQL v plan state call TEXT$ TEXT-ARG +SQL
   v s" ,array_agg(" +SQL v c +SQL v s" ),false," +SQL
   v plan state call NODE-B@ SEPARATOR-SQL +SQL v s" )" +SQL v VEC:BYTES ;

\ Points each aggregate at its combination over the partials it names.
: COMBINE ( ^plan -- )
   {: plan :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      as i VEC:AT {: a:^ :}
      plan a AGGREGATE-NODE @ a AGGREGATE-SQL @ a AGGREGATE-NATIVE @
      COMBINED$ a AGGREGATE-SQL !
   loop ;

\ The groups of r's branches, each grouped on its own, HAVING over them.
: SPLIT ( ^plan n ^relation -- ^relation )
   {: plan query:n r:^ :}
   plan NEW-RELATION {: g:^ :}
   plan query r g plan NEW-SQL KEYS-OF
   plan query r AGGREGATES
   g RELATION-FROM @ {: v:^ :}
   plan query v r false +BRANCHES
   plan v g CLOSE-GROUPS
   plan query g g RELATION-WHERE @ HAVING-SQL g ;

\ The groups of the partial groups of r's branches, combined.
: COMBINED ( ^plan n ^relation -- ^relation )
   {: plan query:n r:^ :}
   plan NEW-RELATION {: g:^ :} plan NEW-SQL {: keys:^ :}
   plan query r g keys KEYS-OF
   plan query r AGGREGATES
   plan NEW-RELATION {: p:^ :}
   p RELATION-FROM @ {: v:^ :}
   plan query v r true +BRANCHES
   plan v g CLOSE-GROUPS
   plan COMBINE
   plan query p g keys GROUPS-OF g ;

\ An EXISTS call ends the walk found; any other leaf walks on.
: EXISTS-LEAF ( bool ^syntax n -- bool n )
   {: found:bool state leaf:n :}
   state leaf NODE-K N-CALL = if
      state leaf EXISTS-CALL? if true 0 exit then
   then found state leaf OPERAND ;

\ Whether an expression of the list calls EXISTS.
: EXISTS-IN? ( ^syntax n -- bool )
   {: state list:n :}
   list begin dup 0<> while
      {: each:n :}
      false state state each NODE-A@ [: EXISTS-LEAF ;] WALK
      if true exit then
      state each NODE-B@
   repeat drop false ;

\ Whether r's solutions are a union's of two or more branches and no
\ GROUP BY key or aggregate calls EXISTS, which each would compile again.
: BRANCHED? ( ^plan n ^relation -- bool )
   {: plan query:n r:^ :}
   r RELATION-BRANCHES @ VEC:COUNT 2 < if false exit then
   plan TREE {: state :}
   state state query QUERY-AGGREGATES EXISTS-IN? if false exit then
   state query GROUP-BY-NODE {: grouping:n :}
   grouping 0= if true exit then
   state state grouping NODE-A@ EXISTS-IN? 0= ;

\ Whether each branch binds var always to a term no other binds it to.
: APART? ( ^plan ^VEC:vector<relation> n -- bool )
   {: plan branches:^ var:n :}
   plan INDEXES {: places:^ :}
   branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k 0< if false unloop exit then
      bs k VEC:AT {: b:^ :}
      b BINDING-SHAPE @ KNOWN <> b BINDING-ALWAYS @ 0= or if
         false unloop exit
      then
      plan b BINDING-TERM @ TERM-PLACE places VEC:PUSH !
   loop
   plan AL plan PLAN-KNOWN @ VEC:COUNT [type] bool VEC:NEW {: seen:^ :}
   places VEC:COUNT 0 ?do
      seen places i VEC:AT @ VEC:AT {: mark:^ :}
      mark @ if false unloop exit then
      true mark !
   loop true ;

\ Whether a GROUP BY variable is APART? in r's branches.
: SPLIT? ( ^plan n ^relation -- bool )
   {: plan query:n r:^ :}
   plan TREE {: state :}
   state query GROUP-BY-NODE {: grouping:n :}
   grouping 0= if false exit then
   state grouping NODE-A@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: e:n :}
      state e NODE-K N-VAR = if
         plan r RELATION-BRANCHES @ plan e VAR APART? if true exit then
      then state list NODE-B@
   repeat drop false ;

\ Whether partial groups regroup to the same answer: a GROUP BY key must
\ bind a variable, which a partial keeps.
: COMBINES? ( ^plan n -- bool )
   {: plan query:n :}
   plan TREE {: state :}
   state query QUERY-AGGREGATES begin dup 0<> while
      {: list:n :} state list NODE-A@ {: call:n :}
      state call NODE NODE-FLAGS @ F-DISTINCT and 0<> if false exit then
      state call s" COUNT MIN MAX SAMPLE" CALL-IN? 0= if false exit then
      state list NODE-B@
   repeat drop
   state query GROUP-BY-NODE {: grouping:n :}
   grouping 0= if true exit then
   state grouping NODE-A@ begin dup 0<> while
      {: list:n :} state state list NODE-A@ NODE-K {: kind:n :}
      kind N-VAR <> kind N-ALIAS <> and if false exit then
      state list NODE-B@
   repeat drop true ;

\ The groups as a relation over r's solutions, grouped per branch where
\ they are a union's and that answers as grouping the whole union does.
: GROUPED ( ^plan n ^relation -- ^relation )
   {: plan query:n r:^ :}
   plan query r BRANCHED? if
      plan query r SPLIT? if plan query r SPLIT exit then
      plan query COMBINES? if plan query r COMBINED exit then
   then
   plan NEW-RELATION {: g:^ :} plan NEW-SQL {: keys:^ :}
   plan query r g keys KEYS-OF
   plan query r AGGREGATES
   plan query r g keys GROUPS-OF g ;

\ Joins the trailing VALUES' data block with s; no rows is no solution.
: JOIN-VALUES ( ^plan ^relation n -- )
   {: plan s:^ node:n :}
   plan AL 0 [type] item VEC:NEW {: items:^ :}
   plan items node VALUES-ITEM
   items 0 VEC:AT {: it:^ :}
   it LIVE-COUNT 0= if s RELATION-WHERE @ s" false" +AND exit then
   plan it s JOIN-STEP ;

\ The solutions SELECT and ASK read: the groups when GROUP BY or an
\ aggregate groups the query, else r's solutions that HAVING keeps, joined
\ with the trailing VALUES.
: GROUPS ( ^plan n ^relation -- ^relation )
   {: plan query:n r:^ :}
   plan TREE {: state :}
   state query QUERY-AGGREGATES 0<> state query GROUP-BY-NODE 0<> or if
      plan query r GROUPED
   else plan query r r RELATION-WHERE @ HAVING-SQL r then {: s:^ :}
   state query QUERY-VALUES@ {: data:n :}
   data 0<> if plan s data JOIN-VALUES then s ;

\ SELECT's expressions in order, so a later one reads an earlier one's
\ variable.
: EXTEND ( ^plan n ^relation -- )
   {: plan query:n r:^ :}
   plan TREE {: state :}
   state query NODE-A@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: e:n :}
      state e NODE-K N-ALIAS = if plan r e +ALIAS then
      state list NODE-B@
   repeat drop ;

\ ORDER BY's keys over the scope, `,`-joined, each `sparql.order(term)`
\ and DESC where written; empty without ORDER BY.
: ORDER-KEYS ( ^plan n ^relation -- str )
   {: plan query:n r:^ :}
   plan TREE {: state :}
   plan NEW-SQL {: v:^ :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: m:n :}
      state m NODE-K N-ORDER = if
         v s" ," +SEP v s" sparql.order(" +SQL
         v plan r state m NODE-A@ EXPRESSION-SQL +SQL v s" )" +SQL
         state m NODE NODE-FLAGS @ F-DESC and 0<> if
            v s"  DESC" +SQL
         then
      then state list NODE-B@
   repeat drop v VEC:BYTES ;

\ SELECT's projected variable nodes.
: NAMES ( ^plan n -- ^VEC:vector<n> )
   {: plan query:n :}
   plan TREE {: state :}
   plan AL 0 [type] n VEC:NEW {: names:^ :}
   state query QUERY-NAMES begin dup 0<> while
      {: list:n :} state list NODE-A@ names VEC:PUSH !
      state list NODE-B@
   repeat drop names ;

\ The document before its first binding.
: HEAD-JSON ( ^plan ^VEC:vector<n> -- str )
   {: plan names:^ :}
   plan WRITER
   JSON:WRITE-OBJECT-START s" head" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" vars" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   names VEC:COUNT 0 ?do
      i 0<> if JSON:WRITE-COMMA then
      plan TREE names i VEC:AT @ TEXT$ JSON:WRITE-STRING
   loop
   JSON:WRITE-ARRAY-END JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" results" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" bindings" JSON:WRITE-KEY JSON:WRITE-ARRAY-START JSON:WRITTEN$ ;

\ Whether a projected variable is maybe bound.
: MAYBE? ( ^plan ^VEC:vector<binding> ^VEC:vector<n> -- bool )
   {: plan bs:^ names:^ :}
   names VEC:COUNT 0 ?do
      bs plan names i VEC:AT @ VAR FIND-VAR {: k:n :}
      k -1 > if
         bs k VEC:AT BINDING-ALWAYS @ 0= if true unloop exit then
      then
   loop false ;

\ PostgreSQL passes a function 100 arguments at most.
50 constant OBJECT-PAIRS

\ Appends what separates an object's next pair from the n before it.
: +NEXT-PAIR ( ^VEC:vector<u8> n -- )
   {: v:^ n:n :}
   n 0= if exit then
   n OBJECT-PAIRS mod 0= if v s" )||jsonb_build_object(" +SQL exit then
   v s" ," +SQL ;

\ A binding object of the projected variables bound in the relation; an
\ unbound one's NULL member is stripped.
: +OBJECT ( ^plan ^relation ^VEC:vector<n> ^VEC:vector<u8> -- )
   {: plan r:^ names:^ v:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan bs names MAYBE? {: maybe:bool :}
   maybe if v s" jsonb_strip_nulls(" +SQL else v s" (" +SQL then
   v s" jsonb_build_object(" +SQL
   0 names VEC:COUNT 0 ?do
      names i VEC:AT @ {: node:n :}
      bs plan node VAR FIND-VAR {: k:n :}
      k -1 > if
         dup v swap +NEXT-PAIR 1+
         v plan plan TREE node TEXT$ TEXT-ARG +SQL v s" ," +SQL
         v plan bs k VEC:AT TERM-SQL +SQL
      then
   loop drop v s" ))::text" +SQL ;

\ DISTINCT's solutions: the projected columns, native where native, made
\ distinct by GROUP BY. With ORDER BY's keys each keeps its first place,
\ column i, whose text is answered; empty without them.
: DISTINCT-SQL ( ^plan ^relation ^VEC:vector<n> str -- ^relation str )
   {: plan r:^ names:^ keys:str :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan NEW-RELATION {: d:^ :}
   d RELATION-BINDINGS @ {: ds:^ :}
   plan NEXT-ALIAS {: alias:str :}
   plan NEW-SQL {: inner:^ :} plan NEW-SQL {: cols:^ :}
   names VEC:COUNT 0 ?do
      bs plan names i VEC:AT @ VAR FIND-VAR {: k:n :}
      k -1 > if
         bs k VEC:AT {: b:^ :}
         b BINDING-VAR @ {: var:n :}
         inner s" ," +SEP plan inner b b BINDING-SHAPE @ +COLUMN
         cols s" ," +SEP cols plan s" p" var COLUMN-OF +SQL
         b ds COPY-BINDING
         plan alias var COLUMN-OF ds LAST BINDING-SQL !
      then
   loop
   d RELATION-FROM @ {: v:^ :}
   v s" (SELECT " +SQL v cols VEC:BYTES +SQL
   keys STR:LEN 0<> if
      cols VEC:COUNT 0<> if v s" ," +SQL then v s" min(p.i) AS i" +SQL
      inner s" ," +SEP inner s"  row_number() OVER (ORDER BY " +SQL
      inner keys +SQL inner s" ) AS i" +SQL
   then
   v s"  FROM (SELECT" +SQL v inner VEC:BYTES +SQL v r +FROM-WHERE
   v s" ) AS p" +SQL
   cols VEC:COUNT 0<> if v s"  GROUP BY " +SQL v cols VEC:BYTES +SQL
   else v s"  HAVING count(*)>0" +SQL then
   v s" ) AS " +SQL v alias +SQL
   d keys STR:LEN 0= if s" " exit then
   plan NEW-SQL {: c:^ :} c alias +SQL c s" .i" +SQL c VEC:BYTES ;

\ Appends ` x AS vK` for each projected variable r binds, K the variable
\ of its name in frame into, and answers them as columns.
: +PROJECTED ( ^plan ^relation ^VEC:vector<n> n ^VEC:vector<u8>
      -- ^VEC:vector<binding> )
   {: plan r:^ names:^ into:n v:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan NEW-BINDINGS {: cols:^ :}
   names VEC:COUNT 0 ?do
      names i VEC:AT @ {: node:n :}
      bs plan node VAR FIND-VAR {: k:n :}
      k -1 > if
         bs k VEC:AT cols COPY-BINDING cols LAST {: c:^ :}
         plan plan node VAR-NAME into VAR-ID c BINDING-VAR !
         cols VEC:COUNT 1 > if v s" ," +SQL then
         plan v c c BINDING-SHAPE @ +COLUMN
      then
   loop cols ;

\ The solutions of r the query projects, as a derived table of the
\ projected variables in frame into, native where native: grouped,
\ extended, made distinct, ordered and sliced. With ORDER BY its column i
\ is a solution's place, DISTINCT's rank or row_number over the keys.
: SELECTED ( ^plan n ^relation n -- str ^VEC:vector<binding> bool )
   {: plan query:n r:^ into:n :}
   plan query NAMES {: names:^ :}
   plan query r GROUPS {: s:^ :}
   plan query s EXTEND
   plan query s ORDER-KEYS {: keys:str :}
   plan NEW-SQL {: w:^ :}
   plan TREE query NODE NODE-FLAGS @ F-DISTINCT and 0<> if
      plan s names keys DISTINCT-SQL dup
   else
      keys STR:LEN 0<> if
         w s" row_number() OVER (ORDER BY " +SQL
         w keys +SQL w s" )" +SQL
      then s w VEC:BYTES keys
   then {: d:^ rank:str order:str :}
   plan NEW-SQL {: v:^ :}
   v s" (SELECT" +SQL
   plan d names into v +PROJECTED {: cols:^ :}
   rank STR:LEN 0<> if
      cols VEC:COUNT 0<> if v s" ," +SQL then
      v s"  " +SQL v rank +SQL v s"  AS i" +SQL
   then
   v d +FROM-WHERE
   order STR:LEN 0<> if v s"  ORDER BY " +SQL v order +SQL then
   plan query v SLICE-SQL
   v s" )" +SQL v VEC:BYTES cols rank STR:LEN 0<> ;

\ A statement whose document, its head and `]}}`, passes the byte limit
\ whatever it holds.
: OVERSIZE ( ^VEC:vector<u8> str n -- )
   {: v:^ head:str bytes:n :}
   v s" SELECT sparql.within(octet_length(" +SQL v head +SQL
   v s" )+3," +SQL v bytes +INT v s" )::text" +SQL ;

\ The document is the head, the bindings joined by commas and `]}}`; each
\ binding passes its running byte total, in the bindings' order, through
\ sparql.within.
: SELECT-SQL ( ^plan n ^relation -- str )
   {: plan query:n r:^ :}
   plan query NAMES {: names:^ :}
   plan names HEAD-JSON {: head:str :}
   plan head TEXT-ARG {: ha:str :}
   head STR:LEN 3 + {: fixed:n :}
   plan PLAN-STORE @ STORE-LIMIT @ {: bytes:n :}
   plan NEW-SQL {: v:^ :}
   fixed bytes > if v ha bytes OVERSIZE v VEC:BYTES exit then
   plan query r plan PLAN-FRAME @ SELECTED
      {: t:str cols:^ ranked:bool :}
   plan NEW-RELATION {: p:^ :}
   plan t cols p RELATION-FROM @ +TABLE p RELATION-BINDINGS !
   v s" SELECT " +SQL v ha +SQL
   v s" ||coalesce(string_agg(r.b,',' ORDER BY r.n),'')||']}}'" +SQL
   v s"  FROM (SELECT q.b,sparql.within(" +SQL v fixed 1- +INT
   v s" +sum(octet_length(q.b)+1) OVER (" +SQL
   ranked if v s" ORDER BY q.i " +SQL then
   v s" ROWS UNBOUNDED PRECEDING)," +SQL
   v bytes +INT v s" ) AS n FROM (SELECT " +SQL
   plan p names v +OBJECT v s"  AS b" +SQL
   ranked if v s" ,i" +SQL then
   v p +FROM-WHERE v s" ) AS q) AS r" +SQL v VEC:BYTES ;

: ASK-SQL ( ^plan n ^relation -- str )
   {: plan query:n r:^ :}
   plan query r GROUPS {: s:^ :}
   plan NEW-SQL {: v:^ :}
   v s" SELECT a.d FROM (SELECT " +SQL
   v plan s\" {\"head\":{},\"boolean\":" TEXT-ARG +SQL
   v s" ||CASE WHEN EXISTS(SELECT" +SQL
   v s +FROM-WHERE
   plan query v SLICE-SQL
   v s" ) THEN 'true}' ELSE 'false}' END AS d) AS a" +SQL
   v s"  WHERE sparql.within(octet_length(a.d)," +SQL
   v plan PLAN-STORE @ STORE-LIMIT @ +INT v s" ) IS NOT NULL" +SQL
   v VEC:BYTES ;

\ The query's group with its FILTERs. A statically empty group answers no
\ solution: its WHERE is false.
: SOLUTIONS ( ^plan n -- ^relation )
   {: plan query:n :}
   plan TREE {: state :}
   plan state query NODE-B@ CLOSED {: r:^ :}
   r RELATION-EMPTY @ if
      r RELATION-WHERE @ {: where:^ :}
      where s"  AND " +SEP where s" false" +SQL
   then r ;

\ { SELECT ... }: its projection, a derived table, joins the group. Its
\ variables are a frame of their own, so one it does not project is not
\ the same-named one outside it.
: SELECT-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ query:n :}
   plan PLAN-FRAME @ {: outer:n :}
   plan PLAN-SCOPE @ {: scope:^ :}
   plan PLAN-OUT @ {: out:^ :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   1 plan PLAN-SERIAL +! plan PLAN-SERIAL @ plan PLAN-FRAME !
   plan query plan query SOLUTIONS outer SELECTED drop
      {: t:str cols:^ :}
   outer plan PLAN-FRAME ! scope plan PLAN-SCOPE !
   out plan PLAN-OUT ! as plan PLAN-AGGREGATES !
   plan items plan t cols SUBSTITUTED cols false CHILD-ITEM ;

\ Gives every parameter its type, whether or not the statement reads it:
\ emission drops SQL it has written, and PostgreSQL rejects a parameter
\ without one. Nothing reads the CTE, so PostgreSQL does not plan it. A
\ document's head is a parameter, so it has a row.
: +TYPES ( ^plan ^VEC:vector<u8> -- )
   {: plan v:^ :}
   plan PLAN-ARGUMENTS @ {: args:^ :}
   v s" t0 AS (VALUES " +SQL
   args VEC:COUNT 0 ?do
      i 0<> if v s" ," +SQL then
      v s" ($" +SQL v i 1+ +INT
      args i VEC:AT ARGUMENT-TERM @ if v s" ::jsonb" +SQL then
      v s" ::text)" +SQL
   loop v s" )" +SQL ;

: STATEMENT ( ^plan -- str )
   {: plan :}
   plan TREE {: state :}
   state SYNTAX-ROOT @ {: query:n :}
   state query NODE-K {: kind:n :}
   plan query SOLUTIONS {: r:^ :}
   kind N-SELECT = if plan query r SELECT-SQL
   else plan query r ASK-SQL then {: t:str :}
   plan NEW-SQL {: v:^ :}
   v s" WITH " +SQL plan v +TYPES
   plan PLAN-CTES @ {: ctes:^ :}
   ctes VEC:COUNT 0<> if v s" ," +SQL v ctes VEC:BYTES +SQL then
   v s"  " +SQL v t +SQL v VEC:BYTES ;

\ Appends the parsed query's statement on the store to the vector and
\ passes its parameters' texts in order.
: EMIT ( ^syntax ^store ^VEC:vector<u8> [ str -- ] -- )
   {: state store:^ sql:^ params :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR {: al:^ :}
   al [type] plan ALLOC:NEW {: plan:^ :}
   state plan PLAN-SYNTAX ! store plan PLAN-STORE !
   plan NEW-SQL plan PLAN-CTES !
   al 0 [type] argument VEC:NEW plan PLAN-ARGUMENTS !
   al 0 [type] name VEC:NEW plan PLAN-VARIABLES !
   al 0 [type] binding VEC:NEW plan PLAN-OUTER !
   al 0 [type] aggregate VEC:NEW plan PLAN-AGGREGATES !
   al 0 [type] known-term VEC:NEW plan PLAN-KNOWN !
   al 0 [type] node-term VEC:NEW plan PLAN-NODES !
   al 0 [type] node-set VEC:NEW plan PLAN-SETS !
   plan STATEMENT sql VEC:APPEND
   plan PLAN-ARGUMENTS @ {: args:^ :}
   args VEC:COUNT 0 ?do args i VEC:AT ARGUMENT-TEXT @ params execute loop ;

: INSTALL-EMIT ( -- )
   ['] GROUP is GROUP-SQL ['] EXISTS+ is EXISTS-SQL
   ['] SELECT-ITEM is SUBQUERY-ITEM ;

INSTALL-EMIT

;package
