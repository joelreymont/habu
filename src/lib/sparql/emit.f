\ emit.f - a parsed query compiled to one PostgreSQL statement on the native
\ columns the store describes. Its parameters are the query's constants and
\ names; no query text enters the statement. The description's texts, a
\ slot's `term`, a source's `graph` and a key set's keys, are string
\ literals: the host wrote them, store.f checked them, and as parameters
\ they would grow with the description past the 65,535 one call takes.
\
\ A relation is a FROM list and a WHERE, usable as a FROM item, and, for
\ each variable in scope, a binding: always bound or maybe, and its form,
\ a native column of one shape or a JSONB term, GENERIC or KNOWN (a term
\ the compiler knows).
\
\ A group's triple pattern matches its candidate sources: those whose
\ slots hold the pattern's constants, a column's shape by its `holds`, and
\ whose graph is the active graph's. Shape inference keeps a candidate
\ only while each of its always-bound variables meets a live candidate of
\ every other joined item binding it always (one shape, or a part and its
\ root; a shape and a term it holds; two terms that are one), to a
\ fixpoint; a joined item left with none makes the group empty, with no
\ SQL. Before shape inference a joined triple loses each candidate whose
\ `cols` entry has `only` {k, N} once an IRI i is pinned in column k of
\ its row, N does not list i and another entry's `only` on k of the slot
\ lists it: the triple's own constant at a slot keyed on k pins it, and so
\ does that of another joined triple of its subject whose every candidate
\ reads the row by the same unique subject slot. A pattern's candidates
\ group by the description source they were read from: one group is that
\ source's FROM item, several a UNION ALL of one branch per group, and
\ where a group's candidates read several columns at a slot, a variable
\ binds their COALESCE, a native binding meets it through the columns of
\ its root, and a constant is held by one of them. A pattern of one such
\ group whose subject slot is a unique column reads the row an
\ earlier pattern of its subject reads from that source's FROM item by
\ that column. A nested group, and a GRAPH of a named graph's IRI, with no
\ OPTIONAL, MINUS, BIND or FILTER directly in it adds its items to its
\ parent's, join being associative; any other is a derived table. A chain
\ of UNIONs is one UNION ALL derived table of its groups that are not
\ statically empty. A union's column is native where every branch binding
\ it has a shape of one root, the root's where they differ, NULL where a
\ branch does not bind it. A subquery is a derived table of its
\ projection; its variables are a frame of their own.
\
\ A group folds its items in order (SPARQL 1.1 section 18.2.2.6) into one
\ FROM of CROSS JOINs: a joined item meets the bindings so far in the
\ WHERE, an OPTIONAL is a LEFT JOIN whose ON holds the meeting and the
\ OPTIONAL's FILTERs, a MINUS is a correlated NOT EXISTS. An OPTIONAL's
\ group with an OPTIONAL in it, no BIND and no KNOWN binding joins as its
\ own FROM in parentheses, its WHERE in the ON; any other is a derived
\ table. An OPTIONAL whose group's solutions are a union's, each branch
\ binding always a variable the bindings so far bind always, joins
\ branch by branch: the FROM becomes a UNION ALL, under a materialized
\ CTE of the bindings so far, each row numbered, of a materialized CTE of
\ each branch joined to it, the FILTERs in each join, and of its rows
\ whose number that CTE does not carry, so each branch is read once.
\ Each join meets the variables both sides bind always by a strict `=`,
\ which PostgreSQL can hash or merge; a LEFT JOIN's ON meets a variable a
\ branch leaves unbound by `IS NULL OR`, which it tests row by row,
\ running the union once per row on the left. A BIND's variable is its
\ expression, written at each read; one that calls RAND, UUID or STRUUID,
\ or that PostgreSQL would plan as more than MOST-COPIED bytes, is a
\ column of a derived table of the bindings
\ so far. Each of the group's FILTERs whose every variable, an EXISTS's
\ included, those bindings bind always holds in that table, which no
\ condition outside it reaches: there it keeps the same rows, and such a
\ variable keeps its value through the items after. A VALUES is a joined
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
\ anchor: native `=` within a shape and between a part and a shape it is
\ within, `col = in(term)` against a term, JSONB `=` between terms. A maybe
\ binding meets also where either is NULL, and the variable is then the
\ first bound of the two. A variable's slot on a column that may hold
\ NULL is tested `IS NOT NULL` where its relation's WHERE is written,
\ unless a strict condition of that WHERE already reads the column (an
\ `=`, an IN's `=`s, another slot's constant or null test), since
\ PostgreSQL would count the column's null fraction twice. The group's
\ other FILTERs read the folded bindings. An
\ expression has two SQL forms, its term, JSONB, and its test, an SQL
\ boolean; a FILTER and HAVING hold its test, all else its term. A
\ comparison (`=`, `!=`, `<`, `<=`, `>`, `>=`, IN or NOT IN of a list)
\ lowers to SQL's own on the native columns, which an index on the
\ column serves, when one operand is a natively bound variable and all
\ are numbers or all strings: a variable bound to a column of a numeric
\ shape holding xsd:integer and xsd:decimal or an xsd:integer or
\ xsd:decimal literal, or a variable bound to a text shape holding
\ xsd:string or a literal with no language tag and no datatype but
\ xsd:string. Strings order by code point, as SPARQL's do: COLLATE "C",
\ which only an index COLLATE "C" serves; `=`, `!=`, IN and NOT IN take
\ the columns' one deterministic collation, under which strings are equal
\ exactly when their code points are, so an index on the column serves
\ them. A lowered comparison answers as the generic call does; an unbound
\ operand and an ill-typed number are NULL in both. Each other conjunct of
\ such a test that is `=` of a natively bound variable and an IRI or a
\ plain or language-tagged string, either way round, IN a list of only
\ those, or sameTerm of it and any constant also adds the native equality
\ it implies; the test stays and decides.
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
\ variable, which no column carries once both are folded, `p|q|r` one
\ UNION of its members, however `|` nests; `!(...)` matches the sources
\ whose predicate is none of its IRIs, its inverse members with subject
\ and object swapped, a UNION when it has both. `p+`, `p*` and `p?` are walks,
\ each folded after the joined items up to the next OPTIONAL, MINUS or
\ BIND, or, with an end the solutions so far bind always, before the
\ first of those items, in connected order, that shares no variable
\ with them, whose cross product the seeds would read: a WITH
\ RECURSIVE CTE of p's steps from its seeds, UNION (distinct) so a
\ cycle ends. The seeds are a constant end or one the
\ substitution binds always, its value, else the distinct values of
\ an end the solutions so far bind always, which are then written once as
\ a CTE both read, else every subject of p's steps for `+` and every
\ subject and object in the active graph for `*` and `?`; from the
\ object end the walk steps backwards. `*` and `?` add each seed as its
\ own end. A walk's steps, when its operand has a walk in it, are a CTE
\ at the statement's WITH, which reads no scope, so a nested path writes
\ each walk once; the active graph's subjects and objects are one CTE
\ there, which the seeds of every walk from every node read. The node test
\ and the node CTE leave out a read that implies another (one FROM item,
\ one column under shapes of one root or one term, every column the
\ other tests non-null tested too), the first of equal ones kept. Where
\ one of the steps' start and end forms is a term, the CTE carries n, the
\ end in the start form, converted once per end so that a step compares
\ columns, not a term the host parses per pair scanned; where the steps
\ hold an, the start's columns of the end's root, each branch's when they
\ are a union, a further step compares an with the end instead, and the
\ first step an with the seed when the seeds' form is of that root, or
\ with the root's `in` of a given term only those columns may hold.
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
\ STORAGE CLASS. CALLER-OWNED: everything lives in the syntax's arena, a
\ step's scratch in its work arena.

require src/lib/sparql/parse.f
require src/lib/sparql/store.f
require src/lib/json/write.f
require src/lib/fmt.f
require src/lib/map.f
require src/lib/sparql/syntax.f
require src/lib/sparql/segs.f
require src/lib/vector.f
require src/lib/json/value.f
require src/lib/sparql/iri.f
require src/lib/map-core.f
require src/lib/sparql/pattern.f
require src/lib/sparql/expression.f
require src/lib/sparql/validate.f
require src/lib/sparql/statement.f
require src/lib/sparql/write.f
require src/lib/sparql/lex.f
require src/lib/siphash.f
require src/lib/sparql/terms.f

package SPARQL
private

-1 constant GENERIC
-2 constant KNOWN

\ The bytes a read of a BIND's variable may copy. A BIND read twice by the
\ next doubles them; as LATERAL items PostgreSQL pulls up, a chain of 100
\ each read once took its planner 2.9 GB.
4096 constant MOST-COPIED

\ The parameters one PostgreSQL call takes.
65535 constant PARAMETERS

\ A parameter with a text: 0 and a string, or a term node and the JSON
\ TERM-TEXT copied.
STRUCTURE argument DERIVE addr FIELD node n FIELD text str ;STRUCTURE

\ A variable's name, or `_:` and a blank node's label or `#` and number,
\ and its frame: 0 for the query's own, a subquery's serial for its own.
STRUCTURE name DERIVE addr FIELD text str FIELD frame n ;STRUCTURE

\ A GENERIC binding's columns: slot k of the records under the alias, at
\ most one non-null in a row.
STRUCTURE spread DERIVE addr
   FIELD recs ^VEC:vector<n>
   FIELD slot n
   FIELD alias str
;STRUCTURE

\ Nothing appends to sql once it is stored, and copies are set only on a
\ rel no other binding holds.
STRUCTURE binding DERIVE addr
   FIELD var n
   FIELD shape n  \ a native column's shape, else GENERIC or KNOWN
   FIELD sql ^rel
   FIELD term str  \ a KNOWN term's text
   FIELD always bool
   FIELD nullable bool  \ sql may be NULL; no WHERE condition drops it yet
   FIELD spread ^spread  \ a GENERIC binding's columns, else NULL-PTR
;STRUCTURE

\ A scope's layer of bindings over the one under it, NULL-PTR outermost; a
\ variable's binding is its innermost one's; no layer changes under another.
STRUCTURE layer DERIVE addr
   FIELD bindings ^VEC:vector<binding>
   FIELD under ^layer
;STRUCTURE

\ A row the FROM reads once under the alias: of the FROM item `from`, by
\ a unique column slot, for the subject node.
STRUCTURE subject-row DERIVE addr
   FIELD node n
   FIELD from str
   FIELD slot slot
   FIELD alias str
;STRUCTURE

\ rows are NULL-PTR until the first is kept; branches are a union's when
\ its solutions are the relation's, else NULL-PTR.
STRUCTURE relation DERIVE addr
   FIELD from ^rel
   FIELD where ^rel
   FIELD bindings ^VEC:vector<binding>
   FIELD rows ^VEC:vector<subject-row>
   FIELD empty bool
   FIELD branches ^VEC:vector<relation>
   FIELD optional bool  \ an OPTIONAL LEFT JOINed into from
   FIELD bind bool  \ a BIND bound a variable here
;STRUCTURE

\ One way an item matches: a source's index, or -1 for a derived table.
STRUCTURE choice DERIVE addr
   FIELD source n
   FIELD alive bool
;STRUCTURE

\ A group's item: a triple's or BIND's node, or zero and a derived table.
\ A triple's columns are its variables by part, made with its first choice.
STRUCTURE item DERIVE addr
   FIELD node n
   \ Zero for a joined item, else N-OPTIONAL, N-MINUS, N-BIND or N-PATH.
   FIELD mode n
   FIELD table ^rel
   FIELD columns ^VEC:vector<binding>
   FIELD filters ^VEC:vector<n>  \ an OPTIONAL's
   FIELD choices ^VEC:vector<choice>
   FIELD graph str  \ the active graph's term
   FIELD union bool  \ whether UNION? holds of it once pruned
   \ A union's, an OPTIONAL's when its group's solutions are a union's,
   \ else NULL-PTR.
   FIELD branches ^VEC:vector<relation>
   \ An OPTIONAL's or MINUS's group's WHERE when the group joins as it is,
   \ else NULL-PTR.
   FIELD where ^rel
;STRUCTURE

\ An IRI a joined triple puts in the row `row` reads by a unique
\ subject slot, in the column `row`'s slot is keyed on.
STRUCTURE pin DERIVE addr
   FIELD row ^source
   FIELD slot n
   FIELD iri n
;STRUCTURE

\ A subject node's pins; the verdict at k*3+j says whether they leave
\ record k's slot j's `only` entry empty: 0 undecided, 1 no, 2 yes.
STRUCTURE pinned DERIVE addr
   FIELD subject n
   FIELD pins ^VEC:vector<pin>
   FIELD verdicts ^VEC:vector<u8>
;STRUCTURE

\ An aggregate call node and its SQL. Nothing appends to sql once it is
\ stored.
STRUCTURE aggregate DERIVE addr
   FIELD node n
   FIELD digest n  \ the node's EXPRESSION-HASH
   FIELD sql ^rel  \ the call within the grouped query, its column above
   FIELD always bool  \ no group leaves it unbound
   FIELD native n  \ the shape whose column it reads natively, else -1
;STRUCTURE

\ An ORDER BY or GROUP BY key's expression and its EXPRESSION-HASH.
STRUCTURE hashed DERIVE addr FIELD node n FIELD digest n ;STRUCTURE

\ What a term's text says: whether the term is an IRI and, if not, its
\ datatype IRI, decoded.
STRUCTURE known-term DERIVE addr FIELD uri bool FIELD datatype str ;STRUCTURE

\ An active graph's every subject and object: the statement's CTE name
\ of them, empty when no source is in the graph, and the form of its k.
STRUCTURE node-set DERIVE addr
   FIELD graph str
   FIELD name str
   FIELD kind n
;STRUCTURE

\ An active graph's node test for a key of form kind: the rel NODE-SQL
\ answers for them.
STRUCTURE node-test DERIVE addr
   FIELD graph str
   FIELD kind n
   FIELD test ^rel
;STRUCTURE

\ A walk's triple, the end it starts from (seed) and the end it reaches
\ (far); its CTEs are named name and a letter.
STRUCTURE walk DERIVE addr
   FIELD node n
   FIELD seed n
   FIELD far n
   FIELD inverse bool
   FIELD rest bool
   FIELD op str
   FIELD operand n
   FIELD name str
   FIELD pairs ^rel  \ the steps' table
   FIELD once bool  \ pairs is a CTE
   FIELD start n  \ the form of the steps' first end
   FIELD end n  \ the form of the steps' second end
   FIELD rooted bool  \ the steps hold an, the start's columns of the end's root
   FIELD held bool  \ the seed is a given term only an may hold
   FIELD kind n  \ the seeds' form
   FIELD ctes ^ctes
   FIELD shared bool  \ its CTEs are the statement's
   FIELD graph str  \ a shared walk's active graph
;STRUCTURE

\ An expression is written to out and reads the bindings in scope and the
\ aggregates.
STRUCTURE plan DERIVE addr
   FIELD syntax ^syntax
   FIELD store ^store
   \ Parameter k's value: a term node, or the invert of its place in texted.
   FIELD arguments ^segs<n>
   FIELD texted ^segs<argument>  \ the parameters with a text
   FIELD strings interns  \ the string parameters' numbers by text
   FIELD variables ^VEC:vector<name>
   FIELD reads n  \ MOST bounds it
   FIELD serial n
   FIELD frame n  \ the variables'
   FIELD graph str  \ the active graph's text, empty for the default graph
   FIELD out ^rel
   FIELD scope ^layer
   FIELD outer ^layer  \ the substitution, NULL-PTR outside EXISTS
   FIELD ctes ^ctes  \ the statement's
   FIELD aggregates ^VEC:vector<aggregate>
   FIELD known ^VEC:vector<known-term>
   FIELD places ^n  \ a MAP map from a text to its place in known
   FIELD constant known-term  \ NODE-KNOWN's, refilled by each call
   FIELD sets ^VEC:vector<node-set>  \ the node CTEs written, one per graph
   FIELD tests ^VEC:vector<node-test>  \ built, one per graph and key kind
   \ The shared walks written, none equal to another: a walk read several
   \ times, or on each outer row of a subquery, is computed once.
   FIELD walks ^VEC:vector<^walk>
   FIELD texts ^texts  \ the statement's copied text
   FIELD writer ^JSON:writer
   \ While CONNECT orders a run, the variables each solution has.
   FIELD bound ^VEC:vector<bool>
   FIELD empty ^rel  \ shared by every placeholder binding
   \ Refilled for each derived table column met into a scope; MEET keeps
   \ no pointer to it.
   FIELD column ^binding
   FIELD fresh ^VEC:vector<n>  \ the variables EXPAND joins `x/y` on
   FIELD cursors ^cursors
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

: WORK-ARENA ( ^plan -- ^ARENA:arena ) TREE SYNTAX-WORK @ ;

\ A record made here is read only until the word that asked for it
\ returns; that word, or one around it, cuts the work arena back.
: WORK-AL ( ^plan -- ^ALLOC:allocator ) WORK-ARENA ARENA:ALLOCATOR ;

: TEXTS ( ^plan -- ^texts ) PLAN-TEXTS @ ;

: NEW-SQL ( ^plan -- ^rel ) TEXTS NEW-REL ;

: +INT ( ^rel n -- ) FMT:INT$ +SQL ;

\ Appends `vK`, variable K's column in a derived table.
: +NAME ( ^rel n -- ) {: v:^ var:n :} v s" v" +SQL v var +INT ;

\ Appends `alias.vK`.
: +NAME-OF ( ^rel str n -- )
   {: v:^ alias:str var:n :} v alias +SQL v s" ." +SQL v var +NAME ;

: SQL+ ( ^plan str -- ) {: plan a:str :} plan PLAN-OUT @ a +SQL ;

: WRITER ( ^plan -- ^JSON:writer )
   PLAN-WRITER @ JSON:RESET ;

\ The writer's text, copied at its length: the next write reuses the writer.
: WRITER$ ( ^plan ^JSON:writer -- str )
   {: plan w :}
   plan TREE w JSON:WRITTEN$ COPY$ ;

: TYPE$ ( ^syntax n -- str )
   NODE-K N-IRI = if s" uri" else s" literal" then ;

\ A headed IRI's term, its value written as the head's bytes then the text.
: HEADED-TERM ( ^JSON:writer ^syntax n -- ^JSON:writer )
   {: writer state term:n :}
   writer JSON:WRITE-OBJECT-START
   s" type" s" uri" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" value" JSON:WRITE-KEY JSON:WRITE-STRING-START
   state term HEAD$ JSON:WRITE-STRING-PART
   state term TEXT$ JSON:WRITE-STRING-PART
   JSON:WRITE-STRING-END JSON:WRITE-OBJECT-END ;

: WRITE-TERM ( ^JSON:writer ^syntax n -- ^JSON:writer )
   {: writer state term:n :}
   state term NODE-C@ 0<> if writer state term HEADED-TERM exit then
   writer state term TYPE$ state term TEXT$ TERM-START
   state term NODE-B@ dup 0<> if
      {: language:n :}
      s" xml:lang" state language TEXT$ +TERM-MEMBER
   else drop state term NODE-A@ dup 0<> if
      state swap IRI$ {: dtype:str :}
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

\ The literal node's datatype IRI.
: NODE-DATATYPE$ ( ^syntax n -- str )
   {: state term:n :}
   state term NODE-B@ 0<> if
      s" http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" exit
   then
   state term NODE-A@ {: datatype:n :}
   datatype 0= if s" http://www.w3.org/2001/XMLSchema#string" exit then
   state datatype IRI$ ;

\ The IRI or literal node's JSON text, valid until the plan's writer is
\ next used.
: TERM-JSON ( ^plan n -- str )
   {: plan node:n :}
   plan WRITER plan TREE node WRITE-TERM JSON:WRITTEN$ ;

\ The IRI or literal node's term, valid until the next call.
: NODE-KNOWN ( ^plan n -- ^known-term )
   {: plan node:n :}
   plan TREE {: state :}
   plan PLAN-CONSTANT {: term:^ :}
   state node NODE-K N-IRI = {: uri:bool :}
   uri term KNOWN-TERM-URI !
   uri if s" " else state node NODE-DATATYPE$ then
   term KNOWN-TERM-DATATYPE ! term ;

\ Rejects the whole query: what it may have at most, and the fix.
: TOO-MANY ( ^plan str str -- )
   {: plan most:str fix:str :}
   plan TREE dup SYNTAX-SOURCE @ str> nip most fix REJECT-TOKEN ;

: PARAMETER@ ( ^plan n -- ^n )
   {: plan k:n :} plan PLAN-ARGUMENTS @ k 1- SEG-AT ;

\ Parameter k's text, empty for a term TERM-TEXT has not copied.
: PARAMETER-TEXT ( ^plan n -- str )
   {: plan k:n :}
   plan k PARAMETER@ @ {: v:n :}
   v 0< 0= if s" " exit then
   plan PLAN-TEXTED @ v invert SEG-AT ARGUMENT-TEXT @ ;

\ Parameter k's term node, 0 for a string.
: PARAMETER-NODE ( ^plan n -- n )
   {: plan k:n :}
   plan k PARAMETER@ @ {: v:n :}
   v 0< 0= if v exit then
   plan PLAN-TEXTED @ v invert SEG-AT ARGUMENT-NODE @ ;

\ Appends the node, 0 for a string, and the text to texted; the value of
\ their parameter.
: +TEXTED ( ^plan n str -- n )
   {: plan node:n text:str :}
   plan PLAN-TEXTED @ {: all:^ :}
   all SEG-PUSH {: a:^ :}
   node a ARGUMENT-NODE ! text a ARGUMENT-TEXT !
   all SEG-COUNT 1- invert ;

\ A new parameter of the value; its number.
: NEW-PARAMETER ( ^plan n -- n )
   {: plan v:n :}
   plan PLAN-ARGUMENTS @ {: args:^ :}
   args SEG-COUNT PARAMETERS = if
      plan s" at most 65535 constants" s" split the query" TOO-MANY
   then
   v args SEG-PUSH ! args SEG-COUNT ;

\ The term node's parameter number, kept as its value from its first use.
: TERM-PARAMETER ( ^plan n -- n )
   {: plan node:n :}
   plan TREE {: state :}
   state node NODE-VALUE@ {: k:n :}
   k 0<> if k exit then
   plan node NEW-PARAMETER dup state node rot NODE-VALUE! ;

\ The IRI or literal node's JSON text, copied once into its parameter's
\ argument: it lives as long as the plan.
: TERM-TEXT ( ^plan n -- str )
   {: plan node:n :}
   plan node TERM-PARAMETER {: k:n :}
   plan k PARAMETER-TEXT {: had:str :}
   had STR:LEN 0<> if had exit then
   plan TREE plan node TERM-JSON COPY$ {: text:str :}
   plan node text +TEXTED plan k PARAMETER@ ! text ;

: ABSORB-TEXT ( ^SIPHASH:state str -- )
   {: sip text:str :}
   sip text STR:LEN SIPHASH:ABSORB-WORD sip text ABSORB$ ;

: STRING-HASH ( ^plan str -- n )
   {: plan text:str :}
   plan PLAN-STRINGS INTERNS-HASH {: sip :}
   sip text ABSORB-TEXT sip SIPHASH:FINISH ;

\ The string's parameter number, added on its first use. The text must
\ live as long as the plan.
: STRING-PARAMETER ( ^plan str -- n )
   {: plan text:str :}
   plan PLAN-STRINGS {: table:^ :}
   table plan text STRING-HASH
      [: plan swap PARAMETER-TEXT text STR:= ;]
      INTERNED {: found:n :}
   found 0<> if found exit then
   plan plan 0 text +TEXTED NEW-PARAMETER {: k:n :}
   table k
      [: plan swap PARAMETER-TEXT plan swap STRING-HASH ;]
      +INTERN k ;

\ Appends parameter k, `$k`.
: +PARAMETER ( ^rel n -- )
   {: v:^ k:n :} v s" $" +SQL v k +INT ;

\ Appends the term node's parameter, JSONB.
: +TERM-ARG ( ^plan ^rel n -- )
   {: plan v:^ node:n :} v plan node TERM-PARAMETER +PARAMETER ;

\ A leaf of the term node's parameter, JSONB.
: TERM-ARG ( ^plan n -- ^rel )
   {: plan node:n :} plan NEW-SQL {: v:^ :} plan v node +TERM-ARG v ;

\ Appends the text inside an E'' literal, each `\` and `'` doubled: `\'`
\ is rejected under backslash_quote off and unsafe in a multibyte encoding.
: +ESCAPED ( ^rel str -- )
   {: v:^ t:str :}
   t STR:LEN 0 ?do
      t STR:DATA i + c@ {: b:n :}
      b [char] \ = b [char] ' = or if b v +BYTE c! then
      b v +BYTE c!
   loop ;

\ Appends the description's JSON term as a literal, JSONB.
: +TERM-LITERAL ( ^rel str -- )
   {: v:^ t:str :} v s" E'" +SQL v t +ESCAPED v s" '::jsonb" +SQL ;

\ A leaf of the description's JSON term as a literal.
: TERM-LITERAL ( ^plan str -- ^rel )
   {: plan t:str :} plan NEW-SQL {: v:^ :} v t +TERM-LITERAL v ;

\ Appends the string's parameter, text.
: +TEXT-ARG ( ^plan ^rel str -- )
   {: plan v:^ text:str :} v plan text STRING-PARAMETER +PARAMETER ;

: STRING+ ( ^plan str -- )
   {: plan a:str :} plan plan PLAN-OUT @ a +TEXT-ARG ;

\ Appends the number's parameter, text.
: +NUMBER-ARG ( ^plan ^rel n -- )
   {: plan v:^ value:n :}
   plan v plan TREE s" " value FMT:INT$ JOIN$ +TEXT-ARG ;

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
   state node NODE-FLAGS@ F-GENERATED and 0<> if
      state s" _:#" state node NODE-VALUE@ FMT:INT$ JOIN$ exit
   then state s" _:" state node TEXT$ JOIN$ ;

\ Rejects the query when n reads more than those so far pass MOST.
: CHECK-READS ( ^plan n -- )
   {: plan more:n :}
   plan PLAN-READS @ more + MOST > if
      plan MOST-TEXT TOO-MANY
   then ;

\ Counts one more read.
: +READ ( ^plan -- )
   {: plan :} plan 1 CHECK-READS 1 plan PLAN-READS +! ;

: NAMED? ( ^name str n -- bool )
   {: v:^ name:str frame:n :}
   v NAME-FRAME @ frame = v NAME-TEXT @ name STR:= and ;

\ The variable of the name in the frame, or -1.
: FIND-VAR-ID ( ^plan str n -- n )
   {: plan name:str frame:n :}
   plan PLAN-VARIABLES @ {: vars:^ :}
   vars VEC:COUNT 0 ?do
      vars i VEC:AT name frame NAMED? if i unloop exit then
   loop -1 ;

\ The variable of the name in the frame, added if new.
: VAR-ID ( ^plan str n -- n )
   {: plan name:str frame:n :}
   plan name frame FIND-VAR-ID {: k:n :}
   k 0 >= if k exit then
   plan +READ
   plan PLAN-VARIABLES @ {: vars:^ :}
   vars VEC:PUSH {: v:^ :}
   name v NAME-TEXT ! frame v NAME-FRAME !
   vars VEC:COUNT 1- ;

: VAR ( ^plan n -- n )
   {: plan node:n :}
   plan plan node VAR-NAME plan PLAN-FRAME @ VAR-ID ;

\ A leaf of `alias.vK`.
: COLUMN-OF ( ^plan str n -- ^rel )
   {: plan alias:str var:n :}
   plan NEW-SQL {: v:^ :} v alias var +NAME-OF v ;

\ A new alias, `tK`.
: NEXT-ALIAS ( ^plan -- str )
   {: plan :}
   plan +READ
   1 plan PLAN-SERIAL +!
   plan TREE s" t" plan PLAN-SERIAL @ FMT:INT$ JOIN$ ;

: LEN ( ^VEC:vector<t> -- n )
   dup NULL-PTR = if drop 0 exit then VEC:COUNT ;

\ No bindings, with room for n.
: BINDINGS-IN ( ^ALLOC:allocator n -- ^VEC:vector<binding> )
   {: al:^ k:n :}
   al 0 [type] binding VEC:NEW {: list:^ :}
   list k VEC:RESERVE list ;

: NEW-BINDINGS ( ^plan n -- ^VEC:vector<binding> )
   {: plan k:n :} plan AL k BINDINGS-IN ;

\ r becomes the FROM, the WHERE and the bindings, with no rows, no
\ branches, no OPTIONAL and no BIND; its emptiness stays.
: INIT-RELATION ( ^rel ^rel ^VEC:vector<binding> ^relation -- )
   {: from:^ where:^ bs:^ r:^ :}
   from r RELATION-FROM ! where r RELATION-WHERE !
   bs r RELATION-BINDINGS !
   NULL-PTR r RELATION-ROWS ! NULL-PTR r RELATION-BRANCHES !
   false r RELATION-OPTIONAL ! false r RELATION-BIND ! ;

: RELATION-OF ( ^plan ^rel ^rel ^VEC:vector<binding> -- ^relation )
   {: plan from:^ where:^ bs:^ :}
   plan AL [type] relation ALLOC:NEW {: r:^ :}
   from where bs r INIT-RELATION r ;

\ A relation of an empty FROM, WHERE and bindings.
: NEW-RELATION ( ^plan -- ^relation )
   {: plan :}
   plan plan NEW-SQL plan NEW-SQL plan 0 NEW-BINDINGS RELATION-OF ;

\ b binds var in the shape to sql and the term, always and not nullable.
: SET-BINDING ( ^binding n n ^rel str -- )
   {: b:^ var:n shape:n sql:^ term:str :}
   var b BINDING-VAR ! shape b BINDING-SHAPE !
   sql b BINDING-SQL ! term b BINDING-TERM !
   true b BINDING-ALWAYS ! false b BINDING-NULLABLE !
   NULL-PTR b BINDING-SPREAD ! ;

\ Appends a binding as SET-BINDING fills it.
: BIND ( ^VEC:vector<binding> n n ^rel str -- )
   {: list:^ var:n shape:n sql:^ term:str :}
   list VEC:PUSH var shape sql term SET-BINDING ;

\ The index of variable n's first binding in the list, or -1.
: FIND-VAR ( ^VEC:vector<binding> n -- n )
   {: list:^ var:n :}
   list VEC:COUNT 0 ?do
      list i VEC:AT BINDING-VAR @ var = if i unloop exit then
   loop -1 ;

: NEW-LAYER ( ^plan ^VEC:vector<binding> ^layer -- ^layer )
   {: plan bs:^ under:^ :}
   plan AL [type] layer ALLOC:NEW {: l:^ :}
   bs l LAYER-BINDINGS ! under l LAYER-UNDER ! l ;

\ Variable n's binding in the scope, NULL-PTR where no layer binds it.
: LOOKUP ( ^layer n -- ^binding )
   {: scope:^ var:n :}
   scope begin dup NULL-PTR <> while
      {: l:^ :}
      l LAYER-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k -1 > if bs k VEC:AT exit then
      l LAYER-UNDER @
   repeat drop NULL-PTR ;

\ Whether any layer of the scope binds a variable.
: BINDS-ANY? ( ^layer -- bool )
   begin dup NULL-PTR <> while
      dup LAYER-BINDINGS @ VEC:COUNT 0<> if drop true exit then
      LAYER-UNDER @
   repeat drop false ;

: SHAPE@ ( ^plan n -- ^shape )
   {: plan k:n :} plan PLAN-STORE @ STORE-SHAPES @ k VEC:AT ;

: SOURCE@ ( ^plan n -- ^source )
   {: plan k:n :} plan PLAN-STORE @ STORE-SOURCES @ k VEC:AT ;

\ Appends `f(a)`.
: +CALL ( ^rel str str -- )
   {: v:^ f:str a:str :}
   v f +SQL v s" (" +SQL v a +SQL v s" )" +SQL ;

\ Appends `f(` the rel `)`.
: +APPLY ( ^rel str ^rel -- )
   {: v:^ f:str a:^ :}
   v f +SQL v s" (" +SQL v a +SPLICE v s" )" +SQL ;

\ Appends the binding's value.
: +BINDING ( ^rel ^binding -- ) BINDING-SQL @ +SPLICE ;

\ Appends `f(` the binding's value `)`.
: +CALL-OF ( ^rel str ^binding -- )
   {: v:^ f:str b:^ :}
   v f +SQL v s" (" +SQL v b +BINDING v s" )" +SQL ;

\ Appends `f(` the term node's parameter `)`.
: +CALL-TERM ( ^plan ^rel str n -- )
   {: plan v:^ f:str node:n :}
   v f +SQL v s" (" +SQL plan v node +TERM-ARG v s" )" +SQL ;

\ Appends the binding's value as a JSONB term.
: +TERM ( ^plan ^rel ^binding -- )
   {: plan v:^ b:^ :}
   b BINDING-SHAPE @ {: shape:n :}
   shape 0< if v b +BINDING exit then
   v plan shape SHAPE@ SHAPE-OUT @ b +CALL-OF ;

\ The shape's root: itself when it is within none.
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

\ Whether shape k holds the KNOWN term.
: HOLDS-KNOWN? ( ^plan n str -- bool )
   {: plan k:n t:str :}
   plan k plan plan t TERM-PLACE TERM@ HOLDS? ;

\ Whether shape x is within shape y, directly or through parts.
: WITHIN? ( ^plan n n -- bool )
   {: plan x:n y:n :}
   plan x SHAPE@ SHAPE-PARENT @
   begin dup 0< 0= while
      dup y = if drop true exit then
      plan swap SHAPE@ SHAPE-PARENT @
   repeat drop false ;

\ Whether shape x, term s and shape y, term t can hold one term: equal terms,
\ a shape and a term it holds, a shape and itself or one it is within.
: KINDS-COMPATIBLE? ( ^plan n str n str -- bool )
   {: plan x:n s:str y:n t:str :}
   x GENERIC = y GENERIC = or if true exit then
   x KNOWN = y KNOWN = and if s t STR:= exit then
   x KNOWN = if plan y s HOLDS-KNOWN? exit then
   y KNOWN = if plan x t HOLDS-KNOWN? exit then
   x y = if true exit then
   plan x y WITHIN? plan y x WITHIN? or ;

: COMPATIBLE? ( ^plan ^binding ^binding -- bool )
   {: plan a:^ b:^ :}
   plan a BINDING-SHAPE @ a BINDING-TERM @ b BINDING-SHAPE @ b BINDING-TERM @
   KINDS-COMPATIBLE? ;

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
   b BINDING-ALWAYS @ list LAST BINDING-ALWAYS !
   b BINDING-SPREAD @ list LAST BINDING-SPREAD ! ;

: COPY-SCOPE ( ^plan ^VEC:vector<binding> -- ^VEC:vector<binding> )
   {: plan scope:^ :}
   plan 0 NEW-BINDINGS {: copy:^ :}
   scope VEC:COUNT 0 ?do scope i VEC:AT copy COPY-BINDING loop copy ;

: SLOT@ ( ^source n -- slot )
   {: src:^ k:n :}
   k 0= if src SOURCE-S @ exit then
   k 1 = if src SOURCE-P @ exit then
   src SOURCE-O @ ;

\ The column's shape and no term, or KNOWN and the term's text.
: SLOT-KIND ( slot -- n str )
   MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :} shape s" " ENDOF
      term OF {: t:str :} KNOWN t ENDOF
   ;MATCH ;

\ Appends the slot under the alias: its column, or its term's reference.
: +SLOT-SQL ( ^rel slot str -- )
   {: v:^ sl alias:str :}
   sl MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :}
         v alias +SQL v s" ." +SQL v c +SQL
      ENDOF
      term OF {: t:str :} v t +TERM-LITERAL ENDOF
   ;MATCH ;

: RECORD-SLOT ( ^plan n n -- slot )
   {: plan r:n k:n :} plan r SOURCE@ k SLOT@ ;

: SLOT-SHAPE ( ^plan ^VEC:vector<n> n n -- n )
   {: plan recs:^ j:n k:n :}
   plan recs j VEC:AT @ k RECORD-SLOT SLOT-KIND drop ;

\ The spread's columns whose shape is one root with shape x: one alone,
\ several in COALESCE, empty for none.
: ROOT-COLUMNS ( ^plan n ^spread -- ^rel )
   {: plan x:n sp:^ :}
   sp SPREAD-RECS @ {: recs:^ :} sp SPREAD-SLOT @ {: k:n :}
   plan NEW-SQL {: cs:^ :}
   0 recs LEN 0 ?do
      plan plan recs i k SLOT-SHAPE x ONE-ROOT? if
         cs s" ," +SEP 1+
         cs plan recs i VEC:AT @ k RECORD-SLOT
         sp SPREAD-ALIAS @ +SLOT-SQL
      then
   loop 2 < if cs exit then
   plan NEW-SQL {: v:^ :} v s" COALESCE(" +SQL
   v cs +SPLICE v s" )" +SQL v ;

\ The condition that bindings a and o, higher-ranked first, hold one term:
\ empty if one, `false` if none fits; native meets GENERIC by its root.
: SAME-SQL ( ^plan ^binding ^binding -- ^rel )
   {: plan a:^ o:^ :}
   plan a o COMPATIBLE? 0= if plan TEXTS s" false" TEXT-REL exit then
   plan PLAN-CURSORS @ a BINDING-SQL @ o BINDING-SQL @ REL= if
      plan NEW-SQL exit
   then
   a BINDING-SHAPE @ KNOWN = o BINDING-SHAPE @ KNOWN = and if
      plan NEW-SQL exit
   then
   a RANK o RANK < if o a else a o then {: x:^ y:^ :}
   x BINDING-SHAPE @ {: shape:n :}
   plan NEW-SQL {: v:^ :}
   v x +BINDING v s"  = " +SQL
   shape -1 > y BINDING-SHAPE @ 0< and if
      y BINDING-SPREAD @ {: sp:^ :}
      sp NULL-PTR = if
         v plan shape SHAPE@ SHAPE-IN @ y +CALL-OF v exit
      then
      plan shape sp ROOT-COLUMNS {: cs:^ :}
      cs EMPTY? if plan TEXTS s" false" TEXT-REL exit then
      v cs +REL
   else v y +BINDING then v ;

\ The constant's value, a binding outside any scope.
: CONSTANT-VALUE ( ^plan n -- ^binding )
   {: plan node:n :}
   plan AL [type] binding ALLOC:NEW {: b:^ :}
   b -1 KNOWN plan node TERM-ARG plan node TERM-TEXT SET-BINDING b ;

\ Appends ` AND `, unless where is empty, and the condition.
: +AND ( ^rel ^rel -- )
   {: where:^ a:^ :}
   where s"  AND " +SEP a EMPTY? 0= if where a +REL then ;

\ Appends to where that the binding is not NULL.
: +NULL-TEST ( ^rel ^binding -- )
   {: where:^ b:^ :}
   where s"  AND " +SEP where b +BINDING
   where s"  IS NOT NULL" +SQL ;

: +NULL-OR ( ^rel ^binding -- )
   {: v:^ b:^ :}
   b BINDING-ALWAYS @ if exit then
   v b +BINDING v s"  IS NULL OR " +SQL ;

\ Appends to where that s and o meet: one term, or a maybe one NULL.
\ Answers whether that is a strict `=`, which drops a NULL of either.
: +MEETS ( ^plan ^binding ^binding ^rel -- bool )
   {: plan s:^ o:^ where:^ :}
   plan s o SAME-SQL {: eq:^ :}
   eq EMPTY? if false exit then
   s BINDING-ALWAYS @ o BINDING-ALWAYS @ and if
      where eq +AND true exit
   then
   where s"  AND " +SEP where s" (" +SQL
   where s +NULL-OR where o +NULL-OR
   where eq +REL where s" )" +SQL
   false ;

: TAKE ( ^binding ^binding -- )
   {: s:^ o:^ :}
   o BINDING-SHAPE @ s BINDING-SHAPE !
   o BINDING-SQL @ s BINDING-SQL !
   o BINDING-SPREAD @ s BINDING-SPREAD !
   o BINDING-TERM @ s BINDING-TERM ! ;

\ b reads sql, which its spread does not describe.
: SET-SQL ( ^rel ^binding -- )
   {: sql:^ b:^ :}
   sql b BINDING-SQL ! NULL-PTR b BINDING-SPREAD ! ;

\ s becomes the first bound of maybe bindings s and o: native where their
\ shapes share a root, of the root where they differ, else a term.
: COALESCED ( ^plan ^binding ^binding -- )
   {: plan s:^ o:^ :}
   s BINDING-SHAPE @ {: x:n :} o BINDING-SHAPE @ {: y:n :}
   plan NEW-SQL {: v:^ :}
   v s" COALESCE(" +SQL
   plan x y ONE-ROOT? if
      v s +BINDING v s" ," +SQL v o +BINDING
      plan x y UNITED s BINDING-SHAPE !
   else
      plan v s +TERM v s" ," +SQL plan v o +TERM
      GENERIC s BINDING-SHAPE !
   then
   v s" )" +SQL v s SET-SQL ;

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

\ The list's bindings on column sql are not nullable: the caller wrote to
\ their relation's WHERE a strict condition that reads sql.
: COLUMN-TESTED ( ^plan ^VEC:vector<binding> ^rel -- )
   {: plan bs:^ sql:^ :}
   bs VEC:COUNT 0 ?do
      plan PLAN-CURSORS @ bs i VEC:AT BINDING-SQL @ sql REL=
      if false bs i VEC:AT BINDING-NULLABLE ! then
   loop ;

\ Joins o into the scope, its condition to where; an OPTIONAL's ON drops
\ no row of the scope. Answers s's column after a strict `=`, else NULL-PTR.
: MEET ( ^plan ^VEC:vector<binding> ^binding ^rel bool -- ^rel )
   {: plan scope:^ o:^ where:^ optional:bool :}
   scope o BINDING-VAR @ FIND-VAR {: k:n :}
   k 0< if
      o scope COPY-BINDING
      optional if false scope LAST BINDING-ALWAYS ! NULL-PTR exit then
      o BINDING-NULLABLE @ scope LAST BINDING-NULLABLE ! NULL-PTR exit
   then
   scope k VEC:AT {: s:^ :} s BINDING-SQL @ {: col:^ :}
   plan s o where +MEETS {: strict:bool :}
   plan PLAN-CURSORS @ col o BINDING-SQL @ REL= {: same:bool :}
   \ s's flag covers an o on s's column.
   o BINDING-NULLABLE @ same 0= and {: open:bool :}
   plan s o optional MERGE
   optional if NULL-PTR exit then
   strict if
      plan scope col COLUMN-TESTED
      plan scope o BINDING-SQL @ COLUMN-TESTED col exit
   then
   open if where o +NULL-TEST then NULL-PTR ;

: MEET-ALL ( ^plan ^VEC:vector<binding> ^VEC:vector<binding>
      ^rel bool -- )
   {: plan scope:^ list:^ where:^ optional:bool :}
   list VEC:COUNT 0 ?do
      list i VEC:AT {: o:^ :}
      plan scope o where optional MEET {: col:^ :}
      col NULL-PTR <> if
         plan list col COLUMN-TESTED plan list o BINDING-SQL @ COLUMN-TESTED
      then
   loop ;

\ The occurrences joined into one binding per variable, their conditions
\ to where.
: JOINS ( ^plan ^VEC:vector<binding> ^rel
      -- ^VEC:vector<binding> )
   {: plan list:^ where:^ :}
   plan list VEC:COUNT NEW-BINDINGS {: scope:^ :}
   plan scope list where false MEET-ALL scope ;

\ The triple's subject, predicate or object.
: PART ( ^syntax n n -- n )
   {: state triple:n k:n :}
   k 0= if state triple NODE-A@ exit then
   k 1 = if state triple NODE-B@ exit then
   state triple NODE-C@ ;

\ How many of the triple's parts are variables.
: TRIPLE-VARS ( ^syntax n -- n )
   {: state triple:n :}
   0 3 0 do state state triple i PART VARIABLE? if 1+ then loop ;

\ A leaf of the slot under the alias.
: SLOT-SQL ( ^plan slot str -- ^rel )
   {: plan sl alias:str :} plan NEW-SQL {: v:^ :} v sl alias +SLOT-SQL v ;

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
      column OF 2drop 2drop drop drop true ENDOF
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
   plan k KEY-SET@ KEY-SET-PLACES @ state node IRI$ MAP:HAS? ;

\ Whether a constant of the pattern fits the slot: a column whose shape
\ holds it and whose key set, if it has a key, lists it; or the slot's
\ own term; a predicate !(...) of IRIs, a slot outside them.
: FITS? ( ^plan n slot -- bool )
   {: plan node:n sl :}
   plan TREE node NEGATED? if
      plan plan TREE node NODE-A@ sl OUTSIDE? exit
   then
   sl MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :}
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

\ The part of the triple holding its kind'th variable.
: VAR-PART ( ^syntax n n -- n )
   {: state triple:n kind:n :}
   0 3 0 do
      state state triple i PART VARIABLE? if
         dup kind = if drop i unloop exit then 1+
      then
   loop drop -1 ;

\ The shape and term choice c gives the item's kind'th column: a derived
\ table's own, else its source's slot at the triple's part.
: KIND@ ( ^plan ^item ^choice n -- n str )
   {: plan it:^ c:^ kind:n :}
   c CHOICE-SOURCE @ 0< if
      it ITEM-COLUMNS @ kind VEC:AT {: b:^ :}
      b BINDING-SHAPE @ b BINDING-TERM @ exit
   then
   plan c CHOICE-SOURCE @ SOURCE@
   plan TREE it ITEM-NODE @ kind VAR-PART SLOT@ SLOT-KIND ;

\ Whether every column of column k's variable can meet it under choice c.
: MEETS? ( ^plan ^item ^choice n -- bool )
   {: plan it:^ c:^ k:n :}
   it ITEM-COLUMNS @ {: cols:^ :}
   cols k VEC:AT BINDING-VAR @ {: var:n :}
   plan it c k KIND@ {: y:n t:str :}
   cols VEC:COUNT 0 ?do
      cols i VEC:AT BINDING-VAR @ var = if
         plan plan it c i KIND@ y t KINDS-COMPATIBLE? 0= if
            false unloop exit
         then
      then
   loop true ;

\ Whether a variable repeated in the item's columns meets itself under
\ choice c.
: CONSISTENT? ( ^plan ^item ^choice -- bool )
   {: plan it:^ c:^ :}
   it ITEM-COLUMNS @ VEC:COUNT 0 ?do
      plan it c i MEETS? 0= if false unloop exit then
   loop true ;

\ Whether the substitution binds the variable always to a term shape y and
\ term t cannot hold.
: CLASHES? ( ^plan n n str -- bool )
   {: plan var:n y:n t:str :}
   plan PLAN-OUTER @ var LOOKUP {: o:^ :}
   o NULL-PTR = if false exit then
   o BINDING-ALWAYS @
   plan o BINDING-SHAPE @ o BINDING-TERM @ y t KINDS-COMPATIBLE? 0= and ;

: SUBSTITUTABLE? ( ^plan ^item ^choice -- bool )
   {: plan it:^ c:^ :}
   it ITEM-COLUMNS @ {: cols:^ :}
   cols VEC:COUNT 0 ?do
      plan cols i VEC:AT BINDING-VAR @ plan it c i KIND@ CLASHES? if
         false unloop exit
      then
   loop true ;

\ The item's choice of source k, alive.
: NEW-CHOICE ( ^item n -- ^choice )
   {: it:^ k:n :}
   it ITEM-CHOICES @ VEC:PUSH {: c:^ :}
   k c CHOICE-SOURCE ! true c CHOICE-ALIVE ! c ;

\ The triple's variables by part, each bound always.
: TRIPLE-COLUMNS ( ^plan n -- ^VEC:vector<binding> )
   {: plan triple:n :}
   plan TREE {: state :}
   plan state triple TRIPLE-VARS NEW-BINDINGS {: cols:^ :}
   3 0 do
      state triple i PART {: node:n :}
      state node VARIABLE? if
         cols plan node VAR GENERIC plan PLAN-EMPTY @ s" " BIND
      then
   loop cols ;

\ Whether source k holds the triple's graph and every constant part.
: TRIPLE-FITS? ( ^plan ^item n -- bool )
   {: plan it:^ k:n :}
   plan k SOURCE@ {: src:^ :}
   plan src GRAPH-FITS? 0= if false exit then
   plan TREE {: state :}
   it ITEM-NODE @ {: triple:n :}
   3 0 do
      state triple i PART {: node:n :}
      state node VARIABLE? 0= if
         plan node src i SLOT@ FITS? 0= if false unloop exit then
      then
   loop true ;

\ The number of the list's records that fit the triple.
: FITTING ( ^plan ^item ^VEC:vector<n> -- n )
   {: plan it:^ list:^ :}
   0 list VEC:COUNT 0 ?do
      plan it list i VEC:AT @ TRIPLE-FITS? if 1+ then
   loop ;

\ Adds source k as the triple's choice when it fits.
: TRIPLE-CHOICE ( ^plan ^item n -- )
   {: plan it:^ k:n :}
   plan it k TRIPLE-FITS? 0= if exit then
   it ITEM-NODE @ {: triple:n :}
   it ITEM-COLUMNS @ NULL-PTR = if
      plan triple TRIPLE-COLUMNS it ITEM-COLUMNS !
   then
   it k NEW-CHOICE {: c:^ :}
   plan it c CONSISTENT? plan it c SUBSTITUTABLE? and c CHOICE-ALIVE ! ;

: NEW-ITEM ( ^plan ^VEC:vector<item> n -- ^item )
   {: plan items:^ node:n :}
   plan +READ
   items VEC:PUSH {: it:^ :}
   node it ITEM-NODE ! 0 it ITEM-MODE !
   NULL-PTR it ITEM-COLUMNS !
   plan PLAN-GRAPH @ it ITEM-GRAPH !
   NULL-PTR it ITEM-BRANCHES ! NULL-PTR it ITEM-WHERE !
   plan AL 0 [type] choice VEC:NEW it ITEM-CHOICES ! it ;

: TRY-EACH ( ^plan ^item ^VEC:vector<n> -- )
   {: plan it:^ list:^ :}
   it ITEM-CHOICES @ plan it list FITTING VEC:RESERVE
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
   a VEC:COUNT b VEC:COUNT + {: n:n :}
   plan it a FITTING plan it b FITTING + {: fit:n :}
   it ITEM-CHOICES @ fit VEC:RESERVE
   0 n 0 ?do
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
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT {: n:n :}
   0 n 0 ?do plan it i TRIPLE-FITS? if 1+ then loop {: fit:n :}
   it ITEM-CHOICES @ fit VEC:RESERVE
   n 0 ?do plan it i TRIPLE-CHOICE loop ;

\ A joined derived table with its columns, the item's one choice.
: CHILD-ITEM ( ^plan ^VEC:vector<item> ^rel ^VEC:vector<binding> bool
      -- )
   {: plan items:^ t:^ cols:^ empty:bool :}
   plan items 0 NEW-ITEM {: it:^ :}
   t it ITEM-TABLE ! cols it ITEM-COLUMNS !
   empty 0= it -1 NEW-CHOICE CHOICE-ALIVE ! ;

: LIVE-COUNT ( ^item -- n )
   ITEM-CHOICES @ {: cs:^ :}
   0 cs VEC:COUNT 0 ?do
      cs i VEC:AT CHOICE-ALIVE @ if 1+ then
   loop ;

\ An index list is NULL-PTR until its first push, which makes it.
: INDEXES ( -- ^VEC:vector<n> ) NULL-PTR ;

: +INDEX-IN ( ^ALLOC:allocator ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: al:^ list:^ k:n :}
   list NULL-PTR = if al 0 [type] n VEC:NEW else list then
   {: out:^ :} k out VEC:PUSH ! out ;

: +INDEX ( ^plan ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: plan list:^ k:n :} plan AL list k +INDEX-IN ;

: +WORK-INDEX ( ^plan ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: plan list:^ k:n :} plan WORK-AL list k +INDEX-IN ;

: INDEXED? ( ^VEC:vector<n> n -- bool )
   {: list:^ k:n :}
   list LEN 0 ?do
      list i VEC:AT @ k = if true unloop exit then
   loop false ;

\ The list with n pushed unless it holds n.
: +ONCE ( ^plan ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: plan list:^ k:n :}
   list k INDEXED? if list exit then
   plan list k +INDEX ;

: +WORK-ONCE ( ^plan ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: plan list:^ k:n :}
   list k INDEXED? if list exit then
   plan list k +WORK-INDEX ;

\ A shape and term as KINDS-COMPATIBLE? tells them apart; answers holds, by
\ a class up to it, whether the two are compatible: 0 unasked, 1 no, 2 yes.
STRUCTURE class DERIVE addr
   FIELD shape n
   FIELD term str
   FIELD answers ^VEC:vector<n>
;STRUCTURE

\ A variable and its sites.
STRUCTURE domain DERIVE addr
   FIELD var n
   FIELD sites ^VEC:vector<n>
   FIELD queued bool
;STRUCTURE

\ Where an item binds a variable always, its kind'th column: its class by
\ choice, and each class's number of live choices.
STRUCTURE site DERIVE addr
   FIELD item ^item
   FIELD kind n
   FIELD first n  \ the item's first site; an item's sites are consecutive
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
   plan WORK-AL {: al:^ :}
   al [type] pruning ALLOC:NEW {: w:^ :}
   plan w PRUNING-PLAN !
   al 0 [type] class VEC:NEW w PRUNING-CLASSES !
   al 0 [type] site VEC:NEW w PRUNING-SITES !
   al 0 [type] domain VEC:NEW w PRUNING-DOMAINS ! w ;

: OF-CLASS? ( n str ^class -- bool )
   {: y:n t:str c:^ :}
   c CLASS-SHAPE @ y <> if false exit then
   c CLASS-TERM @ t STR:= ;

\ The class of the shape and term, new on its first use.
: CLASS-OF ( ^pruning n str -- n )
   {: w:^ y:n t:str :}
   w PRUNING-CLASSES @ {: cs:^ :}
   cs VEC:COUNT {: k:n :}
   k 0 ?do y t cs i VEC:AT OF-CLASS? if i unloop exit then loop
   cs VEC:PUSH {: c:^ :}
   y c CLASS-SHAPE ! t c CLASS-TERM !
   w PRUNING-PLAN @ WORK-AL cs VEC:COUNT [type] n VEC:NEW
      c CLASS-ANSWERS ! k ;

\ The index of the variable's domain, new on its first use.
: DOMAIN-OF ( ^pruning n -- n )
   {: w:^ var:n :}
   w PRUNING-DOMAINS @ {: ds:^ :}
   ds VEC:COUNT {: k:n :}
   k 0 ?do
      ds i VEC:AT DOMAIN-VAR @ var = if i unloop exit then
   loop
   ds VEC:PUSH {: d:^ :}
   var d DOMAIN-VAR ! k ;

: +SITE ( ^pruning ^item n n n -- )
   {: w:^ it:^ kind:n var:n first:n :}
   w PRUNING-SITES @ {: sites:^ :}
   w var DOMAIN-OF {: d:n :}
   w d DOMAIN@ {: dom:^ :}
   w PRUNING-PLAN @ dom DOMAIN-SITES @ sites VEC:COUNT +WORK-INDEX
   dom DOMAIN-SITES !
   sites VEC:PUSH {: s:^ :}
   it s SITE-ITEM ! kind s SITE-KIND ! first s SITE-FIRST !
   d s SITE-DOMAIN ! ;

: +SITES ( ^pruning ^item -- )
   {: w:^ it:^ :}
   it ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0= if exit then
   w PRUNING-SITES @ VEC:COUNT {: first:n :}
   it ITEM-COLUMNS @ {: cols:^ :}
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if w it i b BINDING-VAR @ first +SITE then
   loop ;

: SHARED? ( ^pruning ^site -- bool )
   {: w:^ s:^ :}
   w s SITE-DOMAIN @ DOMAIN@ DOMAIN-SITES @ LEN 1 > ;

: CLASSIFY ( ^pruning ^site -- )
   {: w:^ s:^ :}
   w PRUNING-PLAN @ {: plan :}
   s SITE-ITEM @ {: it:^ :}
   it ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      w plan it cs i VEC:AT s SITE-KIND @ KIND@ CLASS-OF {: c:n :}
      plan s SITE-CLASSES @ c +WORK-INDEX s SITE-CLASSES !
   loop ;

: COUNT-LIVE ( ^pruning ^site -- )
   {: w:^ s:^ :}
   w PRUNING-PLAN @ WORK-AL w PRUNING-CLASSES @ VEC:COUNT
      [type] n VEC:NEW s SITE-LIVE !
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
   true d DOMAIN-QUEUED !
   w PRUNING-PLAN @ w PRUNING-QUEUE @ k +WORK-INDEX
   w PRUNING-QUEUE ! ;

\ Whether classes x and y are compatible, x not before y.
: ASK ( ^pruning n n -- bool )
   {: w:^ x:n y:n :}
   w x CLASS@ {: c:^ :} w y CLASS@ {: o:^ :}
   c CLASS-ANSWERS @ y VEC:AT {: kept:^ :}
   kept @ 0= if
      w PRUNING-PLAN @ c CLASS-SHAPE @ c CLASS-TERM @
         o CLASS-SHAPE @ o CLASS-TERM @ KINDS-COMPATIBLE? if 2 else 1 then
         kept !
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
   list LEN 0 ?do
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
   list LEN 0 ?do
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
   begin w PRUNING-HEAD @ w PRUNING-QUEUE @ LEN < while
      w PRUNING-QUEUE @ w PRUNING-HEAD @ VEC:AT @ {: k:n :}
      w w k DOMAIN@ SWEEP
      1 w PRUNING-HEAD +!
   repeat ;

\ Kills each choice another item cannot support, until none changes; its
\ scratch lives in the work arena until it returns.
: PRUNE ( ^plan ^VEC:vector<item> -- )
   {: plan items:^ :}
   items VEC:COUNT 2 < if exit then
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan NEW-PRUNING {: w:^ :}
   items VEC:COUNT 0 ?do w items i VEC:AT +SITES loop
   w [: CLASSIFY ;] EACH-SHARED
   w [: COUNT-LIVE ;] EACH-SHARED
   w [: SITE-DOMAIN @ ENQUEUE ;] EACH-SHARED
   w DRAIN work mark ARENA:CUT ;

\ The description source the choice's record was read from.
: ORIGIN ( ^plan ^choice -- n ) CHOICE-SOURCE @ SOURCE@ SOURCE-ORIGIN @ ;

\ The description sources of the item's live choices, each once, in
\ the work arena.
: ORIGINS ( ^plan ^item -- ^VEC:vector<n> )
   {: plan it:^ :}
   it ITEM-CHOICES @ {: cs:^ :}
   INDEXES cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if plan swap plan c ORIGIN +WORK-ONCE then
   loop ;

\ The records of the item's live choices read from description source
\ origin, in the work arena.
: LIVE-OF ( ^plan ^item n -- ^VEC:vector<n> )
   {: plan it:^ origin:n :}
   it ITEM-CHOICES @ {: cs:^ :}
   INDEXES cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if
         plan c ORIGIN origin = if
            plan swap c CHOICE-SOURCE @ +WORK-INDEX
         then
      then
   loop ;

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
\ kept out of occs has its null test written to where at once. Answers
\ sql after a strict `=`, else NULL-PTR.
: OCCUR ( ^plan ^VEC:vector<binding> n n ^rel str ^spread bool ^rel
      -- ^rel )
   {: plan occs:^ var:n shape:n sql:^ t:str sp:^ null:bool where:^ :}
   plan PLAN-OUTER @ var LOOKUP {: o:^ :}
   o NULL-PTR = if
      occs var shape sql t BIND sp occs LAST BINDING-SPREAD !
      null occs LAST BINDING-NULLABLE ! NULL-PTR exit
   then
   o BINDING-ALWAYS @ if
      plan AL [type] binding ALLOC:NEW dup var shape sql t SET-BINDING
   else occs var shape sql t BIND occs LAST then {: b:^ :}
   sp b BINDING-SPREAD !
   plan b o where +MEETS {: strict:bool :}
   strict 0= null and
   plan PLAN-CURSORS @ sql o BINDING-SQL @ REL= 0= and
   {: open:bool :}
   o BINDING-ALWAYS @ 0= if open b BINDING-NULLABLE ! NULL-PTR exit then
   open if where b +NULL-TEST then
   strict if sql else NULL-PTR then ;

\ Appends to where that b meets the substitution's binding of its variable.
: +OUTER ( ^plan ^binding ^rel -- )
   {: plan b:^ where:^ :}
   plan PLAN-OUTER @ b BINDING-VAR @ LOOKUP {: o:^ :}
   o NULL-PTR = if exit then
   plan b o where +MEETS drop ;

\ The column slot's key, none for a term slot.
: SLOT-KEY ( slot -- nullable<key> )
   MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :} k ENDOF
      term OF drop NULLABLE:NONE ENDOF
   ;MATCH ;

\ Appends to where that the key column holds one of the IRI's keys,
\ `<alias>.<col> = ANY(E'<keys>'::<type>)`.
: +KEY-MATCH ( ^plan n ^key str ^rel -- )
   {: plan node:n k:^ alias:str where:^ :}
   plan k KEY-SET@ {: set:^ :}
   where alias +SQL where s" ." +SQL
   where k KEY-COL @ +SQL where s"  = ANY(E'" +SQL
   where set plan TREE node IRI$ KEYS$ +ESCAPED
   where s" '::" +SQL where set KEY-SET-TYPE @ +SQL where s" )" +SQL ;

\ Appends to where that the column slot holds the constant, through its
\ key when it has one; for a predicate !(...), a predicate other than its
\ IRIs. A member outside the shape has a NULL `in`, which `IS NOT TRUE`
\ reads as not matched.
: +CONSTANT ( ^plan n slot str ^rel -- )
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
   where sl alias +SLOT-SQL
   plan TREE node NEGATED? 0= if
      where s"  = " +SQL
      plan where f node +CALL-TERM exit
   then
   where s"  IS NOT NULL" +SQL
   plan TREE node NODE-A@ {: list:n :}
   list 0= if exit then
   where s"  AND (" +SQL where sl alias +SLOT-SQL where s"  IN (" +SQL
   list begin dup 0<> while
      {: each:n :}
      plan where f plan TREE each NODE-A@ +CALL-TERM
      plan TREE each NODE-B@ dup 0<> if where s" ," +SQL then
   repeat drop where s" )) IS NOT TRUE" +SQL ;

\ A variable's occurrence of the slot under alias to occs; a constant a
\ column holds, to where. covered: another part drops the column's NULL.
\ Answers what OCCUR answers, else NULL-PTR.
: +SLOT ( ^plan n slot str ^VEC:vector<binding> ^rel bool -- ^rel )
   {: plan node:n sl alias:str occs:^ where:^ covered:bool :}
   sl SLOT-KIND {: shape:n t:str :}
   plan TREE node VARIABLE? if
      plan sl alias SLOT-SQL {: sql:^ :}
      shape -1 >  sl SLOT-NOTNULL? 0=  and  covered 0=  and {: null:bool :}
      plan occs plan node VAR shape sql t NULL-PTR null where OCCUR
      exit
   then
   shape -1 > if
      where s"  AND " +SEP plan node sl alias where +CONSTANT
   then NULL-PTR ;

: SLOT-COL ( slot -- str )
   MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :} c ENDOF
      term OF drop s" " ENDOF
   ;MATCH ;

\ Whether two slots of one FROM item are one: a column of one name and
\ shape, or a term of one description source.
: SAME-SLOT? ( slot slot -- bool )
   {: a b :}
   a SLOT-COL b SLOT-COL STR:=
   a SLOT-KIND drop b SLOT-KIND drop = and ;

\ Whether every record but the first has sl at part other.
: SLOT-SHARED? ( ^plan ^VEC:vector<n> n slot -- bool )
   {: plan recs:^ other:n sl :}
   recs LEN 1 ?do
      plan recs i VEC:AT @ other RECORD-SLOT sl SAME-SLOT? 0= if
         false unloop exit
      then
   loop true ;

\ Whether an unbound variable before part this drops its slot's NULL.
: VAR-COVERS? ( ^plan n slot n n -- bool )
   {: plan node:n sl other:n this:n :}
   other this <  sl SLOT-NOTNULL? 0=  and 0= if false exit then
   plan PLAN-OUTER @ plan node VAR LOOKUP NULL-PTR = ;

\ Whether part other, one slot across recs, reads col and drops its NULL.
: PART-COVERS? ( ^plan n ^VEC:vector<n> str n n -- bool )
   {: plan triple:n recs:^ col:str other:n this:n :}
   plan recs 0 VEC:AT @ other RECORD-SLOT {: sl :}
   other this =  sl SLOT-COL col STR:= 0=  or if false exit then
   sl SLOT-KIND drop -1 > 0= if false exit then
   plan recs other sl SLOT-SHARED? 0= if false exit then
   plan TREE triple other PART {: node:n :}
   plan TREE node VARIABLE? if
      plan node sl other this VAR-COVERS? exit
   then
   sl SLOT-KEY MATCH nullable
      none OF true ENDOF
      some OF drop false ENDOF
   ;MATCH ;

\ Whether a part from first on drops the NULL of part this's column:
\ a keyless constant, or an earlier unbound variable.
: COVERED? ( ^plan n ^VEC:vector<n> n n -- bool )
   {: plan triple:n recs:^ first:n this:n :}
   plan recs 0 VEC:AT @ this RECORD-SLOT SLOT-COL {: col:str :}
   col STR:LEN 0= if false exit then
   3 first ?do
      plan triple recs col i this PART-COVERS? if
         true unloop exit
      then
   loop false ;

\ Whether a record of the list has record r's slot k.
: SLOT-LISTED? ( ^plan ^VEC:vector<n> n n -- bool )
   {: plan list:^ r:n k:n :}
   plan r k RECORD-SLOT {: sl :}
   list LEN 0 ?do
      plan list i VEC:AT @ k RECORD-SLOT sl SAME-SLOT? if
         true unloop exit
      then
   loop false ;

\ Of the records, the first of each different slot k, in the work arena.
: DISTINCT-AT ( ^plan ^VEC:vector<n> n -- ^VEC:vector<n> )
   {: plan recs:^ k:n :}
   INDEXES recs LEN 0 ?do
      recs i VEC:AT @ {: r:n :}
      {: out:^ :}
      plan out r k SLOT-LISTED? if out
      else plan out r +WORK-INDEX then
   loop ;

\ The form of the records' columns at slot k: their one shape, the root
\ of shapes of one root that differ, else GENERIC.
: COLUMNS-FORM ( ^plan ^VEC:vector<n> n -- n )
   {: plan recs:^ k:n :}
   plan recs 0 k SLOT-SHAPE
   recs LEN 1 ?do
      {: form:n :} plan recs i k SLOT-SHAPE {: y:n :}
      plan form y ONE-ROOT? 0= if GENERIC unloop exit then
      plan form y UNITED
   loop ;

\ The records' columns at slot k under the alias as one value of their
\ form, a leaf: COALESCE of the columns, or of their terms for GENERIC.
: COALESCE-COLUMNS ( ^plan ^VEC:vector<n> n str -- n ^rel )
   {: plan recs:^ k:n alias:str :}
   plan recs k COLUMNS-FORM {: form:n :}
   plan NEW-SQL {: v:^ :}
   v s" COALESCE(" +SQL
   recs LEN 0 ?do
      i 0<> if v s" ," +SQL then
      plan recs i VEC:AT @ k RECORD-SLOT {: sl :}
      form 0< if
         v plan plan recs i k SLOT-SHAPE SHAPE@ SHAPE-OUT @ +SQL
         v s" (" +SQL v sl alias +SLOT-SQL v s" )" +SQL
      else v sl alias +SLOT-SQL then
   loop v s" )" +SQL form v ;

\ Appends to where what one column slot needs for the node: not null for
\ a variable, the constant held otherwise.
: +COLUMN-COND ( ^plan n slot str ^rel -- )
   {: plan node:n sl alias:str where:^ :}
   plan TREE node VARIABLE? if
      where sl alias +SLOT-SQL where s"  IS NOT NULL" +SQL exit
   then
   where s" (" +SQL plan node sl alias where +CONSTANT where s" )" +SQL ;

\ Appends to where `(c1 OR ...)`, each the need of one of the records'
\ columns at slot k.
: +ANY-COLUMN ( ^plan n ^VEC:vector<n> n str ^rel -- )
   {: plan node:n recs:^ k:n alias:str where:^ :}
   where s"  AND " +SEP where s" (" +SQL
   recs LEN 0 ?do
      i 0<> if where s"  OR " +SQL then
      plan recs i VEC:AT @ k RECORD-SLOT {: sl :}
      plan node sl alias where +COLUMN-COND
   loop where s" )" +SQL ;

\ The list copied into the syntax's arena.
: KEEP-LIST ( ^plan ^VEC:vector<n> -- ^VEC:vector<n> )
   {: plan list:^ :}
   plan AL list LEN [type] n VEC:NEW {: out:^ :}
   list LEN 0 ?do list i VEC:AT @ out i VEC:AT ! loop out ;

\ Slot k over several columns: a variable binds their COALESCE, and one
\ of them is not null or holds the constant. A spread keeps its own copy
\ of recs, which may be in the work arena. Answers what OCCUR answers,
\ else NULL-PTR.
: +COLUMNS ( ^plan n ^VEC:vector<n> n str ^VEC:vector<binding>
      ^rel -- ^rel )
   {: plan node:n recs:^ k:n alias:str occs:^ where:^ :}
   NULL-PTR plan TREE node VARIABLE? if
      drop plan recs k alias COALESCE-COLUMNS {: form:n sql:^ :}
      NULL-PTR form 0< if
         drop plan AL [type] spread ALLOC:NEW {: sp:^ :}
         plan recs KEEP-LIST sp SPREAD-RECS !
         k sp SPREAD-SLOT !
         alias sp SPREAD-ALIAS ! sp
      then {: sp:^ :}
      plan occs plan node VAR form sql s" " sp false where OCCUR
   then
   plan node recs k alias where +ANY-COLUMN ;

\ How many of the triple's variable parts from part first on OCCUR binds
\ into occs: those the substitution does not always bind.
: OCCURRENCES ( ^plan n n -- n )
   {: plan triple:n first:n :}
   plan TREE {: state :}
   0 3 first do
      state triple i PART {: node:n :}
      state node VARIABLE? if
         plan PLAN-OUTER @ plan node VAR LOOKUP {: o:^ :}
         o NULL-PTR = if 1+ else o BINDING-ALWAYS @ 0= if 1+ then then
      then
   loop ;

\ The slots from the first of the records, of one description source,
\ for the triple under the alias: one where they read one column or term
\ there, else their several columns. An occurrence in occs on a column a
\ slot's strict meeting reads is not nullable, a later slot's too.
: SLOTS-SQL ( ^plan n ^VEC:vector<n> str ^VEC:vector<binding>
      ^rel n -- )
   {: plan triple:n recs:^ alias:str occs:^ where:^ first:n :}
   plan TREE {: state :}
   \ One answer per slot rides below the loop, NULL-PTR for one before
   \ first.
   NULL-PTR NULL-PTR NULL-PTR
   3 first do
      rot drop
      state triple i PART {: node:n :}
      plan recs i DISTINCT-AT {: cols:^ :}
      cols LEN 1 = if
         plan node plan cols 0 VEC:AT @ i RECORD-SLOT alias occs where
            plan triple recs first i COVERED? +SLOT
      else plan node cols i alias occs where +COLUMNS then
   loop {: a:^ b:^ c:^ :}
   a NULL-PTR <> if plan occs a COLUMN-TESTED then
   b NULL-PTR <> if plan occs b COLUMN-TESTED then
   c NULL-PTR <> if plan occs c COLUMN-TESTED then ;

: +FROM ( ^rel ^source str -- )
   {: from:^ src:^ alias:str :}
   from s"  CROSS JOIN " +SEP from src SOURCE-FROM @ +TEXT
   from s"  AS " +TEXT from alias +TEXT ;

: SLOT-UNIQUE? ( slot -- bool )
   MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :} u ENDOF
      term OF drop false ENDOF
   ;MATCH ;

\ Whether the records read one subject slot, a unique column.
: UNIQUE-SUBJECT? ( ^plan ^VEC:vector<n> -- bool )
   {: plan recs:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan recs 0 DISTINCT-AT LEN work mark ARENA:CUT
   1 <> if false exit then
   plan recs 0 VEC:AT @ 0 RECORD-SLOT SLOT-UNIQUE? ;

\ Whether two nodes are one variable, or constants that are one term.
: SAME-NODE? ( ^plan n n -- bool )
   {: plan a:n b:n :}
   plan TREE a VARIABLE? {: var:bool :}
   plan TREE b VARIABLE? var xor if false exit then
   var if plan a VAR-NAME plan b VAR-NAME STR:= exit then
   a b = ;

\ Whether the node is an operator, call, list cell or ORDER BY condition
\ but a call of RAND, UUID or STRUUID, which is new at each call.
: HEADED? ( ^syntax n -- bool )
   {: state node:n :}
   state node NODE-K {: k:n :}
   k N-CALL = k N-UNARY = or k N-BINARY = or
   k N-LIST = or k N-ORDER = or 0= if false exit then
   state node s" RAND UUID STRUUID" CALL-IN? 0= ;

\ Whether nodes a and b of one kind are one head by text and flags; a node
\ that is not HEADED? is one with no other.
: SAME-HEAD? ( ^syntax n n -- bool )
   {: state a:n b:n :}
   state a HEADED? 0= if false exit then
   state a TEXT$ state b TEXT$ STR:=
   state a NODE-FLAGS@ state b NODE-FLAGS@ = and ;

\ Whether expressions a and b are one: terms SAME-NODE? finds one, or one
\ head over parts that are one.
: SAME-EXPRESSION? ( ^plan n n -- bool )
   {: plan a:n b:n :}
   a b = if true exit then
   plan TREE {: state :}
   a 0= b 0= or if false exit then
   state a NODE-K {: k:n :}
   state b NODE-K k <> if false exit then
   k N-UNDEF <= if plan a b SAME-NODE? exit then
   state a b SAME-HEAD? 0= if false exit then
   plan state a NODE-A@ state b NODE-A@ RECURSE 0= if false exit then
   plan state a NODE-B@ state b NODE-B@ RECURSE 0= if false exit then
   plan state a NODE-C@ state b NODE-C@ RECURSE ;

\ Absorbs expression e, or 0, so that expressions SAME-EXPRESSION? finds
\ one absorb the same words.
: ABSORB-EXPRESSION ( ^plan ^SIPHASH:state n -- )
   {: plan sip e:n :}
   e 0= if sip -1 SIPHASH:ABSORB-WORD exit then
   plan TREE {: state :}
   state e NODE-K {: k:n :}
   k N-UNDEF <= if
      sip k SIPHASH:ABSORB-WORD
      state e VARIABLE? if sip state e TEXT$ ABSORB-TEXT exit then
      sip e SIPHASH:ABSORB-WORD exit
   then
   state e HEADED? 0= if sip e SIPHASH:ABSORB-WORD exit then
   sip k SIPHASH:ABSORB-WORD sip state e TEXT$ ABSORB-TEXT
   sip state e NODE-FLAGS@ SIPHASH:ABSORB-WORD
   plan sip state e NODE-A@ RECURSE
   plan sip state e NODE-B@ RECURSE
   plan sip state e NODE-C@ RECURSE ;

: EXPRESSION-HASH ( ^plan n -- n )
   {: plan e:n :}
   plan PLAN-STRINGS INTERNS-HASH {: sip :}
   plan sip e ABSORB-EXPRESSION sip SIPHASH:FINISH ;

\ Whether seen holds an expression one with e; adds e if not.
: SEEN? ( ^plan ^VEC:vector<hashed> n -- bool )
   {: plan seen:^ e:n :}
   plan e EXPRESSION-HASH {: h:n :}
   seen VEC:COUNT 0 ?do
      seen i VEC:AT HASHED-DIGEST @ h = if
         plan seen i VEC:AT HASHED-NODE @ e SAME-EXPRESSION? if
            true unloop exit
         then
      then
   loop
   seen VEC:PUSH {: k:^ :} e k HASHED-NODE ! h k HASHED-DIGEST ! false ;

\ A vector of keys, room for those of the list.
: NEW-SEEN ( ^plan n -- ^VEC:vector<hashed> )
   {: plan list:n :}
   plan AL 0 [type] hashed VEC:NEW {: seen:^ :}
   seen plan TREE list LIST-LENGTH VEC:RESERVE seen ;

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
   rows LEN 0 ?do
      plan rows i VEC:AT node src ROW-OF? if i unloop exit then
   loop -1 ;

\ The column slot's `only`, none for a term slot.
: SLOT-ONLY ( slot -- nullable<key> )
   MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :} o ENDOF
      term OF drop NULLABLE:NONE ENDOF
   ;MATCH ;

\ Whether the slot is a column keyed on column k.
: KEYED-ON? ( slot str -- bool )
   {: sl k:str :}
   sl SLOT-KEY MATCH nullable
      none OF false ENDOF
      some OF {: key:^ :} key KEY-COL @ k STR:= ENDOF
   ;MATCH ;

\ Whether the slot's `only` is on column k and lists the node.
: ONLY-LISTS? ( ^plan slot str n -- bool )
   {: plan sl k:str node:n :}
   sl SLOT-ONLY MATCH nullable
      none OF false ENDOF
      some OF {: o:^ :}
         o KEY-COL @ k STR:= plan node o LISTED-KEY? and
      ENDOF
   ;MATCH ;

\ The slots the description source's slot j reads to.
: ORIGIN-SLOTS ( ^plan n n -- ^VEC:vector<alternative> )
   {: plan origin:n j:n :}
   plan PLAN-STORE @ STORE-ORIGINS @ origin VEC:AT {: org:^ :}
   j 0= if org ORIGIN-S @ exit then
   j 1 = if org ORIGIN-P @ exit then
   org ORIGIN-O @ ;

\ Whether one of the slots the description source's slot j reads to has
\ an `only` on column k that lists the node.
: ORIGIN-LISTS? ( ^plan n n str n -- bool )
   {: plan origin:n j:n k:str node:n :}
   plan origin j ORIGIN-SLOTS {: alts:^ :}
   alts VEC:COUNT 0 ?do
      plan alts i NTH-SLOT k node ONLY-LISTS? if true unloop exit then
   loop false ;

\ Whether the node, pinned on r's row, leaves r's entry o at slot j empty.
: EXCLUDES? ( ^plan ^source n ^key n -- bool )
   {: plan r:^ j:n o:^ node:n :}
   plan TREE node NODE-K N-IRI <> if false exit then
   plan node o LISTED-KEY? if false exit then
   plan r SOURCE-ORIGIN @ j o KEY-COL @ node ORIGIN-LISTS? ;

\ Whether the triple pins, at a slot of r keyed on o's column, an IRI
\ that leaves r's entry o at slot j empty.
: OWN-PIN? ( ^plan n ^source n ^key -- bool )
   {: plan triple:n r:^ j:n o:^ :}
   3 0 do
      r i SLOT@ o KEY-COL @ KEYED-ON? if
         plan r j o plan TREE triple i PART EXCLUDES?
         if true unloop exit then
      then
   loop false ;

\ Whether record ra reads r's row through a unique subject slot, with
\ slot m keyed on column k.
: ROW-SLOT? ( ^source ^source n str -- bool )
   {: ra:^ r:^ m:n k:str :}
   ra SOURCE-FROM @ r SOURCE-FROM @ STR:= 0= if false exit then
   ra SOURCE-S @ r SOURCE-S @ SAME-SLOT?
   ra SOURCE-S @ SLOT-UNIQUE? and
   ra m SLOT@ k KEYED-ON? and ;

\ Whether every live choice of the item reads r's row, slot m keyed on
\ column k.
: SHARES-ROW? ( ^plan ^item ^source n str -- bool )
   {: plan a:^ r:^ m:n k:str :}
   a ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if
         plan c CHOICE-SOURCE @ SOURCE@ r m k ROW-SLOT? 0= if
            false unloop exit
         then
      then
   loop true ;

: JOINED-TRIPLE? ( ^item -- bool )
   {: it:^ :} it ITEM-MODE @ 0= it ITEM-NODE @ 0<> and ;

\ The source of the item's first live choice, else -1.
: FIRST-LIVE ( ^item -- n )
   ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if c CHOICE-SOURCE @ unloop exit then
   loop -1 ;

: PUSH-PIN ( ^VEC:vector<pin> ^source n n -- )
   {: pins:^ ra:^ m:n iri:n :}
   pins VEC:PUSH {: p:^ :}
   ra p PIN-ROW !  m p PIN-SLOT !  iri p PIN-IRI ! ;

\ Whether a pin has the IRI and reads ra's row by a slot keyed on
\ column k.
: SAME-PIN? ( ^plan ^VEC:vector<pin> ^source str n -- bool )
   {: plan pins:^ ra:^ k:str iri:n :}
   pins VEC:COUNT 0 ?do
      pins i VEC:AT {: p:^ :}
      plan p PIN-IRI @ iri SAME-NODE?
      p PIN-ROW @ ra p PIN-SLOT @ k ROW-SLOT? and
      if true unloop exit then
   loop false ;

\ The subject node's `pinned`, none when it has no pins.
: FIND-PINNED ( ^plan ^VEC:vector<pinned> n -- nullable<pinned> )
   {: plan pinned:^ subj:n :}
   pinned VEC:COUNT 0 ?do
      pinned i VEC:AT {: g:^ :}
      plan g PINNED-SUBJECT @ subj SAME-NODE? if
         g NULLABLE:SOME unloop exit
      then
   loop NULLABLE:NONE ;

\ A new `pinned` of the subject node, no pins and every verdict 0.
: NEW-PINNED ( ^plan ^VEC:vector<pinned> n -- ^pinned )
   {: plan pinned:^ subj:n :}
   pinned VEC:PUSH {: g:^ :}
   subj g PINNED-SUBJECT !
   plan WORK-AL 0 [type] pin VEC:NEW g PINNED-PINS !
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 3 * {: n:n :}
   plan WORK-AL n [type] u8 VEC:NEW g PINNED-VERDICTS !
   g ;

\ The subject node's `pinned`, made when none.
: PINNED-FOR ( ^plan ^VEC:vector<pinned> n -- ^pinned )
   {: plan pinned:^ subj:n :}
   plan pinned subj FIND-PINNED MATCH nullable
      none OF plan pinned subj NEW-PINNED ENDOF
      some OF ENDOF
   ;MATCH ;

\ Pushes the pin into the subject's pins unless an equal pin, one that
\ kills exactly the choices this one would, is there.
: ADD-PIN ( ^plan ^VEC:vector<pinned> n ^source n str n -- )
   {: plan pinned:^ subj:n ra:^ m:n k:str iri:n :}
   plan pinned subj PINNED-FOR PINNED-PINS @ {: pins:^ :}
   plan pins ra k iri SAME-PIN? 0= if pins ra m iri PUSH-PIN then ;

\ Pins the IRI at slot m of triple a among its subject's pins when every
\ live choice of a reads ra's row, slot m keyed on its column there.
: +PIN ( ^plan ^VEC:vector<pinned> ^item ^source n -- )
   {: plan pinned:^ a:^ ra:^ m:n :}
   plan TREE {: state :}
   state a ITEM-NODE @ m PART {: iri:n :}
   state iri NODE-K N-IRI <> if exit then
   state a ITEM-NODE @ 0 PART {: subj:n :}
   ra m SLOT@ SLOT-KEY MATCH nullable
      none OF ENDOF
      some OF {: key:^ :}
         plan a ra m key KEY-COL @ SHARES-ROW? if
            plan pinned subj ra m key KEY-COL @ iri ADD-PIN
         then
      ENDOF
   ;MATCH ;

\ Pins the triple's IRIs through the record of its first live choice.
: +PINS ( ^plan ^VEC:vector<pinned> ^item -- )
   {: plan pinned:^ a:^ :}
   a FIRST-LIVE {: k:n :}
   k 0< if exit then
   plan k SOURCE@ {: ra:^ :}
   3 0 do plan pinned a ra i +PIN loop ;

\ The pins of the joined triples, one `pinned` per subject.
: PINS ( ^plan ^VEC:vector<item> -- ^VEC:vector<pinned> )
   {: plan items:^ :}
   plan WORK-AL 0 [type] pinned VEC:NEW {: pinned:^ :}
   items VEC:COUNT 0 ?do
      items i VEC:AT {: a:^ :}
      a JOINED-TRIPLE? if plan pinned a +PINS then
   loop pinned ;

\ Whether the pin puts in r's row an IRI that leaves r's entry o at
\ slot j empty.
: PIN-EXCLUDES? ( ^plan ^pin ^source n ^key -- bool )
   {: plan p:^ r:^ j:n o:^ :}
   p PIN-ROW @ r p PIN-SLOT @ o KEY-COL @ ROW-SLOT? 0= if
      false exit
   then
   plan r j o p PIN-IRI @ EXCLUDES? ;

\ Whether one of the pins leaves r's entry o at slot j empty.
: PINS-EXCLUDE? ( ^plan ^pinned ^source n ^key -- bool )
   {: plan g:^ r:^ j:n o:^ :}
   g PINNED-PINS @ {: pins:^ :}
   pins VEC:COUNT 0 ?do
      plan pins i VEC:AT r j o PIN-EXCLUDES?
      if true unloop exit then
   loop false ;

\ Whether the subject's pins leave record k's entry o at slot j empty,
\ decided at the first call and kept.
: VERDICT? ( ^plan ^pinned n n ^key -- bool )
   {: plan g:^ k:n j:n o:^ :}
   g PINNED-VERDICTS @ k 3 * j + VEC:AT {: v:^ :}
   v c@ 0= if
      plan g plan k SOURCE@ j o PINS-EXCLUDE? if 2 else 1 then v c!
   then
   v c@ 2 = ;

\ Whether item b's own constant, or one of its subject's pins g,
\ leaves record k's entry o at slot j empty.
: PINNED-OUT? ( ^plan nullable<pinned> ^item n n ^key -- bool )
   {: plan g b:^ k:n j:n o:^ :}
   plan b ITEM-NODE @ plan k SOURCE@ j o OWN-PIN? if true exit then
   g MATCH nullable
      none OF false ENDOF
      some OF {: p:^ :} plan p k j o VERDICT? ENDOF
   ;MATCH ;

\ Whether item b's own constants or its subject's pins g
\ leave an `only` entry of record k, b's, empty.
: UNFILLED? ( ^plan nullable<pinned> ^item n -- bool )
   {: plan g b:^ k:n :}
   plan k SOURCE@ {: r:^ :}
   3 0 do
      i {: j:n :}
      r j SLOT@ SLOT-ONLY MATCH nullable
         none OF false ENDOF
         some OF {: o:^ :} plan g b k j o PINNED-OUT? ENDOF
      ;MATCH
      if true unloop exit then
   loop false ;

\ Kills each live choice of b its own constants or the pins of its
\ subject leave empty.
: DROP-CHOICES ( ^plan ^VEC:vector<pinned> ^item -- )
   {: plan pinned:^ b:^ :}
   b FIRST-LIVE 0< if exit then
   plan TREE b ITEM-NODE @ 0 PART {: subj:n :}
   plan pinned subj FIND-PINNED {: g :}
   b ITEM-CHOICES @ {: cs:^ :}
   cs VEC:COUNT 0 ?do
      cs i VEC:AT {: c:^ :}
      c CHOICE-ALIVE @ if
         plan g b c CHOICE-SOURCE @ UNFILLED?
         if false c CHOICE-ALIVE ! then
      then
   loop ;

\ Kills the choices an `only` leaves empty, by pins taken before any
\ kill; the pins live in the work arena until it returns.
: DROP-UNFILLED ( ^plan ^VEC:vector<item> -- )
   {: plan items:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan items PINS {: pinned:^ :}
   items VEC:COUNT 0 ?do
      items i VEC:AT {: b:^ :}
      b JOINED-TRIPLE? if plan pinned b DROP-CHOICES then
   loop work mark ARENA:CUT ;

\ The group's rows, made at the first kept.
: ROWS ( ^plan ^relation -- ^VEC:vector<subject-row> )
   {: plan group:^ :}
   group RELATION-ROWS @ dup NULL-PTR = if
      drop plan AL 0 [type] subject-row VEC:NEW
      dup group RELATION-ROWS !
   then ;

\ Keeps the row the records read under the alias for the subject node
\ when their subject slot is unique.
: KEEP-ROW ( ^plan n ^VEC:vector<n> str ^relation -- )
   {: plan node:n recs:^ alias:str group:^ :}
   plan recs UNIQUE-SUBJECT? 0= if exit then
   plan recs 0 VEC:AT @ SOURCE@ {: src:^ :}
   plan group ROWS VEC:PUSH {: row:^ :}
   node row SUBJECT-ROW-NODE !
   src SOURCE-FROM @ row SUBJECT-ROW-FROM !
   src SOURCE-S @ row SUBJECT-ROW-SLOT !
   alias row SUBJECT-ROW-ALIAS ! ;

\ A pattern whose live choices are records of one description source, and
\ its occurrences: it reads its subject's row in the group, else joins it.
: SINGLE-SQL ( ^plan n ^VEC:vector<n> ^relation -- ^VEC:vector<binding> )
   {: plan triple:n recs:^ group:^ :}
   group RELATION-WHERE @ {: where:^ :}
   plan TREE triple 0 PART {: node:n :}
   plan node recs group FIND-ROW {: k:n :}
   k -1 > if
      plan plan triple 1 OCCURRENCES NEW-BINDINGS {: occs:^ :}
      group RELATION-ROWS @ k VEC:AT SUBJECT-ROW-ALIAS @ {: kept:str :}
      plan triple recs kept occs where 1 SLOTS-SQL occs exit
   then
   plan plan triple 0 OCCURRENCES NEW-BINDINGS {: occs:^ :}
   plan NEXT-ALIAS {: alias:str :}
   group RELATION-FROM @ plan recs 0 VEC:AT @ SOURCE@ alias +FROM
   plan node recs alias group KEEP-ROW
   plan triple recs alias occs where 0 SLOTS-SQL occs ;

\ One branch of a union: the triple over the records, of one description
\ source, as r, a relation of its own.
: SOURCE-BRANCH ( ^plan n ^VEC:vector<n> ^relation -- )
   {: plan triple:n recs:^ r:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan NEW-SQL {: from:^ :} plan NEW-SQL {: where:^ :}
   from plan recs 0 VEC:AT @ SOURCE@ s" u" +FROM
   plan WORK-AL plan triple 0 OCCURRENCES BINDINGS-IN {: occs:^ :}
   plan triple recs s" u" occs where 0 SLOTS-SQL
   from where plan occs where JOINS r INIT-RELATION
   work mark ARENA:CUT ;

\ Record k alone.
: RECORD ( ^plan n -- ^VEC:vector<n> )
   {: plan k:n :}
   plan INDEXES k +INDEX ;

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

\ The bytes a column of the binding copies: its value and the copies
\ behind it.
: COPIED ( ^binding -- n ) BINDING-SQL @ dup SQL-LEN swap COPIES + ;

\ The most a column of a branch's binding of the variable copies.
: MOST-OF ( ^VEC:vector<relation> n -- n )
   {: branches:^ var:n :}
   0 branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k -1 > if bs k VEC:AT COPIED max then
   loop ;

\ The union's columns: each variable a branch binds, in its form, its
\ leaf's copies the most a branch's binding of it copies.
: COLUMNS ( ^plan ^VEC:vector<relation> -- ^VEC:vector<binding> )
   {: plan branches:^ :}
   plan 0 NEW-BINDINGS {: cols:^ :}
   branches VEC:COUNT 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs VEC:COUNT 0 ?do
         bs i VEC:AT BINDING-VAR @ {: var:n :}
         cols var FIND-VAR 0< if
            cols var plan branches var FORM plan NEW-SQL s" " BIND
            branches var EVERYWHERE? cols LAST BINDING-ALWAYS !
            branches var MOST-OF cols LAST BINDING-SQL @ REL-COPIES !
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
         r RELATION-WHERE @ b +NULL-TEST
         false b BINDING-NULLABLE !
      then
   loop ;

\ Leaves r the bindings of the listed variables it binds always, its
\ null tests written first, since it drops bindings.
: KEEP-READ ( ^plan ^VEC:vector<n> ^relation -- )
   {: plan vars:^ r:^ :}
   r +NULL-TESTS
   r RELATION-BINDINGS @ {: bs:^ :}
   plan 0 NEW-BINDINGS {: cols:^ :}
   vars LEN 0 ?do
      bs vars i VEC:AT @ FIND-VAR {: k:n :}
      k -1 > if
         bs k VEC:AT {: b:^ :}
         b BINDING-ALWAYS @ if b cols COPY-BINDING then
      then
   loop cols r RELATION-BINDINGS ! ;

\ s takes every field of o.
: MOVE-BINDING ( ^binding ^binding -- )
   {: s:^ o:^ :}
   o BINDING-VAR @ s BINDING-VAR ! s o TAKE
   o BINDING-ALWAYS @ s BINDING-ALWAYS !
   o BINDING-NULLABLE @ s BINDING-NULLABLE ! ;

\ r drops its binding of the variable, writing its null test first when
\ one is pending; the later bindings move down.
: FORGET ( ^relation n -- )
   {: r:^ var:n :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs var FIND-VAR {: k:n :}
   bs k VEC:AT {: b:^ :}
   b BINDING-NULLABLE @ if r RELATION-WHERE @ b +NULL-TEST then
   bs VEC:COUNT 1- k ?do bs i VEC:AT bs i 1+ VEC:AT MOVE-BINDING loop
   bs bs VEC:COUNT 1- VEC:RESIZE ;

\ The variables EXPAND made that both the scope and occs bind: occs is
\ the second of the two items that mention each, so nothing reads it later.
: SETTLED ( ^plan ^VEC:vector<binding> ^VEC:vector<binding>
      -- ^VEC:vector<n> )
   {: plan scope:^ occs:^ :}
   plan PLAN-FRESH @ {: fresh:^ :}
   INDEXES occs VEC:COUNT 0 ?do
      occs i VEC:AT BINDING-VAR @ {: var:n :}
      fresh var INDEXED? if
         scope var FIND-VAR -1 > if plan swap var +INDEX then
      then
   loop ;

: FORGET-ALL ( ^relation ^VEC:vector<n> -- )
   {: r:^ vars:^ :}
   vars LEN 0 ?do r vars i VEC:AT @ FORGET loop ;

\ Appends ` FROM ` and ` WHERE ` with r's FROM and WHERE as they are
\ now: a view of each, since either may grow.
: +FROM-WHERE ( ^rel ^relation -- )
   {: out:^ r:^ :}
   r +NULL-TESTS
   r RELATION-FROM @ {: from:^ :}
   r RELATION-WHERE @ {: where:^ :}
   from EMPTY? 0= if out s"  FROM " +TEXT out from VIEW +REL then
   where EMPTY? if exit then
   out s"  WHERE " +TEXT out where VIEW +REL ;

\ Appends the binding's value in form shape: native as it is, else a term.
: +VALUE ( ^plan ^rel ^binding n -- )
   {: plan v:^ b:^ shape:n :}
   shape 0< if plan v b +TERM else v b +BINDING then ;

\ Appends ` x AS vK`, the binding as a column of form shape.
: +COLUMN ( ^plan ^rel ^binding n -- )
   {: plan v:^ b:^ shape:n :}
   v s"  " +SQL plan v b shape +VALUE
   v s"  AS " +SQL v b BINDING-VAR @ +NAME ;

\ Appends NULL typed as form shape.
: +NULL ( ^plan ^rel n -- )
   {: plan v:^ shape:n :}
   shape 0< if v s" NULL::jsonb" +SQL
   else v plan shape SHAPE@ SHAPE-IN @ s" NULL::jsonb" +CALL then ;

\ Appends `SELECT columns`, the relation's bindings.
: +SELECT-LIST ( ^plan ^rel ^relation -- )
   {: plan out:^ r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   out s" SELECT" +SQL
   bs VEC:COUNT 0 ?do
      i 0<> if out s" ," +SQL then
      bs i VEC:AT {: b:^ :}
      plan out b b BINDING-SHAPE @ +COLUMN
   loop ;

\ Appends `SELECT columns FROM ... WHERE ...`, the relation's solutions.
: +SELECT ( ^plan ^rel ^relation -- )
   {: plan out:^ r:^ :} plan out r +SELECT-LIST out r +FROM-WHERE ;

\ The same with x, their row number, as a last column rn. No variable's
\ column takes the name rn.
: +NUMBERED ( ^plan ^rel ^relation str -- )
   {: plan out:^ r:^ x:str :}
   plan out r +SELECT-LIST
   r RELATION-BINDINGS @ VEC:COUNT 0<> if out s" ," +SQL then
   out s"  " +SQL out x +SQL out s"  AS rn" +SQL out r +FROM-WHERE ;

\ Binds var to what the union's column of form shape holds in branch b:
\ b's value in that form, NULL where b binds none.
: +PRESENTED ( ^plan ^VEC:vector<binding> ^relation n n -- )
   {: plan list:^ b:^ var:n shape:n :}
   b RELATION-BINDINGS @ {: bs:^ :}
   bs var FIND-VAR {: k:n :}
   plan NEW-SQL {: v:^ :}
   k 0< if plan v shape +NULL else plan v bs k VEC:AT shape +VALUE then
   list var shape v s" " BIND
   k 0< if false list LAST BINDING-ALWAYS ! exit then
   bs k VEC:AT BINDING-ALWAYS @ list LAST BINDING-ALWAYS ! ;

\ Branch b as the union of columns cols presents it, u in the work
\ arena. u shares b's WHERE, so b's null tests are written first.
: PRESENTED ( ^plan ^relation ^VEC:vector<binding> -- ^relation )
   {: plan b:^ cols:^ :}
   b +NULL-TESTS
   plan WORK-AL {: al:^ :}
   al [type] relation ALLOC:NEW {: u:^ :}
   b RELATION-FROM @ b RELATION-WHERE @ al cols VEC:COUNT BINDINGS-IN
   u INIT-RELATION
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: c:^ :}
      plan u RELATION-BINDINGS @ b c BINDING-VAR @ c BINDING-SHAPE @
         +PRESENTED
   loop u ;

: +BRANCH ( ^plan ^rel ^relation ^VEC:vector<binding> -- )
   {: plan out:^ r:^ cols:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan out plan r cols PRESENTED +SELECT
   work mark ARENA:CUT ;

\ Appends the branches as the union of columns cols presents them.
: +UNION-ALL ( ^plan ^rel ^VEC:vector<relation> ^VEC:vector<binding>
      -- )
   {: plan out:^ branches:^ cols:^ :}
   branches VEC:COUNT 0 ?do
      i 0<> if out s"  UNION ALL " +TEXT then
      plan out branches i VEC:AT cols +BRANCH
   loop ;

\ The branches as one UNION ALL derived table, and its columns.
: UNION-TABLE ( ^plan ^VEC:vector<relation>
      -- ^rel ^VEC:vector<binding> )
   {: plan branches:^ :}
   plan branches COLUMNS {: cols:^ :}
   plan NEW-SQL {: out:^ :}
   out s" (" +TEXT plan out branches cols +UNION-ALL
   out s" )" +TEXT out cols ;

\ The relation as a derived table, and its columns.
: SUBSELECT ( ^plan ^relation -- ^rel ^VEC:vector<binding> )
   {: plan r:^ :}
   plan NEW-SQL {: out:^ :}
   out s" (" +TEXT plan out r +SELECT out s" )" +TEXT
   out r RELATION-BINDINGS @ ;

\ Appends the derived table under a new alias to from; answers the alias.
: +AS ( ^plan ^rel ^rel -- str )
   {: plan t:^ from:^ :}
   plan NEXT-ALIAS {: alias:str :}
   from t +REL from s"  AS " +TEXT from alias +TEXT alias ;

\ b binds column c of the derived table under alias, each leaf's copies
\ what c copies; b may be c.
: SET-COLUMN ( ^plan ^binding str ^binding -- )
   {: plan b:^ alias:str c:^ :}
   c BINDING-VAR @ {: var:n :}
   c BINDING-ALWAYS @ {: always:bool :} c COPIED {: copied:n :}
   b var c BINDING-SHAPE @ plan alias var COLUMN-OF
      c BINDING-TERM @ SET-BINDING
   always b BINDING-ALWAYS !
   copied b BINDING-SQL @ REL-COPIES ! ;

\ Appends the derived table under a new alias to from and answers its
\ columns as bindings there, and the alias.
: +TABLE-ALIAS ( ^plan ^rel ^VEC:vector<binding> ^rel
      -- ^VEC:vector<binding> str )
   {: plan t:^ cols:^ from:^ :}
   plan t from +AS {: alias:str :}
   plan cols VEC:COUNT NEW-BINDINGS {: occs:^ :}
   cols VEC:COUNT 0 ?do
      plan occs VEC:PUSH alias cols i VEC:AT SET-COLUMN
   loop occs alias ;

: +TABLE ( ^plan ^rel ^VEC:vector<binding> ^rel
      -- ^VEC:vector<binding> )
   +TABLE-ALIAS drop ;

\ b's variable may be missing on a row: no WHERE condition tests it.
: MAY-MISS ( ^binding -- )
   {: b:^ :}
   false b BINDING-ALWAYS ! false b BINDING-NULLABLE ! ;

\ The list drops its bindings of the variables the scope binds; the
\ others keep their order.
: DROP-MET ( ^VEC:vector<binding> ^VEC:vector<binding> -- )
   {: scope:^ list:^ :}
   0 list VEC:COUNT 0 ?do
      list i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR 0< if
         list over VEC:AT o MOVE-BINDING 1+
      then
   loop list swap VEC:RESIZE ;

\ The list keeps only its bindings of the variables the scope binds, in
\ their order.
: KEEP-MET ( ^VEC:vector<binding> ^VEC:vector<binding> -- )
   {: scope:^ list:^ :}
   0 list VEC:COUNT 0 ?do
      list i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR -1 > if
         list over VEC:AT o MOVE-BINDING 1+
      then
   loop list swap VEC:RESIZE ;

\ The list's bindings move up behind copies of the scope's.
: SCOPE-FIRST ( ^VEC:vector<binding> ^VEC:vector<binding> -- )
   {: scope:^ list:^ :}
   scope VEC:COUNT {: s:n :} list VEC:COUNT {: m:n :}
   list s m + VEC:RESIZE
   m 0 ?do
      m 1- i - {: k:n :}
      list s k + VEC:AT list k VEC:AT MOVE-BINDING
   loop
   s 0 ?do list i VEC:AT scope i VEC:AT MOVE-BINDING loop ;

\ Meets the optional list into r's bindings as MEET-ALL does; a list longer
\ than the scope is consumed: r adopts it, the scope's bindings first.
: ADOPT-ALL ( ^plan ^relation ^VEC:vector<binding> ^rel -- )
   {: plan r:^ list:^ where:^ :}
   r RELATION-BINDINGS @ {: scope:^ :}
   list VEC:COUNT scope VEC:COUNT <= if
      plan scope list where true MEET-ALL exit
   then
   list VEC:COUNT 0 ?do
      list i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR -1 > if
         plan scope o where true MEET drop
      else o MAY-MISS then
   loop
   scope list DROP-MET scope list SCOPE-FIRST
   list r RELATION-BINDINGS ! ;

\ Appends the OPTIONAL's derived table under a new alias to r's FROM; its
\ columns, rewritten in place, meet r's bindings in on and r may adopt them.
: ADOPT-TABLE ( ^plan ^rel ^VEC:vector<binding> ^relation ^rel -- )
   {: plan t:^ cols:^ r:^ on:^ :}
   plan t r RELATION-FROM @ +AS {: alias:str :}
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: c:^ :} plan c alias c SET-COLUMN
   loop plan r cols on ADOPT-ALL ;

\ Joins the derived table into r and drops the variables it SETTLED. Each
\ column meets through the plan's column binding: the item's stay for LATE.
: JOIN-TABLE ( ^plan ^rel ^VEC:vector<binding> ^relation -- )
   {: plan t:^ cols:^ r:^ :}
   r RELATION-BINDINGS @ {: scope:^ :}
   plan scope cols SETTLED {: done:^ :}
   plan t r RELATION-FROM @ +AS {: alias:str :}
   plan PLAN-COLUMN @ {: b:^ :}
   cols VEC:COUNT 0 ?do
      plan b alias cols i VEC:AT SET-COLUMN
      plan scope b r RELATION-WHERE @ false MEET drop
   loop r done FORGET-ALL ;

\ The derived table's rows whose columns meet the substitution.
: SUBSTITUTED ( ^plan ^rel ^VEC:vector<binding> -- ^rel )
   {: plan t:^ cols:^ :}
   plan PLAN-OUTER @ BINDS-ANY? 0= if t exit then
   plan NEW-SQL {: from:^ :} plan NEW-SQL {: where:^ :}
   plan t cols from +TABLE {: occs:^ :}
   occs VEC:COUNT 0 ?do plan occs i VEC:AT where +OUTER loop
   where EMPTY? if t exit then
   plan NEW-SQL {: out:^ :}
   out s" (SELECT * FROM " +TEXT out from +REL
   out s"  WHERE " +TEXT out where +REL
   out s" )" +TEXT out ;

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

\ Appends `b = row.k` to r's WHERE, through b's shape's `in` for a term
\ key; that strict `=` drops the NULL of b's column.
: +MEET-KEY ( ^plan ^binding ^binding str ^relation -- )
   {: plan key:^ b:^ row:str r:^ :}
   r RELATION-WHERE @ {: where:^ :}
   plan NEW-SQL {: k:^ :} k row +SQL k s" .k" +SQL
   where s"  AND " +SEP where b +BINDING where s"  = " +SQL
   key BINDING-SHAPE @ GENERIC <> if where k SQL$ +SQL else
      where plan b BINDING-SHAPE @ SHAPE@ SHAPE-IN @ k SQL$ +CALL
   then plan r RELATION-BINDINGS @ b BINDING-SQL @ COLUMN-TESTED ;

\ Meets the key of row in each branch that PROBED it; answers the
\ indexes of those branches, then of the others.
: +KEYS ( ^plan ^binding str ^VEC:vector<relation>
      -- ^VEC:vector<n> ^VEC:vector<n> )
   {: plan key:^ row:str branches:^ :}
   INDEXES INDEXES branches VEC:COUNT 0 ?do
      {: probed:^ rest:^ :}
      branches i VEC:AT {: r:^ :}
      plan key r PROBED {: k:n :}
      k 0< if probed plan rest i +INDEX else
         plan key r RELATION-BINDINGS @ k VEC:AT row r +MEET-KEY
         plan probed i +INDEX rest
      then
   loop ;

\ Appends the listed branches as the union of columns cols presents
\ them.
: +UNION ( ^plan ^rel ^VEC:vector<relation> ^VEC:vector<n>
      ^VEC:vector<binding> -- )
   {: plan out:^ branches:^ list:^ cols:^ :}
   list LEN 0 ?do
      i 0<> if out s"  UNION ALL " +TEXT then
      plan out branches list i VEC:AT @ VEC:AT cols +BRANCH
   loop ;

\ `tK AS row CROSS JOIN LATERAL (<the listed branches>)`: the branches
\ read once per key of the key's CTE tK.
: PROBE-SQL ( ^plan ^binding str ^VEC:vector<relation> ^VEC:vector<n>
      ^VEC:vector<binding> -- ^rel )
   {: plan key:^ row:str branches:^ list:^ cols:^ :}
   plan NEW-SQL {: v:^ :}
   v key +BINDING v s"  AS " +SQL v row +SQL
   v s"  CROSS JOIN LATERAL (" +SQL
   plan v branches list cols +UNION v s" )" +TEXT v ;

\ `(SELECT x.* FROM probe AS x UNION ALL <the listed branches>)`.
: BESIDE ( ^plan ^rel ^VEC:vector<relation> ^VEC:vector<n>
      ^VEC:vector<binding> -- ^rel )
   {: plan probe:^ branches:^ list:^ cols:^ :}
   plan NEXT-ALIAS {: x:str :}
   plan NEW-SQL {: out:^ :}
   out s" (SELECT " +TEXT out x +TEXT out s" .* FROM " +TEXT
   out probe +REL out s"  AS " +TEXT out x +TEXT
   out s"  UNION ALL " +TEXT
   plan out branches list cols +UNION out s" )" +TEXT out ;

\ The branches driven by the key as a FROM rel whose last item takes the
\ alias +AS appends, and its columns.
: DRIVEN-TABLE ( ^plan ^binding ^VEC:vector<relation>
      -- ^rel ^VEC:vector<binding> )
   {: plan key:^ branches:^ :}
   plan branches COLUMNS {: cols:^ :}
   plan NEXT-ALIAS {: row:str :}
   plan key row branches +KEYS {: probed:^ rest:^ :}
   plan key row branches probed cols PROBE-SQL {: t:^ :}
   rest LEN 0= if t cols exit then
   plan t branches rest cols BESIDE cols ;

\ Branch b over the records of description source origin; what it reads
\ in the work arena is cut back once b is built.
: ORIGIN-BRANCH ( ^plan ^item n ^relation -- )
   {: plan it:^ origin:n b:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan it origin LIVE-OF {: recs:^ :}
   plan it ITEM-NODE @ recs b SOURCE-BRANCH
   work mark ARENA:CUT ;

\ Sets the item's branches, one per description source it reads. Each
\ is a scan PostgreSQL plans: one read, the first being the item's own.
: BRANCHES ( ^plan ^item -- )
   {: plan it:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan it ORIGINS {: origins:^ :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   branches origins LEN VEC:RESERVE
   origins LEN 0 ?do
      i 0<> if plan +READ then
      plan it origins i VEC:AT @ branches VEC:PUSH ORIGIN-BRANCH
   loop branches it ITEM-BRANCHES !
   work mark ARENA:CUT ;

\ Joins a pattern of several description sources into the group: a UNION
\ ALL of a branch per source, driven by the first natively bound key.
: UNION-SQL ( ^plan ^item ^VEC:vector<binding> ^relation -- )
   {: plan it:^ keys:^ group:^ :}
   it ITEM-BRANCHES @ {: branches:^ :}
   group RELATION-FROM @ {: from:^ :}
   from s"  CROSS JOIN " +SEP
   plan keys branches DRIVER {: d:n :}
   d 0< if plan plan branches UNION-TABLE group JOIN-TABLE exit then
   plan plan keys d VEC:AT branches DRIVEN-TABLE group JOIN-TABLE ;

\ Copies into seen the substitution's binding of o's variable, unless
\ seen or no layer binds it.
: INHERIT ( ^VEC:vector<binding> ^layer ^binding -- )
   {: seen:^ outer:^ o:^ :}
   seen o BINDING-VAR @ FIND-VAR -1 > if exit then
   outer o BINDING-VAR @ LOOKUP {: b:^ :}
   b NULL-PTR = if exit then
   b seen COPY-BINDING ;

\ A layer of the scope's bindings over the substitution, each met with its
\ binding there; MEET's conditions are dropped: each slot meets it.
: SEEN ( ^plan ^VEC:vector<binding> -- ^layer )
   {: plan scope:^ :}
   plan PLAN-OUTER @ {: outer:^ :}
   outer BINDS-ANY? 0= if plan scope outer NEW-LAYER exit then
   plan scope VEC:COUNT NEW-BINDINGS {: seen:^ :}
   plan NEW-SQL {: where:^ :}
   scope VEC:COUNT 0 ?do
      scope i VEC:AT {: o:^ :}
      seen outer o INHERIT plan seen o where false MEET drop
   loop plan seen outer NEW-LAYER ;

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
   plan TREE {: state :}
   state node CONSTANT? 0= if false exit then
   state node NODE-K N-IRI = if true exit then
   state node NODE-B@ 0<> if true exit then
   state node NODE-DATATYPE$ s" http://www.w3.org/2001/XMLSchema#string" STR:= ;

\ Whether the list of expressions is not empty and each is UNTYPED?.
: ALL-UNTYPED? ( ^plan n -- bool )
   {: plan list:n :}
   list 0= if false exit then
   list begin dup 0<> while
      {: each:n :}
      plan plan TREE each NODE-A@ UNTYPED? 0= if false exit then
      plan TREE each NODE-B@
   repeat drop true ;

\ Whether the datatypes are iri and other alone.
: ONLY$? ( ^VEC:vector<datatype> str str -- bool )
   {: list:^ iri:str other:str :}
   list VEC:COUNT 0 ?do
      list i VEC:AT DATATYPE-IRI @ {: d:str :}
      d iri STR:= d other STR:= or 0= if false unloop exit then
   loop list iri LISTED$? list other LISTED$? and ;

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

\ The expression's binding, a variable's in the plan's scope, when
\ native; else NULL-PTR.
: NATIVE-VAR ( ^plan n -- ^binding )
   {: plan e:n :}
   plan TREE e NODE-K N-VAR <> if NULL-PTR exit then
   plan PLAN-SCOPE @ plan e VAR LOOKUP {: b:^ :}
   b NULL-PTR = if b exit then
   b BINDING-SHAPE @ 0< if NULL-PTR else b then ;

\ The classes of a comparison's operands that compare natively.
1 constant NUMERIC
2 constant TEXTUAL

\ The literal's class: NUMERIC for xsd:integer or xsd:decimal, TEXTUAL
\ for a string with no language tag, else 0.
: LITERAL-CLASS ( ^plan n -- n )
   {: plan e:n :}
   plan TREE e NODE-K N-LITERAL <> if 0 exit then
   plan e NODE-KNOWN KNOWN-TERM-DATATYPE @ {: d:str :}
   d s" http://www.w3.org/2001/XMLSchema#integer" STR:=
   d s" http://www.w3.org/2001/XMLSchema#decimal" STR:= or
   if NUMERIC exit then
   d s" http://www.w3.org/2001/XMLSchema#string" STR:=
   if TEXTUAL else 0 then ;

\ Operand e's class: a natively bound variable's shape's, else a
\ literal's.
: OPERAND-CLASS ( ^plan n -- n )
   {: plan e:n :}
   plan e NATIVE-VAR {: b:^ :}
   b NULL-PTR = if plan e LITERAL-CLASS exit then
   b BINDING-SHAPE @ {: y:n :}
   plan y NUMBERS? if NUMERIC exit then
   plan y STRINGS? if TEXTUAL else 0 then ;

\ Folds operand e into class c of the operands before it, -1 before the
\ first: 0 once two differ or one has none.
: JOIN-CLASS ( ^plan n n -- n )
   {: plan c:n e:n :}
   plan e OPERAND-CLASS {: x:n :}
   c 0< if x exit then
   c x = if c else 0 then ;

\ The binding of the comparison's first natively bound operand, else
\ NULL-PTR.
: PAIR-NATIVE ( ^plan n -- ^binding )
   {: plan e:n :} plan TREE {: state :}
   plan state e NODE-A@ NATIVE-VAR dup NULL-PTR <> if exit then drop
   plan state e NODE-B@ NATIVE-VAR ;

\ The class the comparison's operands share, else 0; 0 also when
\ neither is a natively bound variable.
: PAIR-CLASS ( ^plan n -- n )
   {: plan e:n :} plan TREE {: state :}
   plan e PAIR-NATIVE NULL-PTR = if 0 exit then
   plan plan -1 state e NODE-A@ JOIN-CLASS state e NODE-B@ JOIN-CLASS ;

\ The binding of the first natively bound operand of `a IN (list)`,
\ else NULL-PTR.
: IN-NATIVE ( ^plan n -- ^binding )
   {: plan e:n :} plan TREE {: state :}
   plan state e NODE-A@ NATIVE-VAR dup NULL-PTR <> if exit then drop
   state e NODE-B@ begin dup 0<> while
      {: each:n :}
      plan state each NODE-A@ NATIVE-VAR dup NULL-PTR <> if exit then
      drop state each NODE-B@
   repeat drop NULL-PTR ;

\ The class `a IN (list)`'s operands share, else 0; 0 also for an empty
\ list, which SQL's IN cannot write, and when no operand is a natively
\ bound variable.
: IN-CLASS ( ^plan n -- n )
   {: plan e:n :} plan TREE {: state :}
   state e NODE-B@ 0= if 0 exit then
   plan e IN-NATIVE NULL-PTR = if 0 exit then
   plan -1 state e NODE-A@ JOIN-CLASS
   state e NODE-B@ begin dup 0<> while
      {: c:n each:n :}
      plan c state each NODE-A@ JOIN-CLASS state each NODE-B@
   repeat drop ;

\ Appends operand e of class c: a variable's column, a number through
\ sparql.numeric (`in` is NULL for a term k's shape lacks), else k's `in`.
: +NATIVE ( ^plan ^rel n ^binding n -- )
   {: plan v:^ c:n k:^ e:n :}
   plan e NATIVE-VAR {: b:^ :}
   b NULL-PTR <> if v b +BINDING exit then
   c NUMERIC = if plan v s" sparql.numeric" e +CALL-TERM
   else
      plan v plan k BINDING-SHAPE @ SHAPE@ SHAPE-IN @ e +CALL-TERM
   then ;

\ The condition that native binding k is constant c.
: FIXED-SQL ( ^plan ^binding n -- ^rel )
   {: plan k:^ c:n :}
   plan k plan c CONSTANT-VALUE SAME-SQL ;

\ Appends that variable e is constant c, when they are such and e native;
\ its strict `=` drops the NULL of e's column in clear's bindings.
: +FIXED ( ^plan n n ^rel ^VEC:vector<binding> -- )
   {: plan e:n c:n where:^ clear:^ :}
   plan TREE c CONSTANT? 0= if exit then
   plan e NATIVE-VAR {: k:^ :}
   k NULL-PTR = if exit then
   plan k c FIXED-SQL {: eq:^ :}
   where eq +AND
   clear NULL-PTR <> eq EMPTY? 0= and if
      plan clear k BINDING-SQL @ COLUMN-TESTED
   then ;

: +FIXED-EITHER ( ^plan n n ^rel ^VEC:vector<binding> -- )
   {: plan a:n b:n where:^ clear:^ :}
   plan a b where clear +FIXED plan b a where clear +FIXED ;

: +EQUAL-FIXED ( ^plan n ^rel ^VEC:vector<binding> -- )
   {: plan e:n where:^ clear:^ :}
   plan e PAIR-CLASS 0<> if exit then
   plan TREE e NODE-A@ {: a:n :} plan TREE e NODE-B@ {: b:n :}
   plan a UNTYPED? plan b UNTYPED? or 0= if exit then
   plan a b where clear +FIXED-EITHER ;

: +SAME-FIXED ( ^plan n ^rel ^VEC:vector<binding> -- )
   {: plan call:n where:^ clear:^ :}
   plan TREE {: state :}
   state call NODE-A@ {: list:n :}
   plan state list NODE-A@ state state list NODE-B@ NODE-A@
      where clear +FIXED-EITHER ;

: +IN-FIXED ( ^plan n ^rel ^VEC:vector<binding> -- )
   {: plan e:n where:^ clear:^ :}
   plan e IN-CLASS 0<> if exit then
   plan TREE {: state :}
   state e NODE-B@ {: list:n :}
   plan list ALL-UNTYPED? 0= if exit then
   plan state e NODE-A@ NATIVE-VAR {: k:^ :}
   k NULL-PTR = if exit then
   where s"  AND " +SEP where s" (" +SQL
   list begin dup 0<> while
      {: each:n :}
      each list <> if where s"  OR " +SQL then
      where plan k state each NODE-A@ FIXED-SQL +REL
      state each NODE-B@
   repeat drop where s" )" +SQL
   clear NULL-PTR <> if plan clear k BINDING-SQL @ COLUMN-TESTED then ;

\ Appends the native equalities the test's conjuncts imply, clearing as
\ +FIXED does.
: +IMPLIED ( ^plan n ^rel ^VEC:vector<binding> -- )
   {: plan e:n where:^ clear:^ :}
   plan TREE {: state :}
   state e s" &&" INFIX? if
      plan state e NODE-A@ where clear RECURSE
      plan state e NODE-B@ where clear RECURSE exit
   then
   state e s" =" INFIX? if plan e where clear +EQUAL-FIXED exit then
   state e s" IN" INFIX? if plan e where clear +IN-FIXED exit then
   state e NODE-K N-CALL <> if exit then
   state e s" SAMETERM" CALL-IS? if plan e where clear +SAME-FIXED then ;

\ Appends the expression's test, then the equalities it implies; own:
\ where is the scope's WHERE, so they clear the scope's bindings.
: +TEST ( ^plan ^VEC:vector<binding> n ^rel bool -- )
   {: plan scope:^ e:n where:^ own:bool :}
   plan PLAN-SCOPE @ {: outer:^ :}
   plan PLAN-OUT @ {: out:^ :}
   own if scope else NULL-PTR then {: clear:^ :}
   plan scope SEEN plan PLAN-SCOPE ! where plan PLAN-OUT !
   where s"  AND " +SEP
   plan e SQL-TEST plan e where clear +IMPLIED
   outer plan PLAN-SCOPE ! out plan PLAN-OUT ! ;

: +FILTER ( ^plan ^VEC:vector<binding> n ^rel bool -- )
   {: plan scope:^ filter:n where:^ own:bool :}
   plan scope plan TREE filter NODE-A@ where own +TEST ;

\ The group with its FILTERs on all its bindings; a FILTER leaves its
\ solutions no union's.
: CLOSED ( ^plan n -- ^relation )
   {: plan group:n :}
   plan group GROUP-SQL {: r:^ filters:^ :}
   filters VEC:COUNT 0<> if NULL-PTR r RELATION-BRANCHES ! then
   filters VEC:COUNT 0 ?do
      plan r RELATION-BINDINGS @ filters i VEC:AT @ r RELATION-WHERE @
         true +FILTER
   loop r ;

\ A nested group: its relation as a derived table.
: GROUP-ITEM ( ^plan ^VEC:vector<item> ^relation -- )
   {: plan items:^ r:^ :}
   r RELATION-EMPTY @ if
      plan items plan NEW-SQL r RELATION-BINDINGS @ true CHILD-ITEM
      exit
   then plan items plan r SUBSELECT false CHILD-ITEM ;

\ Adds a relation that is not statically empty to the branches.
: +RELATION ( ^plan ^VEC:vector<relation> ^relation -- )
   {: plan branches:^ r:^ :}
   branches VEC:PUSH {: c:^ :}
   r RELATION-FROM @ r RELATION-WHERE @ r RELATION-BINDINGS @
   c INIT-RELATION
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
      plan items plan NEW-SQL plan 0 NEW-BINDINGS true CHILD-ITEM exit
   then plan items plan branches UNION-TABLE false CHILD-ITEM
   branches items dup VEC:COUNT 1- VEC:AT ITEM-BRANCHES ! ;

\ A chain of UNIONs: one UNION ALL derived table of its groups.
: UNION-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan TREE {: state :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   state node N-UNION N-UNION SPINE {: spine:^ leaf:n :}
   plan branches leaf +GROUP
   spine begin dup NULL-PTR <> while
      {: at:^ :}
      plan branches state at LINK-NODE @ NODE-B@ +GROUP
      at LINK-NEXT @
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
      plan 0 NEW-BINDINGS {: own:^ :}
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

\ An OPTIONAL's group as a derived table.
: SIDE-ITEM ( ^plan ^VEC:vector<item> ^relation -- ^item )
   {: plan items:^ r:^ :}
   plan items 0 NEW-ITEM {: it:^ :}
   N-OPTIONAL it ITEM-MODE !
   plan r SUBSELECT it ITEM-COLUMNS ! it ITEM-TABLE ! it ;

\ Whether an OPTIONAL of the items binds b's variable, which b reads
\ through several columns: each later meeting repeats b's COALESCE.
: MET-SPREAD? ( ^VEC:vector<item> ^binding -- bool )
   {: items:^ b:^ :}
   b BINDING-SPREAD @ NULL-PTR = if false exit then
   items VEC:COUNT 0 ?do
      items i VEC:AT {: it:^ :}
      it ITEM-MODE @ N-OPTIONAL = if
         it ITEM-COLUMNS @ b BINDING-VAR @ FIND-VAR -1 >
         if true unloop exit then
      then
   loop false ;

\ Whether r's FROM holds a join, each binding is NULL on a row a LEFT JOIN
\ extends with NULLs and none is MET-SPREAD? in the group's items so far.
: AS-IS? ( ^VEC:vector<item> ^relation -- bool )
   {: items:^ r:^ :}
   r RELATION-OPTIONAL @ 0= r RELATION-BIND @ or if false exit then
   r RELATION-BRANCHES @ NULL-PTR <> if false exit then
   r RELATION-BINDINGS @ {: bs:^ :}
   bs VEC:COUNT 0 ?do
      bs i VEC:AT {: b:^ :}
      b BINDING-SHAPE @ KNOWN = if false unloop exit then
      items b MET-SPREAD? if false unloop exit then
   loop true ;

\ An OPTIONAL's or MINUS's group as it is: its FROM, its bindings and its
\ WHERE, its null tests written.
: AS-IS-ITEM ( ^plan ^VEC:vector<item> ^relation n -- ^item )
   {: plan items:^ r:^ mode:n :}
   r +NULL-TESTS
   plan items 0 NEW-ITEM {: it:^ :}
   mode it ITEM-MODE !
   r RELATION-FROM @ it ITEM-TABLE ! r RELATION-WHERE @ it ITEM-WHERE !
   r RELATION-BINDINGS @ it ITEM-COLUMNS ! it ;

\ OPTIONAL { P }: P before the FILTERs it has not applied, which the
\ LeftJoin holds, and the branches of P's solutions when they are a union's.
: OPTIONAL-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan plan TREE node NODE-A@ GROUP-SQL {: r:^ filters:^ :}
   r RELATION-EMPTY @ if exit then
   items r AS-IS? if plan items r N-OPTIONAL AS-IS-ITEM
   else plan items r SIDE-ITEM then
   {: it:^ :} filters it ITEM-FILTERS !
   r RELATION-BRANCHES @ it ITEM-BRANCHES ! ;

: MINUS-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan plan TREE node NODE-A@ CLOSED {: r:^ :}
   r RELATION-EMPTY @ if exit then
   plan items r N-MINUS AS-IS-ITEM drop ;

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
   plan 0 NEW-BINDINGS {: cols:^ :}
   0 state node NODE-A@ begin dup 0<> while
      {: k:n list:n :}
      cols plan state list NODE-A@ VAR GENERIC plan PLAN-EMPTY @ s" " BIND
      state state node NODE-B@ k UNDEF-AT? 0= cols LAST BINDING-ALWAYS !
      k 1+ state list NODE-B@
   repeat 2drop cols ;

\ Appends the row `(t,...)`, each term a parameter, UNDEF NULL.
: +DATA-ROW ( ^plan ^rel n -- )
   {: plan v:^ row:n :}
   plan TREE {: state :}
   v s" (" +SQL
   row begin dup 0<> while
      {: list:n :} state list NODE-A@ {: term:n :}
      list row <> if v s" ," +SQL then
      state term NODE-K N-UNDEF = if v s" NULL::jsonb" +SQL
      else plan v term +TERM-ARG then
      state list NODE-B@
   repeat drop v s" )" +SQL ;

\ The data block as a derived table, `(SELECT * FROM (VALUES ...) AS
\ d(vK,...))`; with no variables, one empty solution per row.
: DATA-TABLE ( ^plan n ^VEC:vector<binding> -- ^rel )
   {: plan node:n cols:^ :}
   plan TREE {: state :}
   state node NODE-B@ {: rows:n :}
   plan NEW-SQL {: v:^ :}
   cols VEC:COUNT 0= if
      v s" (SELECT FROM generate_series(1," +SQL
      v state rows LIST-LENGTH +INT v s" ))" +SQL
      v exit
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
      v cols i VEC:AT BINDING-VAR @ +NAME
   loop v s" ))" +SQL v ;

\ VALUES: its data block as a joined derived table; one with no rows is
\ statically empty.
: VALUES-ITEM ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ node:n :}
   plan node DATA-COLUMNS {: cols:^ :}
   plan TREE node NODE-B@ 0= if
      plan items plan NEW-SQL cols true CHILD-ITEM exit
   then
   plan plan node cols DATA-TABLE cols SUBSTITUTED {: t:^ :}
   plan items t cols false CHILD-ITEM ;

\ A triple node s p o, placed at node at's token.
: TRIPLE-NODE ( ^plan n n n n -- n )
   {: plan at:n s:n p:n o:n :}
   plan TREE {: state :}
   state N-TRIPLE s p BINARY-NODE {: t:n :}
   state t o NODE-C!
   state t state at NODE-TOKEN@ NODE-TOKEN! t ;

: GROUP-NODE ( ^plan n -- n )
   {: plan triple:n :}
   plan TREE {: state :}
   state N-GROUP ADD-NODE {: g:n :}
   state triple 0 CONS state g rot NODE-A! g ;

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

\ The members of path's `|` chain.
: CHAIN-MEMBERS ( ^syntax n -- n )
   {: state path:n :}
   state path s" |" OPERATOR? 0= if 1 exit then
   state state path NODE-A@ RECURSE state state path NODE-B@ RECURSE + ;

\ Union u, 0 for none, and the groups of s m o for each member m of
\ path's `|` chain, in written order, nested left as a written UNION.
: PATH-UNION ( ^plan n n n n n -- n )
   {: plan u:n at:n s:n path:n o:n :}
   plan TREE {: state :}
   state path s" |" OPERATOR? if
      plan u at s state path NODE-A@ o RECURSE {: v:n :}
      plan v at s state path NODE-B@ o RECURSE exit
   then
   plan plan at s path o TRIPLE-NODE GROUP-NODE {: g:n :}
   u 0= if g exit then
   state N-UNION u g BINARY-NODE ;

\ A `+`, `*` or `?` triple, a walk the group joins after its joined items.
: WALK-ITEM ( ^plan ^VEC:vector<item> n -- )
   NEW-ITEM N-PATH swap ITEM-MODE ! ;

\ s path o expanded into items: `^` swaps s and o, `/` joins two triples
\ on a fresh variable, which the plan records, a `|` chain is one UNION.
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
      plan items at f y o RECURSE
      plan plan PLAN-FRESH @ plan f VAR +INDEX plan PLAN-FRESH ! exit
   then
   state path s" |" OPERATOR? if
      \ Each member reads at least once: reject a long chain before it is built.
      plan state path CHAIN-MEMBERS CHECK-READS
      plan items plan 0 at s path o PATH-UNION UNION-ITEM exit
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

\ The triple of s p and each object, a further one at its list cell.
: OBJECTS-ITEMS ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ run:n :}
   plan TREE {: state :}
   state run NODE-A@ state run NODE-B@ {: s:n p:n :}
   plan items plan run s p state run NODE-C@ TRIPLE-NODE PATTERN-ITEM
   state run NODE-D@ begin dup 0<> while
      {: cell:n :} state cell NODE-A@ {: o:n :}
      plan items plan cell s p o TRIPLE-NODE PATTERN-ITEM
      state cell NODE-B@
   repeat drop ;

\ The object of the member's rdf:rest triple: the next member's blank
\ node, else the run's rdf:nil.
: REST-OBJECT ( ^syntax n n -- n )
   {: state run:n cell:n :}
   state cell NODE-D@ {: next:n :}
   next 0<> if next exit then
   state run NODE-D@ ;

\ Each member's rdf:first and rdf:rest triples, at its list cell.
: COLLECTION-ITEMS ( ^plan ^VEC:vector<item> n -- )
   {: plan items:^ run:n :}
   plan TREE {: state :}
   state run NODE-B@ state run NODE-C@ {: first:n rest:n :}
   state run NODE-A@ {: head:n :}
   state state head NODE-VALUE@ CELL head
   begin dup 0<> while
      {: b:n cell:n :} state cell NODE-A@ {: m:n :}
      plan items plan cell b first m TRIPLE-NODE PATTERN-ITEM
      state run cell REST-OBJECT {: next:n :}
      plan items plan cell b rest next TRIPLE-NODE PATTERN-ITEM
      next state cell NODE-B@
   repeat 2drop ;

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
         N-OBJECTS of plan items node OBJECTS-ITEMS endof
         N-COLLECTION of plan items node COLLECTION-ITEMS endof
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

\ Joins the item, a derived table or a triple of one description source,
\ into r, the meetings in r's WHERE; r then drops the variables SETTLED by it.
: JOIN-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   r RELATION-BINDINGS @ {: scope:^ :}
   it ITEM-NODE @ 0<> if
      plan WORK-ARENA {: work:^ :}
      work ARENA:MARK {: mark:n :}
      plan it plan it ORIGINS 0 VEC:AT @ LIVE-OF {: recs:^ :}
      plan it ITEM-NODE @ recs r SINGLE-SQL {: occs:^ :}
      plan scope occs SETTLED {: done:^ :}
      plan scope occs r RELATION-WHERE @ false MEET-ALL
      r done FORGET-ALL
      work mark ARENA:CUT exit
   then
   r RELATION-FROM @ {: from:^ :}
   from s"  CROSS JOIN " +SEP
   plan it ITEM-TABLE @ it ITEM-COLUMNS @ r JOIN-TABLE ;

\ Appends the OPTIONAL's table to r's FROM, its columns meeting r's bindings
\ in on: a group in parentheses, its WHERE in on, else a derived table.
: +OPTIONAL-TABLE ( ^plan ^item ^relation ^rel -- )
   {: plan it:^ r:^ on:^ :}
   it ITEM-WHERE @ {: where:^ :}
   where NULL-PTR = if
      plan it ITEM-TABLE @ it ITEM-COLUMNS @ r on ADOPT-TABLE exit
   then
   r RELATION-FROM @ {: from:^ :}
   from s" (" +TEXT from it ITEM-TABLE @ VIEW +REL from s" )" +TEXT
   plan r it ITEM-COLUMNS @ on ADOPT-ALL
   where EMPTY? if exit then
   on s"  AND " +SEP on s" (" +SQL on where VIEW +REL on s" )" +SQL ;

\ Appends to where the OPTIONAL's FILTERs on the scope's bindings.
: +OPTIONAL-FILTERS ( ^plan ^VEC:vector<binding> ^item ^rel -- )
   {: plan scope:^ it:^ where:^ :}
   it ITEM-FILTERS @ {: filters:^ :}
   filters VEC:COUNT 0 ?do
      plan scope filters i VEC:AT @ where false +FILTER
   loop ;

\ Whether the scope binds always a variable branch b binds always.
: ANCHORED? ( ^VEC:vector<binding> ^relation -- bool )
   {: scope:^ b:^ :}
   b RELATION-BINDINGS @ {: bs:^ :}
   bs VEC:COUNT 0 ?do
      bs i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR {: k:n :}
      o BINDING-ALWAYS @ k -1 > and if
         scope k VEC:AT BINDING-ALWAYS @ if true unloop exit then
      then
   loop false ;

\ Whether the OPTIONAL's group is a union whose every branch is
\ ANCHORED? in the scope.
: BY-BRANCH? ( ^item ^VEC:vector<binding> -- bool )
   {: it:^ scope:^ :}
   it ITEM-BRANCHES @ {: bs:^ :}
   bs LEN 0= if false exit then
   bs VEC:COUNT 0 ?do
      scope bs i VEC:AT ANCHORED? 0= if false unloop exit then
   loop true ;

\ Appends branch b's FROM to from, and to where its WHERE, its meeting
\ with the scope and the OPTIONAL's FILTERs; the scope takes b's bindings.
: +MET-BRANCH ( ^plan ^item ^relation ^VEC:vector<binding> ^rel ^rel
      -- )
   {: plan it:^ b:^ scope:^ from:^ where:^ :}
   b +NULL-TESTS
   b RELATION-FROM @ {: bf:^ :}
   bf EMPTY? 0= if
      from s"  CROSS JOIN " +SEP from bf VIEW +REL
   then
   b RELATION-WHERE @ {: bw:^ :}
   bw EMPTY? 0= if where bw VIEW +REL then
   plan scope b RELATION-BINDINGS @ where false MEET-ALL
   plan scope it where +OPTIONAL-FILTERS ;

\ Branch b joined to the outer rows, CTE name of columns cols, and the
\ alias the CTE takes there.
: JOINED-BRANCH ( ^plan ^item ^relation str ^VEC:vector<binding>
      -- ^relation str )
   {: plan it:^ b:^ name:str cols:^ :}
   plan NEW-SQL {: from:^ :} plan NEW-SQL {: where:^ :}
   plan plan TEXTS name TEXT-REL cols from +TABLE-ALIAS {: scope:^ c:str :}
   plan it b scope from where +MET-BRANCH
   plan from where scope RELATION-OF c ;

\ The outer rows, CTE name of columns cols, that meet no branch: those
\ whose row number rn no row of CTE met carries.
: UNMET ( ^plan str str ^VEC:vector<binding> -- ^relation )
   {: plan name:str met:str cols:^ :}
   plan NEW-SQL {: from:^ :} plan NEW-SQL {: where:^ :}
   plan plan TEXTS name TEXT-REL cols from +TABLE-ALIAS {: scope:^ c:str :}
   plan NEXT-ALIAS {: m:str :}
   where s" NOT EXISTS (SELECT 1 FROM " +SQL where met +SQL
   where s"  AS " +SQL where m +SQL where s"  WHERE " +SQL
   where m +SQL where s" .rn = " +SQL where c +SQL where s" .rn)" +SQL
   plan from where scope RELATION-OF ;

\ LeftJoin of a union, branch by branch: r becomes `(WITH c AS
\ MATERIALIZED (r, numbered rn), m AS MATERIALIZED (<each branch joined to
\ c, with c's rn>) <m's rows> UNION ALL <c's UNMET rows>)`. A branch
\ stays joined to c: as a CTE of its own it would lose c's join pushed
\ into it.
: BY-BRANCH ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   plan NEW-SQL {: t:^ :}
   t s" (" +TEXT plan t r s" row_number() OVER ()" +NUMBERED t s" )" +TEXT
   r RELATION-BINDINGS @ {: cols:^ :}
   plan NEXT-ALIAS {: name:str :} plan NEXT-ALIAS {: met:str :}
   plan AL 0 [type] relation VEC:NEW {: arms:^ :}
   plan AL 0 [type] str VEC:NEW {: nums:^ :}
   it ITEM-BRANCHES @ {: bs:^ :}
   bs VEC:COUNT 0 ?do
      plan it bs i VEC:AT name cols JOINED-BRANCH {: a:^ c:str :}
      plan arms a +RELATION
      plan TREE c s" .rn" JOIN$ nums VEC:PUSH !
   loop
   plan name met cols UNMET {: un:^ :}
   plan arms un +RELATION
   plan arms COLUMNS {: ucols:^ :}
   plan NEW-SQL {: mb:^ :}
   mb s" (" +TEXT
   plan WORK-ARENA {: work:^ :}
   bs VEC:COUNT 0 ?do
      i 0<> if mb s"  UNION ALL " +TEXT then
      work ARENA:MARK {: mark:n :}
      plan mb plan arms i VEC:AT ucols PRESENTED nums i VEC:AT @ +NUMBERED
      work mark ARENA:CUT
   loop mb s" )" +TEXT
   plan NEW-SQL {: mf:^ :}
   plan plan TEXTS met TEXT-REL ucols mf +TABLE {: mcols:^ :}
   plan AL 0 [type] relation VEC:NEW {: outs:^ :}
   plan outs plan mf plan NEW-SQL mcols RELATION-OF +RELATION
   plan outs un +RELATION
   plan AL NEW-CTES {: ws:^ :}
   plan AL ws name true t +CTE plan AL ws met true mb +CTE
   plan NEW-SQL {: u:^ :}
   ws CTES-FIRST @ u REL-WITH !
   plan u outs ucols +UNION-ALL
   plan NEW-SQL {: out:^ :}
   out s" (" +TEXT out u +REL out s" )" +TEXT
   plan NEW-SQL {: from:^ :}
   plan out ucols from +TABLE {: bound:^ :}
   from plan NEW-SQL bound r INIT-RELATION ;

\ LeftJoin: P LEFT JOINs the FROM, ON P meeting the scope and P's FILTERs,
\ unless it joins BY-BRANCH. An empty FROM is one empty row.
: OPTIONAL-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   it r RELATION-BINDINGS @ BY-BRANCH? if plan it r BY-BRANCH exit then
   r RELATION-FROM @ {: from:^ :}
   from EMPTY? if
      plan plan TEXTS s" (SELECT)" TEXT-REL from +AS drop
   then
   from s"  LEFT JOIN " +TEXT true r RELATION-OPTIONAL !
   plan NEW-SQL {: on:^ :}
   plan it r on +OPTIONAL-TABLE
   plan r RELATION-BINDINGS @ it on +OPTIONAL-FILTERS
   from s"  ON " +TEXT
   on EMPTY? if from s" true" +TEXT exit then
   from on +REL ;

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

: +NOT-NULL ( ^rel ^binding -- )
   {: v:^ b:^ :}
   b BINDING-ALWAYS @ if exit then
   v s"  AND " +SEP v b +BINDING v s"  IS NOT NULL" +SQL ;

\ Appends to where that some variable of occs the scope binds is bound on
\ both sides.
: +BOUND-BOTH ( ^plan ^VEC:vector<binding> ^VEC:vector<binding>
      ^rel -- )
   {: plan scope:^ occs:^ where:^ :}
   plan NEW-SQL {: any:^ :}
   occs VEC:COUNT 0 ?do
      occs i VEC:AT {: o:^ :}
      scope o BINDING-VAR @ FIND-VAR {: k:n :}
      k -1 > if
         plan NEW-SQL {: both:^ :}
         both scope k VEC:AT +NOT-NULL both o +NOT-NULL
         any s"  OR " +SEP any s" (" +SQL
         any both +REL any s" )" +SQL
      then
   loop
   where s"  AND " +SEP where s" (" +SQL
   where any +REL where s" )" +SQL ;

\ Minus: NOT EXISTS a row of P that meets the scope with a shared
\ variable bound on both sides; nothing when no variable can be.
: MINUS-STEP ( ^plan ^item ^relation -- )
   {: plan it:^ r:^ :}
   r RELATION-BINDINGS @ {: scope:^ :}
   it ITEM-COLUMNS @ {: cols:^ :}
   scope cols SHARING {: sharing:n :}
   sharing 0= if exit then
   \ Cuts the item's own column list: nothing reads it after.
   scope cols KEEP-MET
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan WORK-AL [type] relation ALLOC:NEW {: u:^ :}
   it ITEM-TABLE @ it ITEM-WHERE @ cols u INIT-RELATION
   plan u SUBSELECT {: t:^ ts:^ :}
   r RELATION-WHERE @ {: where:^ :}
   where s"  AND " +SEP
   where s" NOT EXISTS (SELECT 1 FROM " +SQL
   plan t ts where +TABLE {: occs:^ :}
   work mark ARENA:CUT
   plan NEW-SQL {: cond:^ :}
   plan plan scope COPY-SCOPE occs cond false MEET-ALL
   sharing 1 = if plan scope occs cond +BOUND-BOTH then
   cond EMPTY? 0= if where s"  WHERE " +SQL where cond +REL then
   where s" )" +SQL ;

\ The expression's SQL over the relation's bindings through the
\ substitution, a rel.
: EXPRESSION-SQL ( ^plan ^relation n -- ^rel )
   {: plan r:^ e:n :}
   plan PLAN-SCOPE @ {: outer:^ :}
   plan PLAN-OUT @ {: out:^ :}
   plan NEW-SQL {: v:^ :}
   plan r RELATION-BINDINGS @ SEEN plan PLAN-SCOPE ! v plan PLAN-OUT !
   plan e SQL-EXPRESSION
   outer plan PLAN-SCOPE ! out plan PLAN-OUT ! v ;

\ The index of the plan's aggregate that is one with the call of hash h,
\ or -1.
: HASHED-AGGREGATE ( ^plan n n -- n )
   {: plan call:n h:n :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      as i VEC:AT AGGREGATE-DIGEST @ h = if
         plan as i VEC:AT AGGREGATE-NODE @ call SAME-EXPRESSION? if
            i unloop exit
         then
      then
   loop -1 ;

\ The index of the plan's aggregate that is one with expression e, or -1.
: FIND-AGGREGATE ( ^plan n -- n )
   {: plan e:n :}
   plan TREE e AGGREGATE-CALL? 0= if -1 exit then
   plan e plan e EXPRESSION-HASH HASHED-AGGREGATE ;

\ The variable node n is, -1 for another node or a name no pattern has.
: FOUND-VAR ( ^plan n -- n )
   {: plan node:n :}
   plan TREE node VARIABLE? 0= if -1 exit then
   plan plan node VAR-NAME plan PLAN-FRAME @ FIND-VAR-ID ;

\ Whether the node is a variable r binds always.
: BOUND? ( ^plan ^relation n -- bool )
   {: plan r:^ node:n :}
   plan node FOUND-VAR {: var:n :}
   var 0< if false exit then
   r RELATION-BINDINGS @ var FIND-VAR {: k:n :}
   k 0< if false exit then
   r RELATION-BINDINGS @ k VEC:AT BINDING-ALWAYS @ ;

\ Whether shape k may hold a blank node: it holds any term.
: BLANKS? ( ^plan n -- bool )
   SHAPE@ SHAPE-HOLDS @ MATCH holds
      any OF true ENDOF
      uris OF false ENDOF
      literals OF drop false ENDOF
   ;MATCH ;

\ Whether no term the store holds is a blank node: a term slot holds a
\ uri or a literal, and a query makes no blank node.
: BLANKLESS? ( ^plan -- bool )
   {: plan :}
   plan PLAN-STORE @ STORE-SHAPES @ VEC:COUNT 0 ?do
      plan i BLANKS? if false unloop exit then
   loop true ;

\ The variable the IF call's condition tests with BOUND, else -1.
: BOUND-TESTED ( ^plan n -- n )
   {: plan call:n :}
   plan TREE {: state :}
   state state call NODE-A@ NODE-A@ {: test:n :}
   state test NODE-K N-CALL <> if -1 exit then
   state test s" BOUND" CALL-IS? 0= if -1 exit then
   plan state state test NODE-A@ NODE-A@ FOUND-VAR ;

\ Whether expression e over r's bindings always has a value, variable given
\ counted bound (-1 for none); STR of a blank node is NULL.
: SURE-GIVEN? ( ^plan ^relation n n -- bool )
   {: plan r:^ e:n given:n :}
   plan TREE {: state :}
   state e CONSTANT? if true exit then
   plan r e BOUND? if true exit then
   given -1 > plan e FOUND-VAR given = and if true exit then
   state e NODE-K N-CALL = if
      state e NODE-A@ {: args:n :}
      state e s" COALESCE" CALL-IS? if
         args begin dup 0<> while
            {: each:n :}
            plan r state each NODE-A@ given RECURSE if true exit then
            state each NODE-B@
         repeat drop
      then
      state e s" STR" CALL-IS? if
         plan BLANKLESS? 0= if false exit then
         plan r state args NODE-A@ given RECURSE exit
      then
      state e s" IF" CALL-IS? if
         state args NODE-B@ {: rest:n :}
         state rest NODE-A@ state state rest NODE-B@ NODE-A@
         {: a:n b:n :}
         plan e BOUND-TESTED {: v:n :}
         v 0< if false exit then
         plan r b given RECURSE 0= if false exit then
         plan r a v RECURSE exit
      then
   then
   plan e FIND-AGGREGATE {: k:n :}
   k 0< if false exit then
   plan PLAN-AGGREGATES @ k VEC:AT AGGREGATE-ALWAYS @ ;

: SURE? ( ^plan ^relation n -- bool ) -1 SURE-GIVEN? ;

\ Binds the variable to the expression itself, `(x)`, x a piece.
: +COPIED ( ^plan ^relation n ^rel -- )
   {: plan r:^ var:n x:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (" +SQL v x +REL v s" )" +SQL
   r RELATION-BINDINGS @ var GENERIC v s" " BIND ;

\ Node n's children, 0 for none: three walked on their own, then the one the
\ walk goes on to: a list's tail, else a chain's long side, child a.
: NODE-PARTS ( ^syntax n -- n n n n )
   {: state node:n :}
   state node NODE-K {: kind:n :}
   kind N-IRI >= kind N-UNDEF <= and if 0 0 0 0 exit then
   state node NODE-A@ state node NODE-B@ {: a:n b:n :}
   state node NODE-C@ state node NODE-D@ {: c:n d:n :}
   kind N-LIST = if a c d b exit then
   kind N-SELECT = if b state node QUERY-VALUES@ d a exit then
   b c d a ;

\ Whether node n or a node below it, an EXISTS's group's included, calls
\ RAND, UUID or STRUUID, which give a new value at each call.
: VOLATILE? ( ^syntax n -- bool )
   {: state top:n :}
   top begin dup 0<> while
      {: node:n :}
      state node NODE-K N-CALL = if
         state node s" RAND UUID STRUUID" CALL-IN? if true exit then
      then
      state node NODE-PARTS {: one:n two:n three:n next:n :}
      state one RECURSE state two RECURSE or
      state three RECURSE or if true exit then
      next
   repeat drop false ;

\ PostgreSQL runs a materialized CTE once, but may rerun a LATERAL or OFFSET 0
\ table per joined row, so r reads a CTE, whose column leaves copy nothing.
: +EVALUATED ( ^plan ^relation n ^rel -- )
   {: plan r:^ var:n x:^ :}
   r RELATION-BINDINGS @ var GENERIC x s" " BIND
   plan r SUBSELECT {: t:^ cols:^ :}
   plan NEW-SQL {: out:^ :}
   out s" (WITH c AS MATERIALIZED " +TEXT out t +REL
   out s"  SELECT * FROM c)" +TEXT
   plan NEW-SQL {: from:^ :}
   plan out cols from +TABLE {: bs:^ :}
   bs VEC:COUNT 0 ?do 0 bs i VEC:AT BINDING-SQL @ REL-COPIES ! loop
   from plan NEW-SQL bs r INIT-RELATION ;

\ Whether each variable at or below node n is one r binds always.
: ALL-BOUND? ( ^plan ^relation n -- bool )
   {: plan r:^ top:n :}
   plan TREE {: state :}
   top begin dup 0<> while
      {: node:n :}
      state node VARIABLE? if
         plan r node BOUND? 0= if false exit then
      then
      state node NODE-PARTS {: one:n two:n three:n next:n :}
      plan r one RECURSE 0= if false exit then
      plan r two RECURSE 0= if false exit then
      plan r three RECURSE 0= if false exit then
      next
   repeat drop true ;

\ Applies to r's WHERE each of the filters whose variables r binds
\ always, and keeps the others in the list.
: +EARLY-FILTERS ( ^plan ^relation ^VEC:vector<n> -- )
   {: plan r:^ filters:^ :}
   0 filters VEC:COUNT 0 ?do
      filters i VEC:AT @ {: f:n :}
      plan r plan TREE f NODE-A@ ALL-BOUND? if
         plan r RELATION-BINDINGS @ f r RELATION-WHERE @ true +FILTER
      else f over filters swap VEC:AT ! 1+ then
   loop filters swap VEC:RESIZE ;

\ Binds ?x of `(e AS ?x)` or BIND(e AS ?x) in r, applying the group's
\ filters it can before an evaluated one.
: +ALIAS ( ^plan ^relation n ^VEC:vector<n> -- )
   {: plan r:^ e:n filters:^ :}
   plan TREE {: state :}
   plan state e NODE-B@ VAR {: var:n :}
   state e NODE-A@ {: value:n :}
   plan r value EXPRESSION-SQL {: x:^ :}
   state value VOLATILE? x SQL-LEN x COPIES + MOST-COPIED > or if
      plan r filters +EARLY-FILTERS plan r var x +EVALUATED
   else plan r var x +COPIED then
   r RELATION-BINDINGS @ LAST {: b:^ :}
   plan r value SURE? b BINDING-ALWAYS !
   plan b r RELATION-WHERE @ +OUTER ;

: FOLD ( ^plan ^item ^relation ^VEC:vector<n> -- )
   {: plan it:^ r:^ filters:^ :}
   it ITEM-MODE @ {: mode:n :}
   mode N-OPTIONAL = if plan it r OPTIONAL-STEP exit then
   mode N-MINUS = if plan it r MINUS-STEP exit then
   mode N-BIND = if
      plan r it ITEM-NODE @ filters +ALIAS
      true r RELATION-BIND ! exit
   then
   plan it r JOIN-STEP ;

: SEED! ( ^walk n n bool bool -- )
   {: w:^ seed:n far:n inverse:bool rest:bool :}
   seed w WALK-SEED ! far w WALK-FAR !
   inverse w WALK-INVERSE ! rest w WALK-REST ! ;

\ The substitution's binding of the end when it binds it always, else
\ NULL-PTR.
: OUTER-ALWAYS ( ^plan n -- ^binding )
   {: plan node:n :}
   plan TREE node VARIABLE? 0= if NULL-PTR exit then
   plan PLAN-OUTER @ plan node VAR LOOKUP {: b:^ :}
   b NULL-PTR = if b exit then
   b BINDING-ALWAYS @ if b else NULL-PTR then ;

\ Whether the end's value is given: a constant, or a variable the
\ substitution binds always.
: GIVEN? ( ^plan n -- bool )
   {: plan node:n :}
   plan TREE node VARIABLE? 0= if true exit then
   plan node OUTER-ALWAYS NULL-PTR <> ;

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
   state triple NODE-A@ {: s:n :} state triple NODE-C@ {: o:n :}
   plan s GIVEN? if w s o false false SEED! w exit then
   plan o GIVEN? if w o s true false SEED! w exit then
   plan r s BOUND? if w s o false true SEED! w exit then
   plan r o BOUND? if w o s true true SEED! w exit then
   w s o false false SEED! w ;

: PLUS? ( ^walk -- bool ) WALK-OP @ s" +" STR:= ;

: WALK-CTE-NAME ( ^plan ^walk str -- str )
   {: plan w:^ s:str :} plan TREE w WALK-NAME @ s JOIN$ ;

\ The variable's native shape in the list, else GENERIC.
: SHAPE-OF ( ^VEC:vector<binding> n -- n )
   {: bs:^ var:n :}
   bs var FIND-VAR {: k:n :}
   k 0< if GENERIC exit then
   bs k VEC:AT BINDING-SHAPE @ dup 0< if drop GENERIC then ;

\ A value of the shape at sql, a leaf of its bytes, outside any scope.
: VALUE ( ^plan n str -- ^binding )
   {: plan shape:n s:str :}
   plan NEW-SQL {: v:^ :} v s +SQL
   plan AL [type] binding ALLOC:NEW {: b:^ :}
   b -1 shape v s" " SET-BINDING b ;

\ A given end's value: the constant, or the substitution's binding.
: GIVEN ( ^plan n -- ^binding )
   {: plan node:n :}
   plan node OUTER-ALWAYS {: b:^ :}
   b NULL-PTR = if plan node CONSTANT-VALUE else b then ;

\ Appends ` AND ` and the condition unless it is empty.
: +CONDITION ( ^rel ^rel -- )
   {: v:^ a:^ :}
   a EMPTY? if exit then v a +AND ;

\ Appends the variable's value in the list in the form, ` AS ` and name.
: +PAIR ( ^plan ^rel ^VEC:vector<binding> n n str -- )
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

\ Sets the steps' query as the walk's pairs, until PAIRS-CTE.
: PAIRS! ( ^plan ^walk ^rel -- )
   {: plan w:^ t:^ :}
   plan TREE w WALK-OPERAND @ WALKED? w WALK-ONCE !
   t w WALK-PAIRS ! ;

\ Makes the walk's pairs CTE nameP when its operand has a walk in it, which
\ each step would otherwise walk again, its text growing exponentially.
: PAIRS-CTE ( ^plan ^walk -- )
   {: plan w:^ :}
   w WALK-ONCE @ 0= if exit then
   plan w s" p" WALK-CTE-NAME {: n:str :}
   plan AL plan PLAN-CTES @ n true w WALK-PAIRS @ +CTE
   plan TEXTS n TEXT-REL w WALK-PAIRS ! ;

\ The columns of the end's root that b, a binding of the steps' start,
\ reads: b when native of that root, else its spread's, if any.
: AN ( ^plan n ^binding -- ^rel )
   {: plan end:n b:^ :}
   plan NEW-SQL {: v:^ :}
   plan b BINDING-SHAPE @ end ONE-ROOT? if v b +BINDING v exit then
   b BINDING-SPREAD @ {: sp:^ :}
   sp NULL-PTR = if v exit then
   plan end sp ROOT-COLUMNS ;

\ The relations the steps read: a union's branches, else r alone.
: STEP-COUNT ( ^relation -- n ) RELATION-BRANCHES @ LEN 1 max ;

: STEP-REL ( ^relation n -- ^relation )
   {: r:^ k:n :}
   r RELATION-BRANCHES @ {: bs:^ :}
   bs NULL-PTR = if r else bs k VEC:AT then ;

\ AN of relation r's binding of the start's variable var, empty where r
\ binds none.
: REL-AN ( ^plan n n ^relation -- ^rel )
   {: plan end:n var:n r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs var FIND-VAR {: k:n :}
   k 0< if plan NEW-SQL exit then
   plan end bs k VEC:AT AN ;

\ Whether a column of kind x, holding term s when KNOWN, cannot hold
\ term t or is of the end's root.
: ROOT-FOR? ( ^plan n n str str -- bool )
   {: plan end:n x:n s:str t:str :}
   plan x s KNOWN t KINDS-COMPATIBLE? 0= if true exit then
   plan x end ONE-ROOT? ;

\ Whether of the columns b, a binding of the steps' start, reads, only
\ those of the end's root may hold term t.
: HELD? ( ^plan n str ^binding -- bool )
   {: plan end:n t:str b:^ :}
   b BINDING-SPREAD @ {: sp:^ :}
   sp NULL-PTR = if
      plan end b BINDING-SHAPE @ b BINDING-TERM @ t ROOT-FOR? exit
   then
   sp SPREAD-RECS @ {: recs:^ :} sp SPREAD-SLOT @ {: k:n :}
   recs LEN 0 ?do
      plan recs i VEC:AT @ k RECORD-SLOT SLOT-KIND {: x:n s:str :}
      plan end x s t ROOT-FOR? 0= if false unloop exit then
   loop true ;

\ Whether every relation of the steps binds var to columns of the end's
\ root.
: ROOTED? ( ^plan n n ^relation -- bool )
   {: plan end:n var:n r:^ :}
   r STEP-COUNT 0 ?do
      plan end var r i STEP-REL REL-AN EMPTY? if false unloop exit then
   loop true ;

\ Whether, in every relation of the steps, HELD? holds of term t and its
\ binding of var, which ROOTED? found.
: ALL-HELD? ( ^plan n n str ^relation -- bool )
   {: plan end:n var:n t:str r:^ :}
   r STEP-COUNT 0 ?do
      r i STEP-REL RELATION-BINDINGS @ {: bs:^ :}
      plan end t bs bs var FIND-VAR VEC:AT HELD? 0= if
         false unloop exit
      then
   loop true ;

\ Sets whether the steps of r, from var, hold an, and whether the seed
\ is a given term only an may hold.
: ROOTS! ( ^plan ^walk ^relation n -- )
   {: plan w:^ r:^ var:n :}
   false w WALK-ROOTED ! false w WALK-HELD !
   w WALK-END @ {: end:n :}
   end 0< w WALK-START @ GENERIC <> or if exit then
   plan end var r ROOTED? 0= if exit then
   true w WALK-ROOTED !
   plan w WALK-SEED @ GIVEN? 0= if exit then
   plan w WALK-SEED @ GIVEN {: g:^ :}
   g BINDING-SHAPE @ KNOWN <> if exit then
   plan end var g BINDING-TERM @ r ALL-HELD? w WALK-HELD ! ;

\ Appends `SELECT a, b[, an]` and r's FROM and WHERE: r's steps from
\ variable x to y.
: +PAIRS ( ^plan ^walk ^rel ^relation n n -- )
   {: plan w:^ v:^ r:^ x:n y:n :}
   r RELATION-BINDINGS @ {: bs:^ :}
   v s" SELECT " +SQL plan v bs x w WALK-START @ s" a," +PAIR
   plan v bs y w WALK-END @ s" b" +PAIR
   w WALK-ROOTED @ if
      v s" ," +SQL v plan w WALK-END @ x r REL-AN +REL
      v s"  AS an" +SQL
   then v r +FROM-WHERE ;

\ The walk's steps `(SELECT a, b[, an] ...)`, fresh a to b, b to a when
\ inverse; an is a's columns of native b's root, each union branch its own.
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
      plan w
         plan TEXTS s" (SELECT NULL::jsonb AS a,NULL::jsonb AS b WHERE false)"
         TEXT-REL PAIRS! exit
   then
   rp RELATION-BINDINGS @ {: bs:^ :}
   plan a VAR {: av:n :} plan b VAR {: bv:n :}
   bs av SHAPE-OF w WALK-START ! bs bv SHAPE-OF w WALK-END !
   plan w rp av ROOTS!
   plan NEW-SQL {: v:^ :} v s" (" +SQL
   w WALK-ROOTED @ 0= rp RELATION-BRANCHES @ NULL-PTR = or if
      plan w v rp av bv +PAIRS
   else
      rp STEP-COUNT 0 ?do
         i 0<> if v s"  UNION ALL " +SQL then
         plan w v rp i STEP-REL av bv +PAIRS
      loop
   then v s" )" +TEXT plan w v PAIRS! ;

\ r's solutions become CTE nameR, which r then reads.
: REST ( ^plan ^walk ^relation -- )
   {: plan w:^ r:^ :}
   plan r SUBSELECT {: t:^ cols:^ :}
   plan w s" r" WALK-CTE-NAME {: n:str :}
   plan AL w WALK-CTES @ n false t +CTE
   plan NEW-SQL {: from:^ :}
   from plan NEW-SQL plan plan TEXTS n TEXT-REL cols from +TABLE
   r INIT-RELATION ;

\ Appends the given end's value as k, native where it has a shape. A KNOWN
\ term stays one: `*` and `?` answer it even where no shape's `in` takes it.
: GIVEN-SEEDS ( ^plan ^walk ^rel -- )
   {: plan w:^ v:^ :}
   plan w WALK-SEED @ GIVEN {: b:^ :}
   b BINDING-SHAPE @ dup 0< if drop GENERIC then {: kind:n :}
   kind w WALK-KIND !
   plan v b kind +VALUE v s"  AS k" +SQL ;

: PAIR-SEEDS ( ^walk ^rel -- )
   {: w:^ out:^ :}
   w WALK-START @ w WALK-KIND !
   out s" p.a AS k FROM " +TEXT
   out w WALK-PAIRS @ +REL out s"  AS p" +TEXT ;

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

\ Whether slots a and b of one FROM item read one value: one column
\ under shapes of one root, or one term.
: ONE-VALUE? ( ^plan slot slot -- bool )
   {: plan a b :}
   a SLOT-KIND {: x:n s:str :} b SLOT-KIND {: y:n t:str :}
   x KNOWN = y KNOWN = and if s t STR:= exit then
   a SLOT-COL b SLOT-COL STR:= plan x y ONE-ROOT? and ;

\ Whether the slot is column c and may hold NULL.
: NULLABLE-ON? ( slot str -- bool )
   {: sl c:str :}
   sl MATCH slot
      column OF {: col:str shape:n k o u:bool nn:bool :}
         col c STR:= nn 0= and
      ENDOF
      term OF drop false ENDOF
   ;MATCH ;

\ Whether read (r, e) tests column c IS NOT NULL: another slot of record
\ r may hold NULL there, and the end's `=` does not read c.
: NEEDS? ( ^plan n n str -- bool )
   {: plan r:n e:n c:str :}
   plan r e RECORD-SLOT SLOT-COL c STR:= if false exit then
   3 0 do
      plan r i RECORD-SLOT c NULLABLE-ON? if true unloop exit then
   loop false ;

\ Whether read (r, e) tests IS NOT NULL every column read (j, f) tests.
: TESTS-WITHIN? ( ^plan n n n n -- bool )
   {: plan r:n e:n j:n f:n :}
   3 0 do
      plan j i RECORD-SLOT SLOT-COL {: c:str :}
      plan j f c NEEDS? plan r e c NEEDS? 0= and
      if false unloop exit then
   loop true ;

\ Whether every row giving read (r, e) its triple gives read (j, f) its
\ own with the same end value.
: IMPLIES? ( ^plan n n n n -- bool )
   {: plan r:n e:n j:n f:n :}
   plan r SOURCE@ SOURCE-FROM @ plan j SOURCE@ SOURCE-FROM @ STR:=
   plan plan r e RECORD-SLOT plan j f RECORD-SLOT ONE-VALUE? and
   0= if false exit then
   plan r e j f TESTS-WITHIN? ;

\ Whether read (r, e) is in the active graph and its end can hold a
\ value of the kind.
: NODE-SLOT? ( ^plan n n n -- bool )
   {: plan r:n e:n kind:n :}
   plan plan r SOURCE@ GRAPH-FITS? 0= if false exit then
   plan r e RECORD-SLOT SLOT-KIND {: x:n s:str :}
   plan x s kind s" " KINDS-COMPATIBLE? ;

\ Whether read (r, e) implies read (j, f) of the active graph that is
\ earlier or does not imply (r, e).
: ABSORBED? ( ^plan n n n n -- bool )
   {: plan r:n e:n j:n f:n :}
   plan r e j f IMPLIES? 0= if false exit then
   plan plan j SOURCE@ GRAPH-FITS? 0= if false exit then
   j r < j r = f e < and or if true exit then
   plan j f r e IMPLIES? 0= ;

\ Whether read (r, e) implies another read of the active graph, the
\ first of equal ones kept.
: REDUNDANT? ( ^plan n n -- bool )
   {: plan r:n e:n :}
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 0 ?do
      plan r e i 0 ABSORBED? if true unloop exit then
      plan r e i 2 ABSORBED? if true unloop exit then
   loop false ;

\ Whether the node test and the node CTE write read (r, e) for the kind.
: NODE-READ? ( ^plan n n n -- bool )
   {: plan r:n e:n kind:n :}
   plan r e kind NODE-SLOT? 0= if false exit then
   plan r e REDUNDANT? 0= ;

\ Every subject and object in the active graph: a UNION ALL of one branch
\ per read NODE-READ? keeps, binding only a fresh variable, and that
\ variable.
: NODE-BRANCHES ( ^plan ^walk -- ^VEC:vector<relation> n )
   {: plan w:^ :}
   plan w NODE-TRIPLES {: down:n up:n var:n :}
   plan INDEXES var +INDEX {: kept:^ :}
   plan AL 0 [type] relation VEC:NEW {: branches:^ :}
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 0 ?do
      plan i 0 GENERIC NODE-READ? if
         plan down plan i RECORD branches VEC:PUSH SOURCE-BRANCH
      then
      plan i 2 GENERIC NODE-READ? if
         plan up plan i RECORD branches VEC:PUSH SOURCE-BRANCH
      then
   loop
   branches VEC:COUNT 0 ?do plan kept branches i VEC:AT KEEP-READ loop
   branches var ;

\ Appends to the statement's WITH the CTE of the active graph's nodes,
\ k their column; answers its name and k's form, empty when no source
\ is in the graph.
: NODE-CTE ( ^plan ^walk -- str n )
   {: plan w:^ :}
   plan w NODE-BRANCHES {: branches:^ var:n :}
   branches VEC:COUNT 0= if s" " GENERIC exit then
   plan branches UNION-TABLE {: t:^ cols:^ :}
   plan NEXT-ALIAS {: name:str :}
   plan TEXTS s" (SELECT " TEXT-REL {: body:^ :}
   body s" n" var +NAME-OF body s"  AS k FROM " +TEXT
   body t +REL body s"  AS n)" +TEXT
   plan AL plan PLAN-CTES @ name false body +CTE
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

\ Appends ` OR `-joined EXISTS of the triple over record rec, var's slot
\ meeting k inside it for the column's index; that `=` drops its NULL.
: +EXISTS ( ^plan n n n ^binding ^rel -- )
   {: plan triple:n rec:n var:n k:^ out:^ :}
   plan AL [type] relation ALLOC:NEW {: r:^ :}
   plan triple plan rec RECORD r SOURCE-BRANCH
   r RELATION-BINDINGS @ {: bs:^ :}
   bs bs var FIND-VAR VEC:AT {: b:^ :}
   plan b k SAME-SQL {: same:^ :}
   same EMPTY? 0= if plan bs b BINDING-SQL @ COLUMN-TESTED then
   r RELATION-WHERE @ same +CONDITION
   out s"  OR " +SEP out s" EXISTS (SELECT 1" +TEXT
   out r +FROM-WHERE out s" )" +TEXT ;

\ The condition that `x.k` of the walk's kind is a node of the active
\ graph: one EXISTS per read NODE-READ? keeps, `false` when none is.
: NEW-NODE-TEST ( ^plan ^walk -- ^rel )
   {: plan w:^ :}
   w WALK-KIND @ {: kind:n :}
   plan kind s" x.k" VALUE {: k:^ :}
   plan w NODE-TRIPLES {: down:n up:n var:n :}
   plan NEW-SQL {: t:^ :}
   plan PLAN-STORE @ STORE-SOURCES @ VEC:COUNT 0 ?do
      plan i 0 kind NODE-READ? if plan down i var k t +EXISTS then
      plan i 2 kind NODE-READ? if plan up i var k t +EXISTS then
   loop
   t EMPTY? if t s" false" +TEXT then t ;

\ NEW-NODE-TEST's rel for the active graph and the walk's kind, built
\ once for both: its text is the same for every walk, since the key is
\ always `x.k`.
: NODE-SQL ( ^plan ^walk -- ^rel )
   {: plan w:^ :}
   plan PLAN-TESTS @ {: tests:^ :}
   tests VEC:COUNT 0 ?do
      tests i VEC:AT {: t:^ :}
      t NODE-TEST-GRAPH @ plan PLAN-GRAPH @ STR:=
      t NODE-TEST-KIND @ w WALK-KIND @ = and if
         t NODE-TEST-TEST @ unloop exit
      then
   loop
   plan w NEW-NODE-TEST {: rel:^ :}
   tests VEC:PUSH {: t:^ :}
   plan PLAN-GRAPH @ t NODE-TEST-GRAPH !
   w WALK-KIND @ t NODE-TEST-KIND ! rel t NODE-TEST-TEST ! rel ;

\ Whether the node is variable var.
: VAR-IS? ( ^plan n n -- bool )
   {: plan node:n var:n :}
   plan TREE node VARIABLE? 0= if false exit then
   plan node VAR var = ;

\ Whether the item binds var to a node of its graph: a joined triple's
\ subject or object, or a walk's end when none is given: one answers itself.
: NODE-END? ( ^plan ^item n -- bool )
   {: plan it:^ var:n :}
   plan TREE {: state :}
   it ITEM-NODE @ {: triple:n :}
   it ITEM-MODE @ N-PATH = if
      plan state triple NODE-A@ GIVEN? plan state triple NODE-C@ GIVEN? or
      if false exit then
   else it JOINED-TRIPLE? 0= if false exit then then
   plan state triple NODE-A@ var VAR-IS?
   plan state triple NODE-C@ var VAR-IS? or ;

\ Whether an item of the walk's graph binds var to a node there: one before
\ index at, or a joined triple before to, which the run joins too.
: NODE? ( ^plan ^VEC:vector<item> n n n -- bool )
   {: plan items:^ var:n at:n to:n :}
   items at VEC:AT ITEM-GRAPH @ {: graph:str :}
   to 0 ?do
      items i VEC:AT {: it:^ :}
      i at < it JOINED-TRIPLE? or it ITEM-GRAPH @ graph STR:= and if
         plan it var NODE-END? if true unloop exit then
      then
   loop false ;

\ Appends to out the values r binds the seed to; for `*` and `?`, unless
\ NODE? holds, only the graph's nodes: a seed answers itself at length zero.
: REST-SEEDS ( ^plan ^VEC:vector<item> n n ^walk ^relation ^rel -- )
   {: plan items:^ at:n to:n w:^ r:^ out:^ :}
   plan w WALK-SEED @ VAR {: var:n :}
   r RELATION-BINDINGS @ var SHAPE-OF w WALK-KIND !
   plan w s" r" WALK-CTE-NAME {: n:str :}
   w PLUS? plan items var at to NODE? or if
      out n var +NAME-OF out s"  AS k FROM " +TEXT out n +TEXT exit
   then
   out s" k FROM (SELECT " +TEXT out n var +NAME-OF
   out s"  AS k FROM " +TEXT out n +TEXT out s" ) AS x" +TEXT
   out s"  WHERE (" +TEXT out plan w NODE-SQL +REL out s" )" +TEXT ;

\ Appends to the rel the active graph's nodes as k, none when no source
\ is in the graph.
: NODE-SEEDS ( ^plan ^walk ^rel -- )
   {: plan w:^ v:^ :}
   plan w NODES {: t:str kind:n :}
   kind w WALK-KIND !
   t STR:LEN 0= if v s" NULL::jsonb AS k WHERE false" +SQL exit then
   v s" k FROM " +SQL v t +SQL ;

\ Adds to the walk's CTEs nameS, the distinct seeds k the walk at index
\ at in items, in a run ending before index to, starts from.
: SEEDS ( ^plan ^VEC:vector<item> n n ^walk ^relation -- )
   {: plan items:^ at:n to:n w:^ r:^ :}
   plan TEXTS s" (SELECT DISTINCT " TEXT-REL {: body:^ :}
   plan w WALK-SEED @ GIVEN? if plan w body GIVEN-SEEDS
   else w WALK-REST @ if plan items at to w r body REST-SEEDS
   else w PLUS? if w body PAIR-SEEDS
   else plan w body NODE-SEEDS then then then
   body s" )" +TEXT
   plan w s" s" WALK-CTE-NAME {: n:str :}
   plan AL w WALK-CTES @ n false body +CTE ;

\ Appends to the rel the join to the walk's pairs p on the condition,
\ `true` when empty: ` JOIN nameP AS p ON` it, else ` CROSS JOIN LATERAL
\ (SELECT * FROM` the query ` AS p WHERE` it ` OFFSET 0) AS p`. The fence
\ keeps PostgreSQL from flattening the lookup into a hash join that scans
\ every pair on each step: a 1,000-node `p+` took 1.1 s that way.
: +STEP ( ^rel ^walk ^rel -- )
   {: out:^ w:^ c:^ :}
   w WALK-ONCE @ {: once:bool :}
   once if out s"  JOIN " +TEXT
   else out s"  CROSS JOIN LATERAL (SELECT * FROM " +TEXT then
   out w WALK-PAIRS @ +REL out s"  AS p" +TEXT
   once if out s"  ON " +TEXT else out s"  WHERE " +TEXT then
   c EMPTY? if out s" true" +TEXT else out c +REL then
   once 0= if out s"  OFFSET 0) AS p" +TEXT then ;

\ Appends ` AS n`, the step end p.b in the steps' start form.
: +KEY ( ^plan ^rel ^walk -- )
   {: plan v:^ w:^ :}
   w WALK-START @ {: start:n :}
   v s" ," +SQL
   start 0< if
      plan v plan w WALK-END @ s" p.b" VALUE start +VALUE
   else v plan start SHAPE@ SHAPE-IN @ s" p.b" +CALL then
   v s"  AS n" +SQL ;

\ A step's pairs p start at at: `p.an = at`, which an index serves, where
\ the steps hold an and at's shape is of the end's root, else p.a meets at.
: STEP-FROM ( ^plan ^walk ^binding -- ^rel )
   {: plan w:^ at:^ :}
   w WALK-END @ {: end:n :}
   w WALK-ROOTED @ plan at BINDING-SHAPE @ end ONE-ROOT? and if
      plan plan plan end ROOT s" p.an" VALUE at SAME-SQL exit
   then
   plan plan w WALK-START @ s" p.a" VALUE at SAME-SQL ;

\ The seed as the first step meets it: s.k in the seeds' form, or the
\ end's root's `in` of it where it is a term only an may hold.
: SEED-AT ( ^plan ^walk -- ^binding )
   {: plan w:^ :}
   w WALK-HELD @ 0= if plan w WALK-KIND @ s" s.k" VALUE exit then
   plan w WALK-END @ ROOT {: root:n :}
   plan NEW-SQL {: v:^ :}
   v plan root SHAPE@ SHAPE-IN @ s" s.k" +CALL
   plan AL [type] binding ALLOC:NEW {: b:^ :}
   b -1 root v s" " SET-BINDING b ;

\ Adds to the walk's CTEs nameW(k, e[, n]): each seed k and the ends e of
\ its steps, for `?` one step, else UNION (distinct) each further step.
: WALK-CTE ( ^plan ^walk -- )
   {: plan w:^ :}
   w WALK-OP @ s" ?" STR:= 0= {: more:bool :}
   w WALK-START @ {: start:n :} w WALK-END @ {: end:n :}
   more start 0< end 0< xor and w WALK-ROOTED @ 0= and {: key:bool :}
   plan w s" s" WALK-CTE-NAME {: s:str :}
   plan w s" w" WALK-CTE-NAME {: n:str :}
   plan NEW-SQL {: v:^ :}
   v s" (SELECT s.k AS k,p.b AS e" +SQL
   key if plan v w +KEY then
   v s"  FROM " +SQL v s +SQL v s"  AS s" +SQL
   v w plan w plan w SEED-AT STEP-FROM +STEP
   more if
      v s"  UNION SELECT w.k,p.b" +SQL key if plan v w +KEY then
      v s"  FROM " +SQL v n +SQL v s"  AS w" +SQL
      key if plan start s" w.n" VALUE else plan end s" w.e" VALUE then
         {: at:^ :}
      v w plan w at STEP-FROM +STEP
   then v s" )" +TEXT
   plan AL w WALK-CTES @ n false v +CTE ;

\ An end of the walk whose value in a part is b: a constant meets b; a
\ variable's first end is a column of the form, a later one meets it.
: +END ( ^plan n ^binding n ^VEC:vector<binding> ^VEC:vector<binding>
      ^rel ^rel -- )
   {: plan node:n b:^ form:n seen:^ cols:^ list:^ cond:^ :}
   plan TREE node VARIABLE? 0= if
      cond plan b plan node CONSTANT-VALUE SAME-SQL +CONDITION exit
   then
   plan node VAR {: var:n :}
   seen var FIND-VAR {: k:n :}
   k -1 > if cond plan seen k VEC:AT b SAME-SQL +CONDITION exit then
   b seen COPY-BINDING var seen LAST BINDING-VAR !
   cols var form plan PLAN-EMPTY @ s" " BIND
   list s" ," +SEP plan list b form +VALUE
   list s"  AS " +SQL list var +NAME ;

\ Appends `SELECT ... FROM t WHERE ...`, a part of the walk's answer
\ whose start is k and end e, the end in the form; answers its columns.
: +PART ( ^plan ^walk str ^binding ^binding n ^rel
      -- ^VEC:vector<binding> )
   {: plan w:^ t:str k:^ e:^ form:n v:^ :}
   plan 0 NEW-BINDINGS {: seen:^ :} plan 0 NEW-BINDINGS {: cols:^ :}
   plan NEW-SQL {: list:^ :} plan NEW-SQL {: cond:^ :}
   plan w WALK-SEED @ GIVEN? 0= if
      plan w WALK-SEED @ k w WALK-KIND @ seen cols list cond +END
   then
   plan w WALK-FAR @ e form seen cols list cond +END
   v s" SELECT " +SQL v list +REL v s"  FROM " +SQL v t +SQL
   cond EMPTY? 0= if v s"  WHERE " +SQL v cond +REL then cols ;

\ Gives the rel the walk's CTE list as its WITH RECURSIVE; the final write
\ writes the list as it then stands.
: RECURSIVE! ( ^walk ^rel -- )
   {: w:^ r:^ :}
   w WALK-CTES @ CTES-FIRST @ r REL-WITH ! true r REL-RECURSIVE ! ;

: PARENS ( ^plan ^rel -- ^rel )
   {: plan r:^ :}
   plan TEXTS s" (" TEXT-REL {: out:^ :}
   out r +REL out s" )" +TEXT out ;

\ The walk's answer as a derived table and its columns: its rows, and
\ for `*` and `?` each seed as its own end, the zero-length path. An end
\ is native where the seeds and the steps' ends share a shape.
: ANSWER ( ^plan ^walk -- ^rel ^VEC:vector<binding> )
   {: plan w:^ :}
   w WALK-KIND @ {: kind:n :} w WALK-END @ {: end:n :}
   w PLUS? kind end = or if end else GENERIC then {: form:n :}
   plan NEW-SQL {: v:^ :}
   plan w plan w s" w" WALK-CTE-NAME plan kind s" k" VALUE plan end s" e" VALUE
      form v +PART {: cols:^ :}
   w PLUS? 0= if
      v s"  UNION " +SQL
      plan w plan w s" s" WALK-CTE-NAME plan kind s" k" VALUE
         plan kind s" k" VALUE form v +PART drop
   then
   w WALK-REST @ w WALK-SHARED @ or 0= if w v RECURSIVE! then
   plan v PARENS cols ;

\ r, reading nameR joined with the walk, becomes one derived table under
\ the walk's CTEs.
: WRAP ( ^plan ^walk ^relation -- )
   {: plan w:^ r:^ :}
   plan NEW-SQL {: c:^ :}
   w c RECURSIVE! plan c r +SELECT
   plan c PARENS {: out:^ :}
   r RELATION-BINDINGS @ {: cols:^ :}
   plan NEW-SQL {: from:^ :}
   from plan NEW-SQL plan out cols from +TABLE r INIT-RELATION ;

\ Whether the walk reads no outer binding: it has no rest and an empty
\ substitution, so its seed is a constant or not given.
: SHAREABLE? ( ^plan ^walk -- bool )
   {: plan w:^ :}
   w WALK-REST @ if false exit then
   plan PLAN-OUTER @ BINDS-ANY? 0= ;

\ Whether paths a and b are one path: one operator over equal operands, or
\ one term, which the query holds once.
: SAME-PATH? ( ^syntax n n -- bool )
   {: state a:n b:n :}
   a b = if true exit then
   a 0= b 0= or if false exit then
   state a NODE-K {: k:n :}
   state b NODE-K k <> if false exit then
   k N-PATH = if
      state a TEXT$ state b TEXT$ STR:= 0= if false exit then
   else k N-LIST <> if false exit then then
   state state a NODE-A@ state b NODE-A@ RECURSE 0= if false exit then
   state state a NODE-B@ state b NODE-B@ RECURSE ;

\ Whether shared walks a and b write one nameP, nameS and nameW, names
\ aside: one walk from one seed, a constant or none, in one graph.
: SAME-WALK? ( ^plan ^walk ^walk -- bool )
   {: plan a:^ b:^ :}
   plan TREE {: state :}
   a WALK-OP @ b WALK-OP @ STR:= 0= if false exit then
   a WALK-INVERSE @ b WALK-INVERSE @ xor if false exit then
   a WALK-GRAPH @ b WALK-GRAPH @ STR:= 0= if false exit then
   a WALK-SEED @ {: x:n :} b WALK-SEED @ {: y:n :}
   x y = state x VARIABLE? state y VARIABLE? and or 0= if false exit then
   state a WALK-OPERAND @ b WALK-OPERAND @ SAME-PATH? ;

\ Whether the shared walk is equal to one written before; if so, it takes
\ that one's CTE names and the forms its answer reads.
: SHARE ( ^plan ^walk -- bool )
   {: plan w:^ :}
   plan PLAN-GRAPH @ w WALK-GRAPH !
   plan PLAN-WALKS @ {: ws:^ :}
   ws VEC:COUNT 0 ?do
      ws i VEC:AT @ {: e:^ :}
      plan e w SAME-WALK? if
         e WALK-NAME @ w WALK-NAME ! e WALK-KIND @ w WALK-KIND !
         e WALK-START @ w WALK-START ! e WALK-END @ w WALK-END !
         true unloop exit
      then
   loop false ;

\ Joins to r the walk at index at in items, of a run ending before index to,
\ its ends meeting the substitution; r then drops the variables SETTLED by it.
: WALK-STEP ( ^plan ^VEC:vector<item> n n ^relation -- )
   {: plan items:^ at:n to:n r:^ :}
   plan items at VEC:AT ITEM-NODE @ r NEW-WALK {: w:^ :}
   plan w SHAREABLE? w WALK-SHARED !
   w WALK-SHARED @ if plan PLAN-CTES @ else plan AL NEW-CTES then
      w WALK-CTES !
   w WALK-SHARED @ if plan w SHARE else false then 0= if
      plan w STEPS
      plan w PAIRS-CTE
      w WALK-REST @ if plan w r REST then
      plan items at to w r SEEDS
      plan w WALK-CTE
      w WALK-SHARED @ if w plan PLAN-WALKS @ VEC:PUSH ! then
   then
   plan w ANSWER {: t:^ cols:^ :}
   plan t cols SUBSTITUTED {: a:^ :}
   r RELATION-FROM @ {: from:^ :}
   from s"  CROSS JOIN " +SEP
   plan a cols r JOIN-TABLE
   w WALK-REST @ if plan w r WRAP then ;

\ Whether the walk item has an end r binds always.
: BOUND-END? ( ^plan ^item ^relation -- bool )
   {: plan it:^ r:^ :}
   plan TREE {: state :}
   it ITEM-NODE @ {: triple:n :}
   plan r state triple NODE-A@ BOUND?
   plan r state triple NODE-C@ BOUND? or ;

\ Whether the item is a walk done does not hold and, unless all, one
\ with an end r binds always.
: DUE? ( ^plan ^item n ^VEC:vector<n> bool ^relation -- bool )
   {: plan it:^ at:n done:^ all:bool r:^ :}
   it ITEM-MODE @ N-PATH <> if false exit then
   done at INDEXED? if false exit then
   all if true exit then
   plan it r BOUND-END? ;

\ Folds each walk among items from..to that DUE? takes, under its
\ item's graph; answers done with their indexes added.
: WALKS ( ^plan ^VEC:vector<item> n n ^VEC:vector<n> bool ^relation
      -- ^VEC:vector<n> )
   {: plan items:^ from:n to:n done:^ all:bool r:^ :}
   plan PLAN-GRAPH @ {: outer:str :}
   done to from ?do
      {: now:^ :}
      items i VEC:AT {: it:^ :}
      plan it i now all r DUE? if
         it ITEM-GRAPH @ plan PLAN-GRAPH !
         plan items i to r WALK-STEP
         outer plan PLAN-GRAPH !
         plan now i +INDEX
      else now then
   loop ;

\ Whether the item is a joined triple whose live choices come from
\ several description sources.
: UNION? ( ^plan ^item -- bool )
   {: plan it:^ :}
   it ITEM-MODE @ 0<> it ITEM-NODE @ 0= or if false exit then
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan it ORIGINS LEN 1 > work mark ARENA:CUT ;

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
   it ITEM-COLUMNS @ {: cols:^ :}
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if
         links b BINDING-VAR @ HEAD unloop exit
      then
   loop -1 ;

\ Puts the variables the item binds always in one component.
: LINK ( ^VEC:vector<n> ^item -- )
   {: links:^ it:^ :}
   links it COMPONENT {: head:n :}
   it ITEM-COLUMNS @ {: cols:^ :}
   cols VEC:COUNT 0 ?do
      cols i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if
         head links links b BINDING-VAR @ HEAD VEC:AT !
      then
   loop ;

\ A link per variable: two that a driver among items from..to binds
\ always have one head.
: COMPONENTS ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<n> )
   {: plan items:^ from:n to:n :}
   INDEXES plan PLAN-VARIABLES @ VEC:COUNT 0 ?do plan swap i +INDEX loop
   {: links:^ :}
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

\ Appends to the plan's ctes r's bindings as a CTE, which r then reads.
: HOIST ( ^plan ^relation -- )
   {: plan r:^ :}
   r RELATION-BINDINGS @ {: cols:^ :}
   plan NEXT-ALIAS {: name:str :}
   plan NEW-SQL {: out:^ :}
   out s" (" +TEXT plan out r +SELECT out s" )" +TEXT
   plan AL plan PLAN-CTES @ name true out +CTE
   plan NEW-SQL plan NEW-SQL plan 0 NEW-BINDINGS r INIT-RELATION
   r RELATION-FROM @ name +TEXT
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
   plan NEXT-ALIAS {: name:str :} plan NEW-SQL {: key:^ :} key name +SQL
   plan NEW-SQL {: v:^ :}
   v s" (SELECT DISTINCT " +SQL
   plan v bs bs var FIND-VAR VEC:AT form +VALUE
   v s"  AS k" +SQL
   v r +FROM-WHERE v s" )" +TEXT
   plan AL plan PLAN-CTES @ name true v +CTE
   keys var form key s" " BIND ;

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
   plan PLAN-OUTER @ BINDS-ANY? if false exit then
   plan TREE {: state :}
   it ITEM-NODE @ {: triple:n :}
   plan state triple NODE-A@ GIVEN?
   plan state triple NODE-C@ GIVEN? or 0= ;

\ The list with the item's variables not yet there added.
: +READ-VARS ( ^plan ^item ^VEC:vector<n> -- ^VEC:vector<n> )
   {: plan it:^ vars:^ :}
   plan TREE {: state :}
   vars 3 0 do
      state it ITEM-NODE @ i PART {: node:n :}
      plan node GIVEN? 0= if plan swap plan node VAR +ONCE then
   loop ;

\ The variables that the unions among items from..to that take keys read.
: READ-VARS ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<n> )
   {: plan items:^ from:n to:n :}
   INDEXES to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-UNION @ if
         plan it KEYED? if plan it rot +READ-VARS then
      then
   loop ;

\ The keys r's bindings would give, each in its form, naming no CTE.
: FORMS ( ^plan ^relation -- ^VEC:vector<binding> )
   {: plan r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan 0 NEW-BINDINGS {: keys:^ :}
   bs VEC:COUNT 0 ?do
      bs i VEC:AT BINDING-VAR @ {: var:n :}
      keys var bs var SHAPE-OF plan PLAN-EMPTY @ s" " BIND
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
   INDEXES keys VEC:COUNT 0 ?do
      keys i VEC:AT BINDING-VAR @ {: var:n :}
      hits var INDEXED? if plan swap var +INDEX then
   loop ;

\ Adds to driven the index of each union among items from..to not there
\ that one of keys DRIVES; answers driven and those keys' variables.
: DRIVING ( ^plan ^VEC:vector<item> n n ^VEC:vector<binding>
      ^VEC:vector<n> -- ^VEC:vector<n> ^VEC:vector<n> )
   {: plan items:^ from:n to:n keys:^ driven:^ :}
   driven INDEXES to from ?do
      {: now:^ hits:^ :}
      -1 now i INDEXED? 0= if drop plan keys items i VEC:AT DRIVES then
      {: d:n :}
      d -1 > if
         plan now i +INDEX
         plan hits keys d VEC:AT BINDING-VAR @ +INDEX
      else now hits then
   loop {: out:^ found:^ :} out plan keys found IN-ORDER ;

\ The keys of the unions among items from..to: of each component in
\ turn, those that drive a union no earlier component's keys drive.
: RUN-KEYS ( ^plan ^VEC:vector<item> n n -- ^VEC:vector<binding> )
   {: plan items:^ from:n to:n :}
   plan 0 NEW-BINDINGS {: keys:^ :}
   plan items from to READ-VARS {: vars:^ :}
   vars LEN 0= if keys exit then
   plan items from to COMPONENTS {: links:^ :}
   INDEXES INDEXES vars LEN 0 ?do
      {: done:^ driven:^ :}
      links vars i VEC:AT @ HEAD {: head:n :}
      done head INDEXED? if done driven else
         plan done head +INDEX
         plan items from to links head DRIVERS {: d:^ :}
         plan vars d KEEP-READ
         plan items from to plan d FORMS driven DRIVING
         {: now:^ used:^ :} plan used d keys +KEYS-OF now
      then
   loop 2drop keys ;

\ Joins the union item into r, driven by the run's keys if it takes any;
\ r then drops the variables SETTLED by it.
: UNION-STEP ( ^plan ^item ^VEC:vector<binding> ^relation -- )
   {: plan it:^ run:^ r:^ :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan it KEYED? if run else plan WORK-AL 0 BINDINGS-IN then
   {: keys:^ :}
   plan it keys r UNION-SQL
   work mark ARENA:CUT ;

\ Folds the unions among items from..to, then the walks done does not hold.
: LATE ( ^plan ^VEC:vector<item> n n ^VEC:vector<n> ^relation -- )
   {: plan items:^ from:n to:n done:^ r:^ :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-UNION @ if plan it BRANCHES then
   loop
   plan items from to RUN-KEYS {: keys:^ :}
   to from ?do
      items i VEC:AT {: it:^ :}
      it ITEM-UNION @ if plan it keys r UNION-STEP then
   loop
   plan items from to done true r WALKS drop ;

\ Marks each variable the list binds always.
: +ALWAYS ( ^VEC:vector<binding> ^VEC:vector<bool> -- )
   {: list:^ bound:^ :}
   list VEC:COUNT 0 ?do
      list i VEC:AT {: b:^ :}
      b BINDING-ALWAYS @ if
         true bound b BINDING-VAR @ VEC:AT !
      then
   loop ;

\ Marks in the plan's bound, until the next call, each variable r or a layer
\ binds always; an inner layer's binding is always where an outer one is.
: BOUND-NOW ( ^plan ^relation -- ^VEC:vector<bool> )
   {: plan r:^ :}
   plan PLAN-BOUND @ {: bound:^ :}
   bound 0 VEC:RESIZE
   bound plan PLAN-VARIABLES @ VEC:COUNT VEC:RESIZE
   plan PLAN-OUTER @ begin dup NULL-PTR <> while
      dup LAYER-BINDINGS @ bound +ALWAYS LAYER-UNDER @
   repeat drop
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
   INDEXES items VEC:COUNT 0 ?do
      items i VEC:AT {: it:^ :}
      it ITEM-MODE @ N-PATH = if -1
      else plan it GIVEN-END? if 1 else 0 then then
      {: end:n :} plan swap end +INDEX
   loop ;

: SHARES? ( ^item ^VEC:vector<bool> -- bool )
   {: it:^ bound:^ :}
   it ITEM-COLUMNS @ {: cols:^ :}
   cols VEC:COUNT 0 ?do
      bound cols i VEC:AT BINDING-VAR @ VEC:AT @ if
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
   from ITEM-BRANCHES @ to ITEM-BRANCHES !
   from ITEM-WHERE @ to ITEM-WHERE ! ;

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
         it ITEM-COLUMNS @ bound +ALWAYS
         -1 ends k VEC:AT !
      then
   loop ;

\ Whether the item shares no variable with r's bindings so far.
: UNLINKED? ( ^plan ^item ^relation -- bool )
   {: plan it:^ r:^ :}
   it plan r BOUND-NOW SHARES? 0= ;

\ Folds the run from..to in connected order. A walk with an end r binds
\ always folds before a driver UNLINKED? holds of, which multiplies r's rows.
: RUN ( ^plan ^VEC:vector<item> n n ^relation -- )
   {: plan items:^ from:n to:n r:^ :}
   plan items from to r CONNECT
   INDEXES to from ?do
      {: done:^ :}
      items i VEC:AT {: it:^ :}
      it DRIVER? if
         plan it r UNLINKED? if
            plan items from to done false r WALKS
         else done then
         plan it r JOIN-STEP
      else done then
   loop {: folded:^ :}
   plan items from to folded r LATE ;

\ Folds the items run by run: joins commute; a left join, a minus and an
\ Extend read the solutions so far, so each ends a run.
: FOLD-ALL ( ^plan ^VEC:vector<item> ^relation ^VEC:vector<n>
      -- )
   {: plan items:^ r:^ filters:^ :}
   0 items VEC:COUNT 0 ?do
      items i VEC:AT {: it:^ :}
      plan it UNION? it ITEM-UNION !
      it ITEM-MODE @ {: mode:n :}
      mode 0<> mode N-PATH <> and if
         plan items rot i r RUN plan it r filters FOLD i 1+
      then
   loop
   plan items rot items VEC:COUNT r RUN ;

\ r's solutions are a union's when r folded that one joined item alone.
: KEEP-BRANCHES ( ^VEC:vector<item> ^relation -- )
   {: items:^ r:^ :}
   items VEC:COUNT 1 <> if exit then
   items 0 VEC:AT {: it:^ :}
   it ITEM-MODE @ 0<> if exit then
   it ITEM-BRANCHES @ r RELATION-BRANCHES ! ;

\ The group's items folded in order, and its FILTERs the fold has not applied.
: GROUP ( ^plan n -- ^relation ^VEC:vector<n> )
   {: plan group:n :}
   plan AL 0 [type] item VEC:NEW {: items:^ :}
   plan AL 0 [type] n VEC:NEW {: filters:^ :}
   plan group items filters ITEMS
   plan items DROP-UNFILLED
   plan items PRUNE
   plan NEW-RELATION {: r:^ :}
   items DEAD? r RELATION-EMPTY !
   r RELATION-EMPTY @ 0= if
      plan items r filters FOLD-ALL items r KEEP-BRANCHES
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
   out s" EXISTS (SELECT 1" +SQL
   out r +FROM-WHERE out s" )" +SQL ;

\ The query's LIMIT and OFFSET.
: SLICE-SQL ( ^plan n ^rel -- )
   {: plan query:n v:^ :}
   plan TREE {: state :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: m:n :}
      state m NODE-K {: kind:n :}
      kind N-LIMIT = kind N-OFFSET = or if
         kind N-LIMIT = if v s"  LIMIT " +SQL else v s"  OFFSET " +SQL then
         plan v state m NODE-VALUE@ +NUMBER-ARG
         v s" ::bigint" +SQL
      then state list NODE-B@
   repeat drop ;

\ Appends `ROW(...)` of the anchors of the relation's named variables: a
\ blank node or a path's middle variable is no part of a solution.
: +ROW ( ^plan ^rel ^relation -- )
   {: plan v:^ r:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan PLAN-VARIABLES @ {: vars:^ :}
   v s" ROW(" +SQL
   false bs VEC:COUNT 0 ?do
      bs i VEC:AT {: b:^ :}
      vars b BINDING-VAR @ VEC:AT NAME-TEXT @ s" _:" STR:STARTS-WITH? 0= if
         if v s" ," +SQL then v b +BINDING true
      then
   loop drop v s" )" +SQL ;

\ Appends COUNT's operand: `*`, the named variables' row for DISTINCT *, a
\ bound variable's anchor, native when native, else the expression's term.
: +COUNTED ( ^plan ^rel ^relation n -- )
   {: plan v:^ r:^ call:n :}
   plan TREE {: state :}
   state call NODE-FLAGS@ {: flags:n :}
   flags F-STAR and 0<> if
      flags F-DISTINCT and 0= if v s" *" +SQL exit then
      plan v r +ROW exit
   then
   state state call NODE-A@ NODE-A@ {: e:n :}
   r RELATION-BINDINGS @ {: bs:^ :}
   state e NODE-K N-VAR = if
      bs plan e VAR FIND-VAR {: k:n :}
      k -1 > if v bs k VEC:AT +BINDING exit then
   then v plan r e EXPRESSION-SQL +REL ;

\ Appends GROUP_CONCAT's SEPARATOR, the literal node, or zero for its
\ default.
: +SEPARATOR ( ^plan ^rel n -- )
   {: plan v:^ sep:n :}
   sep 0= if v s" NULL::text" +SQL exit then
   plan v plan TREE sep TEXT$ +TEXT-ARG ;

\ The binding in r of the call's operand, a variable r binds.
: OPERAND-BINDING ( ^plan ^relation n -- ^binding )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   r RELATION-BINDINGS @ {: bs:^ :}
   bs bs plan state state call NODE-A@ NODE-A@ VAR FIND-VAR VEC:AT ;

\ Appends the value of the call's operand in r.
: +OPERAND ( ^plan ^rel ^relation n -- )
   {: plan v:^ r:^ call:n :}
   v plan r call OPERAND-BINDING +BINDING ;

\ The shape the call reads natively over r, else -1: SUM, AVG, MIN or
\ MAX without DISTINCT of a variable r binds always to a column of a
\ NUMBERS? shape, or MIN or MAX of a STRINGS? one.
: NATIVE-SHAPE ( ^plan ^relation n -- n )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   state call NODE-FLAGS@ F-DISTINCT and 0<> if -1 exit then
   state call s" SUM AVG MIN MAX" CALL-IN? 0= if -1 exit then
   plan r state state call NODE-A@ NODE-A@ BOUND? 0= if -1 exit then
   plan r call OPERAND-BINDING BINDING-SHAPE @ {: k:n :}
   k 0< if -1 exit then
   plan k NUMBERS? if k exit then
   state call s" MIN MAX" CALL-IN? plan k STRINGS? and if k exit then
   -1 ;

\ SUM over column c: its datatype decimal where a value has a fractional
\ digit, else integer, and 0 for none.
: SUM-SQL ( ^plan ^rel -- ^rel )
   {: plan c:^ :}
   plan NEW-SQL {: v:^ :}
   v s" sparql.number(coalesce(" +SQL v s" sum" c +APPLY
   v s" ,0),CASE WHEN max(" +SQL v s" min_scale" c +APPLY
   v s" )>0 THEN 'decimal' ELSE 'integer' END)" +SQL v ;

\ AVG over column c as sparql.aggregate answers it: the sum's canonical
\ value over the count, unbound where the sum's text is past the 16,383
\ bytes sparql.numeric reads, an integer 0 for no value.
: AVG-SQL ( ^plan ^rel -- ^rel )
   {: plan c:^ :}
   plan NEW-SQL {: cnt:^ :} cnt s" count" c +APPLY
   plan NEW-SQL {: total:^ :} total s" sum" c +APPLY
   plan NEW-SQL {: v:^ :}
   v s" sparql.number(CASE WHEN " +SQL v cnt +SPLICE
   v s" =0 THEN 0 WHEN octet_length(" +SQL
   v s" sparql.format_decimal" total +APPLY
   v s" )<=16383 THEN " +SQL v s" trim_scale" total +APPLY
   v s" /" +SQL v cnt +SPLICE v s"  END,CASE WHEN " +SQL v cnt +SPLICE
   v s" =0 THEN 'integer' ELSE 'decimal' END)" +SQL v ;

\ The call's min or max of column c of shape k, text by code point.
: EXTREME-SQL ( ^plan n n ^rel -- ^rel )
   {: plan call:n k:n c:^ :}
   plan NEW-SQL {: v:^ :}
   plan TREE call s" MIN" CALL-IS? {: least:bool :}
   least if v s" min(" +SQL else v s" max(" +SQL then
   v c +SPLICE
   plan k STRINGS? if v s\"  COLLATE \"C\"" +SQL then
   v s" )" +SQL v ;

\ The call over column c of shape k, as NATIVE-SHAPE found it.
: NATIVE-SQL ( ^plan n n ^rel -- ^rel )
   {: plan call:n k:n c:^ :}
   plan TREE {: state :}
   state call s" SUM" CALL-IS? if plan c SUM-SQL exit then
   state call s" AVG" CALL-IS? if plan c AVG-SQL exit then
   plan call k c EXTREME-SQL {: e:^ :}
   plan NEW-SQL {: v:^ :}
   v plan k SHAPE@ SHAPE-OUT @ e +APPLY v ;

\ Whether m is an ORDER BY condition whose expression seen does not
\ hold; that expression joins seen.
: NEW-KEY? ( ^plan ^VEC:vector<hashed> n -- bool )
   {: plan seen:^ m:n :}
   plan TREE {: state :}
   state m NODE-K N-ORDER <> if false exit then
   plan seen state m NODE-A@ SEEN? 0= ;

\ List n's ORDER BY keys over r, `,`-joined. An expression an earlier key
\ sorts by adds none: rows that tie on that key hold one value for it.
: CONDITION-KEYS ( ^plan ^relation n -- ^rel )
   {: plan r:^ list:n :}
   plan TREE {: state :}
   plan NEW-SQL {: v:^ :}
   plan list NEW-SEEN {: seen:^ :}
   list begin dup 0<> while
      {: each:n :} state each NODE-A@ {: m:n :}
      plan seen m NEW-KEY? if
         v s" ," +SEP v s" sparql.order(" +SQL
         v plan r state m NODE-A@ EXPRESSION-SQL +REL v s" )" +SQL
         state m NODE-FLAGS@ F-DESC and 0<> if
            v s"  DESC" +SQL
         then
      then state each NODE-B@
   repeat drop v ;

\ array_agg of the call's operand terms over r, in its ORDER BY's order
\ where it has one.
: TERMS-SQL ( ^plan ^relation n -- ^rel )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   plan NEW-SQL {: v:^ :}
   v s" array_agg(" +SQL
   v plan r state state call NODE-A@ NODE-A@ EXPRESSION-SQL +REL
   state call NODE-C@ {: conditions:n :}
   conditions 0<> if
      v s"  ORDER BY " +SQL v plan r conditions CONDITION-KEYS +REL
   then v s" )" +SQL v ;

\ The aggregate call over the relation: COUNT is count, a NATIVE-SHAPE
\ one native; any other goes through sparql.aggregate over array_agg of
\ its operand's terms.
: CALL-SQL ( ^plan ^relation n -- ^rel )
   {: plan r:^ call:n :}
   plan TREE {: state :}
   plan r call NATIVE-SHAPE {: k:n :}
   plan NEW-SQL {: v:^ :}
   k -1 > if
      plan v r call +OPERAND plan call k v NATIVE-SQL exit
   then
   state call NODE-FLAGS@ F-DISTINCT and 0<> {: unique:bool :}
   state call s" COUNT" CALL-IS? if
      v s" sparql.number(count(" +SQL
      unique if v s" DISTINCT " +SQL then
      plan v r call +COUNTED v s" ),'integer')" +SQL v exit
   then
   v s" sparql.aggregate(" +SQL plan v state call TEXT$ +TEXT-ARG
   v s" ," +SQL v plan r call TERMS-SQL +REL
   unique if v s" ,true," +SQL else v s" ,false," +SQL then
   plan v state call NODE-B@ +SEPARATOR v s" )" +SQL v ;

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

\ The call of hash h over the relation, a new aggregate of the plan.
: +AGGREGATE ( ^plan n ^relation n n -- )
   {: plan query:n r:^ call:n h:n :}
   plan r call CALL-SQL {: sql:^ :}
   plan PLAN-AGGREGATES @ VEC:PUSH {: a:^ :}
   call a AGGREGATE-NODE ! h a AGGREGATE-DIGEST ! sql a AGGREGATE-SQL !
   plan query r call BOUND-CALL? a AGGREGATE-ALWAYS !
   plan r call NATIVE-SHAPE a AGGREGATE-NATIVE ! ;

\ The query's aggregates over the relation, into the plan, one for calls
\ that are one.
: AGGREGATES ( ^plan n ^relation -- )
   {: plan query:n r:^ :}
   plan TREE {: state :}
   state query QUERY-AGGREGATES {: calls:n :}
   plan AL 0 [type] aggregate VEC:NEW {: as:^ :}
   as state calls LIST-LENGTH VEC:RESERVE  as plan PLAN-AGGREGATES !
   calls begin dup 0<> while
      {: list:n :} state list NODE-A@ {: call:n :}
      plan call EXPRESSION-HASH {: h:n :}
      plan call h HASHED-AGGREGATE 0< if plan query r call h +AGGREGATE then
      state list NODE-B@
   repeat drop ;

\ Appends the test of each expression of the list over the scope to v.
: CONDITIONS ( ^plan n ^relation ^rel -- )
   {: plan list:n r:^ v:^ :}
   plan TREE {: state :}
   list begin dup 0<> while
      {: each:n :}
      plan r RELATION-BINDINGS @ state each NODE-A@ v false +TEST
      state each NODE-B@
   repeat drop ;

\ Appends HAVING's conditions over the scope to v.
: HAVING-SQL ( ^plan n ^relation ^rel -- )
   {: plan query:n r:^ v:^ :}
   plan TREE {: state :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: m:n :}
      state m NODE-K N-HAVING = if plan state m NODE-A@ r v CONDITIONS then
      state list NODE-B@
   repeat drop ;

\ GROUP BY key e: a bound variable binds its anchor in g, `(e AS ?v)` ?v
\ to e's term; another expression's term goes to keys unless seen has it.
: GROUP-KEY ( ^plan ^relation ^relation ^VEC:vector<hashed> n ^rel -- )
   {: plan r:^ g:^ seen:^ e:n keys:^ :}
   plan TREE {: state :}
   g RELATION-BINDINGS @ {: gs:^ :}
   state e NODE-K N-ALIAS = if
      plan state e NODE-B@ VAR {: var:n :}
      gs var GENERIC plan r state e NODE-A@ EXPRESSION-SQL
      s" " BIND
      plan r state e NODE-A@ SURE? gs LAST BINDING-ALWAYS ! exit
   then
   state e NODE-K N-VAR <> if
      plan seen e SEEN? if exit then
      keys s" ," +SEP keys plan r e EXPRESSION-SQL +REL exit
   then
   r RELATION-BINDINGS @ {: bs:^ :}
   bs plan e VAR FIND-VAR {: k:n :}
   k 0< gs plan e VAR FIND-VAR -1 > or if exit then
   bs k VEC:AT gs COPY-BINDING ;

: GROUP-KEYS ( ^plan n ^relation ^relation ^rel -- )
   {: plan exprs:n r:^ g:^ keys:^ :}
   plan TREE {: state :}
   plan exprs NEW-SEEN {: seen:^ :}
   exprs begin dup 0<> while
      {: list:n :} plan r g seen state list NODE-A@ keys GROUP-KEY
      state list NODE-B@
   repeat drop ;

\ Points g's bindings and the plan's aggregates at their columns under
\ the alias.
: RENAME ( ^plan str ^relation -- )
   {: plan alias:str g:^ :}
   g RELATION-BINDINGS @ {: gs:^ :}
   gs VEC:COUNT 0 ?do
      gs i VEC:AT {: b:^ :}
      plan alias b BINDING-VAR @ COLUMN-OF b SET-SQL
   loop
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      plan NEW-SQL {: c:^ :}
      c alias +SQL c s" .a" +SQL c i +INT
      c as i VEC:AT AGGREGATE-SQL !
   loop ;

\ Appends `SELECT` g's columns and the aggregates `FROM` r, grouped by
\ keys and kept by having.
: +GROUPED-SELECT ( ^plan ^rel ^relation ^relation
      ^rel ^rel -- )
   {: plan out:^ r:^ g:^ keys:^ having:^ :}
   g RELATION-BINDINGS @ {: gs:^ :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   out s" SELECT" +SQL
   gs VEC:COUNT 0 ?do
      i 0<> if out s" ," +SQL then
      gs i VEC:AT {: b:^ :} plan out b b BINDING-SHAPE @ +COLUMN
   loop
   as VEC:COUNT 0 ?do
      gs VEC:COUNT 0<> i 0<> or if out s" ," +SQL then
      out s"  " +SQL out as i VEC:AT AGGREGATE-SQL @ +SPLICE
      out s"  AS a" +SQL out i +INT
   loop
   out r +FROM-WHERE
   keys EMPTY? 0= if out s"  GROUP BY " +TEXT out keys +REL then
   having EMPTY? 0= if out s"  HAVING " +TEXT out having +REL then ;

\ Closes the derived table in from under a new alias, whose columns then
\ bind g's variables and hold the aggregates.
: CLOSE-GROUPS ( ^plan ^rel ^relation -- )
   {: plan from:^ g:^ :}
   plan NEXT-ALIAS {: alias:str :}
   from s" ) AS " +TEXT from alias +TEXT
   plan alias g RENAME ;

\ g's FROM item: the grouped SELECT of g's keys and the aggregates over r.
: GROUPS-FROM ( ^plan ^relation ^relation ^rel
      ^rel -- )
   {: plan r:^ g:^ keys:^ having:^ :}
   g RELATION-FROM @ {: from:^ :}
   from s" (" +TEXT plan from r g keys having +GROUPED-SELECT
   plan from g CLOSE-GROUPS ;

: +KEY-COLUMNS ( ^rel ^relation -- )
   {: keys:^ g:^ :}
   g RELATION-BINDINGS @ {: gs:^ :}
   gs VEC:COUNT 0 ?do
      keys s" ," +SEP keys gs i VEC:AT +BINDING
   loop ;

\ GROUP BY's keys over r: g's bindings, the other keys' terms to keys.
: KEYS-OF ( ^plan n ^relation ^relation ^rel -- )
   {: plan query:n r:^ g:^ keys:^ :}
   plan TREE {: state :}
   state query GROUP-BY-NODE {: grouping:n :}
   grouping 0<> if plan state grouping NODE-A@ r g keys GROUP-KEYS then ;

\ g's FROM item over r, with HAVING. A GROUP BY that leaves no key still
\ answers no group for no solution.
: GROUPS-OF ( ^plan n ^relation ^relation ^rel -- )
   {: plan query:n r:^ g:^ keys:^ :}
   plan NEW-SQL {: having:^ :}
   plan query g having HAVING-SQL
   keys g +KEY-COLUMNS
   plan TREE query GROUP-BY-NODE 0<> keys EMPTY? and if
      having s"  AND " +SEP having s" count(*)>0" +SQL
   then
   plan r g keys having GROUPS-FROM ;

\ The aggregate call over u; when partial, COUNT's bare count and the
\ native min or max of a call native in shape k.
: PART-SQL ( ^plan ^relation n bool n -- ^rel )
   {: plan u:^ call:n partial:bool k:n :}
   plan NEW-SQL {: v:^ :}
   partial k -1 > and if
      plan v u call +OPERAND plan call k v EXTREME-SQL exit
   then
   partial plan TREE call s" COUNT" CALL-IS? and 0= if
      plan u call CALL-SQL exit
   then
   v s" count(" +SQL plan v u call +COUNTED v s" )" +SQL v ;

\ Points the plan's aggregates at their SQL over u.
: PARTS ( ^plan ^relation bool -- )
   {: plan u:^ partial:bool :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      as i VEC:AT {: a:^ :}
      plan u a AGGREGATE-NODE @ partial a AGGREGATE-NATIVE @ PART-SQL
      a AGGREGATE-SQL !
   loop ;

\ Appends branch b's groups, b as the union of r's columns presents it;
\ without a key, a branch with no solution has none.
: +BRANCH-GROUPS ( ^plan n ^rel ^relation ^relation bool -- )
   {: plan query:n from:^ b:^ r:^ partial:bool :}
   plan WORK-ARENA {: work:^ :}
   work ARENA:MARK {: mark:n :}
   plan b r RELATION-BINDINGS @ PRESENTED {: u:^ :}
   plan NEW-RELATION {: gb:^ :}
   plan NEW-SQL {: keys:^ :} plan NEW-SQL {: having:^ :}
   plan query u gb keys KEYS-OF
   plan u partial PARTS
   keys gb +KEY-COLUMNS
   keys EMPTY? if having s" count(*)>0" +SQL then
   plan from u gb keys having +GROUPED-SELECT
   work mark ARENA:CUT ;

\ Appends `(` and the groups of r's branches, a UNION ALL.
: +BRANCHES ( ^plan n ^rel ^relation bool -- )
   {: plan query:n from:^ r:^ partial:bool :}
   r RELATION-BRANCHES @ {: bs:^ :}
   from s" (" +TEXT
   bs LEN 0 ?do
      i 0<> if from s"  UNION ALL " +TEXT then
      plan query from bs i VEC:AT r partial +BRANCH-GROUPS
   loop ;

\ The aggregate call over the partials in column c: the sum of COUNT's
\ counts, the min or max of the native partials of a call native in
\ shape k, else the aggregate of the partial terms.
: COMBINED-SQL ( ^plan n ^rel n -- ^rel )
   {: plan call:n c:^ k:n :}
   plan TREE {: state :}
   k -1 > if plan call k c NATIVE-SQL exit then
   plan NEW-SQL {: v:^ :}
   state call s" COUNT" CALL-IS? if
      v s" sparql.number(coalesce(sum(" +SQL v c +SPLICE
      v s" ),0),'integer')" +SQL v exit
   then
   v s" sparql.aggregate(" +SQL plan v state call TEXT$ +TEXT-ARG
   v s" ,array_agg(" +SQL v c +SPLICE v s" ),false," +SQL
   plan v state call NODE-B@ +SEPARATOR v s" )" +SQL v ;

\ Points each aggregate at its combination over the partials it names.
: COMBINE ( ^plan -- )
   {: plan :}
   plan PLAN-AGGREGATES @ {: as:^ :}
   as VEC:COUNT 0 ?do
      as i VEC:AT {: a:^ :}
      plan a AGGREGATE-NODE @ a AGGREGATE-SQL @ a AGGREGATE-NATIVE @
      COMBINED-SQL a AGGREGATE-SQL !
   loop ;

\ The groups of r's branches, each grouped on its own, HAVING over them.
: SPLIT ( ^plan n ^relation -- ^relation )
   {: plan query:n r:^ :}
   plan NEW-RELATION {: g:^ :}
   plan query r g plan NEW-SQL KEYS-OF
   plan query r AGGREGATES
   g RELATION-FROM @ {: from:^ :}
   plan query from r false +BRANCHES
   plan from g CLOSE-GROUPS
   plan query g g RELATION-WHERE @ HAVING-SQL g ;

\ The groups of the partial groups of r's branches, combined.
: COMBINED ( ^plan n ^relation -- ^relation )
   {: plan query:n r:^ :}
   plan NEW-RELATION {: g:^ :} plan NEW-SQL {: keys:^ :}
   plan query r g keys KEYS-OF
   plan query r AGGREGATES
   plan NEW-RELATION {: p:^ :}
   p RELATION-FROM @ {: from:^ :}
   plan query from r true +BRANCHES
   plan from g CLOSE-GROUPS
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

\ Whether r's union may group per branch, which writes each key and
\ aggregate per branch: none may call EXISTS; their copies stay in MOST.
: BRANCHED? ( ^plan n ^relation -- bool )
   {: plan query:n r:^ :}
   r RELATION-BRANCHES @ LEN {: branches:n :}
   branches 2 < if false exit then
   plan TREE {: state :}
   state query QUERY-AGGREGATES {: calls:n :}
   state query GROUP-BY-NODE {: grouping:n :}
   grouping 0= if 0 else state grouping NODE-A@ then {: keys:n :}
   state calls EXISTS-IN? state keys EXISTS-IN? or if false exit then
   state calls LIST-LENGTH state keys LIST-LENGTH + branches * MOST > 0= ;

\ Whether each branch binds var always to a term no other binds it to.
: APART? ( ^plan ^VEC:vector<relation> n -- bool )
   {: plan branches:^ var:n :}
   INDEXES branches LEN 0 ?do
      branches i VEC:AT RELATION-BINDINGS @ {: bs:^ :}
      bs var FIND-VAR {: k:n :}
      k 0< if drop false unloop exit then
      bs k VEC:AT {: b:^ :}
      b BINDING-SHAPE @ KNOWN <> b BINDING-ALWAYS @ 0= or if
         drop false unloop exit
      then
      plan swap plan b BINDING-TERM @ TERM-PLACE +INDEX
   loop {: places:^ :}
   plan AL plan PLAN-KNOWN @ VEC:COUNT [type] bool VEC:NEW {: seen:^ :}
   places LEN 0 ?do
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
      state call NODE-FLAGS@ F-DISTINCT and 0<> if false exit then
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
   it LIVE-COUNT 0= if
      s RELATION-WHERE @ plan TEXTS s" false" TEXT-REL +AND exit
   then
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
   plan AL 0 [type] n VEC:NEW {: filters:^ :}
   state query NODE-A@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: e:n :}
      state e NODE-K N-ALIAS = if plan r e filters +ALIAS then
      state list NODE-B@
   repeat drop ;

\ ORDER BY's keys over the scope; empty without ORDER BY.
: ORDER-KEYS ( ^plan n ^relation -- ^rel )
   {: plan query:n r:^ :}
   plan r plan TREE query NODE-D@ CONDITION-KEYS ;

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
   plan plan WRITER
   JSON:WRITE-OBJECT-START s" head" JSON:WRITE-KEY
   JSON:WRITE-OBJECT-START s" vars" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   names VEC:COUNT 0 ?do
      i 0<> if JSON:WRITE-COMMA then
      plan TREE names i VEC:AT @ TEXT$ JSON:WRITE-STRING
   loop
   JSON:WRITE-ARRAY-END JSON:WRITE-OBJECT-END JSON:WRITE-COMMA
   s" results" JSON:WRITE-KEY JSON:WRITE-OBJECT-START
   s" bindings" JSON:WRITE-KEY JSON:WRITE-ARRAY-START WRITER$ ;

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
: +NEXT-PAIR ( ^rel n -- )
   {: v:^ n:n :}
   n 0= if exit then
   n OBJECT-PAIRS mod 0= if v s" )||jsonb_build_object(" +SQL exit then
   v s" ," +SQL ;

\ A binding object of the projected variables bound in the relation; an
\ unbound one's NULL member is stripped.
: +OBJECT ( ^plan ^relation ^VEC:vector<n> ^rel -- )
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
         plan v plan TREE node TEXT$ +TEXT-ARG v s" ," +SQL
         plan v bs k VEC:AT +TERM
      then
   loop drop v s" ))::text" +SQL ;

\ The projected columns, native where native, made distinct by GROUP BY; with
\ ORDER BY's keys each keeps its first place, column i, answered, else empty.
: DISTINCT-SQL ( ^plan ^relation ^VEC:vector<n> ^rel -- ^relation ^rel )
   {: plan r:^ names:^ keys:^ :}
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
         cols s" ," +SEP cols s" p" var +NAME-OF
         b ds COPY-BINDING
         plan alias var COLUMN-OF ds LAST SET-SQL
      then
   loop
   d RELATION-FROM @ {: from:^ :}
   from s" (SELECT " +TEXT from cols +REL
   keys EMPTY? 0= if
      cols EMPTY? 0= if from s" ," +TEXT then
      from s" min(p.i) AS i" +TEXT
      inner s" ," +SEP inner s"  row_number() OVER (ORDER BY " +SQL
      inner keys +REL inner s" ) AS i" +SQL
   then
   from s"  FROM (SELECT" +TEXT from inner +REL
   from r +FROM-WHERE from s" ) AS p" +TEXT
   cols EMPTY? 0= if
      from s"  GROUP BY " +TEXT from cols +REL
   else from s"  HAVING count(*)>0" +TEXT then
   from s" ) AS " +TEXT from alias +TEXT
   plan NEW-SQL {: c:^ :}
   keys EMPTY? 0= if c alias +SQL c s" .i" +SQL then d c ;

\ Appends ` x AS vK` for each projected variable r binds, K the variable
\ of its name in frame into, and answers them as columns.
: +PROJECTED ( ^plan ^relation ^VEC:vector<n> n ^rel
      -- ^VEC:vector<binding> )
   {: plan r:^ names:^ into:n v:^ :}
   r RELATION-BINDINGS @ {: bs:^ :}
   plan 0 NEW-BINDINGS {: cols:^ :}
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
: SELECTED ( ^plan n ^relation n -- ^rel ^VEC:vector<binding> bool )
   {: plan query:n r:^ into:n :}
   plan query NAMES {: names:^ :}
   plan query r GROUPS {: s:^ :}
   plan query s EXTEND
   plan query s ORDER-KEYS {: keys:^ :}
   plan NEW-SQL {: w:^ :}
   plan TREE query NODE-FLAGS@ F-DISTINCT and 0<> if
      plan s names keys DISTINCT-SQL dup
   else
      keys EMPTY? 0= if
         w s" row_number() OVER (ORDER BY " +SQL
         w keys +REL w s" )" +SQL
      then s w keys
   then {: d:^ rank:^ order:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (SELECT" +SQL
   plan d names into v +PROJECTED {: cols:^ :}
   rank EMPTY? 0= if
      cols VEC:COUNT 0<> if v s" ," +SQL then
      v s"  " +SQL v rank +REL v s"  AS i" +SQL
   then
   v d +FROM-WHERE
   order EMPTY? 0= if v s"  ORDER BY " +SQL v order +REL then
   plan query v SLICE-SQL
   v s" )" +SQL
   v cols rank EMPTY? 0= ;

\ A statement whose document, its head (parameter k) and `]}}`, passes the
\ byte limit whatever it holds.
: OVERSIZE ( ^rel n n -- )
   {: v:^ k:n bytes:n :}
   v s" SELECT sparql.within(octet_length(" +SQL v k +PARAMETER
   v s" )+3," +SQL v bytes +INT v s" )::text" +SQL ;

\ The document is the head, the bindings joined by commas and `]}}`; each
\ binding passes its running byte total, in the bindings' order, through
\ sparql.within.
: SELECT-SQL ( ^plan n ^relation -- ^rel )
   {: plan query:n r:^ :}
   plan query NAMES {: names:^ :}
   plan names HEAD-JSON {: head:str :}
   plan head STRING-PARAMETER {: ha:n :}
   head STR:LEN 3 + {: fixed:n :}
   plan PLAN-STORE @ STORE-LIMIT @ {: bytes:n :}
   plan NEW-SQL {: v:^ :}
   fixed bytes > if v ha bytes OVERSIZE v exit then
   plan query r plan PLAN-FRAME @ SELECTED
      {: t:^ cols:^ ranked:bool :}
   plan NEW-SQL {: from:^ :}
   plan from plan NEW-SQL plan t cols from +TABLE RELATION-OF {: p:^ :}
   v s" SELECT " +SQL v ha +PARAMETER
   v s" ||coalesce(string_agg(r.b,',' ORDER BY r.n),'')||']}}'" +SQL
   v s"  FROM (SELECT q.b,sparql.within(" +SQL v fixed 1- +INT
   v s" +sum(octet_length(q.b)+1) OVER (" +SQL
   ranked if v s" ORDER BY q.i " +SQL then
   v s" ROWS UNBOUNDED PRECEDING)," +SQL
   v bytes +INT v s" ) AS n FROM (SELECT " +SQL
   plan p names v +OBJECT v s"  AS b" +SQL
   ranked if v s" ,i" +SQL then
   v p +FROM-WHERE v s" ) AS q) AS r" +TEXT v ;

: ASK-SQL ( ^plan n ^relation -- ^rel )
   {: plan query:n r:^ :}
   plan query r GROUPS {: s:^ :}
   plan NEW-SQL {: v:^ :}
   v s" SELECT a.d FROM (SELECT " +SQL
   plan v s\" {\"head\":{},\"boolean\":" +TEXT-ARG
   v s" ||CASE WHEN EXISTS(SELECT" +SQL
   v s +FROM-WHERE
   plan query v SLICE-SQL
   v s" ) THEN 'true}' ELSE 'false}' END AS d) AS a" +SQL
   v s"  WHERE sparql.within(octet_length(a.d)," +SQL
   v plan PLAN-STORE @ STORE-LIMIT @ +INT
   v s" ) IS NOT NULL" +SQL v ;

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
      {: t:^ cols:^ :}
   outer plan PLAN-FRAME ! scope plan PLAN-SCOPE !
   out plan PLAN-OUT ! as plan PLAN-AGGREGATES !
   plan items plan t cols SUBSTITUTED cols false CHILD-ITEM ;

\ Gives every parameter its type, whether or not the statement reads it:
\ emission drops SQL it has written, and PostgreSQL rejects a parameter
\ without one. Nothing reads the CTE, so PostgreSQL does not plan it. A
\ document's head is a parameter, so it has a row. Answers CTE t0.
: +TYPES ( ^plan -- ^cte )
   {: plan :}
   plan PLAN-ARGUMENTS @ {: args:^ :}
   plan NEW-SQL {: v:^ :}
   v s" (VALUES " +SQL
   args SEG-COUNT 0 ?do
      i 0<> if v s" ," +SQL then
      v s" ($" +SQL v i 1+ +INT
      plan i 1+ PARAMETER-NODE 0<> if v s" ::jsonb" +SQL then
      v s" ::text)" +SQL
   loop v s" )" +SQL
   plan AL s" t0" false v NEW-CTE ;

\ The statement: t0 and the plan's CTEs, then the query's text; its WITH
\ is recursive when a shared walk wrote CTEs there.
: STATEMENT ( ^plan -- ^rel )
   {: plan :}
   plan TREE {: state :}
   state SYNTAX-ROOT @ {: query:n :}
   state query NODE-K {: kind:n :}
   plan query SOLUTIONS {: r:^ :}
   kind N-SELECT = if plan query r SELECT-SQL
   else plan query r ASK-SQL then {: t:^ :}
   plan +TYPES {: t0:^ :}
   plan PLAN-CTES @ CTES-FIRST @ t0 CTE-NEXT !
   plan PLAN-WALKS @ VEC:COUNT 0<> if true t REL-RECURSIVE ! then
   t0 t REL-WITH ! t ;

\ Appends the query's statement to the vector, counted first so the arena
\ holds no copy; each parameter's text is valid until the quotation returns.
: EMIT ( ^syntax ^store ^VEC:vector<u8> [ str -- ] -- )
   {: state store:^ sql:^ params :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR {: al:^ :}
   al [type] plan ALLOC:NEW {: plan:^ :}
   state plan PLAN-SYNTAX ! store plan PLAN-STORE !
   al NEW-CTES plan PLAN-CTES ! al NEW-TEXTS plan PLAN-TEXTS !
   plan NEW-SQL plan PLAN-EMPTY !
   al [type] binding ALLOC:NEW plan PLAN-COLUMN !
   state SYNTAX-ARENA @ NEW-WRITER plan PLAN-WRITER !
   al [type] n SEG-NEW plan PLAN-ARGUMENTS !
   al [type] argument SEG-NEW plan PLAN-TEXTED !
   al 0 [type] name VEC:NEW plan PLAN-VARIABLES !
   NULL-PTR plan PLAN-OUTER !
   al 0 [type] aggregate VEC:NEW plan PLAN-AGGREGATES !
   al 0 [type] known-term VEC:NEW plan PLAN-KNOWN !
   plan PLAN-STRINGS {: strings:^ :}
   strings state SYNTAX-ARENA @ INTERNS-INIT
   al 0 [type] node-set VEC:NEW plan PLAN-SETS !
   al 0 [type] node-test VEC:NEW plan PLAN-TESTS !
   al 0 [type] ^walk VEC:NEW plan PLAN-WALKS !
   al 0 [type] bool VEC:NEW plan PLAN-BOUND !
   INDEXES plan PLAN-FRESH !
   al NEW-CURSORS plan PLAN-CURSORS !
   plan STATEMENT {: r:^ :}
   al NULL-PTR NEW-SINK {: count:^ :}
   count r WRITE-REL
   sql  sql VEC:COUNT count SINK-COUNT @ +  VEC:RESERVE
   al sql NEW-SINK r WRITE-REL
   plan PLAN-ARGUMENTS @ {: args:^ :}
   args SEG-COUNT 0 ?do
      plan i 1+ PARAMETER-NODE {: node:n :}
      node 0<> if plan node TERM-JSON
      else plan i 1+ PARAMETER-TEXT then
      params execute
   loop ;

: INSTALL-EMIT ( -- )
   ['] GROUP is GROUP-SQL ['] EXISTS+ is EXISTS-SQL
   ['] SELECT-ITEM is SUBQUERY-ITEM ;

INSTALL-EMIT

: COMPILE-IN ( str str ^VEC:vector<u8> [ str -- ] ^ARENA:arena -- )
   {: query:str store:str sql:^ params arena:^ :}
   query arena PARSE {: state:^ :}
   state store arena READ-STORE sql params EMIT ;

;package
