\ PostgreSQL lowering uses one CTE per algebra step. Query-controlled text
\ is always a parameter; only compiler-owned SQL enters the statement.

require src/lib/sparql/parse.f
require src/lib/json/write.f
require src/lib/fmt.f

package SPARQL
private

STRUCTURE argument DERIVE addr
   FIELD text ptr u8 FIELD size n
;STRUCTURE

STRUCTURE plan DERIVE addr
   FIELD syntax ptr syntax
   FIELD text ptr VEC:vector<u8>
   FIELD arguments ptr VEC:vector<argument>
   FIELD serial n
   FIELD dataset n
   FIELD graph n
   FIELD context n
   FIELD correlated bool
   FIELD scopes bool
   FIELD merged bool
   FIELD aggregates n
   FIELD grouping n
   FIELD grouped bool
   FIELD bound ptr VEC:vector<n>
   FIELD path-subject n
   FIELD path-object n
   FIELD schema ptr u8
   FIELD schema-size n
;STRUCTURE

defer SQL-EXPRESSION ( ptr plan n -- )
defer SQL-PATTERN ( ptr plan n n -- n )
defer SQL-QUERY ( ptr plan n -- n )
defer SQL-PATH ( ptr plan n -- n )
defer PATH-TRIPLE ( ptr plan n n -- n )

: TREE ( ptr plan -- ptr syntax ) PLAN-SYNTAX @ ;

: RAW-SQL+ ( ptr plan ptr u8 n -- )
   {: plan text size:n :}
   plan TREE CHECK-BUDGET
   plan PLAN-TEXT @ {: vector :}
   vector VEC:COUNT {: start:n :}
   vector start size + VEC:ENSURE
   size 0<> if text vector start VEC:AT size BYTE-COPY then ;

: SQL-C ( ptr plan u8 -- )
   swap PLAN-TEXT @ VEC:PUSH c! ;

: SCHEMA+ ( ptr plan -- )
   {: plan :}
   plan 34 SQL-C
   plan plan PLAN-SCHEMA @ plan PLAN-SCHEMA-SIZE @ RAW-SQL+
   plan 34 SQL-C plan 46 SQL-C ;

\ Only compiler-owned templates reach SQL+. @. names the trusted backend
\ schema; query text reaches PARAMETER and is never interpolated here.
: SQL+ ( ptr plan ptr u8 n -- )
   {: plan text size:n :}
   plan s"  " RAW-SQL+
   0 begin dup size < while
      {: start:n :}
      text size s" @." start FIND-SUB-FROM MATCH option
         none OF plan text start + size start - RAW-SQL+ exit ENDOF
         some OF IDX>N ENDOF
      ;MATCH {: at:n :}
      plan text start + at start - RAW-SQL+
      plan SCHEMA+ at 2 +
   repeat drop ;

: SQL-N ( ptr plan n -- ) FMT:INT$ RAW-SQL+ ;

: SQL$ ( ptr plan -- ptr u8 n )
   PLAN-TEXT @ dup 0 VEC:AT swap VEC:COUNT ;

: SQL-NAME ( ptr plan n -- )
   {: plan index:n :}
   plan s" q" SQL+ plan index SQL-N plan s"  " SQL+ ;

: GRAPH-NAME+ ( ptr plan n -- )
   {: plan index:n :}
   plan s" g" SQL+ plan index SQL-N ;

: GRAPH-REF+ ( ptr plan n -- )
   {: plan index:n :}
   plan index GRAPH-NAME+ plan s" .g" SQL+ ;

: CONTEXT-NAME+ ( ptr plan n -- )
   {: plan index:n :}
   plan s" e" SQL+ plan index SQL-N ;

: CONTEXT+ ( ptr plan -- )
   {: plan :}
   plan plan PLAN-CONTEXT @ CONTEXT-NAME+ plan s" .v" SQL+ ;

: CTE ( ptr plan -- n )
   {: plan :}
   1 plan PLAN-SERIAL +!
   0 plan PLAN-BOUND @ VEC:PUSH !
   plan s" ," SQL+
   plan plan PLAN-SERIAL @ SQL-NAME plan s" AS(" SQL+
   plan PLAN-SERIAL @ ;

: CTE-END ( ptr plan -- ) s" )" SQL+ ;

: PARAMETER ( ptr plan ptr u8 n -- n )
   {: plan text size:n :}
   plan PLAN-ARGUMENTS @ VEC:COUNT 0 ?do
      plan PLAN-ARGUMENTS @ i VEC:AT {: existing :}
      text size existing ARGUMENT-TEXT @
      existing ARGUMENT-SIZE @ STR= if i 1+ unloop exit then
   loop
   plan PLAN-ARGUMENTS @ VEC:PUSH {: argument :}
   text argument ARGUMENT-TEXT ! size argument ARGUMENT-SIZE !
   plan PLAN-ARGUMENTS @ VEC:COUNT ;

: PARAMETER+ ( ptr plan ptr u8 n -- )
   {: plan text size:n :}
   plan text size PARAMETER {: index:n :}
   plan s" $" SQL+ plan index SQL-N ;

: STRING+ ( ptr plan ptr u8 n -- )
   {: plan text size:n :}
   plan text size PARAMETER+ plan s" ::text" SQL+ ;

: TYPE$ ( ptr syntax n -- ptr u8 n )
   NODE-K case
      N-IRI of s" uri" endof
      N-BNODE of s" bnode" endof
      N-LITERAL of s" literal" endof
      E-SYNTAX throw
   endcase ;

: TERM-SIZE ( ptr syntax n -- n )
   {: state term:n :}
   state term TEXT$ nip
   state term NODE-A@ dup 0<> if state swap TEXT$ nip +
   else drop then
   state term NODE-B@ dup 0<> if state swap TEXT$ nip +
   else drop then 6 * 256 + ;

: TERM-JSON ( ptr plan n -- ptr u8 n )
   {: plan term:n :} plan TREE {: state :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR {: allocator :}
   allocator [type] JSON:writer ALLOC:NEW {: writer :}
   writer allocator state term TERM-SIZE ALLOC:BYTES
      JSON:WRITE-OPEN
   JSON:WRITE-OBJECT-START
   s" type" state term TYPE$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA
   s" value" state term TEXT$ JSON:WRITE-FIELD-S
   state term NODE-A@ dup 0<> if
      {: datatype:n :}
      JSON:WRITE-COMMA
      s" datatype" state datatype TEXT$ JSON:WRITE-FIELD-S
   else drop then
   state term NODE-B@ dup 0<> if
      {: language:n :}
      JSON:WRITE-COMMA
      s" xml:lang" state language TEXT$ JSON:WRITE-FIELD-S
   else drop then
   JSON:WRITE-OBJECT-END JSON:WRITTEN$ ;

: RDF+ ( ptr plan n -- )
   {: plan term:n :}
   plan s" @.sparql_normalize(" SQL+
   plan term TERM-JSON plan -rot PARAMETER+
   plan s" ::text::jsonb)" SQL+ ;

: VARIABLE$ ( ptr plan n -- ptr u8 n )
   {: plan term:n :} plan TREE {: state :}
   state term NODE-K N-VAR = if state term TEXT$ exit then
   state term NODE NODE-FLAGS @ F-GENERATED and 0<> if
      state term NODE NODE-VALUE @ FMT:INT$
      {: number length:n :}
      state s" @generated:" number length JOIN$
   else state s" @label:" state term TEXT$ JOIN$ then ;

: VARIABLE? ( ptr plan n -- bool )
   swap TREE swap NODE-K dup N-VAR = swap N-BNODE = or ;

: BINDINGS+ ( ptr plan -- )
   dup PLAN-MERGED @ if s" (r.v||t.v)" else s" r.v" then SQL+ ;

: CONTEXT-TERM+ ( ptr plan n -- )
   {: plan term:n :}
   plan PLAN-CONTEXT @ 0= if plan s" NULL::jsonb" SQL+ exit then
   plan s" (" SQL+ plan CONTEXT+ plan s" ->" SQL+
   plan plan term VARIABLE$ STRING+ plan s" )" SQL+ ;

: TERM+ ( ptr plan n -- )
   {: plan term:n :}
   term 0= if plan s" NULL::jsonb" SQL+ exit then
   plan term VARIABLE? if
      plan PLAN-CONTEXT @ 0<> if
         plan s" coalesce(" SQL+ plan term CONTEXT-TERM+
         plan s" ," SQL+
      then
      plan s" (" SQL+ plan BINDINGS+ plan s" ->" SQL+
      plan plan term VARIABLE$ STRING+
      plan s" )" SQL+
      plan PLAN-CONTEXT @ 0<> if plan s" )" SQL+ then exit
   then
   plan TREE term NODE-K N-UNDEF = if
      plan s" NULL::jsonb" SQL+ exit
   then plan term RDF+ ;

: SCOPE+ ( ptr plan n -- )
   {: plan relation:n :}
   plan PLAN-SCOPES @ if
      plan s" (r.u||'/'||" SQL+ plan relation SQL-N
      plan s" ::text||'/'||row_number()OVER()::text) AS u," SQL+
   else plan s" ''::text AS u," SQL+ then ;

: FROM+ ( ptr plan n -- )
   {: plan relation:n :}
   plan s"  FROM " SQL+ plan relation SQL-NAME plan s" r" SQL+ ;

: BOOLEAN+ ( ptr plan -- )
   s" @.sparql_boolean(" SQL+ ;

: CALL+ ( ptr plan ptr u8 n -- )
   {: plan name size:n :}
   plan s" @.sparql_function(" SQL+
   plan name size STRING+ plan s" ,ARRAY[" SQL+ ;

: CALL-END ( ptr plan -- ) s" ]::jsonb[])" SQL+ ;

: SCOPED? ( ptr syntax -- bool )
   {: state :}
   state SYNTAX-NODES @ VEC:COUNT 1 ?do
      state i NODE-K N-CALL = if
         state i s" BNODE" CALL-IS? if true unloop exit then
      then
      state i NODE-K N-BNODE = if true unloop exit then
   loop false ;

: SCHEMA-CHECK ( ptr u8 n -- )
   {: name size:n :}
   size 0 <= if E-SYNTAX throw then
   name c@ dup LETTER? swap 95 = or 0= if E-SYNTAX throw then
   size 0 ?do
      name i + c@ {: char:n :}
      char LETTER? char DIGIT? or char 95 = or 0= if
         E-SYNTAX throw
      then
   loop ;

: PLAN-NEW ( ptr syntax ptr u8 n -- ptr plan )
   {: state schema schema-size:n :}
   schema schema-size SCHEMA-CHECK
   state SYNTAX-ARENA @ ARENA:ALLOCATOR {: allocator :}
   allocator [type] plan ALLOC:NEW {: plan :}
   state plan PLAN-SYNTAX !
   schema plan PLAN-SCHEMA ! schema-size plan PLAN-SCHEMA-SIZE !
   allocator 0 [type] u8 VEC:NEW plan PLAN-TEXT !
   allocator 0 [type] argument VEC:NEW plan PLAN-ARGUMENTS !
   allocator 1 [type] n VEC:NEW plan PLAN-BOUND !
   state SCOPED? plan PLAN-SCOPES ! plan ;

: SEED ( ptr plan -- )
   {: plan :}
   plan s" WITH RECURSIVE q0 AS(SELECT " SQL+
   plan PLAN-CORRELATED @ if
      plan s" r.u" SQL+
      plan PLAN-GRAPH @ dup 0<> if
         plan s" ||'/graph/'||" SQL+ plan swap GRAPH-REF+
         plan s" ::text" SQL+
      else drop then
      plan s" AS u," SQL+ plan BINDINGS+ plan s" AS v" SQL+
   else
      plan PLAN-SCOPES @ if
         plan PLAN-CONTEXT @ dup 0<> if
            plan swap CONTEXT-NAME+ plan s" .u AS u," SQL+
         else drop plan s" gen_random_uuid()::text AS u," SQL+ then
      else plan s" ''::text AS u," SQL+ then
      plan s" '{}'::jsonb AS v" SQL+
   then plan CTE-END ;

: CHILD-PLAN ( ptr plan bool -- ptr plan )
   {: parent correlated:bool :}
   parent TREE parent PLAN-SCHEMA @ parent PLAN-SCHEMA-SIZE @
      PLAN-NEW {: child :}
   parent PLAN-ARGUMENTS @ child PLAN-ARGUMENTS !
   parent PLAN-DATASET @ child PLAN-DATASET !
   parent PLAN-GRAPH @ child PLAN-GRAPH !
   parent PLAN-CONTEXT @ child PLAN-CONTEXT !
   parent PLAN-MERGED @ child PLAN-MERGED !
   correlated child PLAN-CORRELATED ! child ;

: CHILD-SEED ( ptr plan -- )
   dup SEED false swap PLAN-MERGED ! ;

: INTERNAL$ ( ptr plan ptr u8 n n -- ptr u8 n )
   {: plan prefix size:n index:n :}
   index FMT:INT$ {: number length:n :}
   plan TREE prefix size number length JOIN$ ;

: AGGREGATE-KEY$ ( ptr plan n -- ptr u8 n )
   {: plan expression:n :}
   plan s" @aggregate:" expression INTERNAL$ ;

: GROUP-KEY$ ( ptr plan n -- ptr u8 n )
   {: plan expression:n :}
   plan s" @group:" expression INTERNAL$ ;

: NODE-IN? ( ptr syntax n n -- bool )
   {: state node:n list:n :}
   list begin dup 0<> while
      {: current:n :}
      state current NODE-A@ node = if true exit then
      state current NODE-B@
   repeat drop false ;

: BINDING+ ( ptr plan ptr u8 n -- )
   {: plan key size:n :}
   plan s" (" SQL+ plan BINDINGS+ plan s" ->" SQL+
   plan key size STRING+ plan s" )" SQL+ ;

: AGGREGATED+ ( ptr plan n -- bool )
   {: plan expression:n :}
   plan PLAN-GROUPED @ 0= if false exit then
   plan TREE expression plan PLAN-AGGREGATES @ NODE-IN? if
      plan plan expression AGGREGATE-KEY$ BINDING+ true exit
   then false ;

: BOUND-NAMES ( ptr plan n -- n )
   swap PLAN-BOUND @ swap VEC:AT @ ;

: BOUND-NAMES! ( ptr plan n n -- )
   {: plan relation:n names:n :}
   names plan PLAN-BOUND @ relation VEC:AT ! ;

: BOUND-VARIABLE? ( ptr plan n n -- bool )
   {: plan term:n relation:n :}
   plan relation BOUND-NAMES begin dup 0<> while
      {: list:n :}
      plan plan TREE list NODE-A@ VARIABLE$
      plan term VARIABLE$ STR= if true exit then
      plan TREE list NODE-B@
   repeat drop false ;

: GUARANTEE ( ptr plan n n -- )
   {: plan term:n relation:n :}
   plan PLAN-CONTEXT @ 0<> if exit then
   plan term VARIABLE? 0= if exit then
   plan term relation BOUND-VARIABLE? if exit then
   plan TREE term plan relation BOUND-NAMES CONS
      plan relation rot BOUND-NAMES! ;

: INHERIT-BOUNDS ( ptr plan n n -- )
   {: plan source:n destination:n :}
   plan destination plan source BOUND-NAMES BOUND-NAMES! ;

: UNION-BOUNDS ( ptr plan n n -- )
   {: plan source:n destination:n :}
   plan source BOUND-NAMES begin dup 0<> while
      {: list:n :}
      plan plan TREE list NODE-A@ destination GUARANTEE
      plan TREE list NODE-B@
   repeat drop ;

;package
