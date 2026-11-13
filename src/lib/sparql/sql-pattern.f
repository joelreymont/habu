\ Algebra relations carry a binding map v and solution scope u. A graph
\ pattern is compiled once; PostgreSQL owns its joins and row cardinality.

require src/lib/sparql/sql-expression.f

package SPARQL
private

defer GROUP-SQL ( ptr plan n n bool -- n )

: COMPATIBLE+ ( ptr plan -- )
   s" NOT EXISTS(SELECT 1 FROM jsonb_each(r.v)x JOIN jsonb_each(t.v)y USING(key) WHERE x.value<>y.value)" SQL+ ;

: SHARED-BOUND+ ( ptr plan n n -- )
   {: plan left:n right:n :} plan TREE {: state :}
   plan left BOUND-NAMES begin dup 0<> while
      {: list:n :} state list NODE-A@ {: term:n :}
      plan term right BOUND-VARIABLE? if
         plan s" AND(r.v->" SQL+ plan plan term VARIABLE$ STRING+
         plan s" )=(t.v->" SQL+ plan plan term VARIABLE$ STRING+
         plan s" )" SQL+
      then state list NODE-B@
   repeat drop ;

: SHARED+ ( ptr plan -- )
   s" EXISTS(SELECT 1 FROM jsonb_object_keys(r.v)x(k) JOIN jsonb_object_keys(t.v)y(k) USING(k))" SQL+ ;

: MERGE+ ( ptr plan -- ) s" (r.v||t.v) AS v" SQL+ ;

: JOIN+ ( ptr plan n n -- n )
   {: plan left:n right:n :}
   plan CTE {: relation:n :}
   plan left relation INHERIT-BOUNDS
   plan right relation UNION-BOUNDS
   plan s" SELECT " SQL+ plan relation SCOPE+ plan MERGE+
   plan left FROM+ plan s" JOIN " SQL+
   plan right SQL-NAME plan s" t ON " SQL+
   plan COMPATIBLE+ plan left right SHARED-BOUND+
   plan CTE-END relation ;

: PATTERN-TERM+ ( ptr plan n -- )
   {: plan term:n :}
   term 0= if plan 0 TERM+ exit then
   plan term VARIABLE? if plan term CONTEXT-TERM+
   else plan term TERM+ then ;

: MATCH-START+ ( ptr plan n n n -- )
   {: plan subject:n predicate:n object:n :}
   plan s" @.sparql_match(" SQL+
   plan subject PATTERN-TERM+ plan s" ," SQL+
   plan predicate PATTERN-TERM+ plan s" ," SQL+
   plan object PATTERN-TERM+ plan s" ," SQL+ ;

: MATCH+ ( ptr plan n n n n -- )
   {: plan subject:n predicate:n object:n graph:n :}
   plan subject predicate object MATCH-START+
   plan graph TERM+ plan s" )" SQL+ ;

: DEFAULT-DATASET? ( ptr plan -- bool )
   {: plan :} plan TREE {: state :}
   plan PLAN-DATASET @ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: dataset:n :}
      state dataset NODE NODE-FLAGS @ F-NAMED and 0= if
         true exit
      then state list NODE-B@
   repeat drop false ;

: MERGED-TERM+ ( ptr plan n ptr u8 n -- )
   {: plan graph:n column size:n :}
   plan s" CASE WHEN m." SQL+ plan column size SQL+
   plan s" ->>'type'='bnode' THEN @.sparql_bnode(jsonb_build_object('type','literal','value',m." SQL+
   plan column size SQL+ plan s" ->>'value'),'dataset/'||" SQL+
   plan plan TREE graph TEXT$ STRING+
   plan s" ) ELSE m." SQL+ plan column size SQL+ plan s" END" SQL+ ;

: MERGED-MATCH+ ( ptr plan n n n n -- )
   {: plan subject:n predicate:n object:n graph:n :}
   plan s" SELECT " SQL+ plan graph s" s" MERGED-TERM+
   plan s" AS s,m.p," SQL+ plan graph s" o" MERGED-TERM+
   plan s" AS o FROM " SQL+
   plan PLAN-CONTEXT @ {: context:n :}
   0 plan PLAN-CONTEXT !
   plan subject predicate object graph MATCH+
   context plan PLAN-CONTEXT ! plan s" m" SQL+ ;

: DEFAULT-MATCHES+ ( ptr plan n n n -- )
   {: plan subject:n predicate:n object:n :}
   plan TREE {: state :}
   plan PLAN-DATASET @ 0= if
      plan subject predicate object 0 MATCH+ exit
   then
   plan s" (SELECT DISTINCT s,p,o FROM(" SQL+
   plan DEFAULT-DATASET? if
      false plan PLAN-DATASET @ begin dup 0<> while
         {: written:bool list:n :}
         state list NODE-A@ {: dataset:n :}
         state dataset NODE NODE-FLAGS @ F-NAMED and 0= if
            written if plan s" UNION ALL " SQL+ then
            plan subject predicate object
               state dataset NODE-A@ MERGED-MATCH+ true
         else written then
         state list NODE-B@
      repeat 2drop
   else
      plan s" SELECT NULL::jsonb s,NULL::jsonb p,NULL::jsonb o WHERE false" SQL+
   then plan s" )dataset)" SQL+ ;

: SOURCE+ ( ptr plan n n n -- )
   {: plan subject:n predicate:n object:n :}
   plan PLAN-GRAPH @ dup 0<> if
      {: graph:n :}
      plan subject predicate object MATCH-START+
      plan graph GRAPH-REF+ plan s" )" SQL+
   else drop plan subject predicate object DEFAULT-MATCHES+ then ;

: BIND-COLUMN+ ( ptr plan n ptr u8 n -- )
   {: plan term:n column size:n :}
   plan term VARIABLE? if
      plan s" ||" SQL+
      plan PLAN-CONTEXT @ 0<> if
         plan s" CASE WHEN " SQL+ plan term CONTEXT-TERM+
         plan s" IS NOT NULL THEN '{}'::jsonb ELSE " SQL+
      then
      plan s" jsonb_build_object(" SQL+
      plan plan term VARIABLE$ STRING+ plan s" ,t." SQL+
      plan column size SQL+ plan s" )" SQL+
      plan PLAN-CONTEXT @ 0<> if plan s" END" SQL+ then
   then ;

: SAME-VARIABLE? ( ptr plan n n -- bool )
   {: plan left:n right:n :}
   plan left VARIABLE? plan right VARIABLE? and 0= if
      false exit
   then plan left VARIABLE$ plan right VARIABLE$ STR= ;

: REPEATED-VAR+ ( ptr plan n n ptr u8 n -- )
   {: plan left:n right:n test size:n :}
   plan left right SAME-VARIABLE? if
      plan s" AND " SQL+ plan test size SQL+
   then ;

: BOUND-COLUMN+ ( ptr plan n n ptr u8 n -- )
   {: plan term:n input:n column size:n :}
   plan term VARIABLE? 0= if exit then
   plan s" AND (" SQL+
   plan term input BOUND-VARIABLE? 0= if
      plan term TERM+ plan s" IS NULL OR " SQL+
   then
   plan term TERM+ plan s" =t." SQL+
   plan column size SQL+ plan s" )" SQL+ ;

: TRIPLE-SQL ( ptr plan n n -- n )
   {: plan triple:n input:n :} plan TREE {: state :}
   state triple NODE-A@ {: subject:n :}
   state triple NODE-B@ {: predicate:n :}
   state triple NODE-C@ {: object:n :}
   state predicate NODE-K N-PATH = if
      plan triple input PATH-TRIPLE exit
   then
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan subject relation GUARANTEE
   plan predicate relation GUARANTEE
   plan object relation GUARANTEE
   plan s" SELECT " SQL+ plan relation SCOPE+
   plan s" r.v" SQL+
   plan subject s" s" BIND-COLUMN+
   plan predicate s" p" BIND-COLUMN+
   plan object s" o" BIND-COLUMN+
   plan s" AS v" SQL+ plan input FROM+
   plan s" CROSS JOIN LATERAL " SQL+
   plan subject predicate object SOURCE+ plan s" t WHERE true " SQL+
   plan subject input s" s" BOUND-COLUMN+
   plan predicate input s" p" BOUND-COLUMN+
   plan object input s" o" BOUND-COLUMN+
   plan subject predicate s" t.s=t.p" REPEATED-VAR+
   plan subject object s" t.s=t.o" REPEATED-VAR+
   plan predicate object s" t.p=t.o" REPEATED-VAR+
   plan CTE-END relation ;

: FILTER-SQL ( ptr plan n n -- n )
   {: plan expression:n input:n :}
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan s" SELECT r.u,r.v" SQL+ plan input FROM+
   plan s" WHERE " SQL+ plan expression EBV+
   plan CTE-END relation ;

: BIND-SQL ( ptr plan n n -- n )
   {: plan binding:n input:n :} plan TREE {: state :}
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan s" SELECT r.u,r.v||" SQL+
   plan PLAN-CONTEXT @ 0<> if
      plan s" CASE WHEN " SQL+
      plan state binding NODE-B@ CONTEXT-TERM+
      plan s" IS NOT NULL THEN '{}'::jsonb ELSE " SQL+
   then
   plan s" jsonb_strip_nulls(jsonb_build_object(" SQL+
   plan plan state binding NODE-B@ VARIABLE$ STRING+
   plan s" ," SQL+
   plan state binding NODE-A@ SQL-EXPRESSION
   plan s" ))" SQL+
   plan PLAN-CONTEXT @ 0<> if plan s" END" SQL+ then
   plan s" AS v" SQL+ plan input FROM+
   plan CTE-END relation ;

: GROUP-FILTERS+ ( ptr plan n -- )
   {: plan group:n :} plan TREE {: state :}
   true plan PLAN-MERGED !
   state group NODE-A@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: pattern:n :}
      state pattern NODE-K N-FILTER = if
         plan s" AND " SQL+ plan state pattern NODE-A@ EBV+
      then state list NODE-B@
   repeat drop false plan PLAN-MERGED ! ;

: OPTIONAL-SQL ( ptr plan n n -- n )
   {: plan group:n input:n :}
   plan group 0 false GROUP-SQL {: right:n :}
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan s" SELECT " SQL+ plan relation SCOPE+
   plan s" r.v||coalesce(t.v,'{}'::jsonb) AS v" SQL+
   plan input FROM+ plan s" LEFT JOIN " SQL+
   plan right SQL-NAME plan s" t ON " SQL+
   plan COMPATIBLE+ plan input right SHARED-BOUND+
   plan group GROUP-FILTERS+
   plan CTE-END relation ;

: MINUS-SQL ( ptr plan n n -- n )
   {: plan group:n input:n :}
   plan group 0 true GROUP-SQL {: right:n :}
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan s" SELECT r.u,r.v" SQL+ plan input FROM+
   plan s" WHERE NOT EXISTS(SELECT 1 FROM " SQL+
   plan right SQL-NAME plan s" t WHERE " SQL+
   plan SHARED+ plan s" AND " SQL+ plan COMPATIBLE+
   plan s" )" SQL+ plan CTE-END relation ;

: UNION-SQL ( ptr plan n n -- n )
   {: plan union:n input:n :} plan TREE {: state :}
   plan state union NODE-A@ 0 SQL-PATTERN {: left:n :}
   plan state union NODE-B@ 0 SQL-PATTERN {: right:n :}
   plan CTE {: relation:n :}
   plan s" SELECT * FROM " SQL+ plan left SQL-NAME
   plan s" UNION ALL SELECT * FROM " SQL+ plan right SQL-NAME
   plan CTE-END
   plan input relation JOIN+ ;

: NAMED-RESTRICTION+ ( ptr plan n -- )
   {: plan graph:n :} plan TREE {: state :}
   plan PLAN-DATASET @ 0= if exit then
   plan s" AND " SQL+ plan graph GRAPH-REF+
   plan s" IN(NULL::jsonb" SQL+
   plan PLAN-DATASET @ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: dataset:n :}
      state dataset NODE NODE-FLAGS @ F-NAMED and 0<> if
         plan s" ," SQL+ plan state dataset NODE-A@ RDF+
      then state list NODE-B@
   repeat drop plan s" )" SQL+ ;

: GRAPH-ID ( ptr syntax -- n )
   dup SYNTAX-SERIAL 1 swap +! SYNTAX-SERIAL @ ;

: GRAPH-MAP+ ( ptr plan n n -- )
   {: plan term:n graph:n :}
   plan s" t.v" SQL+
   plan term VARIABLE? if
      plan s" ||" SQL+
      plan PLAN-CONTEXT @ 0<> if
         plan s" CASE WHEN " SQL+ plan term CONTEXT-TERM+
         plan s" IS NOT NULL THEN '{}'::jsonb ELSE " SQL+
      then
      plan s" jsonb_build_object(" SQL+
      plan plan term VARIABLE$ STRING+ plan s" ," SQL+
      plan graph GRAPH-REF+ plan s" )" SQL+
      plan PLAN-CONTEXT @ 0<> if plan s" END" SQL+ then
   then ;

: GRAPH-TEST+ ( ptr plan n n -- )
   {: plan term:n graph:n :}
   plan s" WHERE " SQL+ plan graph GRAPH-REF+
   plan s" IS NOT NULL AND(" SQL+
   plan term TERM+ plan s" IS NULL OR " SQL+
   plan graph GRAPH-REF+ plan s" =" SQL+
   plan term TERM+ plan s" )" SQL+
   plan term VARIABLE? if
      plan s" AND(t.v->" SQL+ plan plan term VARIABLE$ STRING+
      plan s" IS NULL OR t.v->" SQL+
      plan plan term VARIABLE$ STRING+ plan s" =" SQL+
      plan graph GRAPH-REF+ plan s" )" SQL+
   then plan graph NAMED-RESTRICTION+ ;

: GRAPH-SQL ( ptr plan n n -- n )
   {: plan node:n input:n :} plan TREE {: state :}
   state node NODE-A@ {: term:n :}
   state GRAPH-ID {: graph:n :}
   plan true CHILD-PLAN {: child :}
   graph child PLAN-GRAPH ! child CHILD-SEED
   child state node NODE-B@ 0 true GROUP-SQL {: body:n :}
   child s" SELECT u,v FROM " SQL+ child body SQL-NAME
   plan CTE {: relation:n :}
   plan relation child body BOUND-NAMES BOUND-NAMES!
   plan term relation GUARANTEE
   plan s" SELECT t.u," SQL+ plan term graph GRAPH-MAP+
   plan s" AS v" SQL+ plan 0 FROM+
   plan s" CROSS JOIN @.sparql_graphs()" SQL+
   plan graph GRAPH-NAME+
   plan s" CROSS JOIN LATERAL(" SQL+ plan child SQL$ SQL+
   plan s" )t" SQL+ plan term graph GRAPH-TEST+
   plan CTE-END plan input relation JOIN+ ;

: VALUE-MAP+ ( ptr plan n n -- )
   {: plan names:n values:n :} plan TREE {: state :}
   plan s" jsonb_strip_nulls(jsonb_build_object(" SQL+
   names values begin over 0<> while
      {: variables:n row:n :}
      plan plan state variables NODE-A@ VARIABLE$ STRING+
      plan s" ," SQL+ plan state row NODE-A@ TERM+
      state variables NODE-B@ state row NODE-B@
      over 0<> if plan s" ," SQL+ then
   repeat 2drop plan s" ))" SQL+ ;

: CONTEXT-VALUES ( ptr plan n -- n )
   {: plan input:n :}
   plan PLAN-CONTEXT @ 0= if input exit then
   plan CTE {: relation:n :}
   plan s" SELECT r.u,r.v-ARRAY(SELECT jsonb_object_keys(" SQL+
   plan CONTEXT+ plan s" )) AS v" SQL+ plan input FROM+
   plan s" WHERE NOT EXISTS(SELECT 1 FROM jsonb_each(r.v)x JOIN jsonb_each(" SQL+
   plan CONTEXT+ plan s" )y USING(key) WHERE x.value<>y.value)" SQL+
   plan CTE-END relation ;

: VALUES-SQL ( ptr plan n n -- n )
   {: plan node:n input:n :} plan TREE {: state :}
   state node NODE-A@ {: names:n :}
   state node NODE-B@ {: rows:n :}
   plan CTE {: relation:n :}
   rows 0= if
      plan s" SELECT ''::text u,'{}'::jsonb v WHERE false" SQL+
   else
      rows begin dup 0<> while
         {: list:n :}
         plan s" SELECT ''::text u," SQL+
         plan names state list NODE-A@ VALUE-MAP+
         plan s" AS v" SQL+
         state list NODE-B@ dup 0<> if plan s" UNION ALL " SQL+ then
      repeat drop
   then plan CTE-END
   plan relation CONTEXT-VALUES {: matched:n :}
   plan input matched JOIN+ ;

: READ-GROUP-SQL ( ptr plan n n bool -- n )
   {: plan group:n input:n filters:bool :} plan TREE {: state :}
   input state group NODE-A@ begin dup 0<> while
      {: previous:n list:n :}
      state list NODE-A@ {: pattern:n :}
      state pattern NODE-K N-FILTER <> if
         plan pattern previous SQL-PATTERN
      else previous then state list NODE-B@
   repeat drop
   filters if
      state group NODE-A@ begin dup 0<> while
         {: previous:n list:n :}
         state list NODE-A@ {: pattern:n :}
         state pattern NODE-K N-FILTER = if
            plan state pattern NODE-A@ previous FILTER-SQL
         else previous then state list NODE-B@
      repeat drop
   then ;

' READ-GROUP-SQL is GROUP-SQL

: PATTERN-SQL-BODY ( ptr plan n n -- n )
   {: plan pattern:n input:n :} plan TREE {: state :}
   state pattern NODE-K case
      N-TRIPLE of plan pattern input TRIPLE-SQL endof
      N-BIND of plan pattern input BIND-SQL endof
      N-VALUES of plan pattern input VALUES-SQL endof
      N-GRAPH of plan pattern input GRAPH-SQL endof
      N-OPTIONAL of
         plan state pattern NODE-A@ input OPTIONAL-SQL
      endof
      N-MINUS of plan state pattern NODE-A@ input MINUS-SQL endof
      N-UNION of plan pattern input UNION-SQL endof
      N-GROUP of
         plan pattern 0 true GROUP-SQL {: right:n :}
         plan input right JOIN+
      endof
      N-SELECT of plan input plan pattern SQL-QUERY JOIN+ endof
      N-SERVICE of
         state pattern NODE NODE-FLAGS @ F-SILENT and 0<> if
            input
         else E-SERVICE throw then
      endof
      E-SYNTAX throw
   endcase ;

: PATTERN-SQL ( ptr plan n n -- n )
   {: plan pattern:n input:n :} plan TREE NEST
   plan pattern input PATTERN-SQL-BODY plan TREE UNNEST ;

' PATTERN-SQL is SQL-PATTERN

;package
