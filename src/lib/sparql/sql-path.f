\ Fixed paths retain bag multiplicity. Recursive paths use UNION so each
\ reachable pair is emitted once, including cycles; PostgreSQL owns the walk.

require src/lib/sparql/sql-pattern.f

package SPARQL
private

: PATH-IS? ( ptr plan n ptr u8 n -- bool ) INFIX-IS? ;

: EDGE-SQL ( ptr plan n -- n )
   {: plan predicate:n :}
   plan CTE {: relation:n :}
   plan s" SELECT t.s,t.o" SQL+ plan 0 FROM+
   plan s" CROSS JOIN LATERAL " SQL+
   plan 0 predicate 0 SOURCE+ plan s" t" SQL+
   plan CTE-END relation ;

: INVERSE-SQL ( ptr plan n -- n )
   {: plan path:n :}
   plan plan TREE path NODE-A@ SQL-PATH {: child:n :}
   plan CTE {: relation:n :}
   plan s" SELECT o AS s,s AS o FROM " SQL+
   plan child SQL-NAME plan CTE-END relation ;

: COMPOSE-PATH ( ptr plan n -- n )
   {: plan path:n :} plan TREE {: state :}
   plan state path NODE-A@ SQL-PATH {: left:n :}
   plan state path NODE-B@ SQL-PATH {: right:n :}
   plan CTE {: relation:n :}
   plan path s" |" PATH-IS? if
      plan s" SELECT * FROM " SQL+ plan left SQL-NAME
      plan s" UNION ALL SELECT * FROM " SQL+ plan right SQL-NAME
   else
      plan s" SELECT l.s,r.o FROM " SQL+ plan left SQL-NAME
      plan s" l JOIN " SQL+ plan right SQL-NAME
      plan s" r ON l.o=r.s" SQL+
   then plan CTE-END relation ;

: ZERO-TERM+ ( ptr plan n -- )
   2dup VARIABLE? if CONTEXT-TERM+ else RDF+ then ;

\ A variable joined from VALUES/BIND remains a variable in Path. Only an
\ RDF endpoint, including EXISTS substitution, can add an absent graph node.
: ZERO-INPUT+ ( ptr plan n -- )
   {: plan term:n :}
   term 0= if exit then
   plan term VARIABLE? plan PLAN-CONTEXT @ 0= and if exit then
   plan s" UNION SELECT " SQL+ plan term ZERO-TERM+
   plan s" AS n" SQL+ plan 0 FROM+
   plan s" WHERE " SQL+ plan term ZERO-TERM+
   plan s" IS NOT NULL" SQL+ ;

: ZERO-SQL ( ptr plan -- n )
   {: plan :}
   plan 0 EDGE-SQL {: edges:n :}
   plan CTE {: relation:n :}
   plan s" SELECT n AS s,n AS o FROM(SELECT s AS n FROM " SQL+
   plan edges SQL-NAME
   plan s" UNION SELECT o AS n FROM " SQL+ plan edges SQL-NAME
   plan plan PLAN-PATH-SUBJECT @ ZERO-INPUT+
   plan plan PLAN-PATH-OBJECT @ ZERO-INPUT+
   plan s" )nodes" SQL+ plan CTE-END relation ;

: RECURSIVE-PATH ( ptr plan n -- n )
   {: plan edges:n :}
   plan CTE {: relation:n :}
   plan s" SELECT s,o FROM " SQL+ plan edges SQL-NAME
   plan s" UNION SELECT w.s,e.o FROM " SQL+
   plan relation SQL-NAME plan s" w JOIN " SQL+
   plan edges SQL-NAME
   plan s" e ON w.o=e.s" SQL+
   plan CTE-END relation ;

: REPEATED-PATH ( ptr plan n -- n )
   {: plan path:n :}
   plan plan TREE path NODE-A@ SQL-PATH {: edges:n :}
   plan path s" ?" PATH-IS? if edges
   else plan edges RECURSIVE-PATH then {: positive:n :}
   plan path s" +" PATH-IS? if positive exit then
   plan ZERO-SQL {: zero:n :}
   plan CTE {: relation:n :}
   plan s" SELECT * FROM " SQL+ plan zero SQL-NAME
   plan s" UNION SELECT * FROM " SQL+ plan positive SQL-NAME
   plan CTE-END relation ;

: NEGATED-DIRECTION? ( ptr plan n bool -- bool )
   {: plan path:n inverse:bool :} plan TREE {: state :}
   path 0= if inverse 0= exit then
   state path NODE-K N-IRI = if inverse 0= exit then
   plan path s" ^" PATH-IS? if inverse exit then
   plan path s" |" PATH-IS? 0= if E-SYNTAX throw then
   plan state path NODE-A@ inverse recurse
   plan state path NODE-B@ inverse recurse or ;

: EXCLUSIONS+ ( ptr plan n bool -- )
   {: plan path:n inverse:bool :} plan TREE {: state :}
   path 0= if exit then
   state path NODE-K N-IRI = if
      inverse 0= if plan s" ," SQL+ plan path RDF+ then exit
   then
   plan path s" ^" PATH-IS? if
      inverse if
         plan s" ," SQL+ plan state path NODE-A@ RDF+
      then exit
   then
   plan path s" |" PATH-IS? 0= if E-SYNTAX throw then
   plan state path NODE-A@ inverse recurse
   plan state path NODE-B@ inverse recurse ;

: NEGATIVE-BRANCH+ ( ptr plan n bool -- )
   {: plan path:n inverse:bool :}
   inverse if plan s" SELECT DISTINCT t.o AS s,t.s AS o" SQL+
   else plan s" SELECT DISTINCT t.s,t.o" SQL+ then
   plan 0 FROM+ plan s" CROSS JOIN LATERAL " SQL+
   plan 0 0 0 SOURCE+
   plan s" t WHERE NOT(t.p=ANY(ARRAY['{}'::jsonb" SQL+
   plan path inverse EXCLUSIONS+
   plan s" ]::jsonb[]))" SQL+ ;

: NEGATED-PATH ( ptr plan n -- n )
   {: plan path:n :}
   plan TREE path NODE-A@ {: members:n :}
   plan members false NEGATED-DIRECTION? {: forward:bool :}
   plan members true NEGATED-DIRECTION? {: backward:bool :}
   plan CTE {: relation:n :}
   forward if plan members false NEGATIVE-BRANCH+ then
   backward if
      forward if plan s" UNION ALL " SQL+ then
      plan members true NEGATIVE-BRANCH+
   then plan CTE-END relation ;

: PATH-SQL-BODY ( ptr plan n -- n )
   {: plan path:n :}
   plan TREE path NODE-K N-IRI = if plan path EDGE-SQL exit then
   plan path s" ^" PATH-IS? if plan path INVERSE-SQL exit then
   plan path s" /" PATH-IS? plan path s" |" PATH-IS? or if
      plan path COMPOSE-PATH exit
   then
   plan path s" !" PATH-IS? if plan path NEGATED-PATH exit then
   plan path s" ?" PATH-IS? plan path s" *" PATH-IS? or
   plan path s" +" PATH-IS? or if
      plan path REPEATED-PATH
   else E-SYNTAX throw then ;

: PATH-SQL ( ptr plan n -- n )
   {: plan path:n :} plan TREE NEST
   plan path PATH-SQL-BODY plan TREE UNNEST ;

' PATH-SQL is SQL-PATH

: PATH-ENDPOINT+ ( ptr plan n n ptr u8 n -- )
   {: plan term:n input:n column size:n :}
   plan term VARIABLE? if
      plan term input column size BOUND-COLUMN+ exit
   then
   plan s" AND t." SQL+
   plan column size SQL+ plan s" =" SQL+ plan term RDF+ ;

: PATH-TRIPLE-SQL ( ptr plan n n -- n )
   {: plan triple:n input:n :} plan TREE {: state :}
   state triple NODE-A@ {: subject:n :}
   state triple NODE-C@ {: object:n :}
   plan true CHILD-PLAN {: child :}
   subject child PLAN-PATH-SUBJECT ! object child PLAN-PATH-OBJECT !
   child CHILD-SEED
   child state triple NODE-B@ SQL-PATH {: path:n :}
   child s" SELECT s,o FROM " SQL+ child path SQL-NAME
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan subject relation GUARANTEE plan object relation GUARANTEE
   plan s" SELECT " SQL+ plan relation SCOPE+
   plan s" r.v" SQL+ plan subject s" s" BIND-COLUMN+
   plan object s" o" BIND-COLUMN+ plan s" AS v" SQL+
   plan input FROM+ plan s" CROSS JOIN LATERAL(" SQL+
   plan child SQL$ SQL+ plan s" )t WHERE true " SQL+
   plan subject input s" s" PATH-ENDPOINT+
   plan object input s" o" PATH-ENDPOINT+
   plan subject object s" t.s=t.o" REPEATED-VAR+
   plan CTE-END relation ;

' PATH-TRIPLE-SQL is PATH-TRIPLE

;package
