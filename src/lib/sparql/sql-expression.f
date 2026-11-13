\ Expression errors are SQL NULL terms. Boolean wrapping preserves NULL;
\ CASE and COALESCE keep SPARQL's selected terms and short-circuiting.

require src/lib/sparql/sql.f

package SPARQL
private

: EXPRESSIONS+ ( ptr plan n -- )
   {: plan expressions:n :} plan TREE {: state :}
   expressions begin dup 0<> while
      {: list:n :}
      plan state list NODE-A@ SQL-EXPRESSION
      state list NODE-B@ dup 0<> if plan s" ," SQL+ then
   repeat drop ;

: EBV+ ( ptr plan n -- )
   {: plan expression:n :}
   plan s" @.sparql_ebv(" SQL+
   plan expression SQL-EXPRESSION plan s" )" SQL+ ;

: ARGUMENT@ ( ptr syntax n n -- n )
   {: state call:n index:n :}
   state call NODE-A@
   index 0 ?do state swap NODE-B@ loop state swap NODE-A@ ;

: IF+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   plan s" (CASE " SQL+ plan state call 0 ARGUMENT@ EBV+
   plan s" WHEN true THEN " SQL+
   plan state call 1 ARGUMENT@ SQL-EXPRESSION
   plan s" WHEN false THEN " SQL+
   plan state call 2 ARGUMENT@ SQL-EXPRESSION
   plan s" END)" SQL+ ;

: COALESCE+ ( ptr plan n -- )
   {: plan call:n :}
   plan TREE call CALL-COUNT 0= if
      plan s" NULL::jsonb" SQL+ exit
   then
   plan s" coalesce(" SQL+
   plan plan TREE call NODE-A@ EXPRESSIONS+ plan s" )" SQL+ ;

: EXISTS+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   1 state SYNTAX-SERIAL +! state SYNTAX-SERIAL @ {: context:n :}
   plan false CHILD-PLAN {: child :}
   context child PLAN-CONTEXT !
   child CHILD-SEED
   child state call NODE-A@ 0 SQL-PATTERN {: relation:n :}
   child s" SELECT 1 FROM " SQL+ child relation SQL-NAME
   plan BOOLEAN+
   state call s" NOT EXISTS" CALL-IS? if plan s" NOT " SQL+ then
   plan s" EXISTS(SELECT 1 FROM(SELECT " SQL+
   plan PLAN-CONTEXT @ 0<> if
      plan CONTEXT+ plan s" ||" SQL+
   then
   plan BINDINGS+ plan s" AS v,r.u AS u)" SQL+
   plan context CONTEXT-NAME+
   plan s" CROSS JOIN LATERAL(" SQL+ plan child SQL$ SQL+
   plan s" )matches))" SQL+ ;

: BNODE+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call CALL-COUNT 0= if
      plan s" jsonb_build_object('type','bnode','value','b'||replace(gen_random_uuid()::text,'-',''))" SQL+
   else
      plan s" @.sparql_bnode(" SQL+
      plan state call 0 ARGUMENT@ SQL-EXPRESSION
      plan s" ,r.u)" SQL+
   then ;

: COUNT+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call NODE NODE-FLAGS @ {: flags:n :}
   plan s" @.sparql_number(count(" SQL+
   flags F-DISTINCT and 0<> if plan s" DISTINCT " SQL+ then
   flags F-STAR and 0<> if
      flags F-DISTINCT and 0<> if
         plan s" (r.v-ARRAY(SELECT k FROM jsonb_object_keys(r.v)k WHERE left(k,1)='@'))" SQL+
      else plan s" *" SQL+ then
   else plan state call 0 ARGUMENT@ SQL-EXPRESSION then
   plan s" ),'http://www.w3.org/2001/XMLSchema#integer')" SQL+ ;

: AGGREGATE+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call s" COUNT" CALL-IS? if plan call COUNT+ exit then
   plan s" @.sparql_aggregate(" SQL+
   plan state call TEXT$ STRING+
   plan s" ,array_agg(" SQL+
   plan state call 0 ARGUMENT@ SQL-EXPRESSION
   plan s" )," SQL+
   state call NODE NODE-FLAGS @ F-DISTINCT and 0<> if
      plan s" true," SQL+
   else plan s" false," SQL+ then
   state call NODE-B@ dup 0<> if
      state swap TEXT$ plan -rot STRING+
   else drop plan s"  " STRING+ then
   plan s" )" SQL+ ;

: IRI-FUNCTION+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   plan s" @.sparql_iri(" SQL+
   plan state call 0 ARGUMENT@ SQL-EXPRESSION plan s" ," SQL+
   state SYNTAX-BASE-SIZE @ 0= if plan s" NULL::text" SQL+
   else plan state SYNTAX-BASE @ state SYNTAX-BASE-SIZE @ STRING+
   then plan s" )" SQL+ ;

: FUNCTION+ ( ptr plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call s" IF" CALL-IS? if plan call IF+ exit then
   state call s" COALESCE" CALL-IS? if plan call COALESCE+ exit then
   state call EXISTS-CALL? if plan call EXISTS+ exit then
   state call s" BNODE" CALL-IS? if plan call BNODE+ exit then
   state call s" IRI URI" CALL-IN? if
      plan call IRI-FUNCTION+ exit
   then
   state call AGGREGATE? if plan call AGGREGATE+ exit then
   state call s" BOUND" CALL-IS? if
      plan BOOLEAN+ plan state call 0 ARGUMENT@ TERM+
      plan s" IS NOT NULL)" SQL+ exit
   then
   plan state call TEXT$ CALL+
   plan state call NODE-A@ EXPRESSIONS+ plan CALL-END ;

: INFIX-IS? ( ptr plan n ptr u8 n -- bool )
   {: plan node:n text size:n :}
   plan TREE node TEXT$ text size STR=CI ;

: LOGICAL+ ( ptr plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan BOOLEAN+
   plan state node NODE-A@ EBV+
   plan node s" &&" INFIX-IS? if plan s" AND " SQL+
   else plan s" OR " SQL+ then
   plan state node NODE-B@ EBV+ plan s" )" SQL+ ;

: IN+ ( ptr plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan BOOLEAN+
   plan node s" NOT IN" INFIX-IS? if plan s" NOT " SQL+ then
   plan s" @.sparql_in(" SQL+
   plan state node NODE-A@ SQL-EXPRESSION
   plan s" ,ARRAY[" SQL+
   plan state node NODE-B@ EXPRESSIONS+
   plan s" ]::jsonb[]))" SQL+ ;

: RELATIONAL? ( ptr plan n -- bool )
   {: plan node:n :}
   plan node s" =" INFIX-IS? plan node s" !=" INFIX-IS? or
   plan node s" <" INFIX-IS? or plan node s" >" INFIX-IS? or
   plan node s" <=" INFIX-IS? or plan node s" >=" INFIX-IS? or ;

: BINARY+ ( ptr plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan node s" &&" INFIX-IS? plan node s" ||" INFIX-IS? or if
      plan node LOGICAL+ exit
   then
   plan node s" IN" INFIX-IS? plan node s" NOT IN" INFIX-IS? or if
      plan node IN+ exit
   then
   plan node RELATIONAL? {: relational:bool :}
   relational if
      plan BOOLEAN+ plan s" @.sparql_relation(" SQL+
   else plan s" @.sparql_arithmetic(" SQL+ then
   plan state node TEXT$ STRING+ plan s" ," SQL+
   plan state node NODE-A@ SQL-EXPRESSION plan s" ," SQL+
   plan state node NODE-B@ SQL-EXPRESSION plan s" )" SQL+
   relational if plan s" )" SQL+ then ;

: UNARY+ ( ptr plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan node s" !" INFIX-IS? if
      plan BOOLEAN+ plan s" NOT " SQL+
      plan state node NODE-A@ EBV+ plan s" )" SQL+ exit
   then
   plan s" @.sparql_arithmetic(" SQL+
   plan state node TEXT$ STRING+
   plan s" ,@.sparql_number(0,'http://www.w3.org/2001/XMLSchema#integer')," SQL+
   plan state node NODE-A@ SQL-EXPRESSION plan s" )" SQL+ ;

: EXPRESSION-BODY+ ( ptr plan n -- )
   {: plan node:n :}
   plan node AGGREGATED+ if exit then
   plan TREE node NODE-K case
      N-UNARY of plan node UNARY+ endof
      N-BINARY of plan node BINARY+ endof
      N-CALL of plan node FUNCTION+ endof
      N-VAR of plan node TERM+ endof
      N-IRI of plan node TERM+ endof
      N-LITERAL of plan node TERM+ endof
      E-SYNTAX throw
   endcase ;

: EXPRESSION+ ( ptr plan n -- )
   {: plan node:n :} plan TREE NEST
   plan node EXPRESSION-BODY+ plan TREE UNNEST ;

' EXPRESSION+ is SQL-EXPRESSION

;package
