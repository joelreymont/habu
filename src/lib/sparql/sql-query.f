\ Grouping precedes HAVING and SELECT aliases. Sorting records an ordinal
\ before projection; DISTINCT retains each projected row's first ordinal.

require src/lib/sparql/sql-path.f

package SPARQL
private

defer COLLECT-AGGREGATES ( ptr syntax n n -- n )

: COLLECT-AGGREGATES-BODY ( ptr syntax n n -- n )
   {: state expression:n found:n :}
   expression 0= if found exit then
   state expression NODE-K {: kind:n :}
   kind N-LIST = if
      found expression begin dup 0<> while
         {: result:n list:n :}
         state state list NODE-A@ result COLLECT-AGGREGATES
         state list NODE-B@
      repeat drop exit
   then
   kind N-CALL = if
      state expression AGGREGATE? if
         state expression found NODE-IN? if found
         else state expression found CONS then exit
      then
      state expression EXISTS-CALL? if found exit then
   then
   kind N-CALL = kind N-BINARY = or kind N-UNARY = or
   kind N-LIST = or kind N-ALIAS = or if
      state state expression NODE-A@ found COLLECT-AGGREGATES
      kind N-CALL <> kind N-UNARY <> and if
         state state expression NODE-B@ rot COLLECT-AGGREGATES
      then
   else found then ;

: QUERY-COLLECT-AGGREGATES ( ptr syntax n n -- n )
   {: state expression:n found:n :} state NEST
   state expression found COLLECT-AGGREGATES-BODY state UNNEST ;

' QUERY-COLLECT-AGGREGATES is COLLECT-AGGREGATES

: QUERY-AGGREGATES ( ptr syntax n -- n )
   {: state query:n :}
   state state query NODE-A@ 0 COLLECT-AGGREGATES
   state query NODE-D@ begin dup 0<> while
      {: found:n list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K dup N-HAVING = swap N-ORDER = or if
         state state modifier NODE-A@ found COLLECT-AGGREGATES
      else found then state list NODE-B@
   repeat drop ;

: GROUP-VALUE+ ( ptr plan n -- )
   {: plan expression:n :} plan TREE {: state :}
   state expression NODE-K N-ALIAS = if
      plan state expression NODE-B@ TERM+
   else plan expression SQL-EXPRESSION then ;

: GROUP-NAME+ ( ptr plan n -- )
   {: plan expression:n :} plan TREE {: state :}
   state expression NODE-K N-ALIAS = if
      plan plan state expression NODE-B@ VARIABLE$ STRING+ exit
   then
   state expression NODE-K N-VAR = if
      plan plan expression VARIABLE$ STRING+
   else plan plan expression GROUP-KEY$ STRING+ then ;

: GROUP-ALIAS-SQL ( ptr plan n n -- n )
   {: plan grouping:n input:n :} plan TREE {: state :}
   input grouping begin dup 0<> while
      {: previous:n list:n :} state list NODE-A@ {: expression:n :}
      state expression NODE-K N-ALIAS = if
         plan expression previous BIND-SQL
      else previous then state list NODE-B@
   repeat drop ;

: GROUP-MAP+ ( ptr plan -- )
   {: plan :} plan TREE {: state :}
   plan s" '{}'::jsonb" SQL+
   plan PLAN-GROUPING @ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: expression:n :}
      plan s" ||jsonb_strip_nulls(jsonb_build_object(" SQL+
      plan expression GROUP-NAME+ plan s" ," SQL+
      plan expression GROUP-VALUE+ plan s" ))" SQL+
      state list NODE-B@
   repeat drop
   plan PLAN-AGGREGATES @ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: expression:n :}
      plan s" ||jsonb_strip_nulls(jsonb_build_object(" SQL+
      plan plan expression AGGREGATE-KEY$ STRING+ plan s" ," SQL+
      plan expression SQL-EXPRESSION plan s" ))" SQL+
      state list NODE-B@
   repeat drop ;

: GROUP-CLAUSE+ ( ptr plan -- )
   {: plan :} plan TREE {: state :}
   plan PLAN-GROUPING @ dup 0= if drop exit then
   plan s" GROUP BY " SQL+
   begin dup 0<> while
      {: list:n :} plan state list NODE-A@ GROUP-VALUE+
      state list NODE-B@ dup 0<> if plan s" ," SQL+ then
   repeat drop ;

: GROUP-QUERY ( ptr plan n n -- n )
   {: plan query:n input:n :} plan TREE {: state :}
   state query QUERY-AGGREGATES plan PLAN-AGGREGATES !
   state query GROUP-BY-NODE dup 0<> if state swap NODE-A@
   else drop 0 then plan PLAN-GROUPING !
   plan PLAN-AGGREGATES @ 0= plan PLAN-GROUPING @ 0= and if
      input exit
   then
   plan plan PLAN-GROUPING @ input GROUP-ALIAS-SQL {: source:n :}
   plan CTE {: relation:n :}
   plan s" SELECT " SQL+
   plan PLAN-SCOPES @ if
      plan s" coalesce(min(r.u),(SELECT u FROM q0))||'/group/'||row_number()OVER()::text AS u," SQL+
   else plan s" ''::text AS u," SQL+ then
   plan GROUP-MAP+ plan s" AS v" SQL+
   plan source FROM+ plan GROUP-CLAUSE+ plan CTE-END
   true plan PLAN-GROUPED ! relation ;

: HAVING-SQL ( ptr plan n n -- n )
   {: plan query:n input:n :} plan TREE {: state :}
   input state query NODE-D@ begin dup 0<> while
      {: previous:n list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K N-HAVING = if
         previous state modifier NODE-A@ begin dup 0<> while
            {: current:n constraints:n :}
            plan state constraints NODE-A@ current FILTER-SQL
            state constraints NODE-B@
         repeat drop
      else previous then state list NODE-B@
   repeat drop ;

: SELECT-ALIASES ( ptr plan n n -- n )
   {: plan query:n input:n :} plan TREE {: state :}
   input state query NODE-A@ begin dup 0<> while
      {: previous:n list:n :} state list NODE-A@ {: expression:n :}
      state expression NODE-K N-ALIAS = if
         plan expression previous BIND-SQL
      else previous then state list NODE-B@
   repeat drop ;

: ORDER-KEYS+ ( ptr plan n -- bool )
   {: plan query:n :} plan TREE {: state :}
   false state query NODE-D@ begin dup 0<> while
      {: written:bool list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K N-ORDER = if
         written if plan s" ," SQL+ else plan s" ORDER BY " SQL+ then
         plan s" @.sparql_order(" SQL+
         plan state modifier NODE-A@ SQL-EXPRESSION plan s" )" SQL+
         state modifier NODE NODE-FLAGS @ F-DESC and 0<> if
            plan s" DESC NULLS LAST" SQL+
         else plan s" ASC NULLS FIRST" SQL+ then true
      else written then state list NODE-B@
   repeat drop ;

: ORDERED? ( ptr syntax n -- bool )
   {: state query:n :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :}
      state state list NODE-A@ NODE-K N-ORDER = if true exit then
      state list NODE-B@
   repeat drop false ;

: ORDER-SQL ( ptr plan n n -- n )
   {: plan query:n input:n :}
   plan CTE {: relation:n :}
   plan input relation INHERIT-BOUNDS
   plan s" SELECT r.u,r.v," SQL+
   plan TREE query ORDERED? if
      plan s" row_number()OVER(" SQL+
      plan query ORDER-KEYS+ drop plan s" )" SQL+
   else plan s" 0::bigint" SQL+ then
   plan s" AS z" SQL+
   plan input FROM+ plan CTE-END relation ;

: SELECT-NAMES ( ptr syntax n -- n )
   {: state query:n :}
   state query NODE NODE-FLAGS @ F-STAR and 0<> if
      state query QUERY-NAMES exit
   then
   0 state query NODE-A@ begin dup 0<> while
      {: names:n list:n :} state list NODE-A@ {: projection:n :}
      state projection NODE-K N-ALIAS = if
         state projection NODE-B@
      else projection then
      state swap names CONS state list NODE-B@
   repeat drop state swap REVERSE ;

: PROJECT-MAP+ ( ptr plan n -- )
   {: plan names:n :} plan TREE {: state :}
   plan s" '{}'::jsonb" SQL+
   names begin dup 0<> while
      {: list:n :} state list NODE-A@ {: name:n :}
      plan s" ||jsonb_strip_nulls(jsonb_build_object(" SQL+
      plan plan name VARIABLE$ STRING+ plan s" ," SQL+
      plan name TERM+ plan s" ))" SQL+
      state list NODE-B@
   repeat drop ;

: PROJECT-SQL ( ptr plan n n -- n )
   {: plan query:n input:n :}
   plan TREE query SELECT-NAMES {: names:n :}
   plan CTE {: relation:n :}
   plan s" SELECT r.u," SQL+ plan names PROJECT-MAP+
   plan s" AS v,r.z" SQL+ plan input FROM+ plan CTE-END relation ;

: DISTINCT-SQL ( ptr plan n -- n )
   {: plan input:n :}
   plan CTE {: relation:n :}
   plan s" SELECT min(r.u) AS u,r.v,min(r.z) AS z" SQL+
   plan input FROM+ plan s" GROUP BY r.v" SQL+
   plan CTE-END relation ;

: SLICE+ ( ptr plan n -- )
   {: plan query:n :} plan TREE {: state :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K {: kind:n :}
      kind N-LIMIT = kind N-OFFSET = or if
         kind N-LIMIT = if plan s" LIMIT " SQL+
         else plan s" OFFSET " SQL+ then
         plan state modifier NODE NODE-VALUE @ SQL-N
      then state list NODE-B@
   repeat drop ;

: SLICE-SQL ( ptr plan n n -- n )
   {: plan query:n input:n :}
   plan CTE {: relation:n :}
   plan s" SELECT r.u,r.v,r.z" SQL+ plan input FROM+
   plan s" ORDER BY r.z" SQL+ plan query SLICE+
   plan CTE-END relation ;

: QUERY-SOLUTIONS ( ptr plan n -- n )
   {: plan query:n :} plan TREE {: state :}
   state query NODE-C@ dup 0<> if plan PLAN-DATASET !
   else drop then
   plan state query NODE-B@ 0 true GROUP-SQL
   plan query rot GROUP-QUERY
   plan query rot HAVING-SQL
   state query NODE NODE-VALUE @ dup 0<> if
      plan swap rot VALUES-SQL
   else drop then
   state query NODE-K N-SELECT = if
      plan query rot SELECT-ALIASES
   then
   plan query rot ORDER-SQL
   state query NODE-K N-SELECT = if
      plan query rot PROJECT-SQL
      state query NODE NODE-FLAGS @ F-DISTINCT and 0<> if
         plan swap DISTINCT-SQL
      then
   then plan query rot SLICE-SQL ;

: SUBQUERY-SQL ( ptr plan n -- n )
   {: plan query:n :}
   plan false CHILD-PLAN {: child :}
   child CHILD-SEED
   child query QUERY-SOLUTIONS {: rows:n :}
   child s" SELECT u,v FROM " SQL+ child rows SQL-NAME
   plan CTE {: relation:n :}
   plan s" SELECT u,v FROM(" SQL+ plan child SQL$ SQL+
   plan s" )subquery" SQL+ plan CTE-END relation ;

' SUBQUERY-SQL is SQL-QUERY

;package
