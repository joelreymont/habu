\ Compile a parsed query for the trusted backend schema. PostgreSQL returns
\ SELECT binding maps, ASK's boolean, or graph triples with s/p/o members.

require src/lib/sparql/sql-query.f

package SPARQL
private

: TEMPLATE-TERM+ ( ptr plan n -- )
   {: plan term:n :} plan TREE {: state :}
   plan s" @.sparql_normalize(" SQL+
   state term NODE-K N-BNODE = if
      plan s" @.sparql_bnode(jsonb_build_object('type','literal','value'," SQL+
      plan plan term VARIABLE$ STRING+
      plan s" ),'template/'||r.u)" SQL+
   else plan term TERM+ then plan s" )" SQL+ ;

: TEMPLATE-TRIPLE+ ( ptr plan n n -- )
   {: plan triple:n input:n :} plan TREE {: state :}
   plan s" SELECT jsonb_build_object('s'," SQL+
   plan state triple NODE-A@ TEMPLATE-TERM+
   plan s" ,'p'," SQL+ plan state triple NODE-B@ TEMPLATE-TERM+
   plan s" ,'o'," SQL+ plan state triple NODE-C@ TEMPLATE-TERM+
   plan s" ) AS v" SQL+ plan input FROM+ ;

: CONSTRUCT-SQL ( ptr plan n n -- n )
   {: plan query:n input:n :} plan TREE {: state :}
   state state query NODE-A@ NODE-A@ {: triples:n :}
   plan CTE {: raw:n :}
   triples 0= if plan s" SELECT NULL::jsonb AS v WHERE false" SQL+
   else
      triples begin dup 0<> while
         {: list:n :} plan state list NODE-A@ input TEMPLATE-TRIPLE+
         state list NODE-B@ dup 0<> if plan s" UNION ALL " SQL+ then
      repeat drop
   then plan CTE-END
   plan CTE {: relation:n :}
   plan s" SELECT DISTINCT v FROM " SQL+ plan raw SQL-NAME
   plan s" WHERE v->'s'->>'type' IN('uri','bnode') AND v->'p'->>'type'='uri' AND v->'o'->>'type' IN('uri','bnode','literal')" SQL+
   plan CTE-END relation ;

: DESCRIBE-NAMES ( ptr syntax n -- n )
   {: state query:n :}
   state query NODE NODE-FLAGS @ F-STAR and 0<> if
      state query QUERY-NAMES
   else state query NODE-A@ then ;

: DESCRIBE-TARGET+ ( ptr plan n n -- )
   {: plan target:n input:n :}
   plan s" SELECT " SQL+ plan target TERM+ plan s" AS n" SQL+
   plan target VARIABLE? if plan input FROM+ then ;

: DESCRIBE-SEED ( ptr plan n n -- n )
   {: plan query:n input:n :} plan TREE {: state :}
   state query DESCRIBE-NAMES {: names:n :}
   plan CTE {: relation:n :}
   names 0= if plan s" SELECT NULL::jsonb AS n WHERE false" SQL+
   else
      names begin dup 0<> while
         {: list:n :} plan state list NODE-A@ input DESCRIBE-TARGET+
         state list NODE-B@ dup 0<> if plan s" UNION " SQL+ then
      repeat drop
   then plan CTE-END relation ;

: DESCRIBE-NODES ( ptr plan n -- n )
   {: plan input:n :}
   plan CTE {: relation:n :}
   plan s" SELECT n FROM " SQL+ plan input SQL-NAME
   plan s" WHERE n->>'type' IN('uri','bnode') UNION SELECT t.o FROM " SQL+
   plan relation SQL-NAME plan s" d JOIN " SQL+
   plan 0 0 0 SOURCE+
   plan s" t ON t.s=d.n WHERE t.o->>'type'='bnode'" SQL+
   plan CTE-END relation ;

: DESCRIBE-SQL ( ptr plan n n -- n )
   {: plan query:n input:n :}
   plan query input DESCRIBE-SEED plan swap DESCRIBE-NODES
      {: nodes:n :}
   plan CTE {: relation:n :}
   plan s" SELECT DISTINCT jsonb_build_object('s',t.s,'p',t.p,'o',t.o) AS v FROM " SQL+
   plan nodes SQL-NAME plan s" d JOIN " SQL+
   plan 0 0 0 SOURCE+
   plan s" t ON t.s=d.n" SQL+
   plan CTE-END relation ;

: FINISH-QUERY ( ptr plan n n -- )
   {: plan query:n rows:n :}
   plan TREE query NODE-K case
      N-SELECT of
         plan s" SELECT v FROM " SQL+ plan rows SQL-NAME
         plan s" ORDER BY z" SQL+
      endof
      N-ASK of
         plan s" SELECT to_jsonb(EXISTS(SELECT 1 FROM " SQL+
         plan rows SQL-NAME plan s" )) AS v" SQL+
      endof
      N-CONSTRUCT of
         plan query rows CONSTRUCT-SQL {: graph:n :}
         plan s" SELECT v FROM " SQL+ plan graph SQL-NAME
      endof
      N-DESCRIBE of
         plan query rows DESCRIBE-SQL {: graph:n :}
         plan s" SELECT v FROM " SQL+ plan graph SQL-NAME
      endof
      E-SYNTAX throw
   endcase ;

: COMPILE ( ptr syntax ptr u8 n -- ptr plan )
   PLAN-NEW {: plan :}
   plan TREE SYNTAX-ROOT @ {: query:n :}
   plan SEED plan query QUERY-SOLUTIONS {: rows:n :}
   plan query rows FINISH-QUERY plan ;

: REQUEST-CAPACITY ( ptr plan -- n )
   {: plan :}
   plan SQL$ nip 6 * 512 +
   plan PLAN-ARGUMENTS @ VEC:COUNT 0 ?do
      plan PLAN-ARGUMENTS @ i VEC:AT ARGUMENT-SIZE @ 6 * 8 + +
   loop
   plan TREE SYNTAX-LENGTH @ 6 * + ;

: FORM$ ( n -- ptr u8 n )
   case
      N-SELECT of s" select" endof
      N-ASK of s" ask" endof
      N-CONSTRUCT of s" construct" endof
      N-DESCRIBE of s" describe" endof
      E-SYNTAX throw
   endcase ;

: WRITE-PARAMETERS ( ptr JSON:writer ptr plan -- ptr JSON:writer )
   {: writer plan :}
   writer JSON:WRITE-ARRAY-START
   plan PLAN-ARGUMENTS @ VEC:COUNT 0 ?do
      i 0<> if JSON:WRITE-COMMA then
      plan PLAN-ARGUMENTS @ i VEC:AT {: argument :}
      argument ARGUMENT-TEXT @ argument ARGUMENT-SIZE @
         JSON:WRITE-STRING
   loop JSON:WRITE-ARRAY-END ;

: WRITE-NAMES ( ptr JSON:writer ptr plan -- ptr JSON:writer )
   {: writer plan :} plan TREE {: state :}
   writer JSON:WRITE-ARRAY-START
   state state SYNTAX-ROOT @ NODE-K N-SELECT <> if
      JSON:WRITE-ARRAY-END exit
   then
   state state SYNTAX-ROOT @ SELECT-NAMES
   begin dup 0<> while
      {: out list:n :}
      out state state list NODE-A@ TEXT$ JSON:WRITE-STRING
      state list NODE-B@ dup 0<> if
         swap JSON:WRITE-COMMA swap
      then
   repeat drop JSON:WRITE-ARRAY-END ;

: PLAN-REQUEST$ ( ptr plan -- ptr u8 n )
   {: plan :} plan TREE {: state :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR {: allocator :}
   allocator [type] JSON:writer ALLOC:NEW {: writer :}
   writer allocator plan REQUEST-CAPACITY ALLOC:BYTES
      JSON:WRITE-OPEN JSON:WRITE-OBJECT-START
   s" op" s" query" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" sql" plan SQL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" params" JSON:WRITE-KEY plan WRITE-PARAMETERS
   JSON:WRITE-COMMA s" form"
   state state SYNTAX-ROOT @ NODE-K FORM$ JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" vars" JSON:WRITE-KEY plan WRITE-NAMES
   JSON:WRITE-OBJECT-END JSON:WRITTEN$ ;

;package
