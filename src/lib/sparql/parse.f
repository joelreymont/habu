\ SPARQL Query grammar. PARSE owns no memory: the supplied arena retains
\ its syntax and decoded terms until the caller finishes the query.

require src/lib/sparql/pattern.f
require src/lib/sparql/validate.f
require src/lib/sparql/terms.f
require src/lib/sparql/expression.f
require src/lib/sparql/lex.f
require src/lib/sparql/syntax.f

package SPARQL
private

: PROJECTION ( ^syntax n -- )
   {: state query:n :}
   state s" DISTINCT" ACCEPT$ if
      state query F-DISTINCT NODE-FLAGS!
   else state s" REDUCED" ACCEPT$ drop then
   state s" *" IS$ if
      state query state SYNTAX-CURSOR @ QUERY-STAR!
      state NEXT-TOKEN
      state query state query NODE-FLAGS@ F-STAR + NODE-FLAGS! exit
   then
   0 begin
      state TOKEN-K T-VAR = state s" (" IS$ or
   while
      state TOKEN-K T-VAR = if state READ-TERM
      else state ALIAS then
      state swap rot CONS
   repeat
   dup 0= if
      state s" *, a variable or (expression AS ?name)"
         s" write * or the variables to select" REJECT
   then
   state swap REVERSE state query rot NODE-A! ;

: MODIFIER? ( ^syntax -- bool )
   {: state :}
   state s" GROUP" IS$ state s" HAVING" IS$ or
   state s" ORDER" IS$ or state s" LIMIT" IS$ or
   state s" OFFSET" IS$ or ;

: MODIFIER-END? ( ^syntax -- bool )
   {: state :}
   state END? state s" }" IS$ or
   state s" VALUES" IS$ or state MODIFIER? or ;

: GROUP-EXPRESSION ( ^syntax -- n )
   {: state :}
   state s" (" ACCEPT$ if
      state 0 EXPRESSION {: expression:n :}
      state s" AS" ACCEPT$ if
         state T-VAR s" a variable" REQUIRE-KIND
         state N-ALIAS expression state READ-TERM BINARY-NODE
      else expression then
      state s" )" EXPECT$ exit
   then
   state TOKEN-K T-VAR = if state READ-TERM
   else state CONSTRAINT then ;

: GROUP-BY ( ^syntax -- n )
   {: state :}
   state s" BY" EXPECT$
   0 begin state MODIFIER-END? 0= while
      state GROUP-EXPRESSION state swap rot CONS
   repeat
   dup 0= if
      state s" a variable or expression to group by"
         s" write the variables to group by after GROUP BY" REJECT
   then
   state swap REVERSE {: expressions:n :}
   state N-GROUPBY expressions UNARY-NODE ;

: HAVING-EXPRESSIONS ( ^syntax -- n )
   {: state :}
   0 begin state MODIFIER-END? 0= while
      state CONSTRAINT state swap rot CONS
   repeat
   dup 0= if
      state s" a condition in parentheses or a function call"
         s" write a condition in parentheses after HAVING" REJECT
   then
   state swap REVERSE {: expressions:n :}
   state N-HAVING expressions UNARY-NODE ;

: ORDER-BY ( ^syntax -- n )
   {: state :}
   state s" BY" EXPECT$
   0 begin state MODIFIER-END? 0= while
      state ORDER-CONDITION state swap rot CONS
   repeat
   dup 0= if
      state s" a variable or expression to order by"
         s" write the variables to order by after ORDER BY" REJECT
   then state swap REVERSE ;

: READ-SLICE ( ^syntax n -- n )
   {: state kind:n :}
   state T-INTEGER s" a non-negative integer" REQUIRE-KIND
   state TOKEN$ STR:>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH {: value:n :}
   value 0 < if
      state s" an integer that fits 64 bits"
         s" write at most 9223372036854775807" REJECT
   then state NEXT-TOKEN
   state kind ADD-NODE {: node:n :}
   state node value NODE-VALUE! node ;

: CONCAT-LISTS ( ^syntax n n -- n )
   {: state left:n right:n :}
   left 0= if right exit then
   left begin state over NODE-B@ 0<> while
      state swap NODE-B@
   repeat
   state swap right NODE-B! left ;

: ADD-SLICE ( ^syntax n n -- n )
   {: state list:n kind:n :}
   state list state state kind READ-SLICE 0 CONS CONCAT-LISTS ;

: MODIFIERS ( ^syntax n -- )
   {: state query:n :}
   state s" GROUP" ACCEPT$ if
      state state GROUP-BY 0 CONS
   else 0 then
   state s" HAVING" ACCEPT$ if
      state HAVING-EXPRESSIONS state swap rot CONS
   then state swap REVERSE
   state s" ORDER" ACCEPT$ if
      state swap state ORDER-BY CONCAT-LISTS
   then
   state s" LIMIT" ACCEPT$ if
      state swap N-LIMIT ADD-SLICE
      state s" OFFSET" ACCEPT$ if
         state swap N-OFFSET ADD-SLICE
      then
   else state s" OFFSET" ACCEPT$ if
      state swap N-OFFSET ADD-SLICE
      state s" LIMIT" ACCEPT$ if
         state swap N-LIMIT ADD-SLICE
      then
   then then state query rot NODE-D! ;

: WHERE-FIX ( ^syntax -- str )
   s" FROM" IS$ if
      s" remove FROM: the base you query is the dataset"
   else s" write the pattern as WHERE { ... }" then ;

: WHERE-GROUP ( ^syntax n -- )
   {: state query:n :}
   state s" WHERE" ACCEPT$ 0= state s" {" IS$ 0= and if
      state s" WHERE or {" state WHERE-FIX REJECT
   then state GROUP-PATTERN state query rot NODE-B! ;

: TAIL-VALUES ( ^syntax n -- )
   {: state query:n :}
   state s" VALUES" ACCEPT$ 0= if exit then
   state VALUES state query rot QUERY-VALUES! ;

: SELECT-QUERY ( ^syntax -- n )
   {: state :}
   state N-SELECT NEW-QUERY {: query:n :}
   state s" SELECT" EXPECT$
   state query PROJECTION state query WHERE-GROUP
   state query MODIFIERS state query TAIL-VALUES query ;

: ASK-QUERY ( ^syntax -- n )
   {: state :}
   state N-ASK NEW-QUERY {: query:n :}
   state query WHERE-GROUP
   state query MODIFIERS state query TAIL-VALUES query ;

\ The rewrite of a query form this endpoint drops, or the forms to start with.
: FORM-FIX ( ^syntax -- str )
   {: state :}
   state s" CONSTRUCT" IS$ if
      s" write SELECT ?s ?p ?o WHERE { ... } to read the triples"
      exit
   then
   state s" DESCRIBE" IS$ if
      s" write SELECT ?p ?o WHERE { <iri> ?p ?o } to read what is said about <iri>"
      exit
   then s" start the query with SELECT or ASK" ;

: QUERY-FORM ( ^syntax -- n )
   {: state :}
   state s" SELECT" IS$ if state SELECT-QUERY exit then
   state s" ASK" ACCEPT$ if state ASK-QUERY exit then
   state s" SELECT or ASK" state FORM-FIX REJECT ;

\ The fix of text after the query: a modifier out of order, or extra text.
: END-FIX ( ^syntax -- str )
   MODIFIER? if
      s" write GROUP BY, HAVING, ORDER BY, LIMIT and OFFSET in that order, each once"
   else s" remove the text from here on" then ;

: PARSE ( str ^ARENA:arena -- ^syntax )
   SYNTAX-NEW {: state :}
   state LEX state PROLOGUE
   state QUERY-FORM state SYNTAX-ROOT !
   state END? 0= if
      state s" the end of the query" state END-FIX REJECT
   then
   state state SYNTAX-ROOT @ QUERY-SCOPE drop state ;

: INSTALL-PARSER ( -- )
   ['] SELECT-QUERY is SUBQUERY ;

INSTALL-PARSER

;package
