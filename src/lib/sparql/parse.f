\ SPARQL Query grammar. PARSE owns no memory: the supplied arena retains
\ its syntax and decoded terms until the caller finishes the query.

require src/lib/sparql/pattern.f
require src/lib/sparql/validate.f

package SPARQL
private

: ADD-LIST ( ptr syntax n n -- n ) CONS ;

: PROJECTION ( ptr syntax n -- )
   {: state query:n :}
   state s" DISTINCT" ACCEPT$ if
      F-DISTINCT state query NODE NODE-FLAGS !
   else state s" REDUCED" ACCEPT$ if
      F-REDUCED state query NODE NODE-FLAGS !
   then then
   state s" *" ACCEPT$ if
      F-STAR state query NODE NODE-FLAGS +! exit
   then
   0 begin
      state TOKEN-K T-VAR = state s" (" IS$ or
   while
      state TOKEN-K T-VAR = if state READ-TERM
      else state ALIAS then
      state swap rot CONS
   repeat
   dup 0= if E-SYNTAX throw then
   state swap REVERSE state query NODE NODE-A ! ;

: DESCRIBE-TARGETS ( ptr syntax n -- )
   {: state query:n :}
   state s" *" ACCEPT$ if
      F-STAR state query NODE NODE-FLAGS ! exit
   then
   0 begin
      state TOKEN-K dup T-VAR = over T-IRI = or
      swap T-NAME = or
   while
      state READ-TERM state swap rot CONS
   repeat
   dup 0= if E-SYNTAX throw then
   state swap REVERSE state query NODE NODE-A ! ;

: DATASET ( ptr syntax -- n )
   {: state :}
   state s" NAMED" ACCEPT$ if F-NAMED else 0 then
      {: flags:n :}
   state N-DATASET state READ-IRI UNARY-NODE {: node:n :}
   flags state node NODE NODE-FLAGS ! node ;

: DATASETS ( ptr syntax n -- )
   {: state query:n :}
   0 begin state s" FROM" ACCEPT$ while
      state DATASET state swap rot CONS
   repeat
   state swap REVERSE state query NODE NODE-C ! ;

: MODIFIER-END? ( ptr syntax -- bool )
   {: state :}
   state END? state s" }" IS$ or
   state s" GROUP" IS$ or state s" HAVING" IS$ or
   state s" ORDER" IS$ or state s" LIMIT" IS$ or
   state s" OFFSET" IS$ or state s" VALUES" IS$ or ;

: GROUP-EXPRESSION ( ptr syntax -- n )
   {: state :}
   state s" (" ACCEPT$ if
      state 0 EXPRESSION {: expression:n :}
      state s" AS" ACCEPT$ if
         state T-VAR REQUIRE-KIND
         state N-ALIAS expression state READ-TERM BINARY-NODE
      else expression then
      state s" )" EXPECT$ exit
   then
   state TOKEN-K T-VAR = if state READ-TERM
   else state CONSTRAINT then ;

: GROUP-BY ( ptr syntax -- n )
   {: state :}
   state s" BY" EXPECT$
   0 begin state MODIFIER-END? 0= while
      state GROUP-EXPRESSION state swap rot CONS
   repeat
   dup 0= if E-SYNTAX throw then
   state swap REVERSE {: expressions:n :}
   state N-GROUPBY expressions UNARY-NODE ;

: HAVING-EXPRESSIONS ( ptr syntax -- n )
   {: state :}
   0 begin state MODIFIER-END? 0= while
      state CONSTRAINT state swap rot CONS
   repeat
   dup 0= if E-SYNTAX throw then
   state swap REVERSE {: expressions:n :}
   state N-HAVING expressions UNARY-NODE ;

: ORDER-CONDITION ( ptr syntax -- n )
   {: state :}
   state s" ASC" IS$ state s" DESC" IS$ or if
      state s" DESC" IS$ if F-DESC else 0 then
         {: flags:n :}
      state NEXT-TOKEN state s" (" EXPECT$
      state 0 EXPRESSION {: expression:n :}
      state s" )" EXPECT$
      state N-ORDER expression UNARY-NODE {: node:n :}
      flags state node NODE NODE-FLAGS ! node
   else
      state TOKEN-K T-VAR = if state READ-TERM
      else state CONSTRAINT then
      state N-ORDER rot UNARY-NODE
   then ;

: ORDER-BY ( ptr syntax -- n )
   {: state :}
   state s" BY" EXPECT$
   0 begin state MODIFIER-END? 0= while
      state ORDER-CONDITION state swap rot CONS
   repeat
   dup 0= if E-SYNTAX throw then state swap REVERSE ;

: READ-SLICE ( ptr syntax n -- n )
   {: state kind:n :}
   state T-INTEGER REQUIRE-KIND
   state TOKEN$ STR>NUMBER? MATCH option
      none OF E-SYNTAX throw ENDOF
      some OF ENDOF
   ;MATCH {: value:n :}
   value 0 < if E-SYNTAX throw then state NEXT-TOKEN
   state kind ADD-NODE {: node:n :}
   value state node NODE NODE-VALUE ! node ;

: CONCAT-LISTS ( ptr syntax n n -- n )
   {: state left:n right:n :}
   left 0= if right exit then
   left begin state over NODE-B@ 0<> while
      state swap NODE-B@
   repeat
   right state rot NODE NODE-B ! left ;

: MODIFIERS ( ptr syntax n -- )
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
      state N-LIMIT READ-SLICE state swap 0 CONS
      state -rot CONCAT-LISTS
      state s" OFFSET" ACCEPT$ if
         state N-OFFSET READ-SLICE state swap 0 CONS
         state -rot CONCAT-LISTS
      then
   else state s" OFFSET" ACCEPT$ if
      state N-OFFSET READ-SLICE state swap 0 CONS
      state -rot CONCAT-LISTS
      state s" LIMIT" ACCEPT$ if
         state N-LIMIT READ-SLICE state swap 0 CONS
         state -rot CONCAT-LISTS
      then
   then then state query NODE NODE-D ! ;

: TEMPLATE-CHECK ( ptr syntax n -- )
   {: state group:n :}
   state group NODE-A@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: triple:n :}
      state triple NODE-K N-TRIPLE <> if E-SYNTAX throw then
      state state triple NODE-B@ NODE-K N-PATH = if
         E-SYNTAX throw
      then state list NODE-B@
   repeat drop ;

: WHERE-GROUP ( ptr syntax n bool -- )
   {: state query:n optional:bool :}
   state s" WHERE" ACCEPT$ if
      state GROUP-PATTERN
   else
      state s" {" IS$ if state GROUP-PATTERN else
         optional if state N-GROUP ADD-NODE
         else E-SYNTAX throw then
      then
   then state query NODE NODE-B ! ;

: TAIL-VALUES ( ptr syntax n -- )
   {: state query:n :}
   state s" VALUES" ACCEPT$ 0= if exit then
   state VALUES state query NODE NODE-VALUE ! ;

: SELECT-QUERY ( ptr syntax -- n )
   {: state :}
   state s" SELECT" EXPECT$
   state N-SELECT ADD-NODE {: query:n :}
   state query PROJECTION state query DATASETS
   state query false WHERE-GROUP
   state query MODIFIERS state query TAIL-VALUES query ;

: SELECT-SUBQUERY ( ptr syntax -- n )
   {: state :}
   state SELECT-QUERY {: query:n :}
   state query NODE-C@ 0<> if E-SYNTAX throw then query ;

' SELECT-SUBQUERY is SUBQUERY

: ASK-QUERY ( ptr syntax -- n )
   {: state :}
   state N-ASK ADD-NODE {: query:n :}
   state query DATASETS state query false WHERE-GROUP
   state query MODIFIERS state query TAIL-VALUES query ;

: DESCRIBE-QUERY ( ptr syntax -- n )
   {: state :}
   state N-DESCRIBE ADD-NODE {: query:n :}
   state query DESCRIBE-TARGETS state query DATASETS
   state query true WHERE-GROUP
   state query MODIFIERS state query TAIL-VALUES query ;

: CONSTRUCT-QUERY ( ptr syntax -- n )
   {: state :}
   state N-CONSTRUCT ADD-NODE {: query:n :}
   state s" {" IS$ if
      state GROUP-PATTERN {: template:n :}
      state template TEMPLATE-CHECK
      template state query NODE NODE-A !
      state query DATASETS state query false WHERE-GROUP
   else
      state query DATASETS state s" WHERE" EXPECT$
      state GROUP-PATTERN {: template:n :}
      state template TEMPLATE-CHECK
      template state query NODE NODE-A !
      template state query NODE NODE-B !
   then state query MODIFIERS state query TAIL-VALUES query ;

: QUERY-FORM ( ptr syntax -- n )
   {: state :}
   state s" SELECT" IS$ if state SELECT-QUERY exit then
   state s" ASK" ACCEPT$ if state ASK-QUERY exit then
   state s" CONSTRUCT" ACCEPT$ if state CONSTRUCT-QUERY exit then
   state s" DESCRIBE" ACCEPT$ if state DESCRIBE-QUERY exit then
   E-SYNTAX throw ;

: PARSE-UNTIL ( ptr u8 n ptr u8 n n ptr ARENA:arena -- ptr syntax )
   {: text size:n base base-size:n deadline:n arena :}
   text size base base-size arena SYNTAX-NEW {: state :}
   deadline state SYNTAX-DEADLINE !
   state LEX state PROLOGUE
   state QUERY-FORM state SYNTAX-ROOT !
   state END? 0= if E-SYNTAX throw then
   state state SYNTAX-ROOT @ QUERY-SCOPE drop state ;

: PARSE ( ptr u8 n ptr u8 n ptr ARENA:arena -- ptr syntax )
   0 swap PARSE-UNTIL ;

;package
