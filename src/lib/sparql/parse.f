\ SPARQL Query grammar. PARSE owns no memory: the supplied arena retains
\ its syntax and decoded terms until the caller finishes the query.

require src/lib/sparql/pattern.f
require src/lib/sparql/validate.f
require src/lib/sparql/terms.f
require src/lib/sparql/expression.f
require src/lib/sparql/lex.f
require src/lib/sparql/iri.f
require src/lib/sparql/syntax.f

package SPARQL
private

\ Whether the current token may follow a projected item: another item,
\ FROM, WHERE or the pattern's {.
: PROJECTION-FOLLOWS? ( ^syntax -- bool )
   {: state :}
   state TOKEN-K T-VAR = state s" (" IS$ or
   state s" FROM" IS$ or state s" WHERE" IS$ or
   state s" {" IS$ or ;

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
      state TOKEN-K T-VAR = if state READ-VAR
      else state [: PROJECTION-FOLLOWS? ;] ALIAS then
      state swap rot CONS
   repeat
   dup 0= if
      state s" *, a variable or (expression AS ?name)"
         s" write * or the variables to select" REJECT
   then
   state swap REVERSE state query rot NODE-A! ;

\ Whether the token text starts a solution modifier.
: MODIFIER? ( str -- bool )
   {: t:str :}
   t s" GROUP" STR:=CI  t s" HAVING" STR:=CI or
   t s" ORDER" STR:=CI or  t s" LIMIT" STR:=CI or
   t s" OFFSET" STR:=CI or ;

\ Whether the token text may follow a modifier's list: a modifier, VALUES
\ or the end of the query, which is empty.
: LIST-END? ( str -- bool )
   {: t:str :}
   t STR:LEN 0=  t s" VALUES" STR:=CI or  t MODIFIER? or ;

: MODIFIER-END? ( ^syntax -- bool )
   {: state :} state TOKEN$ LIST-END? state s" }" IS$ or ;

: GROUP-EXPRESSION ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state SYNTAX-CURSOR @ {: start:n :}
   state s" (" ACCEPT$ if
      state 0 follows EXPRESSION {: expression:n :}
      state s" AS" ACCEPT$ if
         state T-VAR s" a variable" [: s" )" IS$ ;] REQUIRE-KIND
         state N-ALIAS expression state READ-VAR BINARY-NODE
         state s" " follows CLOSE-PAREN exit
      then
      state follows CLOSE-OPERATION
      state start WHOLE-CONDITION expression exit
   then
   state TOKEN-K T-VAR = if state VAR-CONDITION
   else state follows CONSTRAINT then ;

\ The fix of an empty GROUP BY: the plain variables query n selects, else
\ those its pattern binds, else its removal.
: GROUP-BY-FIX ( ^syntax n -- str )
   {: state query:n :}
   state NEW-SET {: keys :}
   state query NODE-A@ begin dup 0<> while
      {: list:n :}
      state keys  state list NODE-A@  SCOPE-ADD
      state list NODE-B@
   repeat drop
   keys MEMBER-COUNT 0= if state state query NODE-B@ SCOPE else keys then
      {: names :}
   names MEMBER-COUNT 0= if s" remove GROUP BY" exit then
   state s" write " state query names VARS-FIX JOIN$
   state swap s"  after GROUP BY" JOIN$ ;

: GROUP-BY ( ^syntax n -- n )
   {: state query:n :}
   state s" BY" EXPECT$
   0 begin
      state EXPRESSION-START?
   while
      state [: dup EXPRESSION-START? swap MODIFIER-END? or ;]
      GROUP-EXPRESSION state swap rot CONS
   repeat
   dup 0= if
      state s" a variable or expression to group by"
         state query GROUP-BY-FIX REJECT
   then
   state swap REVERSE {: expressions:n :}
   state N-GROUPBY expressions UNARY-NODE ;

: HAVING-EXPRESSIONS ( ^syntax -- n )
   {: state :}
   0 begin
      state EXPRESSION-START?
   while
      state [: dup EXPRESSION-START? swap MODIFIER-END? or ;]
      CONSTRAINT state swap rot CONS
   repeat
   dup 0= if
      state s" a condition in parentheses or a function call"
         s" write a condition in parentheses after HAVING" REJECT
   then
   state swap REVERSE {: expressions:n :}
   state N-HAVING expressions UNARY-NODE ;

: ORDER-BY ( ^syntax -- n )
   {: state :}
   state s" BY" ACCEPT$ 0= if state state MODIFIER-END? NO-BY then
   0 begin
      state ORDER-START?
   while
      state [: dup ORDER-START? swap MODIFIER-END? or ;]
      ORDER-CONDITION state swap rot CONS
   repeat
   dup 0= if state state MODIFIER-END? NO-ORDER then
   state swap REVERSE ;

: READ-SLICE ( ^syntax n -- n )
   {: state kind:n :}
   state T-INTEGER s" a non-negative integer"
      [: MODIFIER-END? ;] REQUIRE-KIND
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
      state state query GROUP-BY 0 CONS
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

\ Whether the token after the current one is the text.
: NEXT-IS? ( ^syntax str -- bool )
   {: state t:str :} state NEXT-TOKEN$ t STR:= ;

\ The fix of a missing WHERE: a word before { is a misspelt one, and a
\ triple before a } lacks the WHERE { that opens its group.
: WHERE-FIX ( ^syntax -- str )
   {: state :}
   state s" FROM" IS$ if
      s" remove FROM, any NAMED and its IRI: the base you query is the dataset"
      exit
   then
   state TOKEN-K T-WORD =  state s" {" NEXT-IS?  and if
      s" replace it with WHERE" exit
   then
   state GRAPH-NODE?  state s" }" CLOSER-AHEAD?  and if
      s" write WHERE { here" exit
   then s" write the pattern as WHERE { ... }" ;

: WHERE-GROUP ( ^syntax n -- )
   {: state query:n :}
   state s" WHERE" ACCEPT$ 0= state s" {" IS$ 0= and if
      state s" WHERE or {" state WHERE-FIX REJECT
   then state GROUP-PATTERN state query rot NODE-B! ;

: TAIL-VALUES ( ^syntax n -- )
   {: state query:n :}
   state s" VALUES" ACCEPT$ 0= if exit then
   state [: dup END? swap s" }" IS$ or ;] VALUES
   state query rot QUERY-VALUES! ;

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

\ The fix of text after the query: a modifier out of order, a stray token
\ in a modifier's list, or extra text.
: END-FIX ( ^syntax -- str )
   {: state :}
   state TOKEN$ MODIFIER? if
      s" write GROUP BY, HAVING, ORDER BY, LIMIT and OFFSET in that order, each once"
      exit
   then
   state NEXT-TOKEN$ LIST-END? if s" remove it" exit then
   s" remove the text from here on" ;

\ Lexes and parses the query, without validating it.
: READ-QUERY ( ^syntax -- )
   {: state :}
   state LEX state PROLOGUE
   state QUERY-FORM state SYNTAX-ROOT !
   state END? 0= if
      state s" the end of the query" state END-FIX REJECT
   then ;

\ Only reading shares terms, so the term interns' slots live in a work
\ frame cut once the query is read.
: PARSE ( str ^ARENA:arena -- ^syntax )
   SYNTAX-NEW {: state :}
   state SYNTAX-WORK @ {: work:^ :}
   work ARENA:MARK {: mark:n :}
   state READ-QUERY
   work mark ARENA:CUT
   state SYNTAX-INTERNS {: table:^ :}
   NULL-PTR table INTERNS-SLOTS !  0 table INTERNS-COUNT !
   state state SYNTAX-ROOT @ QUERY-SCOPE drop state ;

: INSTALL-PARSER ( -- )
   ['] SELECT-QUERY is SUBQUERY ;

INSTALL-PARSER

;package
