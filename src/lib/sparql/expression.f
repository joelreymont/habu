\ Precedence parsing keeps SPARQL expressions as syntax. Evaluators own
\ error values, effective boolean value, and function dispatch.

require src/lib/sparql/terms.f

package SPARQL
private

defer EXPRESSION ( ptr syntax n -- n )
defer GROUP-PATTERN ( ptr syntax -- n )

: OPERATOR-NODE ( ptr syntax n ptr u8 n n n -- n )
   {: state kind:n text size:n left:n right:n :}
   state kind left right BINARY-NODE {: node:n :}
   state node text size TEXT! node ;

: BUILTIN? ( ptr u8 n -- bool )
   {: text size:n :}
   s" STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC REGEX COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT"
      {: names length:n :}
   0 begin dup length < while
      {: start:n :}
      names start + length start - 32 CHAR-OFF {: count:n :}
      text size names start + count STR=CI if true exit then
      start count + 1+
   repeat drop false ;

: ARGUMENTS ( ptr syntax -- n )
   {: state :}
   state s" )" ACCEPT$ if 0 exit then
   0 begin
      state 0 EXPRESSION {: value:n :}
      state value rot CONS
      state s" ," ACCEPT$ 0= if
         state s" )" EXPECT$ state swap REVERSE exit
      then
   again ;

: CALL-ARGUMENTS ( ptr syntax n -- )
   {: state call:n :}
   state s" DISTINCT" ACCEPT$ if
      F-DISTINCT state call NODE NODE-FLAGS !
   then
   state s" *" ACCEPT$ if
      F-STAR state call NODE NODE-FLAGS +!
      state s" )" EXPECT$ exit
   then
   state s" )" ACCEPT$ if exit then
   0 begin
      state 0 EXPRESSION {: value:n :}
      state value rot CONS
      state s" ," ACCEPT$ 0= if
         state swap REVERSE state call NODE NODE-A !
         state s" ;" ACCEPT$ if
            state s" SEPARATOR" EXPECT$
            state s" =" EXPECT$
            state T-STRING REQUIRE-KIND
            state STRING-TOKEN state call NODE NODE-B !
         then state s" )" EXPECT$ exit
      then
   again ;

: CALL-IS? ( ptr syntax n ptr u8 n -- bool )
   {: state node:n text size:n :}
   state node TEXT$ text size STR=CI ;

: CALL-IN? ( ptr syntax n ptr u8 n -- bool )
   {: state node:n names size:n :}
   0 begin dup size < while
      {: start:n :}
      names start + size start - 32 CHAR-OFF {: width:n :}
      state node names start + width CALL-IS? if true exit then
      start width + 1+
   repeat drop false ;

: AGGREGATE? ( ptr syntax n -- bool )
   s" COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT" CALL-IN? ;

: EXISTS-CALL? ( ptr syntax n -- bool )
   {: state node:n :}
   state node s" EXISTS" CALL-IS?
   state node s" NOT EXISTS" CALL-IS? or ;

: CALL-COUNT ( ptr syntax n -- n )
   {: state call:n :}
   state state call NODE-A@ LIST-LENGTH ;

: CALL-ARITY ( ptr syntax n -- n n )
   {: state call:n :}
   state call s" RAND NOW UUID STRUUID" CALL-IN? if 0 0 exit then
   state call s" BNODE" CALL-IS? if 0 1 exit then
   state call s" CONCAT" CALL-IS? if 0 MEM-MAX-N exit then
   state call s" COALESCE" CALL-IS? if 0 MEM-MAX-N exit then
   state call s" IF" CALL-IS? if 3 3 exit then
   state call s" SUBSTR REGEX" CALL-IN? if 2 3 exit then
   state call s" REPLACE" CALL-IS? if 3 4 exit then
   state call
      s" LANGMATCHES CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER STRLANG STRDT SAMETERM"
      CALL-IN? if 2 2 exit then
   1 1 ;

: CHECK-CALL ( ptr syntax n -- )
   {: state call:n :}
   state call TEXT$ BUILTIN? 0= if exit then
   state call NODE NODE-FLAGS @ {: flags:n :}
   flags F-STAR and 0<> if
      state call s" COUNT" CALL-IS? 0= if E-SYNTAX throw then
      exit
   then
   flags F-DISTINCT and 0<> if
      state call AGGREGATE? 0= if E-SYNTAX throw then
   then
   state call NODE-B@ 0<> if
      state call s" GROUP_CONCAT" CALL-IS? 0= if
         E-SYNTAX throw
      then
   then
   state call CALL-COUNT state call CALL-ARITY BETWEEN?
      0= if E-SYNTAX throw then
   state call s" BOUND" CALL-IS? if
      state state state call NODE-A@ NODE-A@ NODE-K
         N-VAR <> if E-SYNTAX throw then
   then ;

: READ-CALL ( ptr syntax ptr u8 n -- n )
   {: state text size:n :}
   text size BUILTIN? if
      state text size COPY$ {: name length:n :}
      length 0 ?do name i + dup c@ ASCII-UPPER swap c! loop
      name length
   else text size then
   state N-CALL 2swap TEXT-NODE {: call:n :}
   state s" (" EXPECT$ state call CALL-ARGUMENTS
   state call CHECK-CALL call ;

: READ-EXISTS ( ptr syntax bool -- n )
   {: state negative:bool :}
   state s" EXISTS" EXPECT$
   state GROUP-PATTERN {: group:n :}
   state N-CALL
   negative if s" NOT EXISTS" else s" EXISTS" then
   group 0 OPERATOR-NODE ;

: ATOM ( ptr syntax -- n )
   {: state :}
   state SIGNED-NUMBER? if state SIGNED-NUMBER exit then
   state s" (" ACCEPT$ if
      state 0 EXPRESSION state s" )" EXPECT$ exit
   then
   state s" NOT" ACCEPT$ if state true READ-EXISTS exit then
   state s" EXISTS" IS$ if state false READ-EXISTS exit then
   state TOKEN-K T-WORD = if
      state TOKEN$ 2dup BUILTIN? if
         {: text size:n :} state NEXT-TOKEN
         state text size READ-CALL exit
      then 2drop
   then
   state READ-TERM {: term:n :}
   state term NODE-K N-BNODE = if E-SYNTAX throw then
   state term NODE-K N-IRI = state s" (" IS$ and if
      state state term TEXT$ READ-CALL
   else term then ;

\ Longest-token parsing makes -01 a literal and - 01 an expression.
\ The grammar permits one unary operator before a primary expression.
: PRIMARY ( ptr syntax -- n )
   {: state :}
   state SIGNED-NUMBER? if state SIGNED-NUMBER exit then
   state s" !" IS$ state s" +" IS$ or
   state s" -" IS$ or if
      state TOKEN$ {: text size:n :} state NEXT-TOKEN
      state ATOM {: value:n :}
      state N-UNARY text size value 0 OPERATOR-NODE
   else state ATOM then ;

: PRECEDENCE ( ptr syntax -- n )
   {: state :}
   state s" ||" IS$ if 1 exit then
   state s" &&" IS$ if 2 exit then
   state s" =" IS$ state s" !=" IS$ or
   state s" <" IS$ or state s" >" IS$ or
   state s" <=" IS$ or state s" >=" IS$ or
   state s" IN" IS$ or state s" NOT" IS$ or if 3 exit then
   state s" +" IS$ state s" -" IS$ or if 4 exit then
   state s" *" IS$ state s" /" IS$ or if 5 exit then 0 ;

: INFIX ( ptr syntax n n -- n )
   {: state left:n precedence:n :}
   state s" NOT" ACCEPT$ if
      state s" IN" EXPECT$ state s" (" EXPECT$
      state ARGUMENTS {: right:n :}
      state N-BINARY s" NOT IN" left right OPERATOR-NODE exit
   then
   state s" IN" ACCEPT$ if
      state s" (" EXPECT$ state ARGUMENTS {: right:n :}
      state N-BINARY s" IN" left right OPERATOR-NODE exit
   then
   state TOKEN$ {: text size:n :} state NEXT-TOKEN
   state precedence 1+ EXPRESSION {: right:n :}
   state N-BINARY text size left right OPERATOR-NODE ;

: READ-EXPRESSION ( ptr syntax n -- n )
   {: state minimum:n :}
   state NEST
   state PRIMARY false
   begin
      state PRECEDENCE dup 0<> swap minimum >= and
   while
      {: left:n compared:bool :}
      state PRECEDENCE {: precedence:n :}
      precedence 3 = compared and if E-SYNTAX throw then
      state left precedence INFIX compared precedence 3 = or
   repeat drop state UNNEST ;

' READ-EXPRESSION is EXPRESSION

: CONSTRAINT ( ptr syntax -- n )
   {: state :}
   state s" (" ACCEPT$ if
      state 0 EXPRESSION state s" )" EXPECT$ exit
   then
   state PRIMARY dup state swap NODE-K N-CALL <> if
      drop E-SYNTAX throw
   then ;

: ALIAS ( ptr syntax -- n )
   {: state :}
   state s" (" EXPECT$
   state 0 EXPRESSION {: value:n :}
   state s" AS" EXPECT$ state T-VAR REQUIRE-KIND
   state READ-TERM {: name:n :}
   state s" )" EXPECT$
   state N-ALIAS value name BINARY-NODE ;

;package
