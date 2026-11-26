\ Precedence parsing of SPARQL expressions into syntax nodes; nothing is
\ evaluated here.

require src/lib/sparql/terms.f
require src/lib/sparql/syntax.f
require src/lib/sparql/iri.f
require src/lib/fmt.f
require src/lib/sparql/lex.f

package SPARQL
private

defer EXPRESSION ( ^syntax n -- n )
defer GROUP-PATTERN ( ^syntax -- n )

: OPERATOR-NODE ( ^syntax n str n n -- n )
   {: state kind:n text:str left:n right:n :}
   state kind left right BINARY-NODE {: node:n :}
   state node text TEXT! node ;

: LISTS? ( str str -- bool )
   {: list:str text:str :}
   list str> {: names size:n :}
   0 begin dup size < while
      {: start:n :}
      names start + size start - >str 32 CHAR-OFF {: width:n :}
      text names start + width >str STR:=CI if true exit then
      start width + 1+
   repeat drop false ;

: BUILTIN? ( str -- bool )
   s" STR LANG LANGMATCHES DATATYPE BOUND IRI URI RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC REGEX COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT"
   swap LISTS? ;

: EXPRESSIONS ( ^syntax -- n )
   {: state :}
   0 begin
      state 0 EXPRESSION state swap rot CONS
      state s" ," ACCEPT$ 0=
   until state swap REVERSE ;

: ARGUMENTS ( ^syntax -- n )
   {: state :}
   state s" )" ACCEPT$ if 0 exit then
   state EXPRESSIONS state s" )" EXPECT$ ;

: CALL-ARGUMENTS ( ^syntax n -- )
   {: state call:n :}
   state s" DISTINCT" ACCEPT$ if
      F-DISTINCT state call NODE NODE-FLAGS !
   then
   state s" *" ACCEPT$ if
      F-STAR state call NODE NODE-FLAGS +!
      state s" )" EXPECT$ exit
   then
   state s" )" ACCEPT$ if exit then
   state EXPRESSIONS state call NODE NODE-A !
   state s" ;" ACCEPT$ if
      state s" SEPARATOR" EXPECT$
      state s" =" EXPECT$
      state T-STRING s" a string" REQUIRE-KIND
      state STRING-TOKEN state call NODE NODE-B !
   then state s" )" EXPECT$ ;

: CALL-IS? ( ^syntax n str -- bool )
   {: state node:n text:str :}
   state node TEXT$ text STR:=CI ;

: CALL-IN? ( ^syntax n str -- bool )
   {: state node:n list:str :}
   list state node TEXT$ LISTS? ;

: AGGREGATE? ( ^syntax n -- bool )
   s" COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT" CALL-IN? ;

: EXISTS-CALL? ( ^syntax n -- bool )
   {: state node:n :}
   state node s" EXISTS" CALL-IS?
   state node s" NOT EXISTS" CALL-IS? or ;

: CALL-COUNT ( ^syntax n -- n )
   {: state call:n :}
   state state call NODE-A@ LIST-LENGTH ;

: CALL-ARITY ( ^syntax n -- n n )
   {: state call:n :}
   state call s" RAND NOW UUID STRUUID" CALL-IN? if 0 0 exit then
   state call s" CONCAT" CALL-IS? if 0 MEM-MAX-N exit then
   state call s" COALESCE" CALL-IS? if 0 MEM-MAX-N exit then
   state call s" IF" CALL-IS? if 3 3 exit then
   state call s" SUBSTR REGEX" CALL-IN? if 2 3 exit then
   state call s" REPLACE" CALL-IS? if 3 4 exit then
   state call
      s" LANGMATCHES CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER STRLANG STRDT SAMETERM"
      CALL-IN? if 2 2 exit then
   1 1 ;

\ The argument counts a call takes, as `n arguments` or `n or m arguments`.
: ARITY-TEXT ( ^syntax n n -- str )
   {: state least:n most:n :}
   state s" " least FMT:INT$ JOIN$ {: text:str :}
   least most <> if
      state state text s"  or " JOIN$ most FMT:INT$ JOIN$
   else text then {: count:str :}
   state count
   most 1 = least most = and if s"  argument" else s"  arguments" then
   JOIN$ ;

\ `call NAME with <count>` for call node n.
: ARITY-FIX ( ^syntax n str -- str )
   {: state call:n count:str :}
   state s" call " state call TEXT$ JOIN$
   state swap s"  with " JOIN$ state swap count JOIN$ ;

: CHECK-CALL ( ^syntax n -- )
   {: state call:n :}
   state call TEXT$ BUILTIN? 0= if
      state call NODE NODE-FLAGS @ F-STAR and
      state call NODE-B@ or 0<> if
         state call s" arguments without * or SEPARATOR"
            s" remove the * or SEPARATOR" REJECT-NODE
      then exit
   then
   state call NODE NODE-FLAGS @ {: flags:n :}
   flags F-STAR and 0<> if
      state call s" COUNT" CALL-IS? 0= if
         state call s" COUNT, the only function that takes *"
            s" replace * with the arguments" REJECT-NODE
      then exit
   then
   flags F-DISTINCT and 0<> if
      state call AGGREGATE? 0= if
         state call s" an aggregate before (DISTINCT"
            s" remove DISTINCT" REJECT-NODE
      then
   then
   state call NODE-B@ 0<> if
      state call s" GROUP_CONCAT" CALL-IS? 0= if
         state call
            s" GROUP_CONCAT, the only function that takes SEPARATOR"
            s" remove the SEPARATOR" REJECT-NODE
      then
   then
   state call CALL-ARITY {: least:n most:n :}
   state call CALL-COUNT least most BETWEEN? 0= if
      state least most ARITY-TEXT {: count:str :}
      state call count state call count ARITY-FIX REJECT-NODE
   then
   state call s" BOUND" CALL-IS? if
      state state call NODE-A@ NODE-A@ {: argument:n :}
      state argument NODE-K N-VAR <> if
         state argument s" a variable" s" pass BOUND a variable"
            REJECT-NODE
      then
   then ;

: READ-CALL ( ^syntax str -- n )
   {: state text:str :}
   text BUILTIN? if
      state text COPY$ {: name:str :}
      name str> {: bytes length:n :}
      length 0 ?do bytes i + dup c@ STR:ASCII-UPPER swap c! loop
      name
   else text then {: called:str :}
   state N-CALL called TEXT-NODE {: call:n :}
   state SYNTAX-CURSOR @ 1- state call NODE NODE-TOKEN !
   state s" (" EXPECT$ state call CALL-ARGUMENTS
   state call CHECK-CALL call ;

: READ-EXISTS ( ^syntax bool -- n )
   {: state negative:bool :}
   state s" EXISTS" EXPECT$
   state GROUP-PATTERN {: group:n :}
   state N-CALL
   negative if s" NOT EXISTS" else s" EXISTS" then
   group 0 OPERATOR-NODE ;

: DROPPED-CALL ( ^syntax -- )
   {: state :}
   state s" BNODE" IS$ if
      state s" a function this endpoint provides"
         s" bind a variable instead: a query cannot make blank nodes"
         REJECT
   then
   state s" SHA1" IS$ if
      state s" a function this endpoint provides" s" use SHA256"
         REJECT
   then ;

: ATOM ( ^syntax -- n )
   {: state :}
   state SIGNED-NUMBER? if state SIGNED-NUMBER exit then
   state s" (" ACCEPT$ if
      state 0 EXPRESSION state s" )" EXPECT$ exit
   then
   state s" NOT" ACCEPT$ if state true READ-EXISTS exit then
   state s" EXISTS" IS$ if state false READ-EXISTS exit then
   state TOKEN-K T-WORD = if
      state DROPPED-CALL
      state TOKEN$ {: text:str :}
      text BUILTIN? if
         state NEXT-TOKEN state text READ-CALL exit
      then
   then
   state TOKEN-K T-BNODE = if
      state s" a variable, IRI, prefixed name or literal"
         s" replace the blank node with a variable" REJECT
   then
   state READ-TERM {: term:n :}
   state term NODE-K N-IRI = state s" (" IS$ and if
      state state term TEXT$ READ-CALL
   else term then ;

\ Longest-token parsing makes -01 a literal and - 01 an expression.
\ The grammar permits one unary operator before a primary expression.
: PRIMARY ( ^syntax -- n )
   {: state :}
   state SIGNED-NUMBER? if state SIGNED-NUMBER exit then
   state s" !" IS$ state s" +" IS$ or
   state s" -" IS$ or if
      state TOKEN$ {: text:str :} state NEXT-TOKEN
      state ATOM {: value:n :}
      state N-UNARY text value 0 OPERATOR-NODE
   else state ATOM then ;

: PRECEDENCE ( ^syntax -- n )
   {: state :}
   state s" ||" IS$ if 1 exit then
   state s" &&" IS$ if 2 exit then
   state s" =" IS$ state s" !=" IS$ or
   state s" <" IS$ or state s" >" IS$ or
   state s" <=" IS$ or state s" >=" IS$ or
   state s" IN" IS$ or state s" NOT" IS$ or if 3 exit then
   state s" +" IS$ state s" -" IS$ or if 4 exit then
   state s" *" IS$ state s" /" IS$ or if 5 exit then 0 ;

: INFIX ( ^syntax n n -- n )
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
   state TOKEN$ {: text:str :} state NEXT-TOKEN
   state precedence 1+ EXPRESSION {: right:n :}
   state N-BINARY text left right OPERATOR-NODE ;

: READ-EXPRESSION ( ^syntax n -- n )
   {: state minimum:n :}
   state NEST
   state PRIMARY false
   begin
      state PRECEDENCE dup 0<> swap minimum >= and
   while
      {: left:n compared:bool :}
      state PRECEDENCE {: precedence:n :}
      precedence 3 = compared and if
         state s" one comparison per operand pair"
            s" put the first comparison in parentheses" REJECT
      then
      state left precedence INFIX compared precedence 3 = or
   repeat drop state UNNEST ;

: CONSTRAINT ( ^syntax -- n )
   {: state :}
   state s" (" ACCEPT$ if
      state 0 EXPRESSION state s" )" EXPECT$ exit
   then
   state SYNTAX-CURSOR @ {: start:n :}
   state PRIMARY dup state swap NODE-K N-CALL <> if
      state start s" a condition in parentheses or a function call"
         s" put the condition in parentheses" REJECT-TOKEN
   then ;

: ALIAS ( ^syntax -- n )
   {: state :}
   state s" (" EXPECT$
   state 0 EXPRESSION {: value:n :}
   state s" AS" EXPECT$ state T-VAR s" a variable" REQUIRE-KIND
   state READ-TERM {: name:n :}
   state s" )" EXPECT$
   state N-ALIAS value name BINARY-NODE ;

: INSTALL-EXPRESSION ( -- )
   ['] READ-EXPRESSION is EXPRESSION ;

INSTALL-EXPRESSION

;package
