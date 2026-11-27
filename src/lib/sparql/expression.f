\ Precedence parsing of SPARQL expressions into syntax nodes; nothing is
\ evaluated here.
\
\ A call's clauses each follow a `;`, and only GROUP_CONCAT takes any. On
\ another call, a SEPARATOR or ORDER rejects at the call, any other token
\ after a `;` at the `;`: replace it with , before a token that starts an
\ expression, else remove the ; and what follows it up to ). A SEPARATOR
\ or ORDER with no `;` before it rejects there: on GROUP_CONCAT with
\ expected ;, write ; here, but with expected ) a SEPARATOR in or after an
\ ORDER BY condition, in a call with none, write SEPARATOR before ORDER
\ BY, and an ORDER after the conditions of an ORDER BY, write the
\ conditions in one ORDER BY; a SEPARATOR in a call with one, with
\ expected ) after ORDER BY, else ; or ), remove the second SEPARATOR,
\ its = and its string.
\ GROUP_CONCAT's state is none, separated (it has SEPARATOR) or
\ ordered (it has ORDER BY, SEPARATOR or not). "At ;" rejects at the `;`,
\ every other rejection at the token after it:
\   ) or ;     any state: at ;, expected ), remove the ;
\   SEPARATOR  none: read `= string`, go separated; separated: expected
\              ORDER, remove the ;, the second SEPARATOR, its = and its
\              string; ordered: expected ), the same if it has one, else
\              write SEPARATOR before ORDER BY
\   ORDER      none, separated: read BY and the conditions, go ordered,
\              else expected BY: before a condition write BY here,
\              before ;, SEPARATOR, ORDER, ) or the end write BY and
\              the variables to order by after ORDER, else replace it
\              with BY; ordered: expected ), write the conditions in
\              one ORDER BY
\   other      none: expected SEPARATOR or ORDER, write ORDER here before
\              BY, SEPARATOR before =; separated: expected ORDER, at =
\              remove the ;, its = and its string, write ORDER here
\              before BY; none or separated, else at ;, expected ),
\              remove the ; and what follows it up to ); ordered:
\              expected ), at = with SEPARATOR at ; remove the ;, its =
\              and its string, at = without write SEPARATOR before the
\              =, then move SEPARATOR, the = and its string before ORDER
\              BY, else at ; remove the ;

require src/lib/sparql/terms.f
require src/lib/sparql/syntax.f
require src/lib/sparql/iri.f
require src/lib/fmt.f
require src/lib/sparql/lex.f

package SPARQL
private

defer EXPRESSION ( ^syntax n [ ^syntax -- bool ] -- n )
defer GROUP-PATTERN ( ^syntax -- n )
defer ORDER-CONDITION ( ^syntax [ ^syntax -- bool ] -- n )

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

: CALL-IS? ( ^syntax n str -- bool )
   {: state node:n text:str :}
   state node TEXT$ text STR:=CI ;

: CALL-IN? ( ^syntax n str -- bool )
   {: state node:n list:str :}
   list state node TEXT$ LISTS? ;

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

\ `= string` after a call's SEPARATOR, stored in call n.
: CALL-SEPARATOR ( ^syntax n -- )
   {: state call:n :}
   state s" =" EXPECT$
   state T-STRING s" a string"
      [: dup s" ;" IS$ swap s" )" IS$ or ;] REQUIRE-KIND
   state STRING-TOKEN state call rot NODE-B! ;

\ Whether a term other than a blank node starts at the current token.
: TERM? ( ^syntax -- bool )
   {: state :} state TOKEN-K {: kind:n :}
   kind T-VAR = kind T-IRI = or kind T-NAME = or kind T-STRING = or
   kind T-INTEGER T-DOUBLE BETWEEN? or
   state s" true" IS$ or state s" false" IS$ or ;

\ Whether an expression, a blank node, BNODE or SHA1 starts at the current
\ token: PRIMARY names a fix of its own for the three.
: EXPRESSION-START? ( ^syntax -- bool )
   {: state :}
   state TERM? state TOKEN-K T-BNODE = or
   state TOKEN$ BUILTIN? or state s" NOT" IS$ or
   state s" EXISTS" IS$ or state s" BNODE" IS$ or
   state s" SHA1" IS$ or state s" (" IS$ or state s" !" IS$ or
   state s" +" IS$ or state s" -" IS$ or ;

\ Whether an ORDER BY condition starts at the current token.
: ORDER-START? ( ^syntax -- bool )
   {: state :} state EXPRESSION-START?
   state s" ASC" IS$ or state s" DESC" IS$ or ;

\ Whether the current token is SEPARATOR or ORDER, which begin a clause.
: CLAUSE-WORD? ( ^syntax -- bool )
   {: state :} state s" SEPARATOR" IS$ state s" ORDER" IS$ or ;

\ Whether the current token may follow a condition of a call's ORDER BY:
\ another condition, a clause, ; or ).
: CONDITION-FOLLOWS? ( ^syntax -- bool )
   {: state :}
   state ORDER-START? state CLAUSE-WORD? or
   state s" ;" IS$ or state s" )" IS$ or ;

\ The precedence of the operator the text spells, 0 for none.
: OPERATOR-RANK ( str -- n )
   {: t:str :}
   s" ||" t LISTS? if 1 exit then
   s" &&" t LISTS? if 2 exit then
   s" = != < > <= >= IN NOT" t LISTS? if 3 exit then
   s" + -" t LISTS? if 4 exit then
   s" * /" t LISTS? if 5 exit then 0 ;

: PRECEDENCE ( ^syntax -- n )
   TOKEN$ OPERATOR-RANK ;

\ The text of the token at decoded offset n, empty at the end; a scan.
: TOKEN-AT$ ( ^syntax n -- str )
   LEX-AT TOKEN-TEXT @ ;

\ The decoded offset of the token after the closer of the bracket at
\ offset n, else the query's length; a scan.
: PAST-GROUP ( ^syntax n -- n )
   {: state j:n :}
   0 j begin
      state over LEX-AT {: token :}
      token TOKEN-KIND @ 0= if nip exit then
      drop token TOKEN-TEXT @ BRACKET +
      state SYNTAX-POSITION @ over 0= if nip exit then
   again ;

\ The decoded offset of the token after the operand at the current
\ token: past the group it opens or the arguments of the call it names,
\ else the next; a scan.
: OPERAND-END ( ^syntax -- n )
   {: state :}
   state SYNTAX-CURSOR @ {: at:n :}
   state state SYNTAX-NEXT TOKEN-OFFSET {: next:n :}
   state TOKEN$ BRACKET 1 = if state at PAST-GROUP exit then
   state NEXT-TOKEN$ s" (" STR:= if state next PAST-GROUP exit then
   next ;

\ Rejects an operand at the current token where ) is expected and an
\ operator follows it: the fix writes an operator before it.
: CONTINUED-OPERAND ( ^syntax -- )
   {: state :}
   state EXPRESSION-START? 0= if exit then
   state [: state state OPERAND-END TOKEN-AT$ OPERATOR-RANK ;] SCANNING
   0<> if state s" )" s" write an operator here" REJECT then ;

\ Whether the current token may follow a term in an expression.
: TERM-FOLLOWS? ( ^syntax -- bool )
   {: state :}
   state PRECEDENCE 0<> state s" )" IS$ or state s" ," IS$ or
   state s" ;" IS$ or state s" AS" IS$ or ;

\ The ) of a call, a list or a parenthesized expression inside an
\ expression; w is what else the construct takes there, empty for
\ nothing. The quotation tests whether the current token may follow what
\ holds the expression; a token that may follow a term may follow the ).
\ Where w is not empty, an operand an operator follows asks for one.
: CLOSE-NESTED ( ^syntax str [ ^syntax -- bool ] -- )
   {: state w:str follows :}
   w STR:LEN 0<> if state CONTINUED-OPERAND then
   state w [: dup follows execute swap TERM-FOLLOWS? or ;]
   CLOSE-PAREN ;

\ The ) of a condition or an ORDER BY or GROUP BY expression in
\ parentheses, which an operator may continue.
: CLOSE-OPERATION ( ^syntax [ ^syntax -- bool ] -- )
   {: state follows :}
   state CONTINUED-OPERAND
   state s" an operator" follows CLOSE-PAREN ;

\ Expressions separated by commas; one after the last asks for a , before
\ it, or once there are most of them, for an operator before it when one
\ follows it, else for its removal.
: EXPRESSIONS ( ^syntax n [ ^syntax -- bool ] -- n )
   {: state most:n follows :}
   0 begin
      state 0 follows EXPRESSION state swap rot CONS
      state s" ," ACCEPT$ 0=
   until {: list:n :}
   state s" )" IS$ 0=  state EXPRESSION-START? and if
      state list LIST-LENGTH most >= if
         state CONTINUED-OPERAND
         state s" )" s" remove it" REJECT
      then state s" , or )" s" write , here" REJECT
   then state list REVERSE ;

: ARGUMENTS ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state s" )" ACCEPT$ if 0 exit then
   state MEM-MAX-N follows EXPRESSIONS
   state s" an operator" follows CLOSE-NESTED ;

\ Rejects a SEPARATOR at the current token, after or in the ORDER BY of
\ a call that has none.
: LATE-SEPARATOR ( ^syntax -- )
   {: state :}
   state s" SEPARATOR" IS$ if
      state s" )" s" write SEPARATOR before ORDER BY" REJECT
   then ;

\ Rejects an `=` at the current token, after a `;` after the ORDER BY of
\ a call that has no SEPARATOR.
: LATE-EQUALS ( ^syntax -- )
   {: state :}
   state s" =" IS$ if
      state s" )"
      s" write SEPARATOR before the =, then move SEPARATOR, the = and its string before ORDER BY"
      REJECT
   then ;

\ Whether the current token may follow an ORDER BY condition of a call
\ without SEPARATOR, rejecting a SEPARATOR there.
: UNSEPARATED-FOLLOWS? ( ^syntax -- bool )
   {: state :}
   state LATE-SEPARATOR  state CONDITION-FOLLOWS? ;

\ What may follow an ORDER BY condition of call n.
: ORDER-FOLLOWS ( ^syntax n -- [ ^syntax -- bool ] )
   NODE-B@ 0= if [: UNSEPARATED-FOLLOWS? ;] exit then
   [: CONDITION-FOLLOWS? ;] ;

\ Rejects the current token where ORDER BY has no condition: the fix
\ writes the variables before it when the flag says it may follow them,
\ else puts them in its place.
: NO-ORDER ( ^syntax bool -- )
   {: state after:bool :}
   s" a variable or expression to order by" {: x:str :}
   after state END? or if
      state x s" write the variables to order by after ORDER BY" REJECT
   then state x s" replace it with the variables to order by" REJECT ;

\ Rejects the current token after an ORDER without BY: the fix writes BY
\ before a condition, BY and a condition before the end or a token the
\ flag says may follow the conditions, else puts BY in its place.
: NO-BY ( ^syntax bool -- )
   {: state after:bool :}
   state ORDER-START? if state s" BY" s" write BY here" REJECT then
   after state END? or if
      state s" BY"
      s" write BY and the variables to order by after ORDER" REJECT
   then state s" BY" s" BY" REPLACE-WITH ;

\ A call's ORDER BY conditions in written order, up to SEPARATOR, ORDER or
\ a token no condition starts with.
: CALL-ORDER ( ^syntax n -- )
   {: state call:n :}
   state s" BY" ACCEPT$ 0= if
      state state CONDITION-FOLLOWS? NO-BY
   then
   state call ORDER-FOLLOWS {: follows :}
   0 begin
      state ORDER-START?
   while
      state follows ORDER-CONDITION
      state swap rot CONS
   repeat
   dup 0= if state state CONDITION-FOLLOWS? NO-ORDER then
   state call NODE-B@ 0= if state LATE-SEPARATOR then
   state swap REVERSE state call rot NODE-C! ;

: REJECT-IRI-CALL ( ^syntax n str -- )
   {: state call:n fix:str :}
   state call s" arguments without *, SEPARATOR or ORDER BY" fix
   REJECT-NODE ;

\ Rejects the `;` before the current token with the fix.
: REJECT-AT-SEMICOLON ( ^syntax str -- )
   {: state fix:str :}
   state state PREVIOUS-AT s" )" fix REJECT-TOKEN ;

: REJECT-SEMICOLON ( ^syntax -- )
   s" remove the ;" REJECT-AT-SEMICOLON ;

\ Rejects the `;` before the current token, which starts no clause.
: REMOVE-CLAUSE ( ^syntax -- )
   s" remove the ; and what follows it up to )" REJECT-AT-SEMICOLON ;

\ The fix of the clause at the current token, SEPARATOR or ORDER, in a
\ call that cannot take it.
: CLAUSE-FIX ( ^syntax -- str )
   s" SEPARATOR" IS$ if
      s" remove any ;, SEPARATOR, its = and its string"
   else s" remove any ;, ORDER BY and its conditions" then ;

\ Rejects the current token in call n, which takes no clause. What holds
\ the call first rejects a clause word it rejects wherever it stands.
: NO-CLAUSES ( ^syntax n [ ^syntax -- bool ] -- )
   {: state call:n follows :}
   state EXPRESSION-START? if
      state state PREVIOUS-AT s" , or )" s" replace it with ,"
         REJECT-TOKEN
   then
   state CLAUSE-WORD? 0= if state REMOVE-CLAUSE then
   state follows execute drop
   state CLAUSE-FIX {: fix:str :}
   state call TEXT$ BUILTIN? 0= if state call fix REJECT-IRI-CALL then
   state s" SEPARATOR" IS$ if
      state call s" GROUP_CONCAT, the only function that takes SEPARATOR"
         fix REJECT-NODE
   then
   state call s" GROUP_CONCAT, the only function that takes ORDER BY"
      fix REJECT-NODE ;

\ The fix of a SEPARATOR in a call that has one.
: SEPARATOR-FIX ( -- str )
   s" remove the ;, the second SEPARATOR, its = and its string" ;

\ Rejects the token after a `;`, which is not ORDER, in a call with
\ SEPARATOR and no ORDER BY.
: SEPARATED-CLAUSE ( ^syntax -- )
   {: state :}
   state s" SEPARATOR" IS$ if
      state s" ORDER" SEPARATOR-FIX REJECT
   then
   state s" =" IS$ if
      state s" ORDER" s" remove the ;, its = and its string" REJECT
   then
   state s" BY" IS$ if state s" ORDER" s" write ORDER here" REJECT then
   state REMOVE-CLAUSE ;

\ Rejects an ORDER at the current token in a call that has its ORDER BY.
: SECOND-ORDER ( ^syntax -- )
   {: state :}
   state s" ORDER" IS$ if
      state s" )" s" write the conditions in one ORDER BY" REJECT
   then ;

\ Rejects the token after a `;` in call n, which has its ORDER BY.
: ORDERED-CLAUSE ( ^syntax n -- )
   {: state call:n :}
   state call NODE-B@ 0= if
      state LATE-SEPARATOR  state LATE-EQUALS
   then
   state s" SEPARATOR" IS$ if state s" )" SEPARATOR-FIX REJECT then
   state SECOND-ORDER
   state s" =" IS$ if
      state s" remove the ;, its = and its string" REJECT-AT-SEMICOLON
   then state REJECT-SEMICOLON ;

\ Reads or rejects the clause after a `;` in call n by the file header's table.
: CLAUSE ( ^syntax n [ ^syntax -- bool ] -- )
   {: state call:n follows :}
   state call s" GROUP_CONCAT" CALL-IS? 0= if
      state call follows NO-CLAUSES
   then
   state s" )" IS$ state s" ;" IS$ or if state REJECT-SEMICOLON then
   state call NODE-C@ 0<> if state call ORDERED-CLAUSE then
   state s" ORDER" ACCEPT$ if state call CALL-ORDER exit then
   state call NODE-B@ 0<> if state SEPARATED-CLAUSE then
   state s" SEPARATOR" ACCEPT$ if state call CALL-SEPARATOR exit then
   state s" BY" IS$ if
      state s" SEPARATOR or ORDER" s" write ORDER here" REJECT
   then
   state s" =" IS$ if
      state s" SEPARATOR or ORDER" s" write SEPARATOR here" REJECT
   then state REMOVE-CLAUSE ;

\ Rejects a SEPARATOR with no `;` before it at the current token in call
\ n, which has one.
: SECOND-SEPARATOR ( ^syntax n -- )
   {: state call:n :}
   state s" SEPARATOR" IS$ 0=  state call NODE-B@ 0=  or if exit then
   state  state call NODE-C@ 0<> if s" )" else s" ; or )" then
   s" remove the second SEPARATOR, its = and its string" REJECT ;

\ Rejects the SEPARATOR or ORDER at the current token, with no `;` before
\ it, in call n by the file header.
: UNSEPARATED-CLAUSE ( ^syntax n [ ^syntax -- bool ] -- )
   {: state call:n follows :}
   state call s" GROUP_CONCAT" CALL-IS? if
      state call NODE-C@ 0<> if state SECOND-ORDER then
      state call SECOND-SEPARATOR
      state s" ;" EXPECT$
   then state call follows NO-CLAUSES ;

\ The `; SEPARATOR = string` and `; ORDER BY` clauses of call n, by the file
\ header's table; the quotation tests what may follow the call.
: CALL-CLAUSES ( ^syntax n [ ^syntax -- bool ] -- )
   {: state call:n follows :}
   begin
      state CLAUSE-WORD? if state call follows UNSEPARATED-CLAUSE then
      state s" ;" ACCEPT$
   while state call follows CLAUSE repeat ;

\ What call n takes before its ): nothing after * or a clause, else an
\ operator.
: CALL-REST ( ^syntax n -- str )
   {: state call:n :}
   state call NODE-FLAGS@ F-STAR and 0<>
   state call NODE-B@ 0<> or  state call NODE-C@ 0<> or
   if s" " exit then s" an operator" ;

\ The most arguments call n takes.
: CALL-MOST ( ^syntax n -- n )
   {: state call:n :}
   state call TEXT$ BUILTIN? 0= if MEM-MAX-N exit then
   state call CALL-ARITY nip ;

: CALL-ARGUMENTS ( ^syntax n [ ^syntax -- bool ] -- )
   {: state call:n follows :}
   state s" DISTINCT" ACCEPT$ if
      state call F-DISTINCT NODE-FLAGS!
   then
   state s" *" ACCEPT$ if
      state call state call NODE-FLAGS@ F-STAR + NODE-FLAGS!
   else
      state s" )" ACCEPT$ if exit then
      state state call CALL-MOST follows EXPRESSIONS
      state call rot NODE-A!
   then
   state call follows CALL-CLAUSES
   state state call CALL-REST follows CLOSE-NESTED ;

: AGGREGATE? ( ^syntax n -- bool )
   s" COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT" CALL-IN? ;

: EXISTS-CALL? ( ^syntax n -- bool )
   {: state node:n :}
   state node s" EXISTS" CALL-IS?
   state node s" NOT EXISTS" CALL-IS? or ;

: CALL-COUNT ( ^syntax n -- n )
   {: state call:n :}
   state state call NODE-A@ LIST-LENGTH ;

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

\ The decoded offset of the last token from offset a that starts before
\ offset b; a scan.
: LAST-BEFORE ( ^syntax n n -- n )
   {: state a:n b:n :}
   a begin
      state over PAST-TOKEN dup b <
   while nip repeat drop ;

\ Rejects call n's one argument over its text; the parser is past the
\ call's ).
: REJECT-ARGUMENT ( ^syntax n str str -- )
   {: state call:n x:str f:str :}
   state PREVIOUS-AT {: close:n :}
   state state state call NODE-TOKEN@ PAST-TOKEN PAST-TOKEN
      {: start:n :}
   state start close LAST-BEFORE state swap LEX-FROM
   state LEX-ONE state start x f REJECT-SPAN ;

: CHECK-CALL ( ^syntax n -- )
   {: state call:n :}
   state call TEXT$ BUILTIN? 0= if
      state call NODE-FLAGS@ F-STAR and 0<> if
         state call s" remove the *" REJECT-IRI-CALL
      then exit
   then
   state call NODE-FLAGS@ {: flags:n :}
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
   state call CALL-ARITY {: least:n most:n :}
   state call CALL-COUNT least most BETWEEN? 0= if
      state least most ARITY-TEXT {: count:str :}
      state call count state call count ARITY-FIX REJECT-NODE
   then
   state call s" BOUND" CALL-IS? if
      state state call NODE-A@ NODE-A@ {: argument:n :}
      state argument NODE-K N-VAR <> if
         state call s" a variable" s" pass BOUND a variable"
            REJECT-ARGUMENT
      then
   then ;

: READ-CALL ( ^syntax str [ ^syntax -- bool ] -- n )
   {: state text:str follows :}
   text BUILTIN? if
      state text COPY$ {: name:str :}
      name str> {: bytes length:n :}
      length 0 ?do bytes i + dup c@ STR:ASCII-UPPER swap c! loop
      name
   else text then {: called:str :}
   state N-CALL called TEXT-NODE {: call:n :}
   state PREVIOUS-AT state call rot NODE-TOKEN!
   state s" (" EXPECT$ state call follows CALL-ARGUMENTS
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

\ Rejects the current token, which starts no operand. What holds the
\ expression first rejects a token it rejects wherever it stands: a
\ SEPARATOR in an ORDER BY condition of a call without one.
: NO-OPERAND ( ^syntax [ ^syntax -- bool ] -- )
   {: state follows :}
   state follows execute drop
   state EXPRESSION-TERM TERM-EXAMPLE state TERM-FOLLOWS? MISSING ;

: ATOM ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state SIGNED-NUMBER? if state SIGNED-NUMBER exit then
   state s" (" ACCEPT$ if
      state 0 follows EXPRESSION
      state s" an operator" follows CLOSE-NESTED exit
   then
   state s" NOT" ACCEPT$ if state true READ-EXISTS exit then
   state s" EXISTS" IS$ if state false READ-EXISTS exit then
   state TOKEN-K T-WORD = if
      state DROPPED-CALL
      state TOKEN$ {: text:str :}
      text BUILTIN? if
         state NEXT-TOKEN state text follows READ-CALL exit
      then
   then
   state TOKEN-K T-BNODE = if
      state EXPRESSION-TERM
         s" replace the blank node with a variable" REJECT
   then
   state TERM? 0= if state follows NO-OPERAND then
   state [: TERM-FOLLOWS? ;] READ-TERM {: term:n :}
   state term NODE-K N-IRI = state s" (" IS$ and if
      state state term IRI$ follows READ-CALL
   else term then ;

\ Longest-token parsing makes -01 a literal and - 01 an expression.
\ The grammar permits one unary operator before a primary expression.
: PRIMARY ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state SIGNED-NUMBER? if state SIGNED-NUMBER exit then
   state s" !" IS$ state s" +" IS$ or
   state s" -" IS$ or if
      state TOKEN$ {: text:str :} state NEXT-TOKEN
      state follows ATOM {: value:n :}
      state N-UNARY text value 0 OPERATOR-NODE
   else state follows ATOM then ;

: INFIX ( ^syntax n n [ ^syntax -- bool ] -- n )
   {: state left:n precedence:n follows :}
   state s" NOT" ACCEPT$ if
      state s" IN" EXPECT$ state s" (" EXPECT$
      state follows ARGUMENTS {: right:n :}
      state N-BINARY s" NOT IN" left right OPERATOR-NODE exit
   then
   state s" IN" ACCEPT$ if
      state s" (" EXPECT$ state follows ARGUMENTS
      {: right:n :}
      state N-BINARY s" IN" left right OPERATOR-NODE exit
   then
   state TOKEN$ {: text:str :} state NEXT-TOKEN
   state precedence 1+ follows EXPRESSION {: right:n :}
   state N-BINARY text left right OPERATOR-NODE ;

: READ-EXPRESSION ( ^syntax n [ ^syntax -- bool ] -- n )
   {: state minimum:n follows :}
   state NEST
   state follows PRIMARY false
   begin
      state PRECEDENCE dup 0<> swap minimum >= and
   while
      {: left:n compared:bool :}
      state PRECEDENCE {: precedence:n :}
      precedence 3 = compared and if
         state s" one comparison per operand pair"
            s" put the first comparison in parentheses" REJECT
      then
      state left precedence follows INFIX
      compared precedence 3 = or
   repeat drop state UNNEST ;

: ENCLOSE-CONDITION ( ^syntax n -- )
   {: state start:n :}
   state start s" a condition in parentheses or a function call"
      s" put the condition in parentheses" REJECT-TOKEN ;

\ Whether an operator is the current token: a NOT before EXISTS starts a
\ condition of its own.
: AT-OPERATOR? ( ^syntax -- bool )
   {: state :}
   state PRECEDENCE 0= if false exit then
   state s" NOT" IS$ 0= if true exit then
   state NEXT-TOKEN$ s" EXISTS" STR:=CI 0= ;

\ Rejects an operator after the condition that starts at decoded offset
\ start: an expression is a condition only in parentheses.
: WHOLE-CONDITION ( ^syntax n -- )
   {: state start:n :}
   state AT-OPERATOR? if state start ENCLOSE-CONDITION then ;

: CONSTRAINT ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state SYNTAX-CURSOR @ {: start:n :}
   state s" (" ACCEPT$ if
      state 0 follows EXPRESSION
      state follows CLOSE-OPERATION
      state start WHOLE-CONDITION exit
   then
   state follows PRIMARY
   dup state swap NODE-K N-CALL <> if state start ENCLOSE-CONDITION then
   state start WHOLE-CONDITION ;

\ A variable condition of ORDER BY or GROUP BY.
: VAR-CONDITION ( ^syntax -- n )
   {: state :}
   state SYNTAX-CURSOR @ {: start:n :}
   state READ-VAR state start WHOLE-CONDITION ;

: READ-ORDER-CONDITION ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state s" ASC" IS$ state s" DESC" IS$ or if
      state s" DESC" IS$ if F-DESC else 0 then
         {: flags:n :}
      state NEXT-TOKEN state s" (" EXPECT$
      state 0 follows EXPRESSION {: expression:n :}
      state follows CLOSE-OPERATION
      state N-ORDER expression UNARY-NODE {: node:n :}
      state node flags NODE-FLAGS! node
   else
      state TOKEN-K T-VAR = if state VAR-CONDITION
      else state follows CONSTRAINT then
      state N-ORDER rot UNARY-NODE
   then ;

: ALIAS ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state s" (" EXPECT$
   state 0 follows EXPRESSION {: value:n :}
   state s" AS" EXPECT$
   state T-VAR s" a variable" [: s" )" IS$ ;] REQUIRE-KIND
   state READ-VAR {: name:n :}
   state s" " follows CLOSE-PAREN
   state N-ALIAS value name BINARY-NODE ;

: INSTALL-EXPRESSION ( -- )
   ['] READ-EXPRESSION is EXPRESSION
   ['] READ-ORDER-CONDITION is ORDER-CONDITION ;

INSTALL-EXPRESSION

;package
