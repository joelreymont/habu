\ An expression has two SQL forms, a term (JSONB) and a test (an SQL
\ boolean), NULL in either for an error. A boolean-valued expression is
\ written as its test, and its term is sparql.boolean of it; any other is
\ written as its term, and its test is sparql.ebv of it. CASE and COALESCE
\ keep SPARQL's selected terms and short-circuiting. A SPARQL function is
\ `sparql.` and the parser's name for it, one of a fixed list, in lower
\ case.

require src/lib/sparql/emit.f
require src/lib/sparql/statement.f
require src/lib/sparql/write.f
require src/lib/sparql/syntax.f
require src/lib/sparql/iri.f
require src/lib/vector.f
require src/lib/sparql/expression.f

package SPARQL
private

: EXPRESSIONS+ ( ^plan n -- )
   {: plan expressions:n :} plan TREE {: state :}
   expressions begin dup 0<> while
      {: list:n :}
      plan state list NODE-A@ SQL-EXPRESSION
      state list NODE-B@ dup 0<> if plan s" ," SQL+ then
   repeat drop ;

: ARGUMENT@ ( ^syntax n n -- n )
   {: state call:n index:n :}
   state call NODE-A@
   index 0 ?do state swap NODE-B@ loop state swap NODE-A@ ;

\ A variable is its term in the scope, SQL NULL where the scope does not
\ bind it.
: TERM+ ( ^plan n -- )
   {: plan node:n :}
   plan TREE node VARIABLE? 0= if
      plan plan PLAN-OUT @ node +TERM-ARG exit
   then
   plan PLAN-SCOPE @ plan node VAR LOOKUP {: b:^ :}
   b NULL-PTR = if plan s" NULL::jsonb" SQL+ exit then
   plan plan PLAN-OUT @ b +TERM ;

: IF+ ( ^plan n -- )
   {: plan call:n :} plan TREE {: state :}
   plan s" (CASE " SQL+ plan state call 0 ARGUMENT@ SQL-TEST
   plan s"  WHEN true THEN " SQL+
   plan state call 1 ARGUMENT@ SQL-EXPRESSION
   plan s"  WHEN false THEN " SQL+
   plan state call 2 ARGUMENT@ SQL-EXPRESSION
   plan s"  END)" SQL+ ;

: COALESCE+ ( ^plan n -- )
   {: plan call:n :}
   plan TREE call CALL-COUNT 0= if
      plan s" NULL::jsonb" SQL+ exit
   then
   plan s" coalesce(" SQL+
   plan plan TREE call NODE-A@ EXPRESSIONS+ plan s" )" SQL+ ;

: IRI-FUNCTION+ ( ^plan n -- )
   {: plan call:n :} plan TREE {: state :}
   plan s" sparql.iri(" SQL+
   plan state call 0 ARGUMENT@ SQL-EXPRESSION plan s" ," SQL+
   state SYNTAX-DECLARED-BASE @ {: base:n :}
   base 0= if plan s" NULL::text" SQL+
   else plan state base IRI$ STRING+ then
   plan s" )" SQL+ ;

\ The aggregate call's SQL among the plan's aggregates.
: AGGREGATE+ ( ^plan n -- )
   {: plan call:n :}
   plan PLAN-AGGREGATES @ plan call FIND-AGGREGATE VEC:AT {: a:^ :}
   plan PLAN-OUT @ a AGGREGATE-SQL @ +SPLICE ;

: LOWER+ ( ^plan str -- )
   {: plan text:str :} plan PLAN-OUT @ {: out:^ :}
   text str> 0 ?do
      dup i + c@ STR:ASCII-LOWER out +BYTE c!
   loop drop ;

\ The built-in call as its SQL function's.
: NAMED+ ( ^plan n -- )
   {: plan call:n :} plan TREE {: state :}
   plan s" sparql." SQL+ plan state call TEXT$ LOWER+
   plan s" (" SQL+ plan state call NODE-A@ EXPRESSIONS+
   plan s" )" SQL+ ;

: CONCAT+ ( ^plan n -- )
   {: plan call:n :}
   plan s" sparql.concat(ARRAY[" SQL+
   plan plan TREE call NODE-A@ EXPRESSIONS+
   plan s" ]::jsonb[])" SQL+ ;

\ An IRI's call: sparql.cast is NULL for an IRI that is no xsd
\ constructor, which takes one argument.
: CONSTRUCTOR+ ( ^plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call CALL-COUNT 1 <> if
      plan s" NULL::jsonb" SQL+ exit
   then
   plan s" sparql.cast(" SQL+ plan state call TEXT$ STRING+
   plan s" ," SQL+ plan state call 0 ARGUMENT@ SQL-EXPRESSION
   plan s" )" SQL+ ;

: CALL-TERM+ ( ^plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call TEXT$ BUILTIN? 0= if plan call CONSTRUCTOR+ exit then
   state call s" IF" CALL-IS? if plan call IF+ exit then
   state call s" COALESCE" CALL-IS? if plan call COALESCE+ exit then
   state call s" IRI URI" CALL-IN? if
      plan call IRI-FUNCTION+ exit
   then
   state call AGGREGATE? if plan call AGGREGATE+ exit then
   state call s" CONCAT" CALL-IS? if plan call CONCAT+ exit then
   plan call NAMED+ ;

: BOUND+ ( ^plan n -- )
   {: plan call:n :}
   plan plan TREE call 0 ARGUMENT@ VAR {: var:n :}
   plan PLAN-SCOPE @ var LOOKUP {: b:^ :}
   b NULL-PTR = if plan s" false" SQL+ exit then
   plan s" (" SQL+ plan PLAN-OUT @ b +BINDING
   plan s"  IS NOT NULL)" SQL+ ;

: CALL-TEST+ ( ^plan n -- )
   {: plan call:n :} plan TREE {: state :}
   state call EXISTS-CALL? if plan call EXISTS-SQL exit then
   state call s" BOUND" CALL-IS? if plan call BOUND+ exit then
   plan call NAMED+ ;

: INFIX-IS? ( ^plan n str -- bool )
   {: plan node:n text:str :}
   plan TREE node TEXT$ text STR:=CI ;

: LOGICAL+ ( ^plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan s" (" SQL+ plan state node NODE-A@ SQL-TEST
   plan node s" &&" INFIX-IS? if plan s"  AND " SQL+
   else plan s"  OR " SQL+ then
   plan state node NODE-B@ SQL-TEST plan s" )" SQL+ ;

\ Appends `(NOT ` when the operator is the negative one.
: NEGATED+ ( ^plan n str -- bool )
   {: plan node:n negative:str :}
   plan node negative INFIX-IS?
   dup if plan s" (NOT " SQL+ then ;

: NATIVE+ ( ^plan n ^binding n -- )
   {: plan c:n k:^ e:n :} plan plan PLAN-OUT @ c k e +NATIVE ;

\ `(a IN (list))` or NOT IN of the node's native operands in class c.
: NATIVE-IN+ ( ^plan n n -- )
   {: plan node:n c:n :} plan TREE {: state :}
   plan node IN-NATIVE {: k:^ :}
   plan s" (" SQL+ plan c k state node NODE-A@ NATIVE+
   plan node s" NOT IN" INFIX-IS? if plan s"  NOT" SQL+ then
   plan s"  IN (" SQL+
   state node NODE-B@ {: list:n :}
   list begin dup 0<> while
      {: each:n :}
      each list <> if plan s" ," SQL+ then
      plan c k state each NODE-A@ NATIVE+
      state each NODE-B@
   repeat drop plan s" ))" SQL+ ;

: IN+ ( ^plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan node IN-CLASS {: c:n :}
   c 0<> if plan node c NATIVE-IN+ exit then
   plan node s" NOT IN" NEGATED+ {: negated:bool :}
   plan s" sparql.in(" SQL+
   plan state node NODE-A@ SQL-EXPRESSION
   plan s" ,ARRAY[" SQL+
   plan state node NODE-B@ EXPRESSIONS+
   plan s" ]::jsonb[])" SQL+
   negated if plan s" )" SQL+ then ;

\ The operator's two terms and the call's `)`.
: OPERANDS+ ( ^plan n -- )
   {: plan node:n :} plan TREE {: state :}
   plan state node NODE-A@ SQL-EXPRESSION plan s" ," SQL+
   plan state node NODE-B@ SQL-EXPRESSION plan s" )" SQL+ ;

\ `(a op b)` of the node's native operands in class c.
: NATIVE-PAIR+ ( ^plan n n str bool -- )
   {: plan node:n c:n op:str ordered:bool :} plan TREE {: state :}
   plan node PAIR-NATIVE {: k:^ :}
   plan s" (" SQL+ plan c k state node NODE-A@ NATIVE+
   ordered c TEXTUAL = and if plan s\"  COLLATE \"C\"" SQL+ then
   plan s"  " SQL+ plan op SQL+ plan s"  " SQL+
   plan c k state node NODE-B@ NATIVE+ plan s" )" SQL+ ;

: EQUAL+ ( ^plan n -- )
   {: plan node:n :}
   plan node PAIR-CLASS {: c:n :}
   c 0<> if
      plan node s" !=" INFIX-IS? if s" <>" else s" =" then {: op:str :}
      plan node c op false NATIVE-PAIR+ exit
   then
   plan node s" !=" NEGATED+ {: negated:bool :}
   plan s" sparql.equal(" SQL+ plan node OPERANDS+
   negated if plan s" )" SQL+ then ;

: ORDERED+ ( ^plan n -- )
   {: plan node:n :}
   plan node PAIR-CLASS {: c:n :}
   c 0<> if
      plan node c plan TREE node TEXT$ true NATIVE-PAIR+ exit
   then
   plan s" sparql.relation('" SQL+
   plan node s" <" INFIX-IS? if plan s" <" SQL+ then
   plan node s" <=" INFIX-IS? if plan s" <=" SQL+ then
   plan node s" >" INFIX-IS? if plan s" >" SQL+ then
   plan node s" >=" INFIX-IS? if plan s" >=" SQL+ then
   plan s" '," SQL+ plan node OPERANDS+ ;

: EITHER? ( ^plan n str str -- bool )
   {: plan node:n a:str b:str :}
   plan node a INFIX-IS? plan node b INFIX-IS? or ;

: ARITHMETIC? ( ^plan n -- bool )
   {: plan node:n :}
   plan node s" +" s" -" EITHER? plan node s" *" s" /" EITHER? or ;

: BINARY-TEST+ ( ^plan n -- )
   {: plan node:n :}
   plan node s" &&" s" ||" EITHER? if plan node LOGICAL+ exit then
   plan node s" IN" s" NOT IN" EITHER? if plan node IN+ exit then
   plan node s" =" s" !=" EITHER? if plan node EQUAL+ exit then
   plan node ORDERED+ ;

: BINARY-TERM+ ( ^plan n -- )
   {: plan node:n :}
   plan s" sparql.arithmetic(" SQL+
   plan plan TREE node TEXT$ STRING+ plan s" ," SQL+
   plan node OPERANDS+ ;

: NOT+ ( ^plan n -- )
   {: plan node:n :}
   plan s" (NOT " SQL+ plan plan TREE node NODE-A@ SQL-TEST
   plan s" )" SQL+ ;

: SIGN+ ( ^plan n -- )
   {: plan node:n :}
   plan s" sparql.arithmetic(" SQL+
   plan node s" +" INFIX-IS? if plan s" 'u+'," SQL+ else plan s" 'u-'," SQL+ then
   plan s" NULL::jsonb," SQL+
   plan plan TREE node NODE-A@ SQL-EXPRESSION plan s" )" SQL+ ;

: BOOLEAN-CALL? ( ^plan n -- bool )
   {: plan call:n :}
   plan TREE call EXISTS-CALL? if true exit then
   plan TREE call
      s" BOUND CONTAINS STRSTARTS STRENDS LANGMATCHES REGEX ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC SAMETERM"
      CALL-IN? ;

: BOOLEAN? ( ^plan n -- bool )
   {: plan node:n :}
   plan TREE node NODE-K case
      N-UNARY of plan node s" !" INFIX-IS? exit endof
      N-BINARY of plan node ARITHMETIC? 0= exit endof
      N-CALL of plan node BOOLEAN-CALL? exit endof
   endcase false ;

\ The boolean-valued expression as an SQL boolean.
: BOOLEAN+ ( ^plan n -- )
   {: plan node:n :}
   plan TREE node NODE-K case
      N-UNARY of plan node NOT+ endof
      N-BINARY of plan node BINARY-TEST+ endof
      plan node CALL-TEST+
   endcase ;

: TEST+ ( ^plan n -- )
   {: plan node:n :}
   plan node BOOLEAN? if plan node BOOLEAN+ exit then
   plan s" sparql.ebv(" SQL+ plan node SQL-EXPRESSION
   plan s" )" SQL+ ;

: EXPRESSION+ ( ^plan n -- )
   {: plan node:n :}
   plan node BOOLEAN? if
      plan s" sparql.boolean(" SQL+ plan node BOOLEAN+
      plan s" )" SQL+ exit
   then
   plan TREE node NODE-K case
      N-UNARY of plan node SIGN+ endof
      N-BINARY of plan node BINARY-TERM+ endof
      N-CALL of plan node CALL-TERM+ endof
      \ N-VAR, N-IRI or N-LITERAL: the parser makes no other expression.
      plan node TERM+
   endcase ;

: INSTALL-FORMS ( -- )
   ['] EXPRESSION+ is SQL-EXPRESSION ['] TEST+ is SQL-TEST ;

INSTALL-FORMS

;package
