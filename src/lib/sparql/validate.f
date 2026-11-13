\ Scope restrictions from SPARQL 1.1 sections 18.2.1 and 19.8. A set is a
\ list of variable nodes; expression-only uses never introduce bindings.

require src/lib/sparql/pattern.f

package SPARQL
private

defer SCOPE ( ptr syntax n -- n )
defer QUERY-SCOPE ( ptr syntax n -- n )
defer EXPRESSION-CHECK ( ptr syntax n bool -- )
defer CONTAINS-AGGREGATE? ( ptr syntax n -- bool )
defer CHECK-GROUPED ( ptr syntax n n -- )

: NAME-IN? ( ptr syntax n n -- bool )
   {: state name:n names:n :}
   names begin dup 0<> while
      {: list:n :}
      state state list NODE-A@ TEXT$
      state name TEXT$ STR= if true exit then
      state list NODE-B@
   repeat drop false ;

: SCOPE-ADD ( ptr syntax n n -- n )
   {: state name:n names:n :}
   state name NODE-K N-VAR <> if names exit then
   state name names NAME-IN? if names
   else state name names CONS then ;

: SCOPE-UNION ( ptr syntax n n -- n )
   {: state left:n right:n :}
   left right begin dup 0<> while
      {: result:n list:n :}
      state state list NODE-A@ result SCOPE-ADD
      state list NODE-B@
   repeat drop ;

: FRESH-VAR ( ptr syntax n n -- n )
   {: state name:n names:n :}
   state name names NAME-IN? if E-SYNTAX throw then
   state name names CONS ;

: UNIQUE-NAMES ( ptr syntax n -- n )
   {: state list:n :}
   0 list begin dup 0<> while
      {: names:n current:n :}
      state state current NODE-A@ names FRESH-VAR
      state current NODE-B@
   repeat drop ;

: BLANK-LABEL ( ptr syntax n n -- )
   {: state term:n bgp:n :}
   state term NODE-K N-BNODE <> if exit then
   state term NODE NODE-FLAGS @ F-GENERATED and 0<> if exit then
   state SYNTAX-LABELS @ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: label:n :}
      state label TEXT$ state term TEXT$ STR= if
         state label NODE NODE-VALUE @ bgp <> if
            E-SYNTAX throw
         then exit
      then state list NODE-B@
   repeat drop
   state N-ALIAS state term TEXT$ TEXT-NODE {: label:n :}
   bgp state label NODE NODE-VALUE !
   state label state SYNTAX-LABELS @ CONS
      state SYNTAX-LABELS ! ;

: TRIPLE-LABELS ( ptr syntax n n -- )
   {: state triple:n bgp:n :}
   state state triple NODE-A@ bgp BLANK-LABEL
   state state triple NODE-C@ bgp BLANK-LABEL ;

: EXPRESSION-CHECK-BODY ( ptr syntax n bool -- )
   {: state expression:n aggregates:bool :}
   expression 0= if exit then
   state expression NODE-K {: kind:n :}
   kind N-LIST = if
      expression begin dup 0<> while
         {: list:n :}
         state state list NODE-A@ aggregates EXPRESSION-CHECK
         state list NODE-B@
      repeat drop exit
   then
   kind N-CALL = if
      state expression EXISTS-CALL? if
         state state expression NODE-A@ SCOPE drop exit
      then
      state expression AGGREGATE? if
         aggregates 0= if E-SYNTAX throw then
         state state expression NODE-A@ false EXPRESSION-CHECK exit
      then
   then
   kind N-CALL = kind N-UNARY = or kind N-BINARY = or
   kind N-LIST = or kind N-ALIAS = or if
      state state expression NODE-A@ aggregates EXPRESSION-CHECK
      kind N-CALL <> kind N-UNARY <> and if
         state state expression NODE-B@ aggregates EXPRESSION-CHECK
      then
   then ;

: CHECK-EXPRESSION ( ptr syntax n bool -- )
   {: state expression:n aggregates:bool :} state NEST
   state expression aggregates EXPRESSION-CHECK-BODY
   state UNNEST ;

' CHECK-EXPRESSION is EXPRESSION-CHECK

: TRIPLE-SCOPE ( ptr syntax n -- n )
   {: state triple:n :}
   state state triple NODE-A@ 0 SCOPE-ADD
   state state triple NODE-B@ rot SCOPE-ADD
   state state triple NODE-C@ rot SCOPE-ADD ;

: BIND-SCOPE ( ptr syntax n n -- n )
   {: state binding:n names:n :}
   state state binding NODE-A@ false EXPRESSION-CHECK
   state state binding NODE-B@ names FRESH-VAR ;

: GROUP-SCOPE ( ptr syntax n -- n )
   {: state group:n :}
   0 0 state group NODE-A@ begin dup 0<> while
      {: names:n bgp:n list:n :}
      state list NODE-A@ {: pattern:n :}
      state pattern NODE-K {: kind:n :}
      kind N-TRIPLE = if
         bgp 0= if pattern else bgp then {: current:n :}
         state pattern current TRIPLE-LABELS
         state names state pattern SCOPE SCOPE-UNION current
      else
         kind N-BIND = if
            state pattern names BIND-SCOPE 0
         else
            state names state pattern SCOPE SCOPE-UNION
            kind N-FILTER = if bgp else 0 then
         then
      then state list NODE-B@
   repeat 2drop ;

: PATTERN-SCOPE-BODY ( ptr syntax n -- n )
   {: state pattern:n :}
   state pattern NODE-K case
      N-TRIPLE of state pattern TRIPLE-SCOPE endof
      N-GROUP of state pattern GROUP-SCOPE endof
      N-OPTIONAL of state state pattern NODE-A@ SCOPE endof
      N-UNION of
         state state state pattern NODE-A@ SCOPE
         state state pattern NODE-B@ SCOPE SCOPE-UNION
      endof
      N-GRAPH of
         state state pattern NODE-A@
         state state pattern NODE-B@ SCOPE SCOPE-ADD
      endof
      N-SERVICE of
         state state pattern NODE-A@
         state state pattern NODE-B@ SCOPE SCOPE-ADD
      endof
      N-MINUS of state state pattern NODE-A@ SCOPE drop 0 endof
      N-FILTER of
         state state pattern NODE-A@ false EXPRESSION-CHECK 0
      endof
      N-BIND of state pattern 0 BIND-SCOPE endof
      N-VALUES of state state pattern NODE-A@ UNIQUE-NAMES endof
      N-SELECT of state pattern QUERY-SCOPE endof
      E-SYNTAX throw
   endcase ;

: PATTERN-SCOPE ( ptr syntax n -- n )
   {: state pattern:n :} state NEST
   state pattern PATTERN-SCOPE-BODY state UNNEST ;

' PATTERN-SCOPE is SCOPE

: CONTAINS-AGGREGATE-BODY ( ptr syntax n -- bool )
   {: state expression:n :}
   expression 0= if false exit then
   state expression NODE-K {: kind:n :}
   kind N-LIST = if
      expression begin dup 0<> while
         {: list:n :}
         state state list NODE-A@ CONTAINS-AGGREGATE? if
            true exit
         then state list NODE-B@
      repeat drop false exit
   then
   kind N-CALL = if
      state expression AGGREGATE? if true exit then
   then
   kind N-CALL = kind N-UNARY = or kind N-BINARY = or
   kind N-LIST = or kind N-ALIAS = or if
      state state expression NODE-A@ CONTAINS-AGGREGATE?
         if true exit then
      kind N-CALL <> kind N-UNARY <> and if
         state state expression NODE-B@ CONTAINS-AGGREGATE? exit
      then
   then false ;

: CONTAINS-AGGREGATE ( ptr syntax n -- bool )
   {: state expression:n :} state NEST
   state expression CONTAINS-AGGREGATE-BODY state UNNEST ;

' CONTAINS-AGGREGATE is CONTAINS-AGGREGATE?

: MODIFIER-AGGREGATE? ( ptr syntax n -- bool )
   {: state list:n :}
   list begin dup 0<> while
      {: current:n :} state current NODE-A@ {: modifier:n :}
      state modifier NODE-K dup N-ORDER = swap N-HAVING = or if
         state state modifier NODE-A@ CONTAINS-AGGREGATE?
         if true exit then
      then state current NODE-B@
   repeat drop false ;

: GROUPED-NAMES ( ptr syntax n n -- n n )
   {: state expressions:n scope:n :}
   scope 0 expressions begin dup 0<> while
      {: names:n grouped:n list:n :}
      state list NODE-A@ {: expression:n :}
      state expression false EXPRESSION-CHECK
      state expression NODE-K N-ALIAS = if
         state expression NODE-B@ {: name:n :}
         state name names FRESH-VAR
         state name grouped SCOPE-ADD
      else
         names state expression grouped SCOPE-ADD
      then state list NODE-B@
   repeat drop ;

: CHECK-GROUPED-BODY ( ptr syntax n n -- )
   {: state expression:n grouped:n :}
   expression 0= if exit then
   state expression NODE-K {: kind:n :}
   kind N-LIST = if
      expression begin dup 0<> while
         {: list:n :}
         state state list NODE-A@ grouped CHECK-GROUPED
         state list NODE-B@
      repeat drop exit
   then
   kind N-VAR = if
      state expression grouped NAME-IN? 0= if E-SYNTAX throw then
      exit
   then
   kind N-CALL = if
      state expression AGGREGATE? if exit then
      state expression EXISTS-CALL? if exit then
   then
   kind N-CALL = kind N-UNARY = or kind N-BINARY = or
   kind N-LIST = or kind N-ALIAS = or if
      state state expression NODE-A@ grouped CHECK-GROUPED
      kind N-CALL <> kind N-UNARY <> and kind N-ALIAS <> and if
         state state expression NODE-B@ grouped CHECK-GROUPED
      then
   then ;

: GROUPED-CHECK ( ptr syntax n n -- )
   {: state expression:n grouped:n :} state NEST
   state expression grouped CHECK-GROUPED-BODY state UNNEST ;

' GROUPED-CHECK is CHECK-GROUPED

: GROUP-BY-NODE ( ptr syntax n -- n )
   {: state query:n :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K N-GROUPBY = if modifier exit then
      state list NODE-B@
   repeat drop 0 ;

: TAIL-NAMES ( ptr syntax n -- n )
   {: state query:n :}
   state query NODE NODE-VALUE @ dup 0<> if state swap SCOPE
   else drop 0 then ;

: QUERY-NAMES ( ptr syntax n -- n )
   {: state query:n :}
   state state state query NODE-B@ SCOPE
      state query TAIL-NAMES SCOPE-UNION ;

: PROJECT-SCOPE ( ptr syntax n n n bool -- n )
   {: state query:n scope:n grouped:n aggregate:bool :}
   state query NODE NODE-FLAGS @ F-STAR and 0<> if
      aggregate if E-SYNTAX throw then scope exit
   then
   scope 0 state query NODE-A@ begin dup 0<> while
      {: names:n projected:n list:n :}
      state list NODE-A@ {: expression:n :}
      state expression true EXPRESSION-CHECK
      aggregate if
         state expression state grouped projected SCOPE-UNION
            CHECK-GROUPED
      then
      state expression NODE-K N-ALIAS = if
         state expression NODE-B@ {: name:n :}
         state name names FRESH-VAR
         state name projected FRESH-VAR
      else
         names state expression projected FRESH-VAR
      then state list NODE-B@
   repeat drop nip ;

: QUERY-MODIFIERS ( ptr syntax n n n bool -- )
   {: state query:n grouped:n projected:n aggregate:bool :}
   state query NODE-D@ begin dup 0<> while
      {: list:n :} state list NODE-A@ {: modifier:n :}
      state modifier NODE-K dup N-ORDER = swap N-HAVING = or if
         state modifier NODE-A@ {: expression:n :}
         state expression true EXPRESSION-CHECK
         aggregate if
            state modifier NODE-K N-ORDER = if
               state state grouped projected SCOPE-UNION
                  state query TAIL-NAMES SCOPE-UNION
            else grouped then
            state expression rot CHECK-GROUPED
         then
      then state list NODE-B@
   repeat drop ;

: CHECK-QUERY ( ptr syntax n -- n )
   {: state query:n :}
   state state query NODE-B@ SCOPE {: scope:n :}
   state query GROUP-BY-NODE {: grouping:n :}
   state state query NODE-A@ CONTAINS-AGGREGATE?
   state state query NODE-D@ MODIFIER-AGGREGATE? or
   grouping 0<> or {: aggregate:bool :}
   grouping 0<> if
      state state grouping NODE-A@ scope GROUPED-NAMES
   else scope 0 then {: names:n grouped:n :}
   state query NODE-K N-SELECT = if
      state query
      state names state query TAIL-NAMES SCOPE-UNION
      state grouped state query TAIL-NAMES SCOPE-UNION
      aggregate PROJECT-SCOPE
   else names then {: projected:n :}
   state query grouped projected aggregate QUERY-MODIFIERS
   projected ;

' CHECK-QUERY is QUERY-SCOPE

;package
