split-str
\ Graph patterns expand predicate/object lists and RDF collections into
\ triples. Explicit groups retain their boundaries for FILTER and BIND scope.

require src/lib/sparql/expression.f

package SPARQL
private

64 constant F-GENERATED
defer PATH ( ^syntax n -- n )
defer GRAPH-NODE ( ^syntax -- n )
defer PROPERTY-LIST ( ^syntax n bool -- )
defer SUBQUERY ( ^syntax -- n )

: APPEND-PATTERN ( ^syntax n -- )
   {: state pattern:n :}
   state SYNTAX-GROUP @ {: group:n :}
   state pattern state group NODE-A@ CONS
      state group NODE NODE-A ! ;

: TRIPLE+ ( ^syntax n n n -- )
   {: state subject:n predicate:n object:n :}
   state N-TRIPLE subject predicate BINARY-NODE {: node:n :}
   object state node NODE NODE-C !
   state node APPEND-PATTERN ;

: PATH-IRI ( ^syntax -- n )
   {: state :}
   state TOKEN$ s" a" STR= if
      state NEXT-TOKEN state s" type" RDF-IRI
   else state READ-IRI then ;

: NEGATED-MEMBER ( ^syntax -- n )
   {: state :}
   state s" ^" ACCEPT$ {: inverse:bool :}
   state PATH-IRI {: iri:n :}
   inverse if state N-PATH s" ^" iri 0 OPERATOR-NODE
   else iri then ;

: NEGATED-MEMBERS ( ^syntax -- n )
   {: state :}
   state s" (" ACCEPT$ 0= if state NEGATED-MEMBER exit then
   state s" )" ACCEPT$ if 0 exit then
   state NEGATED-MEMBER 1
   begin state s" |" ACCEPT$ while
      {: left:n count:n :}
      count 256 >= if E-NESTING throw then
      state NEGATED-MEMBER {: right:n :}
      state N-PATH s" |" left right OPERATOR-NODE count 1+
   repeat drop state s" )" EXPECT$ ;

: PATH-PRIMARY ( ^syntax -- n )
   {: state :}
   state s" !" ACCEPT$ if
      state NEGATED-MEMBERS {: path:n :}
      state N-PATH s" !" path 0 OPERATOR-NODE exit
   then
   state s" (" ACCEPT$ if
      state 1 PATH state s" )" EXPECT$ exit
   then state PATH-IRI ;

: PATH-ELEMENT ( ^syntax -- n )
   {: state :}
   state s" ^" ACCEPT$ {: inverse:bool :}
   state PATH-PRIMARY {: path:n :}
   state s" *" IS$ state s" +" IS$ or
   state s" ?" IS$ or if
      state TOKEN$ {: text size:n :} state NEXT-TOKEN
      state N-PATH text size path 0 OPERATOR-NODE
   else path then {: element:n :}
   inverse if state N-PATH s" ^" element 0 OPERATOR-NODE
   else element then ;

: PATH-PRECEDENCE ( ^syntax -- n )
   {: state :}
   state s" |" IS$ if 1 exit then
   state s" /" IS$ if 2 exit then 0 ;

: READ-PATH ( ^syntax n -- n )
   {: state minimum:n :}
   state NEST
   state PATH-ELEMENT begin
      state PATH-PRECEDENCE dup 0<> swap minimum >= and
   while
      {: left:n :}
      state PATH-PRECEDENCE {: precedence:n :}
      state TOKEN$ {: text size:n :} state NEXT-TOKEN
      state precedence 1+ PATH {: right:n :}
      state N-PATH text size left right OPERATOR-NODE
   repeat state UNNEST ;

: VERB ( ^syntax -- n )
   {: state :}
   state TOKEN-K T-VAR = if state READ-TERM
   else state 1 PATH then ;

: ANONYMOUS ( ^syntax -- n )
   {: state :}
   state N-BNODE ADD-NODE {: node:n :}
   1 state SYNTAX-SERIAL +!
   state SYNTAX-SERIAL @ state node NODE NODE-VALUE !
   F-GENERATED state node NODE NODE-FLAGS ! node ;

: COLLECTION ( ^syntax -- n )
   {: state :}
   state s" )" ACCEPT$ if state s" nil" RDF-IRI exit then
   state ANONYMOUS {: head:n :}
   head begin
      {: current:n :}
      state current state s" first" RDF-IRI
         state GRAPH-NODE TRIPLE+
      state s" )" ACCEPT$ if
         state current state s" rest" RDF-IRI
            state s" nil" RDF-IRI TRIPLE+
         head exit
      then
      state ANONYMOUS {: next:n :}
      state current state s" rest" RDF-IRI next TRIPLE+
      next
   again ;

: GRAPH-NODE-BODY ( ^syntax -- n )
   {: state :}
   state s" [" ACCEPT$ if
      state ANONYMOUS {: blank:n :}
      state s" ]" ACCEPT$ 0= if
         state blank true PROPERTY-LIST state s" ]" EXPECT$
      then blank exit
   then
   state s" (" ACCEPT$ if state COLLECTION exit then
   state s" +" IS$ state s" -" IS$ or if
      state SIGNED-NUMBER exit
   then state READ-TERM ;

: READ-GRAPH-NODE ( ^syntax -- n )
   {: state :} state NEST
   state GRAPH-NODE-BODY state UNNEST ;

: VERB? ( ^syntax -- bool )
   {: state :}
   state TOKEN-K dup T-IRI = over T-NAME = or swap T-VAR = or
   state TOKEN$ s" a" STR= or
   state s" (" IS$ or state s" ^" IS$ or state s" !" IS$ or ;

: OBJECT-LIST ( ^syntax n n -- )
   {: state subject:n predicate:n :}
   begin
      state subject predicate state GRAPH-NODE TRIPLE+
      state s" ," ACCEPT$ 0=
   until ;

: READ-PROPERTIES ( ^syntax n bool -- )
   {: state subject:n required:bool :}
   state VERB? 0= if
      required if E-SYNTAX throw then exit
   then
   begin
      state VERB? if
         state subject state VERB OBJECT-LIST
      then
      state s" ;" ACCEPT$ 0=
   until ;

: TRIPLES ( ^syntax -- )
   {: state :}
   state state SYNTAX-GROUP @ NODE-A@ {: before:n :}
   state GRAPH-NODE {: subject:n :}
   state state SYNTAX-GROUP @ NODE-A@ before =
      state subject rot PROPERTY-LIST ;

: GRAPH-NODE? ( ^syntax -- bool )
   {: state :} state TOKEN-K {: kind:n :}
   kind T-VAR = kind T-IRI = or kind T-NAME = or
   kind T-BNODE = or kind T-STRING = or kind T-INTEGER = or
   kind T-DECIMAL = or kind T-DOUBLE = or
   state s" true" IS$ or state s" false" IS$ or
   state s" +" IS$ or state s" -" IS$ or
   state s" [" IS$ or state s" (" IS$ or ;

: VALUES-TERM ( ^syntax -- n )
   {: state :}
   state s" UNDEF" ACCEPT$ if state N-UNDEF ADD-NODE exit then
   state s" +" IS$ state s" -" IS$ or if
      state SIGNED-NUMBER exit
   then
   state READ-TERM {: term:n :}
   state term NODE-K dup N-VAR = swap N-BNODE = or if
      E-SYNTAX throw
   then term ;

: VALUES-VARIABLES ( ^syntax -- n bool )
   {: state :}
   state TOKEN-K T-VAR = if
      state state READ-TERM 0 CONS true exit
   then
   state s" (" EXPECT$
   0 begin state s" )" ACCEPT$ 0= while
      state T-VAR REQUIRE-KIND
      state READ-TERM state swap rot CONS
   repeat state swap REVERSE false ;

: VALUES-ROW ( ^syntax bool -- n )
   {: state single:bool :}
   single if state state VALUES-TERM 0 CONS exit then
   state s" (" EXPECT$
   0 begin state s" )" ACCEPT$ 0= while
      state VALUES-TERM state swap rot CONS
   repeat state swap REVERSE ;

: VALUES ( ^syntax -- n )
   {: state :}
   state VALUES-VARIABLES {: names:n single:bool :}
   state names LIST-LENGTH {: width:n :}
   state s" {" EXPECT$
   0 begin state s" }" ACCEPT$ 0= while
      state single VALUES-ROW {: row:n :}
      state row LIST-LENGTH width <> if E-SYNTAX throw then
      state row rot CONS
   repeat
   state swap REVERSE {: rows:n :}
   state N-VALUES names rows BINARY-NODE ;

: GRAPH-PATTERN ( ^syntax n -- n )
   {: state kind:n :}
   kind N-SERVICE = if
      state s" SILENT" ACCEPT$ if F-SILENT else 0 then
   else 0 then {: flags:n :}
   state READ-TERM {: graph:n :}
   state graph NODE-K dup N-IRI = swap N-VAR = or 0= if
      E-SYNTAX throw
   then
   state GROUP-PATTERN {: group:n :}
   state kind graph group BINARY-NODE {: node:n :}
   flags state node NODE NODE-FLAGS ! node ;

: UNION-PATTERN ( ^syntax -- n )
   {: state :}
   state GROUP-PATTERN
   begin state s" UNION" ACCEPT$ while
      {: left:n :} state GROUP-PATTERN {: right:n :}
      state N-UNION left right BINARY-NODE
   repeat ;

: NONTRIPLE ( ^syntax -- n )
   {: state :}
   state s" {" IS$ if state UNION-PATTERN exit then
   state s" OPTIONAL" ACCEPT$ if
      state N-OPTIONAL state GROUP-PATTERN UNARY-NODE exit
   then
   state s" MINUS" ACCEPT$ if
      state N-MINUS state GROUP-PATTERN UNARY-NODE exit
   then
   state s" GRAPH" ACCEPT$ if
      state N-GRAPH GRAPH-PATTERN exit
   then
   state s" SERVICE" ACCEPT$ if
      state N-SERVICE GRAPH-PATTERN exit
   then
   state s" FILTER" ACCEPT$ if
      state N-FILTER state CONSTRAINT UNARY-NODE exit
   then
   state s" BIND" ACCEPT$ if
      state ALIAS {: alias:n :}
      N-BIND state alias NODE NODE-KIND ! alias exit
   then
   state s" VALUES" ACCEPT$ if state VALUES exit then 0 ;

: GROUP-BODY ( ^syntax -- )
   {: state :}
   state s" SELECT" IS$ if
      state state SUBQUERY APPEND-PATTERN
      state s" }" EXPECT$ exit
   then
   begin state s" }" ACCEPT$ 0= while
      state NONTRIPLE dup 0<> if
         state swap APPEND-PATTERN
         state s" ." ACCEPT$ drop
      else
         drop state TRIPLES
         state s" ." ACCEPT$ 0= if
            state GRAPH-NODE? if E-SYNTAX throw then
         then
      then
   repeat ;

: READ-GROUP ( ^syntax -- n )
   {: state :}
   state NEST
   state s" {" EXPECT$
   state SYNTAX-GROUP @ {: outer:n :}
   state N-GROUP ADD-NODE {: group:n :}
   group state SYNTAX-GROUP !
   state GROUP-BODY
   state state group NODE-A@ REVERSE
      state group NODE NODE-A !
   outer state SYNTAX-GROUP ! state UNNEST group ;

: INSTALL-PATTERN ( -- )
   ['] READ-PATH is PATH
   ['] READ-GRAPH-NODE is GRAPH-NODE
   ['] READ-PROPERTIES is PROPERTY-LIST
   ['] READ-GROUP is GROUP-PATTERN ;

INSTALL-PATTERN

;package
