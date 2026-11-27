\ Graph patterns read predicate/object lists and RDF collections as runs
\ that emission expands into their triples. Explicit groups retain their
\ boundaries for FILTER and BIND scope. A run ends where a member's own
\ pattern is appended, so the expansion keeps the triples' order.

require src/lib/sparql/expression.f
require src/lib/sparql/syntax.f
require src/lib/sparql/terms.f
require src/lib/sparql/lex.f

package SPARQL
private

64 constant F-GENERATED
\ s p o, o2, ...: a triple whose d is the list of the further objects.
\ A run's value is its last list cell.
22 constant N-OBJECTS
\ A collection's members: a their list, b rdf:first, c rdf:rest, d rdf:nil
\ when the collection ends here. A member's list cell holds its blank
\ node's serial in value and in d the next member's, 0 for rdf:nil.
23 constant N-COLLECTION
defer PATH ( ^syntax n bool -- n )
defer GRAPH-NODE ( ^syntax [ ^syntax -- bool ] -- n )
defer PROPERTY-LIST ( ^syntax n bool -- )
defer SUBQUERY ( ^syntax -- n )

: APPEND-PATTERN ( ^syntax n -- )
   {: state pattern:n :}
   state SYNTAX-GROUP @ {: group:n :}
   state pattern state group NODE-A@ CONS
      state group rot NODE-A! ;

: TRIPLE+ ( ^syntax n n n -- n )
   {: state subject:n predicate:n object:n :}
   state N-TRIPLE subject predicate BINARY-NODE {: node:n :}
   state node object NODE-C!
   state node APPEND-PATTERN node ;

\ Whether pattern n is the group's last.
: LAST? ( ^syntax n -- bool )
   {: state pattern:n :}
   state state SYNTAX-GROUP @ NODE-A@ {: list:n :}
   list 0= if false exit then
   state list NODE-A@ pattern = ;

\ Links list cell n after the run's last list cell, its value.
: LINK-LAST ( ^syntax n n -- )
   {: state run:n list:n :}
   state state run NODE-VALUE@ list NODE-B!
   state run list NODE-VALUE! ;

: GRAPH-NODE? ( ^syntax -- bool )
   {: state :} state TOKEN-K {: kind:n :}
   kind T-VAR = kind T-IRI = or kind T-NAME = or
   kind T-BNODE = or kind T-STRING = or kind T-INTEGER = or
   kind T-DECIMAL = or kind T-DOUBLE = or
   state s" true" IS$ or state s" false" IS$ or
   state s" +" IS$ or state s" -" IS$ or
   state s" [" IS$ or state s" (" IS$ or ;

\ Whether the current token may follow an IRI in a path: a modifier, / or |,
\ and ) when the flag says the path is in ( ), else an object.
: PATH-FOLLOWS? ( ^syntax bool -- bool )
   {: state nested:bool :}
   state s" *" IS$ state s" ?" IS$ or state s" +" IS$ or
   state s" /" IS$ or state s" |" IS$ or
   nested if state s" )" IS$ else state GRAPH-NODE? then or ;

\ Whether the current token may follow a member of !(...).
: MEMBER-FOLLOWS? ( ^syntax -- bool )
   {: state :} state s" |" IS$ state s" )" IS$ or ;

\ An IRI or a; the quotation tests whether the current token may follow
\ one.
: PATH-IRI ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state TOKEN$ s" a" STR:= if
      state NEXT-TOKEN
      state s" http://www.w3.org/1999/02/22-rdf-syntax-ns#type" IRI-NODE
   else state follows READ-IRI then ;

\ A member of a negated set; the quotation tests whether the current token
\ may follow it.
: NEGATED-MEMBER ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state s" ^" ACCEPT$ {: inverse:bool :}
   state follows PATH-IRI {: iri:n :}
   inverse if state N-PATH s" ^" iri 0 OPERATOR-NODE
   else iri then ;

\ Whether a member of !(...) starts at the current token.
: MEMBER-START? ( ^syntax -- bool )
   {: state :}
   state TOKEN-K dup T-IRI = swap T-NAME = or
   state TOKEN$ s" a" STR:= or state s" ^" IS$ or ;

\ Whether the current token may follow a path's ( ) or !( ) or end what
\ holds it.
: GROUP-FOLLOWS? ( ^syntax -- bool )
   {: state :}
   state false PATH-FOLLOWS? state AFTER-OBJECT? or ;

\ Whether the current token may follow a pattern in a group: it ends a
\ triple or the group, or starts a pattern or a triple.
: PATTERN-FOLLOWS? ( ^syntax -- bool )
   {: state :} state AFTER-OBJECT? state GRAPH-NODE? or ;

\ The members of !(...) as one list, 0 for !(); a member after a member
\ asks for | there, and a token that ends what holds the set asks for )
\ before it. The flag holds inside a path's ( ).
: NEGATED-MEMBERS ( ^syntax bool -- n )
   {: state nested:bool :}
   state s" (" ACCEPT$ 0= if
      state state [: nested PATH-FOLLOWS? ;] NEGATED-MEMBER 0 CONS exit
   then
   state s" )" ACCEPT$ if 0 exit then
   state AFTER-OBJECT? if
      state s" an IRI, prefixed name or )" s" write ) here" REJECT
   then
   state state [: MEMBER-FOLLOWS? ;] NEGATED-MEMBER 0 CONS
   begin state s" |" ACCEPT$ while
      {: list:n :} state state [: MEMBER-FOLLOWS? ;] NEGATED-MEMBER list CONS
   repeat
   state MEMBER-START? if
      state s" | or )" s" write | here" REJECT
   then state swap REVERSE
   state s" |" [: GROUP-FOLLOWS? ;] CLOSE-PAREN ;

: VERB? ( ^syntax -- bool )
   {: state :}
   state TOKEN-K dup T-IRI = over T-NAME = or swap T-VAR = or
   state TOKEN$ s" a" STR:= or
   state s" (" IS$ or state s" ^" IS$ or state s" !" IS$ or ;

\ An IRI, a negated set or a path in ( ); a path element after the path
\ asks for / or | there. The flag holds inside ( ).
: PATH-PRIMARY ( ^syntax bool -- n )
   {: state nested:bool :}
   state s" !" ACCEPT$ if
      state nested NEGATED-MEMBERS {: members:n :}
      state N-PATH s" !" members 0 OPERATOR-NODE exit
   then
   state s" (" ACCEPT$ if
      state 1 true PATH
      state VERB? state TOKEN-K T-VAR <> and if
         state s" ), / or |" s" write / or | here" REJECT
      then
      state s" / or |" [: GROUP-FOLLOWS? ;] CLOSE-PAREN exit
   then state [: nested PATH-FOLLOWS? ;] PATH-IRI ;

: PATH-ELEMENT ( ^syntax bool -- n )
   {: state nested:bool :}
   state s" ^" ACCEPT$ {: inverse:bool :}
   state nested PATH-PRIMARY {: path:n :}
   state s" *" IS$ state s" ?" IS$ or
   state s" +" IS$ state SIGNED-NUMBER? 0= and or if
      state TOKEN$ {: text:str :} state NEXT-TOKEN
      state N-PATH text path 0 OPERATOR-NODE
   else path then {: element:n :}
   inverse if state N-PATH s" ^" element 0 OPERATOR-NODE
   else element then ;

: PATH-PRECEDENCE ( ^syntax -- n )
   {: state :}
   state s" |" IS$ if 1 exit then
   state s" /" IS$ if 2 exit then 0 ;

\ A path whose operators bind at least the minimum; the flag holds inside
\ ( ).
: READ-PATH ( ^syntax n bool -- n )
   {: state minimum:n nested:bool :}
   state NEST
   state nested PATH-ELEMENT begin
      state PATH-PRECEDENCE dup 0<> swap minimum >= and
   while
      {: left:n :}
      state PATH-PRECEDENCE {: precedence:n :}
      state TOKEN$ {: text:str :} state NEXT-TOKEN
      state precedence 1+ nested PATH {: right:n :}
      state N-PATH text left right OPERATOR-NODE
   repeat state UNNEST ;

: VERB ( ^syntax -- n )
   {: state :}
   state TOKEN-K T-VAR = if state READ-VAR
   else state 1 false PATH then ;

\ The generated blank node of serial n.
: CELL ( ^syntax n -- n )
   {: state serial:n :}
   state N-BNODE ADD-NODE {: node:n :}
   state node serial NODE-VALUE!
   state node F-GENERATED NODE-FLAGS! node ;

: ANONYMOUS ( ^syntax -- n )
   {: state :}
   1 state SYNTAX-SERIAL +!
   state state SYNTAX-SERIAL @ CELL ;

: RDF-FIRST ( ^syntax -- n )
   s" http://www.w3.org/1999/02/22-rdf-syntax-ns#first" IRI-NODE ;

: RDF-REST ( ^syntax -- n )
   s" http://www.w3.org/1999/02/22-rdf-syntax-ns#rest" IRI-NODE ;

: RDF-NIL ( ^syntax -- n )
   s" http://www.w3.org/1999/02/22-rdf-syntax-ns#nil" IRI-NODE ;

\ Adds the member, its blank node's serial given, to the run while that
\ is the group's last pattern, else to a new run; answers the run and the
\ member's list cell.
: MEMBER+ ( ^syntax n n n n -- n n )
   {: state run:n first:n cell:n member:n :}
   state member 0 CONS {: list:n :}
   state list cell NODE-VALUE!
   state run LAST? if state run list LINK-LAST run list exit then
   state N-COLLECTION list first BINARY-NODE {: new:n :}
   state new list NODE-VALUE!
   state new APPEND-PATTERN new list ;

\ Rejects a token that starts no member of a list in ( ): ) is written
\ before it, expecting c, when the flag says it ends the list, else it
\ is replaced with w, expecting x.
: CLOSE-OR-REPLACE ( ^syntax str str str bool -- )
   {: state c:str x:str w:str ends:bool :}
   ends if state c s" write ) here" REJECT then
   state x w REPLACE-WITH ;

\ A list after its (. Where no member starts, a token is removed when
\ the list's ) is ahead, else ) is written before a token that may follow
\ the list, else the token is replaced with a term.
: COLLECTION ( ^syntax -- n )
   {: state :}
   state s" )" ACCEPT$ if state RDF-NIL exit then
   state ANONYMOUS {: head:n :}
   0 state head NODE-VALUE@ begin
      {: run:n cell:n :}
      state GRAPH-NODE? 0= if
         state s" )" CLOSER-AHEAD? if
            state s" a term or )" s" remove it" REJECT
         then
         state s" a term or )" ANY-TERM TERM-EXAMPLE
            state AFTER-OBJECT? state VERB? or CLOSE-OR-REPLACE
      then
      state run state RDF-FIRST cell
         state [: dup GRAPH-NODE? swap s" )" IS$ or ;] GRAPH-NODE MEMBER+
      {: last:n list:n :}
      state s" )" ACCEPT$ if
         state RDF-REST state last rot NODE-C!
         state RDF-NIL state last rot NODE-D! head exit
      then
      1 state SYNTAX-SERIAL +! state SYNTAX-SERIAL @ {: next:n :}
      state list state next CELL NODE-D!
      state RDF-REST state last rot NODE-C! last next
   again ;

\ A graph node; the quotation tests whether the current token may follow
\ it.
: GRAPH-NODE-BODY ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state s" [" ACCEPT$ if
      state ANONYMOUS {: blank:n :}
      state s" ]" ACCEPT$ 0= if
         state blank true PROPERTY-LIST state s" ]" EXPECT$
      then blank exit
   then
   state s" (" ACCEPT$ if state COLLECTION exit then
   state s" +" IS$ state s" -" IS$ or if
      state SIGNED-NUMBER exit
   then state follows READ-TERM ;

: READ-GRAPH-NODE ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :} state NEST
   state follows GRAPH-NODE-BODY state UNNEST ;

\ Adds the object to the run, a triple or N-OBJECTS.
: MORE-OBJECTS ( ^syntax n n -- )
   {: state run:n object:n :}
   state object 0 CONS {: list:n :}
   state run NODE-K N-TRIPLE <> if state run list LINK-LAST exit then
   state run N-OBJECTS NODE-KIND!
   state run list NODE-D!
   state run list NODE-VALUE! ;

\ The objects of one predicate. Where none starts, a term is written
\ before a token that may follow an object, else replaces the token.
: OBJECT-LIST ( ^syntax n n -- )
   {: state subject:n predicate:n :}
   [: AFTER-OBJECT? ;] {: follows :}
   0 begin
      state GRAPH-NODE? 0= if
         state ANY-TERM TERM-EXAMPLE state follows execute MISSING
      then
      state follows GRAPH-NODE {: run:n object:n :}
      state run LAST? if state run object MORE-OBJECTS run
      else state subject predicate object TRIPLE+ then
      state s" ," ACCEPT$ 0=
   until drop ;

\ Rejects a subject with no predicate: the fix writes a predicate and an
\ object before a token that may follow an object, else in its place.
: REQUIRE-PROPERTY ( ^syntax -- )
   {: state :}
   s" a predicate: an IRI, prefixed name, variable or a" {: x:str :}
   state AFTER-OBJECT? if
      state x s" write a predicate and an object after the subject"
         REJECT
   then state x s" a predicate and an object" REPLACE-WITH ;

: READ-PROPERTIES ( ^syntax n bool -- )
   {: state subject:n required:bool :}
   state VERB? 0= if
      required if state REQUIRE-PROPERTY then exit
   then
   begin
      state VERB? if
         state subject state VERB OBJECT-LIST
      then
      state s" ;" ACCEPT$ 0=
   until ;

\ Rejects a UNION where a pattern starts, keeping the query a union: the
\ triples or the OPTIONAL, MINUS or GRAPH pattern right before it in its
\ group go in braces; first in its group or after any other pattern, it is
\ removed.
: STRAY-UNION ( ^syntax -- )
   {: state :}
   state state SYNTAX-GROUP @ NODE-A@ {: list:n :}
   list 0= if state ANY-TERM s" remove it" REJECT then
   state  state list NODE-A@  NODE-K {: kind:n :}
   kind N-TRIPLE = if
      state ANY-TERM s" put the triples before it in braces" REJECT
   then
   kind N-OPTIONAL =  kind N-MINUS = or  kind N-GRAPH = or if
      state ANY-TERM s" put the pattern before it in braces" REJECT
   then state ANY-TERM s" remove it" REJECT ;

\ The kind of the token at decoded offset n, 0 at the end; a scan.
: KIND-AT ( ^syntax n -- n )
   LEX-AT TOKEN-KIND @ ;

: NUMBER-AT? ( ^syntax n -- bool )
   KIND-AT T-INTEGER T-DOUBLE BETWEEN? ;

\ The decoded offset of the token after the term at offset n: a sign
\ takes its number, a string its language tag or ^^ and datatype, an
\ opener its group; a scan.
: PAST-TERM ( ^syntax n -- n )
   {: state j:n :}
   state j TOKEN-AT$ {: t:str :}
   t BRACKET 1 = if state j PAST-GROUP exit then
   state j PAST-TOKEN {: k:n :}
   s" + -" t LISTS?  state k NUMBER-AT?  and if
      state k PAST-TOKEN exit
   then
   state j KIND-AT T-STRING <> if k exit then
   state k KIND-AT T-LANG = if state k PAST-TOKEN exit then
   state k TOKEN-AT$ s" ^^" STR:= if
      state state k PAST-TOKEN PAST-TOKEN exit
   then k ;

\ The decoded offset of the token after the path element at offset n and
\ its *, ? or a + no number follows; a scan.
: PAST-ELEMENT ( ^syntax n -- n )
   {: state j:n :}
   state j PAST-TERM {: k:n :}
   state k TOKEN-AT$ {: m:str :}
   state k PAST-TOKEN {: after:n :}
   s" * ?" m LISTS?  m s" +" STR:=  state after NUMBER-AT? 0=  and  or
   if after else k then ;

\ The decoded offset of the token after the graph node, predicate or path
\ at offset n: a ^ or ! takes the element after it, and so does a / or |
\ after an element; a scan.
: PAST-NODE ( ^syntax n -- n )
   {: state j:n :}
   s" ^ !" state j TOKEN-AT$ LISTS? if
      state state j PAST-TOKEN RECURSE exit
   then
   state j PAST-ELEMENT {: k:n :}
   s" / |" state k TOKEN-AT$ LISTS? if
      state state k PAST-TOKEN RECURSE exit
   then k ;

\ Whether the token at decoded offset n ends a triple, a list, a group or
\ the query; a scan.
: ENDS-AT? ( ^syntax n -- bool )
   TOKEN-AT$ {: t:str :}
   t STR:LEN 0=  s" } . ; , ) ]" t LISTS? or ;

\ How many graph nodes, predicates or paths follow the current token
\ before a token that ends the triple.
: NODES-AFTER ( ^syntax -- n )
   {: state :}
   state state SYNTAX-NEXT TOKEN-OFFSET {: next:n :}
   state [: 0 next begin state over ENDS-AT? 0= while
         state swap PAST-NODE  swap 1+ swap
      repeat drop ;] SCANNING ;

\ A subject and its properties. Where no subject starts, a term is
\ written before a predicate, a UNION is rejected as STRAY-UNION says,
\ and any other token is replaced with a term when two nodes, a predicate
\ and an object, follow it before what ends the triple, else removed.
: TRIPLES ( ^syntax -- )
   {: state :}
   state GRAPH-NODE? 0= if
      state s" UNION" IS$ if state STRAY-UNION then
      state VERB? 0=  state NODES-AFTER 2 <>  and if
         state ANY-TERM s" remove it" REJECT
      then state ANY-TERM TERM-EXAMPLE state VERB? MISSING
   then
   state state SYNTAX-GROUP @ NODE-A@ {: before:n :}
   state [: VERB? ;] GRAPH-NODE {: subject:n :}
   state state SYNTAX-GROUP @ NODE-A@ before =
      state subject rot PROPERTY-LIST ;

: VALUE-START? ( ^syntax -- bool )
   {: state :} state TOKEN-K {: kind:n :}
   kind T-IRI = kind T-NAME = or kind T-STRING = or
   kind T-INTEGER = or kind T-DECIMAL = or kind T-DOUBLE = or
   state s" true" IS$ or state s" false" IS$ or state s" UNDEF" IS$ or
   state s" +" IS$ or state s" -" IS$ or ;

\ The value at the current token, which VALUE-START? admits; the quotation
\ tests whether the current token may follow it.
: VALUES-TERM ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state s" UNDEF" ACCEPT$ if state N-UNDEF ADD-NODE exit then
   state s" +" IS$ state s" -" IS$ or if
      state SIGNED-NUMBER exit
   then state follows READ-TERM ;

\ Whether a row ends before the current token: a ( or } or the end of
\ the query.
: ROW-BREAK? ( ^syntax -- bool )
   {: state :} state s" (" IS$ state s" }" IS$ or state END? or ;

: ROW-VALUE ( ^syntax -- n )
   {: state :}
   state VALUE-START? 0= if
      state s" a value or )" s" an IRI, literal or UNDEF"
         s" an IRI, literal or UNDEF" state ROW-BREAK? CLOSE-OR-REPLACE
   then state [: dup VALUE-START? swap s" )" IS$ or ;] VALUES-TERM ;

: VALUES-VARIABLES ( ^syntax -- n bool )
   {: state :}
   state TOKEN-K T-VAR = if
      state state READ-VAR 0 CONS true exit
   then
   state s" (" EXPECT$
   0 begin state s" )" ACCEPT$ 0= while
      state TOKEN-K T-VAR <> if
         \ The list ends before a { or the end of the query.
         state s" a variable or )" s" a variable" s" a variable"
            state s" {" IS$ state END? or CLOSE-OR-REPLACE
      then
      state READ-VAR state swap rot CONS
   repeat state swap REVERSE false ;

\ Rejects a token no value starts with, expecting x: before a token that
\ starts a pattern, the block's } moved there when it is ahead, else }
\ written there; removed before the block's } or a row's ), else replaced
\ with w, or } written before it when the quotation says it may follow the
\ block and no } follows.
: NO-VALUE ( ^syntax str str [ ^syntax -- bool ] -- )
   {: state x:str w:str follows :}
   state NONTRIPLE? if
      state s" }" CLOSER-AHEAD? if
         state x s" move the } that ends this block here" REJECT
      then state x s" write } here" REJECT
   then
   state s" }" CLOSER-NEXT?  state s" )" CLOSER-NEXT?  or if
      state x s" remove it" REJECT
   then
   state s" }" CLOSER-AHEAD? 0=  state follows execute  and if
      state x s" write } here" REJECT
   then state x w REPLACE-WITH ;

\ Rejects a value where a row starts: ( is written before it when the
\ row's ) follows, else the row is put in parentheses.
: UNOPENED ( ^syntax -- )
   {: state :}
   state s" )" CLOSER-AHEAD? if
      state s" ( or }" s" write ( here" REJECT
   then state s" ( or }" s" put the row in parentheses" REJECT ;

\ Rejects the current token of a row whose values do not match its
\ block's variables.
: ROW-WIDTH ( ^syntax -- )
   s" a row with one value per variable"
   s" write one value or UNDEF per variable in each row" REJECT ;

\ Rejects the current token past a row's last value: a value as too many,
\ any other token removed unless the row breaks there.
: FULL-ROW ( ^syntax -- )
   {: state :}
   state VALUE-START? if state ROW-WIDTH then
   state ROW-BREAK? 0= if state s" )" s" remove it" REJECT then ;

\ The values of a row in ( ) from its first, as many as width: rejected
\ past them as FULL-ROW says or at a ) before them.
: ROW-VALUES ( ^syntax n -- n )
   {: state width:n :}
   0 0 begin state s" )" IS$ 0= while
      dup width = if state FULL-ROW then
      1+ swap  state ROW-VALUE state swap rot CONS  swap
   repeat
   width < if state ROW-WIDTH then
   state NEXT-TOKEN  state swap REVERSE ;

\ One value when the flag holds, else width values in ( ); the quotation
\ tests whether the current token may follow the block.
: VALUES-ROW ( ^syntax bool n [ ^syntax -- bool ] -- n )
   {: state single:bool width:n follows :}
   state VALUE-START? {: value:bool :}
   single if
      value 0= if
         state s" a value or }" s" an IRI, literal or UNDEF"
            follows NO-VALUE
      then
      state state [: dup VALUE-START? swap s" }" IS$ or ;] VALUES-TERM
         0 CONS exit
   then
   state s" (" IS$ 0= if
      value if state UNOPENED then
      state s" ( or }" s" a row in parentheses" follows NO-VALUE
   then state NEXT-TOKEN  state width ROW-VALUES ;

\ A VALUES block; the quotation tests whether the current token may follow it.
: VALUES ( ^syntax [ ^syntax -- bool ] -- n )
   {: state follows :}
   state VALUES-VARIABLES {: names:n single:bool :}
   state names LIST-LENGTH {: width:n :}
   state s" {" EXPECT$
   0 begin state s" }" ACCEPT$ 0= while
      state END? if state s" }" EXPECT$ then
      state single width follows VALUES-ROW  state swap rot CONS
   repeat
   state swap REVERSE {: rows:n :}
   state N-VALUES names rows BINARY-NODE ;

: GRAPH-PATTERN ( ^syntax -- n )
   {: state :}
   state TOKEN-K dup T-IRI = over T-NAME = or swap T-VAR = or
   0= if
      state s" a variable, IRI or prefixed name" s" a variable or an IRI"
         state s" {" IS$ MISSING
   then
   state [: s" {" IS$ ;] READ-TERM {: graph:n :}
   state GROUP-PATTERN {: group:n :}
   state N-GRAPH graph group BINARY-NODE ;

: UNION-PATTERN ( ^syntax -- n )
   {: state :}
   state GROUP-PATTERN
   begin state s" UNION" ACCEPT$ while
      {: left:n :} state GROUP-PATTERN {: right:n :}
      state N-UNION left right BINARY-NODE
   repeat ;

\ Read at a token NONTRIPLE? holds for.
: NONTRIPLE-BODY ( ^syntax -- n )
   {: state :}
   state s" {" IS$ if state UNION-PATTERN exit then
   state s" OPTIONAL" ACCEPT$ if
      state N-OPTIONAL state GROUP-PATTERN UNARY-NODE exit
   then
   state s" MINUS" ACCEPT$ if
      state N-MINUS state GROUP-PATTERN UNARY-NODE exit
   then
   state s" GRAPH" ACCEPT$ if state GRAPH-PATTERN exit then
   state s" SERVICE" IS$ if
      state s" a graph pattern"
         s" remove SERVICE, any SILENT, its IRI or variable and its group: a query reads only its base"
         REJECT
   then
   state s" FILTER" ACCEPT$ if
      state N-FILTER state [: PATTERN-FOLLOWS? ;] CONSTRAINT
      UNARY-NODE exit
   then
   state s" BIND" ACCEPT$ if
      state [: PATTERN-FOLLOWS? ;] ALIAS {: alias:n :}
      state alias N-BIND NODE-KIND! alias exit
   then
   state NEXT-TOKEN
   state [: PATTERN-FOLLOWS? ;] VALUES ;

\ A pattern other than triples, read at its first token.
: NONTRIPLE ( ^syntax -- n )
   {: state :}
   state SYNTAX-CURSOR @ {: start:n :}
   state NONTRIPLE-BODY {: pattern:n :}
   state pattern start NODE-TOKEN! pattern ;

\ Whether ENDS-AT? holds at the token after the graph node at the
\ current token.
: ENDS-NEXT? ( ^syntax -- bool )
   {: state :}
   state [: state  state state SYNTAX-CURSOR @ PAST-NODE  ENDS-AT? ;]
   SCANNING ;

\ Past the . after a triple, else rejects a token that may not follow one:
\ a UNION asks for braces around the triples before it; a graph node for ,
\ before itself, as another object, when the token after the node, its
\ tag, datatype or group included, ends a triple, a list, a group or the
\ query, else for . before itself; a predicate for ;
\ before itself; and any other token is removed.
: AFTER-TRIPLE ( ^syntax -- )
   {: state :}
   state s" ." ACCEPT$ if exit then
   state s" }" IS$ state END? or state NONTRIPLE? or if exit then
   s" . or } after a triple" {: x:str :}
   state s" UNION" IS$ if
      state x s" put the triples before it in braces" REJECT
   then
   state GRAPH-NODE? if
      state ENDS-NEXT? if state x s" write , before it" REJECT then
      state x s" end the triple before with ." REJECT
   then
   state VERB? if
      state s" ; before a predicate after an object" s" write ; here"
         REJECT
   then state x s" remove it" REJECT ;

\ A group's patterns up to its }, which the end of the query asks for.
: GROUP-BODY ( ^syntax -- )
   {: state :}
   state s" SELECT" IS$ if
      state state SUBQUERY APPEND-PATTERN
      state s" }" EXPECT$ exit
   then
   begin state s" }" ACCEPT$ 0= while
      state END? if state s" }" EXPECT$ then
      state NONTRIPLE? if
         state state NONTRIPLE APPEND-PATTERN
         state s" ." ACCEPT$ drop
      else
         state TRIPLES state AFTER-TRIPLE
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
      state group rot NODE-A!
   outer state SYNTAX-GROUP ! state UNNEST group ;

: INSTALL-PATTERN ( -- )
   ['] READ-PATH is PATH
   ['] READ-GRAPH-NODE is GRAPH-NODE
   ['] READ-PROPERTIES is PROPERTY-LIST
   ['] READ-GROUP is GROUP-PATTERN ;

INSTALL-PATTERN

;package
