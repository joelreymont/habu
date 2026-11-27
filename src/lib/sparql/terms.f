\ Grammar terminals and RDF terms. Decoding preserves a literal's lexical
\ value; numeric tokens are not round-tripped through machine numbers.

require src/lib/sparql/lex.f
require src/lib/sparql/iri.f
require src/lib/sparql/syntax.f
require src/lib/sparql/segs.f
require src/lib/vector.f
require src/lib/map-core.f
require src/lib/map.f

package SPARQL
private

: END? ( ^syntax -- bool )
   dup SYNTAX-CURSOR @ swap SYNTAX-TOKENS @ SEG-COUNT >= ;

: TOKEN@ ( ^syntax -- ^token )
   dup SYNTAX-TOKENS @ swap SYNTAX-CURSOR @ SEG-AT ;

: TOKEN$ ( ^syntax -- str )
   dup END? if drop s" " exit then
   TOKEN@ TOKEN-TEXT @ ;

: TOKEN-K ( ^syntax -- n )
   dup END? if drop 0 else TOKEN@ TOKEN-KIND @ then ;

: NEXT-TOKEN ( ^syntax -- )
   1 swap SYNTAX-CURSOR +! ;

: IS$ ( ^syntax str -- bool )
   {: state text:str :}
   state TOKEN$ text STR:=CI ;

: ACCEPT$ ( ^syntax str -- bool )
   {: state text:str :}
   state text IS$ if state NEXT-TOKEN true
   else false then ;

: EXPECT$ ( ^syntax str -- )
   {: state text:str :}
   state text ACCEPT$ 0= if
      state s" write " text JOIN$ {: fix:str :}
      state text state fix s"  here" JOIN$ REJECT
   then ;

\ The current token is of kind n, else a rejection expecting the text.
: REQUIRE-KIND ( ^syntax n str -- )
   {: state kind:n x:str :}
   state TOKEN-K kind <> if
      state s" write " x JOIN$ {: fix:str :}
      state x state fix s"  here" JOIN$ REJECT
   then ;

\ What a term may be, expected where none starts.
: ANY-TERM ( -- str )
   s" a variable, IRI, prefixed name, literal or blank node" ;

: TERM-FIX ( -- str ) s" write a term such as ?x or <iri> here" ;

: COPY-TOKEN ( ^syntax n n n -- n )
   {: state kind:n front:n back:n :}
   state TOKEN$ str> {: text size:n :}
   state kind text front + size front - back - >str TEXT-NODE
   state NEXT-TOKEN ;

: ESCAPE-BYTE ( n -- n )
   case
      116 of 9 endof 98 of 8 endof 110 of 10 endof
      114 of 13 endof 102 of 12 endof dup
   endcase ;

\ A local-name escape and \" \' \\ stand for their character; an IRI has none.
: DECODE-ESCAPE ( str ^VEC:vector<u8> bool -- n )
   {: escape:str out:^ string:bool :}
   escape str> drop 1+ c@ {: char:n :}
   string if char ESCAPE-BYTE else char then out VEC:PUSH c! 2 ;

: DECODE$ ( ^syntax str bool -- str )
   {: state source:str string:bool :}
   source str> {: text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out:^ :}
   out size VEC:RESERVE
   0 begin dup size < while
      {: offset:n :}
      text offset + c@ 92 = if
         text offset + size offset - >str out string DECODE-ESCAPE
         offset +
      else
         text offset + c@ out VEC:PUSH c!
         offset 1+
      then
   repeat drop out VEC:BYTES ;

: IRI-NODE ( ^syntax str -- n )
   N-IRI swap TERM-NODE ;

\ The IRI node the prefix is declared as, 0 for none.
: PREFIX-NODE ( ^syntax str -- n )
   {: state prefix:str :}
   state SYNTAX-PREFIXES @ {: names:^ :}
   names NULL-PTR = if 0 exit then
   names prefix MAP:GET MATCH option
      none OF 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ A name as a fix shows it.
: FIX-NAME ( ^syntax str -- str )
   {: state name:str :}
   name SHORTEN if state swap s" ..." JOIN$ then ;

: PREFIX-IRI ( ^syntax str -- n )
   {: state prefix:str :}
   state prefix PREFIX-NODE dup 0<> if exit then drop
   state s" declare it with PREFIX " state prefix FIX-NAME JOIN$
      {: fix:str :}
   state fix s" : <...>" JOIN$ {: full:str :}
   state s" a declared prefix" full REJECT ;

: EXPAND-NAME ( ^syntax str -- str )
   {: state name:str :}
   name str> {: text size:n :}
   name 58 CHAR-OFF {: colon:n :}
   state state text colon >str PREFIX-IRI TEXT$ {: iri:str :}
   state text colon 1+ + size colon - 1- >str false DECODE$
      {: local:str :}
   state iri local JOIN$ ;

: READ-IRI ( ^syntax -- n )
   {: state :}
   state TOKEN$ str> {: text size:n :}
   state TOKEN-K T-IRI = if
      state text 1+ size 2 - >str RESOLVE-IRI
   else
      state T-NAME s" an IRI or prefixed name" REQUIRE-KIND
      state text size >str EXPAND-NAME
   then state swap IRI-NODE state NEXT-TOKEN ;

: LITERAL-NODE ( ^syntax str n -- n )
   {: state text:str datatype:n :}
   state N-LITERAL text TERM-NODE {: term:n :}
   datatype state term NODE NODE-A ! term ;

: READ-LANGUAGE ( ^syntax n -- )
   {: state term:n :}
   state TOKEN$ str> {: token length:n :}
   state token 1+ length 1- >str COPY$ {: tag:str :}
   tag str> {: text size:n :}
   size 0 ?do text i + dup c@ STR:ASCII-LOWER swap c! loop
   state NEXT-TOKEN
   state N-LITERAL tag TEXT-NODE {: language:n :}
   language state term NODE NODE-B !
   state s" http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" IRI-NODE
      state term NODE NODE-A ! ;

: STRING-TOKEN ( ^syntax -- n )
   {: state :}
   state TOKEN$ str> {: text size:n :}
   size 6 >= if
      text c@ text 1+ c@ = text c@ text 2 + c@ = and
   else false then if 3 else 1 then {: edge:n :}
   state text edge + size edge 2 * - >str true DECODE$
      {: value:str :}
   state NEXT-TOKEN
   state s" http://www.w3.org/2001/XMLSchema#string" IRI-NODE
      {: datatype:n :}
   state value datatype LITERAL-NODE ;

: READ-STRING ( ^syntax -- n )
   {: state :}
   state STRING-TOKEN {: term:n :}
   state s" ^^" ACCEPT$ if
      state READ-IRI state term NODE NODE-A !
   else state TOKEN-K T-LANG = if
      state term READ-LANGUAGE
   then then term ;

\ The datatype of a number token of kind n.
: NUMERIC-TYPE ( ^syntax n -- n )
   {: state kind:n :}
   kind T-INTEGER = if
      state s" http://www.w3.org/2001/XMLSchema#integer" IRI-NODE exit
   then
   kind T-DECIMAL = if
      state s" http://www.w3.org/2001/XMLSchema#decimal" IRI-NODE exit
   then
   state s" http://www.w3.org/2001/XMLSchema#double" IRI-NODE ;

: NUMBER-TERM ( ^syntax -- n )
   {: state :}
   state state TOKEN-K NUMERIC-TYPE {: datatype:n :}
   state TOKEN$ {: text:str :}
   state text datatype LITERAL-NODE state NEXT-TOKEN ;

: SIGNED-NUMBER? ( ^syntax -- bool )
   {: state :}
   state s" +" IS$ state s" -" IS$ or 0= if false exit then
   state SYNTAX-CURSOR @ 1+ {: next:n :}
   next state SYNTAX-TOKENS @ SEG-COUNT >= if false exit then
   state SYNTAX-TOKENS @ next SEG-AT {: token :}
   token TOKEN-KIND @ T-INTEGER T-DOUBLE BETWEEN? 0= if
      false exit
   then
   state TOKEN$ str> + token TOKEN-TEXT @ str> drop = ;

: SIGNED-NUMBER ( ^syntax -- n )
   {: state :}
   state SIGNED-NUMBER? 0= if
      state s" a number after + or -"
         s" write the number right after the sign" REJECT
   then
   state TOKEN$ str> {: sign size:n :} state NEXT-TOKEN
   state state TOKEN-K NUMERIC-TYPE {: datatype:n :}
   state sign size state TOKEN$ str> nip + >str datatype
      LITERAL-NODE
   state NEXT-TOKEN ;

: BOOLEAN-TERM ( ^syntax -- n )
   {: state :}
   state s" true" IS$ if s" true" else s" false" then
      {: text:str :}
   state text state s" http://www.w3.org/2001/XMLSchema#boolean" IRI-NODE
      LITERAL-NODE
   state NEXT-TOKEN ;

: VAR-NUMBER ( ^syntax str -- n )
   {: state name:str :}
   state SYNTAX-VARIABLES state SYNTAX-ARENA @ MAP:ROOM
   state SYNTAX-VARIABLES @ {: names:^ :}
   names name MAP:GET MATCH option
      none OF
         names MAP:COUNT@ COUNT>N 1+
         dup names name MAP:SET
      ENDOF
      some OF ENDOF
   ;MATCH ;

\ A variable node; its value is the variable's number.
: READ-VAR ( ^syntax -- n )
   {: state :}
   state N-VAR 1 0 COPY-TOKEN {: node:n :}
   state state node TEXT$ VAR-NUMBER
      state node NODE NODE-VALUE ! node ;

: READ-TERM ( ^syntax -- n )
   {: state :}
   state TOKEN-K case
      T-VAR of state READ-VAR endof
      T-IRI of state READ-IRI endof
      T-NAME of state READ-IRI endof
      T-BNODE of state N-BNODE 2 0 COPY-TOKEN endof
      T-STRING of state READ-STRING endof
      T-INTEGER of state NUMBER-TERM endof
      T-DECIMAL of state NUMBER-TERM endof
      T-DOUBLE of state NUMBER-TERM endof
      T-WORD of
         state s" true" IS$ state s" false" IS$ or if
            state BOOLEAN-TERM
         else state ANY-TERM TERM-FIX REJECT then
      endof
      state ANY-TERM TERM-FIX REJECT
   endcase ;

: READ-PREFIX ( ^syntax -- )
   {: state :}
   state T-NAME s" a prefix such as ex:" REQUIRE-KIND
   state TOKEN$ str> {: text size:n :}
   text size >str 58 CHAR-OFF size 1- <> if
      state s" a prefix such as ex:"
         s" end the prefix name at its colon" REJECT
   then
   state text size 1- >str PREFIX-NODE 0<> if
      state s" a prefix not declared before"
         s" remove this declaration" REJECT
   then
   state NEXT-TOKEN state T-IRI s" an IRI in <...>" REQUIRE-KIND
   state READ-IRI {: iri:n :}
   state SYNTAX-PREFIXES state SYNTAX-ARENA @ MAP:ROOM
   iri state SYNTAX-PREFIXES @ text size 1- >str MAP:SET ;

: BASE-SCHEME ( ^syntax -- )
   {: state :}
   state TOKEN$ str> {: text size:n :}
   text 1+ size 2 - >str {: iri:str :}
   iri str> {: first length:n :}
   iri 58 CHAR-OFF {: colon:n :}
   colon length = first colon >str URI-SCHEME? 0= or if
      state s" an absolute IRI with a scheme"
         s" start the IRI with a scheme such as http:" REJECT
   then ;

: READ-BASE ( ^syntax -- )
   {: state :}
   state T-IRI s" an IRI in <...>" REQUIRE-KIND
   state SYNTAX-DECLARED-BASE @ 0= if state BASE-SCHEME then
   state TOKEN$ str> 2 - swap 1+ swap >str {: written:str :}
   state state READ-IRI written DECLARE-BASE ;

: PROLOGUE ( ^syntax -- )
   {: state :}
   begin
      state s" PREFIX" ACCEPT$ if state READ-PREFIX true else
         state s" BASE" ACCEPT$ if state READ-BASE true
         else false then
      then
   while repeat ;

;package
