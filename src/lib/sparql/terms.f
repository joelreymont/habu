\ Grammar terminals and RDF terms. Decoding preserves a literal's lexical
\ value; numeric tokens are not round-tripped through machine numbers.

require src/lib/sparql/lex.f
require src/lib/sparql/iri.f

package SPARQL
private

: END? ( ptr syntax -- bool )
   dup SYNTAX-CURSOR @ swap SYNTAX-TOKENS @ VEC:COUNT >= ;

: TOKEN@ ( ptr syntax -- ptr token )
   dup SYNTAX-TOKENS @ swap SYNTAX-CURSOR @ VEC:AT ;

: TOKEN$ ( ptr syntax -- ptr u8 n )
   dup END? if drop s" " exit then
   TOKEN@ dup TOKEN-TEXT @ swap TOKEN-SIZE @ ;

: TOKEN-K ( ptr syntax -- n )
   dup END? if drop 0 else TOKEN@ TOKEN-KIND @ then ;

: NEXT-TOKEN ( ptr syntax -- )
   dup END? if E-SYNTAX throw then
   1 swap SYNTAX-CURSOR +! ;

: IS$ ( ptr syntax ptr u8 n -- bool )
   {: state text size:n :}
   state TOKEN$ text size STR=CI ;

: ACCEPT$ ( ptr syntax ptr u8 n -- bool )
   {: state text size:n :}
   state text size IS$ if state NEXT-TOKEN true
   else false then ;

: EXPECT$ ( ptr syntax ptr u8 n -- )
   ACCEPT$ 0= if E-SYNTAX throw then ;

: REQUIRE-KIND ( ptr syntax n -- )
   swap TOKEN-K <> if E-SYNTAX throw then ;

: COPY-TOKEN ( ptr syntax n n n -- n )
   {: state kind:n front:n back:n :}
   state TOKEN$ {: text size:n :}
   state kind text front + size front - back - TEXT-NODE
   state NEXT-TOKEN ;

: ESCAPE-BYTE ( n -- n )
   case
      116 of 9 endof 98 of 8 endof 110 of 10 endof
      114 of 13 endof 102 of 12 endof
      34 of 34 endof 39 of 39 endof 92 of 92 endof
      E-SYNTAX throw
   endcase ;

: DECODE-ESCAPE ( ptr u8 n ptr u8 n -- n n )
   {: text size:n out mode:n :}
   size 2 < if E-SYNTAX throw then
   text 1+ c@ {: char:n :}
   mode 0= if char ESCAPE-BYTE out c! 2 1 exit then
   mode 2 = char LOCAL-ESCAPE? and if
      char out c! 2 1 exit
   then E-SYNTAX throw ;

: DECODE$ ( ptr syntax ptr u8 n n -- ptr u8 n )
   {: state text size:n mode:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size 1+
      ALLOC:BYTES drop {: out :}
   0 0 begin over size < while
      {: offset:n used:n :}
      state CHECK-BUDGET
      text offset + c@ 92 = if
         text offset + size offset - out used + mode
         DECODE-ESCAPE {: consumed:n written:n :}
         offset consumed + used written +
      else
         text offset + c@ out used + c!
         offset 1+ used 1+
      then
   repeat nip out swap ;

: IRI-NODE ( ptr syntax ptr u8 n -- n )
   N-IRI -rot TEXT-NODE ;

: PREFIX-IRI ( ptr syntax ptr u8 n -- n )
   {: state prefix size:n :}
   state SYNTAX-PREFIXES @ begin dup 0<> while
      {: list:n :}
      state list NODE-A@ {: binding:n :}
      state binding TEXT$ prefix size STR= if
         state binding NODE-A@ exit
      then state list NODE-B@
   repeat drop E-SYNTAX throw ;

: EXPAND-NAME ( ptr syntax ptr u8 n -- ptr u8 n )
   {: state text size:n :}
   text size 58 CHAR-OFF {: colon:n :}
   colon size = if E-SYNTAX throw then
   state state text colon PREFIX-IRI TEXT$ {: iri len:n :}
   state text colon 1+ + size colon - 1- 2 DECODE$
      {: local local-size:n :}
   state iri len local local-size JOIN$ ;

: READ-IRI ( ptr syntax -- n )
   {: state :}
   state TOKEN$ {: text size:n :}
   state TOKEN-K T-IRI = if
      state text 1+ size 2 - 1 DECODE$
      state -rot RESOLVE-IRI
   else
      state T-NAME REQUIRE-KIND
      state text size EXPAND-NAME
   then state -rot IRI-NODE state NEXT-TOKEN ;

: XSD-IRI ( ptr syntax ptr u8 n -- n )
   {: state text size:n :}
   state s" http://www.w3.org/2001/XMLSchema#"
      text size JOIN$ state -rot IRI-NODE ;

: RDF-IRI ( ptr syntax ptr u8 n -- n )
   {: state text size:n :}
   state s" http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      text size JOIN$ state -rot IRI-NODE ;

: LITERAL-NODE ( ptr syntax ptr u8 n n -- n )
   {: state text size:n datatype:n :}
   state N-LITERAL text size TEXT-NODE {: term:n :}
   datatype state term NODE NODE-A ! term ;

: READ-LANGUAGE ( ptr syntax n -- )
   {: state term:n :}
   state N-LITERAL 1 0 COPY-TOKEN {: language:n :}
   language state term NODE NODE-B !
   state s" langString" RDF-IRI
      state term NODE NODE-A ! ;

: STRING-TOKEN ( ptr syntax -- n )
   {: state :}
   state TOKEN$ {: text size:n :}
   size 6 >= if
      text c@ text 1+ c@ = text c@ text 2 + c@ = and
   else false then if 3 else 1 then {: edge:n :}
   state text edge + size edge 2 * - 0 DECODE$
      {: value length:n :}
   state NEXT-TOKEN
   state s" string" XSD-IRI {: datatype:n :}
   state value length datatype LITERAL-NODE ;

: READ-STRING ( ptr syntax -- n )
   {: state :}
   state STRING-TOKEN {: term:n :}
   state s" ^^" ACCEPT$ if
      state READ-IRI state term NODE NODE-A !
   else state TOKEN-K T-LANG = if
      state term READ-LANGUAGE
   then then term ;

: NUMERIC-TYPE ( ptr syntax n -- n )
   {: state kind:n :}
   kind T-INTEGER = if state s" integer" XSD-IRI exit then
   kind T-DECIMAL = if state s" decimal" XSD-IRI exit then
   kind T-DOUBLE = if state s" double" XSD-IRI exit then
   E-SYNTAX throw ;

: NUMBER-TERM ( ptr syntax -- n )
   {: state :}
   state state TOKEN-K NUMERIC-TYPE {: datatype:n :}
   state TOKEN$ {: text size:n :}
   state text size datatype LITERAL-NODE state NEXT-TOKEN ;

: SIGNED-NUMBER? ( ptr syntax -- bool )
   {: state :}
   state s" +" IS$ state s" -" IS$ or 0= if false exit then
   state SYNTAX-CURSOR @ 1+ {: next:n :}
   next state SYNTAX-TOKENS @ VEC:COUNT >= if false exit then
   state SYNTAX-TOKENS @ next VEC:AT {: token :}
   token TOKEN-KIND @ T-INTEGER T-DOUBLE BETWEEN? 0= if
      false exit
   then
   state TOKEN$ + token TOKEN-TEXT @ = ;

: SIGNED-NUMBER ( ptr syntax -- n )
   {: state :}
   state SIGNED-NUMBER? 0= if E-SYNTAX throw then
   state TOKEN$ {: sign size:n :} state NEXT-TOKEN
   state state TOKEN-K NUMERIC-TYPE {: datatype:n :}
   state sign size state TOKEN$ nip + datatype LITERAL-NODE
   state NEXT-TOKEN ;

: BOOLEAN-TERM ( ptr syntax -- n )
   {: state :}
   state s" true" IS$ if s" true" else s" false" then
      {: text size:n :}
   state text size state s" boolean" XSD-IRI LITERAL-NODE
   state NEXT-TOKEN ;

: READ-TERM ( ptr syntax -- n )
   {: state :}
   state TOKEN-K case
      T-VAR of state N-VAR 1 0 COPY-TOKEN endof
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
         else E-SYNTAX throw then
      endof
      E-SYNTAX throw
   endcase ;

: READ-PREFIX ( ptr syntax -- )
   {: state :}
   state T-NAME REQUIRE-KIND
   state TOKEN$ {: text size:n :}
   size 0= if E-SYNTAX throw then
   text size 58 CHAR-OFF size 1- <> if E-SYNTAX throw then
   state SYNTAX-PREFIXES @ begin dup 0<> while
      {: list:n :}
      state state list NODE-A@ TEXT$ text size 1- STR= if
         E-SYNTAX throw
      then state list NODE-B@
   repeat drop
   state N-ALIAS text size 1- TEXT-NODE {: binding:n :}
   state NEXT-TOKEN state T-IRI REQUIRE-KIND
   state READ-IRI state binding NODE NODE-A !
   state binding state SYNTAX-PREFIXES @ CONS
      state SYNTAX-PREFIXES ! ;

: READ-BASE ( ptr syntax -- )
   {: state :}
   state T-IRI REQUIRE-KIND
   state TOKEN$ {: text size:n :}
   state text 1+ size 2 - 1 DECODE$ {: decoded length:n :}
   decoded length 58 CHAR-OFF {: colon:n :}
   colon length = decoded colon URI-SCHEME? 0= or if
      E-SYNTAX throw
   then
   state state READ-IRI TEXT$
   state SYNTAX-BASE-SIZE ! state SYNTAX-BASE ! ;

: PROLOGUE ( ptr syntax -- )
   {: state :}
   begin
      state s" PREFIX" ACCEPT$ if state READ-PREFIX true else
         state s" BASE" ACCEPT$ if state READ-BASE true
         else false then
      then
   while repeat ;

;package
