\ Grammar terminals and RDF terms. Decoding preserves a literal's lexical
\ value; numeric tokens are not round-tripped through machine numbers.

require src/lib/sparql/lex.f
require src/lib/sparql/iri.f
require src/lib/sparql/syntax.f
require src/lib/vector.f
require src/lib/siphash.f
require src/lib/le.f

package SPARQL
private

: END? ( ^syntax -- bool )
   SYNTAX-CURRENT TOKEN-KIND @ 0= ;

: TOKEN$ ( ^syntax -- str )
   SYNTAX-CURRENT TOKEN-TEXT @ ;

: TOKEN-K ( ^syntax -- n )
   SYNTAX-CURRENT TOKEN-KIND @ ;

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
   source 92 CHAR-OFF size = if source exit then
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

: ABSORB$ ( ^SIPHASH:state str -- )
   {: sip text:str :}
   text str> {: bytes size:n :}
   size 7 invert and {: whole:n :}
   whole 0 ?do sip bytes i + LE:U64@ SIPHASH:ABSORB-WORD 8 +loop
   size whole ?do sip bytes i + c@ SIPHASH:ABSORB-BYTE loop ;

\ A name's key: its node's kind and its text.
: NAME-HASH ( ^syntax n str -- n )
   {: state kind:n text:str :}
   state SYNTAX-NAMES INTERNS-HASH {: sip :}
   sip kind SIPHASH:ABSORB-WORD
   sip text str> nip SIPHASH:ABSORB-WORD
   sip text ABSORB$
   sip SIPHASH:FINISH ;

: NODE-NAME-HASH ( ^syntax n -- n )
   {: state index:n :}
   state state index NODE-K state index TEXT$ NAME-HASH ;

: SAME-NAME? ( n ^syntax n str -- bool )
   {: index state kind:n text:str :}
   state index NODE-K kind <> if false exit then
   state index TEXT$ text STR:= ;

\ The first node of the kind and text, 0 for none.
: FIRST-NAME ( ^syntax n str -- n )
   {: state kind:n text:str :}
   state SYNTAX-NAMES state kind text NAME-HASH
   [: state kind text SAME-NAME? ;] INTERNED ;

\ Adds node n as the first of its kind and text.
: +FIRST-NAME ( ^syntax n -- )
   {: state index:n :}
   state SYNTAX-NAMES index [: state swap NODE-NAME-HASH ;] +INTERN ;

\ A term's key: its kind, its text after a head's bytes, its datatype node
\ and its language tag, which compares and hashes as if lower case.
: TERM-HASH ( ^syntax n str str n str -- n )
   {: state kind:n head:str text:str datatype:n language:str :}
   state SYNTAX-INTERNS INTERNS-HASH {: sip :}
   sip kind SIPHASH:ABSORB-WORD
   sip datatype SIPHASH:ABSORB-WORD
   sip head str> nip text str> nip + SIPHASH:ABSORB-WORD
   sip head ABSORB$
   sip text ABSORB$
   language str> {: tag size:n :}
   size 0 ?do sip tag i + c@ STR:ASCII-LOWER SIPHASH:ABSORB-BYTE loop
   sip SIPHASH:FINISH ;

\ The hash of term node n's key.
: NODE-HASH ( ^syntax n -- n )
   {: state index:n :}
   state index NODE-B@ {: tag:n :}
   state state index NODE-K state index HEAD$ state index TEXT$
   state index NODE-A@ tag 0= if s" " else state tag TEXT$ then TERM-HASH ;

\ Whether the text is the first part then the second.
: SPLIT-SAME? ( str str str -- bool )
   {: text:str first:str second:str :}
   text str> {: bytes size:n :}
   first str> nip {: length:n :}
   length second str> nip + size <> if false exit then
   bytes length >str first STR:= 0= if false exit then
   bytes length + size length - >str second STR:= ;

\ Whether two texts of two parts each are equal, the first's head no
\ longer than the second's.
: SHORTER-SAME? ( str str str str -- bool )
   {: a-head:str a-tail:str b-head:str b-tail:str :}
   b-head str> {: bytes size:n :}
   a-head str> nip {: length:n :}
   bytes length >str a-head STR:= 0= if false exit then
   a-tail bytes length + size length - >str b-tail SPLIT-SAME? ;

: SAME-PARTS? ( str str str str -- bool )
   {: a-head:str a-tail:str b-head:str b-tail:str :}
   a-head str> nip b-head str> nip <= if
      a-head a-tail b-head b-tail SHORTER-SAME? exit
   then b-head b-tail a-head a-tail SHORTER-SAME? ;

: SAME-TERM? ( n ^syntax n str str n str -- bool )
   {: index state kind:n head:str text:str datatype:n language:str :}
   state index NODE-K kind <> if false exit then
   state index NODE-A@ datatype <> if false exit then
   state index HEAD$ state index TEXT$ head text SAME-PARTS? 0= if
      false exit
   then
   state index NODE-B@ {: tag:n :}
   tag 0= if language str> nip 0= exit then
   state tag TEXT$ language STR:=CI ;

\ A language node: the tag copied in lower case.
: LANGUAGE-NODE ( ^syntax str -- n )
   {: state language:str :}
   state language COPY$ {: tag:str :}
   tag str> {: text size:n :}
   size 0 ?do text i + dup c@ STR:ASCII-LOWER swap c! loop
   state N-LITERAL tag TEXT-NODE ;

\ The IRI or literal node of the key, made when the query has none yet;
\ its text follows the first keep bytes of the head, 0 for none.
: SHARED-TERM ( ^syntax n n n str n str -- n )
   {: state kind:n head:n keep:n text:str datatype:n language:str :}
   state head keep HEAD-BYTES {: bytes:str :}
   state kind bytes text datatype language TERM-HASH {: hash:n :}
   state SYNTAX-INTERNS {: table:^ :}
   table hash [: state kind bytes text datatype language SAME-TERM? ;]
      INTERNED dup 0<> if exit then drop
   state kind text TEXT-NODE {: term:n :}
   state term datatype NODE-A!
   state term head NODE-C! state term keep NODE-D!
   language str> nip 0<> if
      state language LANGUAGE-NODE state term rot NODE-B!
   then
   table term [: state swap NODE-HASH ;] +INTERN term ;

\ The IRI that is the head's first n bytes then the text.
: HEADED-IRI ( ^syntax n n str -- n )
   {: state head:n keep:n text:str :}
   state N-IRI head keep text 0 s" " SHARED-TERM ;

: IRI-NODE ( ^syntax str -- n )
   {: state text:str :}
   state 0 0 text HEADED-IRI ;

\ The IRI node the prefix is declared as, 0 for none.
: PREFIX-NODE ( ^syntax str -- n )
   {: state prefix:str :}
   state N-PREFIX prefix FIRST-NAME {: node:n :}
   node 0= if 0 exit then
   state node NODE-VALUE@ ;

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

\ The prefix's IRI node as the head, all its bytes kept, then the local
\ part decoded.
: EXPAND-NAME ( ^syntax str -- n n str )
   {: state name:str :}
   name str> {: text size:n :}
   name 58 CHAR-OFF {: colon:n :}
   state text colon >str PREFIX-IRI {: prefix:n :}
   prefix state prefix IRI$ str> nip
   state text colon 1+ + size colon - 1- >str false DECODE$ ;

: READ-IRI ( ^syntax -- n )
   {: state :}
   state TOKEN$ str> {: text size:n :}
   state TOKEN-K T-IRI = if
      state text 1+ size 2 - >str RESOLVE-IRI
   else
      state T-NAME s" an IRI or prefixed name" REQUIRE-KIND
      state text size >str EXPAND-NAME
   then {: head:n keep:n tail:str :}
   state head keep tail HEADED-IRI state NEXT-TOKEN ;

: LITERAL-NODE ( ^syntax str n -- n )
   {: state text:str datatype:n :}
   state N-LITERAL 0 0 text datatype s" " SHARED-TERM ;

\ The literal of the text and the language tag at the cursor.
: READ-LANGUAGE ( ^syntax str -- n )
   {: state value:str :}
   state TOKEN$ str> {: token length:n :}
   token 1+ length 1- >str {: tag:str :}
   state NEXT-TOKEN
   state s" http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" IRI-NODE
      {: datatype:n :}
   state N-LITERAL 0 0 value datatype tag SHARED-TERM ;

\ A string token's value, decoded; the token is read.
: STRING-VALUE ( ^syntax -- str )
   {: state :}
   state TOKEN$ str> {: text size:n :}
   size 6 >= if
      text c@ text 1+ c@ = text c@ text 2 + c@ = and
   else false then if 3 else 1 then {: edge:n :}
   state text edge + size edge 2 * - >str true DECODE$
   state NEXT-TOKEN ;

: XSD-STRING ( ^syntax -- n )
   s" http://www.w3.org/2001/XMLSchema#string" IRI-NODE ;

: STRING-TOKEN ( ^syntax -- n )
   {: state :}
   state STRING-VALUE {: value:str :}
   state value state XSD-STRING LITERAL-NODE ;

: READ-STRING ( ^syntax -- n )
   {: state :}
   state STRING-VALUE {: value:str :}
   state s" ^^" ACCEPT$ if
      state value state READ-IRI LITERAL-NODE exit
   then
   state TOKEN-K T-LANG = if state value READ-LANGUAGE exit then
   state value state XSD-STRING LITERAL-NODE ;

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
   state SYNTAX-NEXT {: token :}
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

\ The number of variable node n; a name not seen before takes the next.
: VAR-NUMBER ( ^syntax n -- n )
   {: state node:n :}
   state N-VAR state node TEXT$ FIRST-NAME {: first:n :}
   first 0<> if state first NODE-VALUE@ exit then
   state node +FIRST-NAME
   1 state SYNTAX-VARIABLES +!
   state SYNTAX-VARIABLES @ ;

\ A variable node; its value is the variable's number.
: READ-VAR ( ^syntax -- n )
   {: state :}
   state N-VAR 1 0 COPY-TOKEN {: node:n :}
   state node state node VAR-NUMBER NODE-VALUE! node ;

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
   state N-PREFIX text size 1- >str TEXT-NODE {: prefix:n :}
   state prefix iri NODE-VALUE!
   state prefix +FIRST-NAME ;

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
