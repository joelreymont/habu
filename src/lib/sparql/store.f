\ store.f - the store description a host passes with each query, read
\ into the records the compiler walks. The description is a JSON object:
\ `bytes`, a whole number above 0, the byte limit; `shapes`, an object naming
\ each shape `{"in": f, "out": f, "holds": h, "type": t}`, f a SQL
\ function called as f(x), `in` from a term to the shape's native value,
\ NULL for every term outside `holds`, and `out` back, h, optional, "uri"
\ or an array of datatype IRIs (a plain literal's is xsd:string, a
\ language-tagged one's rdf:langString), absent for any term, and t,
\ optional, a string, the SQL type of its native values, as a key set's
\ `type` names its array's; or `{"within": name}`, a part of shape name,
\ taking its `in`, `out`, `holds` and `type`; a chain of `within` ends at
\ a shape that has none, its root. A shape of type numeric holding
\ xsd:integer and xsd:decimal holds the integer term for a whole value and
\ the decimal one otherwise, so its values are finite, and their text is
\ at most 16,383 bytes; one of type text holding xsd:string holds the
\ plain literal of its text. The columns of shapes of type text share one
\ collation, and it is deterministic. Different shapes hold disjoint terms
\ unless one is within the other, directly or through parts; parts of one
\ parent are disjoint. Optionally `keys`, an object naming each key set
\ `{"type": t, "values": {i: a}}`, t a SQL array type and a,
\ a string, the SQL array literal of IRI i's keys. `sources`, an array of
\ triple sources `{"from", "graph", "s", "p", "o"}`: `from` a SQL FROM item
\ the statement aliases, `graph` null for the default graph or a uri term,
\ and each slot `{"col": c, "shape": name}`, c a SQL column name of
\ `from`, emitted as written, holding native values of a shape in
\ `shapes`, or `{"term": t}`, a constant uri or literal term. A column
\ slot's optional `"key": {"col": k, "set": name}` names a key set in
\ `keys`: a row holds IRI i in the slot exactly where column k of `from`
\ is one of i's keys, and an IRI the set does not list is in no row of the
\ slot. A column slot's optional `"unique"`, a boolean, is true where no
\ two rows of `from` hold one value in the column, and its optional
\ `"notnull"`, a boolean, true where no row of `from` holds NULL in it. A
\ slot `{"cols": [e1, e2, ...]}` lists two or more column slots without
\ `key` or `notnull`: at most one of their columns is non-null in a row,
\ and it holds the slot's value; a row where all are null holds no triple.
\ An entry's optional `"only": {"col": k, "set": name}` names a key set in
\ `keys`, whose keys go unread: in a row of `from` that holds an IRI i in
\ a slot keyed on column k, of this source or another of the same `from`,
\ where the set of some entry of this slot lists i, the slot's value is in
\ an entry whose set lists i; an IRI none of them lists says nothing. A
\ description not in the format is the host's defect, rejected with
\ JSON:E-MEMBER at its place.
\ STORAGE CLASS. CALLER-OWNED: the records live in the caller's arena; their
\ spans lie there or borrow the description text, which outlives the store.

require src/lib/arena.f
require src/lib/vector.f
require src/lib/string.f
require src/lib/map.f
require src/lib/json.f
require src/lib/json/write.f
require src/lib/memory.f
require src/lib/adt/option.f
require src/lib/adt/nullable.f
require src/lib/json/value.f
require src/lib/map-core.f

package SPARQL

STRUCTURE datatype DERIVE addr FIELD iri str ;STRUCTURE

\ The terms a shape holds: any, the uris, or the literals of its datatypes.
ENUM holds
   VARIANT any ;VARIANT
   VARIANT uris ;VARIANT
   VARIANT literals FIELD datatypes ^VEC:vector<datatype> ;VARIANT
;ENUM

\ root is the shape's own index, or for a part its parent's root; parent
\ indexes the shape it is within, -1 for a root.
STRUCTURE shape DERIVE addr
   FIELD name str
   FIELD in str
   FIELD out str
   FIELD holds holds
   FIELD type str
   FIELD root n
   FIELD parent n
;STRUCTURE

\ The SQL array literal of a key set member's keys.
STRUCTURE member DERIVE addr FIELD keys str ;STRUCTURE

\ places is a MAP map from each member's IRI to its place in members.
STRUCTURE key-set DERIVE addr
   FIELD name str
   FIELD type str
   FIELD members ^VEC:vector<member>
   FIELD places ^n
;STRUCTURE

\ set indexes the store's key sets.
STRUCTURE key DERIVE addr FIELD col str FIELD set n ;STRUCTURE

\ shape indexes the store's shapes.
ENUM slot
   VARIANT column
      FIELD col str FIELD shape n FIELD key nullable<key>
      FIELD only nullable<key>
      FIELD unique bool FIELD notnull bool
   ;VARIANT
   VARIANT term FIELD text str ;VARIANT
;ENUM

: SLOT-NOTNULL? ( slot -- bool )
   MATCH slot
      column OF {: c:str shape:n k o u:bool nn:bool :} nn ENDOF
      term OF drop false ENDOF
   ;MATCH ;

\ A record: a source of the description, or one combination of its `cols`
\ slots' columns; graph is empty for the default graph; origin indexes the
\ description's source.
STRUCTURE source DERIVE addr
   FIELD from str
   FIELD graph str
   FIELD s slot
   FIELD p slot
   FIELD o slot
   FIELD origin n
;STRUCTURE

\ One slot a description slot may read to.
STRUCTURE alternative DERIVE addr FIELD slot slot ;STRUCTURE

\ The slots a description source's `s`, `p` and `o` read to.
STRUCTURE origin DERIVE addr
   FIELD s ^VEC:vector<alternative>
   FIELD p ^VEC:vector<alternative>
   FIELD o ^VEC:vector<alternative>
;STRUCTURE

\ The indexes of the records whose `p` is one term.
STRUCTURE predicate DERIVE addr FIELD records ^VEC:vector<n> ;STRUCTURE

\ places is a MAP map from each `p` term's text to its place in
\ predicates; a uri's text holds only its value, so equal texts are one
\ IRI. columns lists the records whose `p` is a column, graphs the first
\ record of each named graph, both in record order. named is a MAP map
\ from each named graph's text to its first record.
STRUCTURE store DERIVE addr
   FIELD limit n
   FIELD shapes ^VEC:vector<shape>
   FIELD keys ^VEC:vector<key-set>
   FIELD sources ^VEC:vector<source>
   FIELD origins ^VEC:vector<origin>  \ at each record's origin
   FIELD predicates ^VEC:vector<predicate>
   FIELD places ^n
   FIELD columns ^VEC:vector<n>
   FIELD graphs ^VEC:vector<n>
   FIELD named ^n
;STRUCTURE

\ The value of each member of a description object the format names,
\ none for one it lacks.
STRUCTURE source-members DERIVE addr
   FIELD from option<JSON:value>
   FIELD graph option<JSON:value>
   FIELD s option<JSON:value>
   FIELD p option<JSON:value>
   FIELD o option<JSON:value>
;STRUCTURE

\ A slot's or a `cols` entry's, beside the object itself.
STRUCTURE slot-members DERIVE addr
   FIELD object JSON:value
   FIELD col option<JSON:value>
   FIELD shape option<JSON:value>
   FIELD term option<JSON:value>
   FIELD cols option<JSON:value>
   FIELD key option<JSON:value>
   FIELD only option<JSON:value>
   FIELD unique option<JSON:value>
   FIELD notnull option<JSON:value>
;STRUCTURE

\ A term's; other is the first member name the format does not know.
STRUCTURE term-members DERIVE addr
   FIELD type option<JSON:value>
   FIELD value option<JSON:value>
   FIELD datatype option<JSON:value>
   FIELD lang option<JSON:value>
   FIELD other option<JSON:value>
;STRUCTURE

: ABSENT ( -- option<JSON:value> ) OPTION:NONE ;

: NO-SOURCE-MEMBERS ( ^source-members -- ) {: k:^ :}
   ABSENT k SOURCE-MEMBERS-FROM !
   ABSENT k SOURCE-MEMBERS-GRAPH !
   ABSENT k SOURCE-MEMBERS-S !
   ABSENT k SOURCE-MEMBERS-P !
   ABSENT k SOURCE-MEMBERS-O ! ;

: NO-SLOT-MEMBERS ( ^slot-members -- ) {: k:^ :}
   ABSENT k SLOT-MEMBERS-COL !
   ABSENT k SLOT-MEMBERS-SHAPE !
   ABSENT k SLOT-MEMBERS-TERM !
   ABSENT k SLOT-MEMBERS-COLS !
   ABSENT k SLOT-MEMBERS-KEY !
   ABSENT k SLOT-MEMBERS-ONLY !
   ABSENT k SLOT-MEMBERS-UNIQUE !
   ABSENT k SLOT-MEMBERS-NOTNULL ! ;

: NO-TERM-MEMBERS ( ^term-members -- ) {: t:^ :}
   ABSENT t TERM-MEMBERS-TYPE !
   ABSENT t TERM-MEMBERS-VALUE !
   ABSENT t TERM-MEMBERS-DATATYPE !
   ABSENT t TERM-MEMBERS-LANG !
   ABSENT t TERM-MEMBERS-OTHER ! ;

: HAS? ( JSON:value str -- bool )
   JSON:FIND MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH ;

: KEPT? ( option<JSON:value> -- bool )
   MATCH option
     none OF false ENDOF
     some OF drop true ENDOF
   ;MATCH ;

\ The value m keeps of v's member name, rejected as JSON:GET rejects a
\ missing member.
: KEPT ( option<JSON:value> JSON:value str -- JSON:value )
   {: m v name:str :}
   m MATCH option
     none OF v name JSON:GET ENDOF
     some OF ENDOF
   ;MATCH ;

\ Keeps v in m when name is want, and answers whether it is.
: KEPT-AS? ( JSON:value JSON:value str ^option<JSON:value> -- bool )
   {: name v want:str m:^ :}
   name want JSON:STRING= dup if v OPTION:SOME m ! then ;

: REQUIRE-OBJECT ( JSON:value str str -- ) {: v want:str fix:str :}
   v JSON:OBJECT? if exit then
   v want fix JSON:REJECT ;

\ Runs body on each member of v; a v that is not an object is rejected.
: KEEP-MEMBERS ( JSON:value [ JSON:value JSON:value -- ] -- )
   {: v body :}
   v s" an object" s" write it as a JSON object" REQUIRE-OBJECT
   v body JSON:EACH-MEMBER ;

: REQUIRE-STRING ( JSON:value -- ) {: v :}
   v JSON:STRING? if exit then
   v s" a string" s" write the value as a JSON string" JSON:REJECT ;

\ The number of the array's elements; another value is rejected at its place.
: ARRAY-LENGTH ( JSON:value -- n ) 0 swap [: drop 1+ ;] JSON:EACH ;

\ The number of the object's members; another value is rejected at its place.
: OBJECT-SIZE ( JSON:value -- n )
   0 swap [: drop drop 1+ ;] JSON:EACH-MEMBER ;

\ A map at mp that takes n entries without growing.
: MAP-FOR ( ^^n n ^ARENA:arena -- )
   {: mp:^ k:n arena :}
   arena k 1 max 2 * >COUNT MAP:IN mp ! ;

: SAVE$ ( str ^ARENA:arena -- str ) {: text:str arena :}
   text str> {: from:^ u:n :}
   arena u ARENA:ALLOC {: out:^ len:n :}
   from out u BYTE-COPY out len >str ;

\ The text inside the quotes, or for a string with an escape its decoding
\ in the arena.
: DECODED$ ( JSON:value ^ARENA:arena -- str ) {: v arena :}
   v JSON:TEXT$ {: text:str :}
   text str> {: raw:^ u:n :}
   text [char] \ STR:INDEX-OF MATCH option
     none OF raw 1+ u 2 - >str ENDOF
     some OF drop v [: arena SAVE$ ;] JSON:WITH-STRING ENDOF
   ;MATCH ;

\ The string decoded, rejected if it holds U+0000: it reaches SQL as text,
\ which cannot hold it.
: TERM-STRING$ ( JSON:value ^ARENA:arena -- str ) {: v arena :}
   v arena DECODED$ dup 0 STR:INDEX-OF MATCH option
     none OF exit ENDOF
     some OF drop ENDOF
   ;MATCH
   v s" a string without U+0000"
   s" remove U+0000 from the string: PostgreSQL text cannot hold it"
   JSON:REJECT ;

: STRING$ ( JSON:value ^ARENA:arena -- str ) {: m arena :}
   m REQUIRE-STRING  m arena DECODED$ ;

: STRING-MEMBER$ ( JSON:value str ^ARENA:arena -- str )
   {: v name:str arena :}
   v name JSON:GET arena STRING$ ;

: READ-BYTES ( JSON:value -- n ) {: v :}
   v s" bytes" JSON:GET {: b :}
   b JSON:INTEGER? if b JSON:>N dup 0 > if exit then drop then
   b s" a whole number of bytes above 0"
   s" give the largest answer in bytes" JSON:REJECT ;

: HOLDS-REJECT ( JSON:value -- )
   s" `uri` or an array of datatype IRIs"
   s\" write \"uri\" or list the datatypes the shape holds" JSON:REJECT ;

: ADD-DATATYPE ( JSON:value ^VEC:vector<datatype> ^ARENA:arena -- )
   {: v list:^ arena :}
   v JSON:STRING? 0= if v HOLDS-REJECT then
   v arena DECODED$ list VEC:PUSH DATATYPE-IRI ! ;

: HOLDS-OF ( JSON:value ^ARENA:arena -- holds ) {: h arena :}
   h s" uri" JSON:STRING= if HOLDS-URIS exit then
   h JSON:ARRAY? 0= if h HOLDS-REJECT then
   arena ARENA:ALLOCATOR 0 [type] datatype VEC:NEW {: list:^ :}
   list h ARRAY-LENGTH VEC:RESERVE
   h [: list arena ADD-DATATYPE ;] JSON:EACH
   list HOLDS-LITERALS ;

: READ-HOLDS ( JSON:value ^ARENA:arena -- holds ) {: v arena :}
   v s" holds" JSON:FIND MATCH option
     none OF HOLDS-ANY ENDOF
     some OF arena HOLDS-OF ENDOF
   ;MATCH ;

\ The shape's `type`, empty when it has none.
: READ-TYPE ( JSON:value ^ARENA:arena -- str ) {: v arena :}
   v s" type" JSON:FIND MATCH option
     none OF s" " ENDOF
     some OF arena STRING$ ENDOF
   ;MATCH ;

\ A part's parent stays -1 until LINK-PART reads its `within`, its root
\ until RESOLVE-PART follows its parents.
: ADD-SHAPE ( JSON:value JSON:value ^store ^ARENA:arena -- )
   {: name v store:^ arena :}
   store STORE-SHAPES @ {: shapes:^ :}
   shapes VEC:PUSH {: row:^ :}
   name arena DECODED$ row SHAPE-NAME !
   -1 row SHAPE-PARENT !
   v s" within" HAS? if -1 row SHAPE-ROOT ! exit then
   shapes VEC:COUNT 1- row SHAPE-ROOT !
   v s" in" arena STRING-MEMBER$ row SHAPE-IN !
   v s" out" arena STRING-MEMBER$ row SHAPE-OUT !
   v arena READ-HOLDS row SHAPE-HOLDS !
   v arena READ-TYPE row SHAPE-TYPE ! ;

\ Adds an IRI and its keys, a string, to the set.
: ADD-MEMBER ( JSON:value JSON:value ^key-set ^ARENA:arena -- )
   {: iri v set:^ arena :}
   v REQUIRE-STRING
   set KEY-SET-MEMBERS @ {: members:^ :}
   set KEY-SET-PLACES arena MAP:ROOM
   members VEC:COUNT set KEY-SET-PLACES @ iri arena DECODED$ MAP:SET
   v arena TERM-STRING$ members VEC:PUSH MEMBER-KEYS ! ;

\ The SQL array literal of the IRI's keys, an empty one for an IRI the set
\ does not list.
: KEYS$ ( ^key-set str -- str )
   {: set:^ iri:str :}
   set KEY-SET-PLACES @ iri MAP:GET MATCH option
     none OF s" {}" ENDOF
     some OF {: k:n :} set KEY-SET-MEMBERS @ k VEC:AT MEMBER-KEYS @ ENDOF
   ;MATCH ;

: ADD-KEY-SET ( JSON:value JSON:value ^store ^ARENA:arena -- )
   {: name v store:^ arena :}
   v s" a key set" s" give the key set as an object with `type` and `values`"
   REQUIRE-OBJECT
   store STORE-KEYS @ VEC:PUSH {: row:^ :}
   name arena DECODED$ row KEY-SET-NAME !
   v s" type" arena STRING-MEMBER$ row KEY-SET-TYPE !
   v s" values" JSON:GET {: values :}
   values s" an object of IRIs and their keys"
   s" map each IRI to the SQL array literal of its keys" REQUIRE-OBJECT
   values OBJECT-SIZE {: count:n :}
   arena ARENA:ALLOCATOR 0 [type] member VEC:NEW {: members:^ :}
   members count VEC:RESERVE  members row KEY-SET-MEMBERS !
   row KEY-SET-PLACES count arena MAP-FOR
   values [: row arena ADD-MEMBER ;] JSON:EACH-MEMBER ;

: READ-KEYS ( JSON:value ^store ^ARENA:arena -- )
   {: v store:^ arena :}
   v s" keys" JSON:FIND MATCH option
     none OF ENDOF
     some OF {: keys :}
        keys s" an object of key sets"
        s" name each key set in `keys`" REQUIRE-OBJECT
        store STORE-KEYS @ keys OBJECT-SIZE VEC:RESERVE
        keys [: store arena ADD-KEY-SET ;] JSON:EACH-MEMBER
     ENDOF
   ;MATCH ;

\ The member m keeps, rejected at its value as want with fix.
: NO-MEMBER ( option<JSON:value> str str -- )
   {: m want:str fix:str :}
   m MATCH option
     none OF ENDOF
     some OF want fix JSON:REJECT ENDOF
   ;MATCH ;

: TERM-MEMBER ( JSON:value JSON:value ^term-members -- )
   {: name v t:^ :}
   name v s" type" t TERM-MEMBERS-TYPE KEPT-AS? if exit then
   name v s" value" t TERM-MEMBERS-VALUE KEPT-AS? if exit then
   name v s" datatype" t TERM-MEMBERS-DATATYPE KEPT-AS? if exit then
   name v s" xml:lang" t TERM-MEMBERS-LANG KEPT-AS? if exit then
   t TERM-MEMBERS-OTHER @ KEPT? if exit then
   name OPTION:SOME t TERM-MEMBERS-OTHER ! ;

: CHECK-MEMBER ( option<JSON:value> -- )
   s" `type`, `value`, `datatype` or `xml:lang`"
   s" remove the member: a term has no other" NO-MEMBER ;

\ The term's type and value.
: CHECK-TERM ( ^term-members JSON:value -- JSON:value JSON:value )
   {: t:^ v :}
   t TERM-MEMBERS-TYPE @ v s" type" KEPT {: kind :}
   kind s" uri" JSON:STRING= kind s" literal" JSON:STRING= or 0= if
      kind s" `uri` or `literal`" s" write a uri or a literal term"
      JSON:REJECT
   then
   t TERM-MEMBERS-VALUE @ v s" value" KEPT {: value :}
   value REQUIRE-STRING
   t TERM-MEMBERS-OTHER @ CHECK-MEMBER
   kind value ;

: URI-MEMBER ( option<JSON:value> -- )
   s" no `datatype` or `xml:lang` on a uri"
   s" remove it: a literal has one, a uri none" NO-MEMBER ;

: CHECK-URI ( ^term-members -- ) {: t:^ :}
   t TERM-MEMBERS-DATATYPE @ URI-MEMBER
   t TERM-MEMBERS-LANG @ URI-MEMBER ;

: CHECK-DATATYPE ( JSON:value -- ) {: d :}
   d REQUIRE-STRING
   d s" http://www.w3.org/2001/XMLSchema#string" JSON:STRING= if
      d s" a datatype other than xsd:string"
      s" remove `datatype`: a plain literal is an xsd:string"
      JSON:REJECT
   then
   d s" http://www.w3.org/1999/02/22-rdf-syntax-ns#langString"
   JSON:STRING= if
      d s" a datatype other than rdf:langString"
      s" replace `datatype` with `xml:lang`: a literal with a language has no datatype"
      JSON:REJECT
   then ;

: LOWER? ( str -- bool ) {: tag:str :}
   tag STR:LEN 0 ?do
      tag STR:DATA i + c@ dup STR:ASCII-LOWER <> if
         false unloop exit
      then
   loop true ;

\ A letter, or in a later group than the first a digit.
: TAG-BYTE? ( n bool -- bool ) {: byte:n first:bool :}
   byte STR:ALNUM? byte STR:DIGIT? first and 0= and ;

\ Letters, then groups of letters or digits, each after a `-`.
: TAG? ( str -- bool ) {: tag:str :}
   true [char] - tag STR:LEN 0 ?do
      tag STR:DATA i + c@ {: first:bool last:n byte:n :}
      byte [char] - = if
         last [char] - = if false unloop exit then
         false
      else
         byte first TAG-BYTE? 0= if false unloop exit then
         first
      then byte
   loop nip [char] - <> ;

: CHECK-LANGUAGE ( JSON:value -- ) {: tag :}
   tag REQUIRE-STRING
   tag [: TAG? ;] JSON:WITH-STRING 0= if
      tag s" a language tag: letters, then letters or digits after each -"
      s" write a tag such as en or en-gb" JSON:REJECT
   then
   tag [: LOWER? ;] JSON:WITH-STRING 0= if
      tag s" a language tag in lower case"
      s" write it in lower case" JSON:REJECT
   then ;

: CHECK-LITERAL ( ^term-members JSON:value -- ) {: t:^ v :}
   t TERM-MEMBERS-DATATYPE @ KEPT? t TERM-MEMBERS-LANG @ KEPT? and if
      v s" a literal with one of `datatype` and `xml:lang`"
      s" remove one of the two" JSON:REJECT
   then
   t TERM-MEMBERS-DATATYPE @ MATCH option
     none OF ENDOF some OF CHECK-DATATYPE ENDOF ;MATCH
   t TERM-MEMBERS-LANG @ MATCH option
     none OF ENDOF some OF CHECK-LANGUAGE ENDOF ;MATCH ;

\ Keeps the term's members in t; answers its checked type and value.
: KEEP-TERM ( ^term-members JSON:value -- JSON:value JSON:value )
   {: t:^ v :}
   v [: t TERM-MEMBER ;] KEEP-MEMBERS
   t v CHECK-TERM {: kind value :}
   kind s" literal" JSON:STRING= if
      t v CHECK-LITERAL
   else t CHECK-URI then
   kind value ;

: NEW-WRITER ( ^ARENA:arena -- ^JSON:writer )
   ARENA:ALLOCATOR {: al:^ :}
   al [type] JSON:writer ALLOC:NEW al 0 [type] u8 VEC:NEW JSON:OPEN ;

\ A term's text is its Results JSON object, compact: TERM-START's members,
\ then `xml:lang` or `datatype` where it has one. Two terms are one exactly
\ when their texts are equal.
: TERM-START ( ^JSON:writer str str -- ^JSON:writer )
   {: writer kind:str value:str :}
   writer JSON:WRITE-OBJECT-START
   s" type" kind JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" value" value JSON:WRITE-FIELD-S ;

: +TERM-MEMBER ( ^JSON:writer str str -- ^JSON:writer )
   {: name:str value:str :}
   JSON:WRITE-COMMA name value JSON:WRITE-FIELD-S ;

\ Writes the member m keeps, if it keeps one.
: COPY-MEMBER ( ^JSON:writer option<JSON:value> str ^ARENA:arena
      -- ^JSON:writer )
   {: m name:str arena :}
   m MATCH option
     none OF ENDOF
     some OF {: v :} name v arena TERM-STRING$ +TERM-MEMBER ENDOF
   ;MATCH ;

\ The checked term's text.
: TERM-TEXT$ ( JSON:value JSON:value ^term-members ^ARENA:arena
      -- str )
   {: kind value t:^ arena :}
   arena NEW-WRITER
   kind arena DECODED$ value arena TERM-STRING$ TERM-START
   t TERM-MEMBERS-LANG @ s" xml:lang" arena COPY-MEMBER
   t TERM-MEMBERS-DATATYPE @ s" datatype" arena COPY-MEMBER
   JSON:WRITE-OBJECT-END JSON:WRITTEN$ ;

: TERM$ ( ^term-members JSON:value ^ARENA:arena -- str )
   {: t:^ v arena :}
   t v KEEP-TERM t arena TERM-TEXT$ ;

: GRAPH-REJECT ( JSON:value -- )
   s" null or a term of type `uri`"
   s" write null for the default graph or a uri term" JSON:REJECT ;

: GRAPH-TERM$ ( ^term-members JSON:value ^ARENA:arena -- str )
   {: t:^ g arena :}
   t g KEEP-TERM {: kind value :}
   kind s" uri" JSON:STRING= 0= if g GRAPH-REJECT then
   kind value t arena TERM-TEXT$ ;

\ The graph's text, empty for the default graph.
: READ-GRAPH ( JSON:value ^ARENA:arena -- str ) {: g arena :}
   g JSON:NULL? if s" " exit then
   g JSON:OBJECT? 0= if g GRAPH-REJECT then
   [type] term-members
   [: dup NO-TERM-MEMBERS g arena GRAPH-TERM$ ;] MEM:WITH-RECORD ;

: SHAPE-INDEX ( JSON:value ^store -- n ) {: v store:^ :}
   store STORE-SHAPES @ {: shapes:^ :}
   shapes VEC:COUNT 0 ?do
      v shapes i VEC:AT SHAPE-NAME @ JSON:STRING= if i unloop exit then
   loop
   v s" the name of a shape in `shapes`"
   s" declare the shape in `shapes` or name a declared one" JSON:REJECT ;

\ The shape row of description shape name.
: SHAPE-ROW ( JSON:value ^store -- ^shape ) {: name store:^ :}
   store STORE-SHAPES @ name store SHAPE-INDEX VEC:AT ;

: LINK-PART ( JSON:value JSON:value ^store -- )
   {: name v store:^ :}
   v s" within" HAS? 0= if exit then
   v s" within" JSON:GET store SHAPE-INDEX
   name store SHAPE-ROW SHAPE-PARENT ! ;

\ The root shape k's parents reach, rejected at w, k's `within`, when
\ they cycle: a chain of more parents than shapes repeats one.
: PART-ROOT ( JSON:value ^VEC:vector<shape> n -- n )
   {: w shapes:^ k:n :}
   k shapes VEC:COUNT 0 ?do
      shapes over VEC:AT SHAPE-PARENT @
      dup 0< if drop unloop exit then nip
   loop drop
   w s" the name of a shape whose chain of `within` ends"
   s" break the cycle of `within` the name leads into" JSON:REJECT ;

\ A part takes its root's in, out, holds, type and index: every shape on
\ its chain of parents shares them.
: RESOLVE-PART ( JSON:value JSON:value ^store -- )
   {: name v store:^ :}
   v s" within" HAS? 0= if exit then
   store STORE-SHAPES @ {: shapes:^ :}
   name store SHAPE-INDEX {: k:n :}
   v s" within" JSON:GET shapes k PART-ROOT {: r:n :}
   shapes r VEC:AT {: root:^ :}
   shapes k VEC:AT {: row:^ :}
   root SHAPE-IN @ row SHAPE-IN !
   root SHAPE-OUT @ row SHAPE-OUT !
   root SHAPE-HOLDS @ row SHAPE-HOLDS !
   root SHAPE-TYPE @ row SHAPE-TYPE !
   r row SHAPE-ROOT ! ;

\ A slot's `col` or `term` beside `cols`, rejected at its value.
: BESIDE-COLS ( option<JSON:value> -- )
   s" no `col` or `term` beside `cols`"
   s" remove it: `cols` names each column" NO-MEMBER ;

\ The `key` m keeps, rejected at its value as want.
: REJECT-KEY ( option<JSON:value> str -- ) {: m want:str :}
   m want s" remove `key`: a key belongs on a column slot" NO-MEMBER ;

\ The `only` m keeps, rejected at its value as want.
: REJECT-ONLY ( option<JSON:value> str -- ) {: m want:str :}
   m want s" remove `only`: it belongs on a column of `cols`" NO-MEMBER ;

: SLOT-MEMBER ( JSON:value JSON:value ^slot-members -- )
   {: name v k:^ :}
   name v s" col" k SLOT-MEMBERS-COL KEPT-AS? if exit then
   name v s" shape" k SLOT-MEMBERS-SHAPE KEPT-AS? if exit then
   name v s" term" k SLOT-MEMBERS-TERM KEPT-AS? if exit then
   name v s" cols" k SLOT-MEMBERS-COLS KEPT-AS? if exit then
   name v s" key" k SLOT-MEMBERS-KEY KEPT-AS? if exit then
   name v s" only" k SLOT-MEMBERS-ONLY KEPT-AS? if exit then
   name v s" unique" k SLOT-MEMBERS-UNIQUE KEPT-AS? if exit then
   name v s" notnull" k SLOT-MEMBERS-NOTNULL KEPT-AS? drop ;

: KEEP-SLOT ( ^slot-members JSON:value -- ) {: k:^ m :}
   m k SLOT-MEMBERS-OBJECT !
   m [: k SLOT-MEMBER ;] KEEP-MEMBERS ;

: ENTRY-REJECT ( JSON:value -- )
   s" a column with `col` and `shape`"
   s\" write the column as {\"col\": c, \"shape\": name}" JSON:REJECT ;

\ An entry that is not an object keeps only itself.
: ADD-ENTRY ( JSON:value ^VEC:vector<slot-members> -- ) {: e list:^ :}
   list VEC:PUSH {: k:^ :}
   k NO-SLOT-MEMBERS
   e JSON:OBJECT? if k e KEEP-SLOT exit then
   e k SLOT-MEMBERS-OBJECT ! ;

: CHECK-ENTRY ( ^slot-members -- ) {: k:^ :}
   k SLOT-MEMBERS-OBJECT @ {: e :}
   e JSON:OBJECT? 0= if e ENTRY-REJECT then
   k SLOT-MEMBERS-COL @ KEPT?  k SLOT-MEMBERS-SHAPE @ KEPT? and
   k SLOT-MEMBERS-TERM @ KEPT? 0= and 0= if e ENTRY-REJECT then
   k SLOT-MEMBERS-KEY @ s" no `key` in a column of `cols`" REJECT-KEY
   k SLOT-MEMBERS-NOTNULL @ s" no `notnull` in a column of `cols`"
   s" remove `notnull`: a column of `cols` is NULL where another holds the value"
   NO-MEMBER ;

\ The entries of a `cols` array, kept.
: KEEP-ENTRIES ( JSON:value ^ARENA:arena -- ^VEC:vector<slot-members> )
   {: list arena :}
   list JSON:ARRAY? 0= if
      list s" an array of columns"
      s" list the columns as objects with `col` and `shape`" JSON:REJECT
   then
   arena ARENA:ALLOCATOR 0 [type] slot-members VEC:NEW {: entries:^ :}
   entries list ARRAY-LENGTH VEC:RESERVE
   list [: entries ADD-ENTRY ;] JSON:EACH
   entries VEC:COUNT 2 < if
      list s" at least two columns"
      s" give one column as a slot with `col` and `shape`" JSON:REJECT
   then entries ;

\ The checked entries of the slot's `cols`.
: CHECK-COLS ( ^slot-members ^ARENA:arena
      -- ^VEC:vector<slot-members> )
   {: k:^ arena :}
   k SLOT-MEMBERS-COL @ BESIDE-COLS  k SLOT-MEMBERS-TERM @ BESIDE-COLS
   k SLOT-MEMBERS-KEY @ s" no `key` beside `cols`" REJECT-KEY
   k SLOT-MEMBERS-ONLY @ s" no `only` beside `cols`" REJECT-ONLY
   k SLOT-MEMBERS-UNIQUE @ s" no `unique` beside `cols`"
   s" remove `unique`: it belongs on a column slot or a column of `cols`" NO-MEMBER
   k SLOT-MEMBERS-NOTNULL @ s" no `notnull` beside `cols`"
   s" remove `notnull`: it belongs on a column slot" NO-MEMBER
   k SLOT-MEMBERS-COLS @ k SLOT-MEMBERS-OBJECT @ s" cols" KEPT
   arena KEEP-ENTRIES {: entries:^ :}
   entries VEC:COUNT 0 ?do entries i VEC:AT CHECK-ENTRY loop
   entries ;

: CHECK-SLOT ( ^slot-members -- ) {: k:^ :}
   k SLOT-MEMBERS-OBJECT @ {: v :}
   k SLOT-MEMBERS-COL @ KEPT? {: col:bool :}
   k SLOT-MEMBERS-TERM @ KEPT? {: term:bool :}
   col term and  col term or 0=  or if
      v s" one of `col`, `term` and `cols`"
      s" give a column with `col` and `shape`, a constant `term`, or columns in `cols`"
      JSON:REJECT
   then
   col k SLOT-MEMBERS-SHAPE @ KEPT? 0= and if
      v s" a `shape` beside `col`" s" name the column's shape in `shape`"
      JSON:REJECT
   then
   k SLOT-MEMBERS-ONLY @ s" no `only` outside a column of `cols`"
   REJECT-ONLY ;

: KEY-SET-INDEX ( JSON:value ^store -- n ) {: v store:^ :}
   store STORE-KEYS @ {: sets:^ :}
   sets VEC:COUNT 0 ?do
      v sets i VEC:AT KEY-SET-NAME @ JSON:STRING= if i unloop exit then
   loop
   v s" the name of a key set in `keys`"
   s" declare the key set in `keys` or name a declared one" JSON:REJECT ;

: NEW-KEY ( JSON:value ^store ^ARENA:arena -- ^key )
   {: v store:^ arena :}
   arena ARENA:ALLOCATOR [type] key ALLOC:NEW {: row:^ :}
   v s" col" arena STRING-MEMBER$ row KEY-COL !
   v s" set" JSON:GET store KEY-SET-INDEX row KEY-SET !
   row ;

: READ-KEY ( option<JSON:value> ^store ^ARENA:arena -- nullable<key> )
   {: m store:^ arena :}
   m MATCH option
     none OF NULLABLE:NONE ENDOF
     some OF store arena NEW-KEY NULLABLE:SOME ENDOF
   ;MATCH ;

: READ-FLAG ( option<JSON:value> -- bool )
   MATCH option
     none OF false ENDOF
     some OF {: u :}
        u JSON:BOOL? 0= if
           u s" true or false" s" write the value as a JSON boolean"
           JSON:REJECT
        then u JSON:>BOOL
     ENDOF
   ;MATCH ;

: TERM-SLOT ( JSON:value ^ARENA:arena -- slot ) {: v arena :}
   [type] term-members [: dup NO-TERM-MEMBERS v arena TERM$ ;]
   MEM:WITH-RECORD SLOT-TERM ;

\ A checked column or term slot.
: SLOT-OF ( ^slot-members ^store ^ARENA:arena -- slot )
   {: k:^ store:^ arena :}
   k SLOT-MEMBERS-OBJECT @ {: m :}
   k SLOT-MEMBERS-COL @ KEPT? 0= if
      k SLOT-MEMBERS-TERM @ m s" term" KEPT arena TERM-SLOT exit
   then
   k SLOT-MEMBERS-COL @ m s" col" KEPT arena STRING$
   k SLOT-MEMBERS-SHAPE @ m s" shape" KEPT store SHAPE-INDEX
   k SLOT-MEMBERS-KEY @ store arena READ-KEY
   k SLOT-MEMBERS-ONLY @ store arena READ-KEY
   k SLOT-MEMBERS-UNIQUE @ READ-FLAG
   k SLOT-MEMBERS-NOTNULL @ READ-FLAG SLOT-COLUMN ;

: +ALTERNATIVE ( slot ^VEC:vector<alternative> -- )
   VEC:PUSH ALTERNATIVE-SLOT ! ;

\ An empty list with room for n alternatives.
: NEW-ALTERNATIVES ( n ^ARENA:arena -- ^VEC:vector<alternative> )
   {: k:n arena :}
   arena ARENA:ALLOCATOR 0 [type] alternative VEC:NEW {: list:^ :}
   list k VEC:RESERVE list ;

: COLUMN-SLOTS ( ^VEC:vector<slot-members> ^store ^ARENA:arena
      -- ^VEC:vector<alternative> )
   {: entries:^ store:^ arena :}
   entries VEC:COUNT arena NEW-ALTERNATIVES {: list:^ :}
   entries VEC:COUNT 0 ?do
      entries i VEC:AT store arena SLOT-OF list +ALTERNATIVE
   loop list ;

\ The slots m reads to: its columns for `cols`, else itself.
: SLOT-ALTERNATIVES ( ^slot-members JSON:value ^store ^ARENA:arena
      -- ^VEC:vector<alternative> )
   {: k:^ m store:^ arena :}
   k m KEEP-SLOT
   k SLOT-MEMBERS-COLS @ KEPT? if
      k arena CHECK-COLS store arena COLUMN-SLOTS exit
   then
   k CHECK-SLOT
   1 arena NEW-ALTERNATIVES {: list:^ :}
   k store arena SLOT-OF list +ALTERNATIVE list ;

\ The slots the source v's member name reads to; m is what v keeps.
: READ-SLOT ( option<JSON:value> JSON:value str ^store ^ARENA:arena
      -- ^VEC:vector<alternative> )
   {: m v name:str store:^ arena :}
   m v name KEPT {: slot :}
   [type] slot-members
   [: dup NO-SLOT-MEMBERS slot store arena SLOT-ALTERNATIVES ;]
   MEM:WITH-RECORD ;

: NTH-SLOT ( ^VEC:vector<alternative> n -- slot )
   VEC:AT ALTERNATIVE-SLOT @ ;

: ADD-RECORD ( ^source slot slot slot ^store -- )
   {: base:^ s p o store:^ :}
   store STORE-SOURCES @ VEC:PUSH {: row:^ :}
   base SOURCE-FROM @ row SOURCE-FROM !
   base SOURCE-GRAPH @ row SOURCE-GRAPH !
   base SOURCE-ORIGIN @ row SOURCE-ORIGIN !
   s row SOURCE-S !  p row SOURCE-P !  o row SOURCE-O ! ;

\ One record of base per combination of the slots' alternatives, the
\ object's varying fastest.
: ADD-RECORDS ( ^source ^VEC:vector<alternative>
      ^VEC:vector<alternative> ^VEC:vector<alternative> ^store -- )
   {: base:^ ss:^ ps:^ os:^ store:^ :}
   os VEC:COUNT {: no:n :}
   ps VEC:COUNT no * {: per:n :}
   ss VEC:COUNT per * 0 ?do
      base ss i per / NTH-SLOT
      ps i no / ps VEC:COUNT mod NTH-SLOT
      os i no mod NTH-SLOT store ADD-RECORD
   loop ;

: SOURCE-MEMBER ( JSON:value JSON:value ^source-members -- )
   {: name v k:^ :}
   name v s" from" k SOURCE-MEMBERS-FROM KEPT-AS? if exit then
   name v s" graph" k SOURCE-MEMBERS-GRAPH KEPT-AS? if exit then
   name v s" s" k SOURCE-MEMBERS-S KEPT-AS? if exit then
   name v s" p" k SOURCE-MEMBERS-P KEPT-AS? if exit then
   name v s" o" k SOURCE-MEMBERS-O KEPT-AS? drop ;

: READ-SOURCE ( ^source-members ^source n JSON:value ^store ^ARENA:arena -- n )
   {: k:^ base:^ origin:n v store:^ arena :}
   v [: k SOURCE-MEMBER ;] KEEP-MEMBERS
   k SOURCE-MEMBERS-FROM @ v s" from" KEPT arena STRING$
   base SOURCE-FROM !
   k SOURCE-MEMBERS-GRAPH @ v s" graph" KEPT arena READ-GRAPH
   base SOURCE-GRAPH !
   origin base SOURCE-ORIGIN !
   k SOURCE-MEMBERS-S @ v s" s" store arena READ-SLOT {: ss:^ :}
   k SOURCE-MEMBERS-P @ v s" p" store arena READ-SLOT {: ps:^ :}
   k SOURCE-MEMBERS-O @ v s" o" store arena READ-SLOT {: os:^ :}
   store STORE-ORIGINS @ VEC:PUSH {: org:^ :}
   ss org ORIGIN-S !  ps org ORIGIN-P !  os org ORIGIN-O !
   base ss ps os store ADD-RECORDS
   origin 1+ ;

\ Adds the description's source origin and answers the next one's index.
: ADD-SOURCE ( n JSON:value ^store ^ARENA:arena -- n )
   {: origin:n v store:^ arena :}
   [type] source-members
   [: dup NO-SOURCE-MEMBERS
      [type] source [: origin v store arena READ-SOURCE ;] MEM:WITH-RECORD ;]
   MEM:WITH-RECORD ;

\ The list of the records whose `p` is the term, made at its first one.
: TERM-RECORDS ( ^store str ^ARENA:arena -- ^VEC:vector<n> )
   {: store:^ t:str arena :}
   store STORE-PREDICATES @ {: list:^ :}
   store STORE-PLACES @ t MAP:GET MATCH option
      none OF
         store STORE-PLACES arena MAP:ROOM
         list VEC:COUNT store STORE-PLACES @ t MAP:SET
         arena ARENA:ALLOCATOR 0 [type] n VEC:NEW
         dup list VEC:PUSH PREDICATE-RECORDS !
      ENDOF
      some OF list swap VEC:AT PREDICATE-RECORDS @ ENDOF
   ;MATCH ;

: INDEX-PREDICATE ( ^store n ^ARENA:arena -- )
   {: store:^ k:n arena :}
   store STORE-SOURCES @ k VEC:AT SOURCE-P @ MATCH slot
      column OF 2drop 2drop drop drop store STORE-COLUMNS @ ENDOF
      term OF {: t:str :} store t arena TERM-RECORDS ENDOF
   ;MATCH VEC:PUSH k swap ! ;

: INDEX-GRAPH ( ^store n ^ARENA:arena -- )
   {: store:^ k:n arena :}
   store STORE-SOURCES @ k VEC:AT SOURCE-GRAPH @ {: g:str :}
   g STR:LEN 0= if exit then
   store STORE-NAMED @ g MAP:GET MATCH option
      none OF
         store STORE-NAMED arena MAP:ROOM
         k store STORE-NAMED @ g MAP:SET
         k store STORE-GRAPHS @ VEC:PUSH !
      ENDOF
      some OF drop ENDOF
   ;MATCH ;

: INDEX-RECORDS ( ^store ^ARENA:arena -- )
   {: store:^ arena :}
   arena ARENA:ALLOCATOR {: al:^ :}
   al 0 [type] predicate VEC:NEW store STORE-PREDICATES !
   al 0 [type] n VEC:NEW store STORE-COLUMNS !
   al 0 [type] n VEC:NEW store STORE-GRAPHS !
   store STORE-PLACES arena MAP:ROOM
   store STORE-NAMED arena MAP:ROOM
   store STORE-SOURCES @ VEC:COUNT 0 ?do
      store i arena INDEX-PREDICATE  store i arena INDEX-GRAPH
   loop ;

\ The alternatives slot m reads to: its `cols` array's length, else 1,
\ a slot READ-SLOT rejects included.
: COLS-LENGTH ( JSON:value -- n ) {: m :}
   m JSON:OBJECT? 0= if 1 exit then
   m s" cols" JSON:FIND MATCH option
     none OF 1 ENDOF
     some OF dup JSON:ARRAY? if ARRAY-LENGTH else drop 1 then ENDOF
   ;MATCH ;

: SLOT-WIDTH ( JSON:value str -- n ) {: v name:str :}
   v name JSON:FIND MATCH option
     none OF 1 ENDOF
     some OF COLS-LENGTH ENDOF
   ;MATCH ;

\ n plus the records the source makes, the product of its slots' widths;
\ a source that is not an object counts one.
: +RECORDS ( n JSON:value -- n ) {: k:n v :}
   v JSON:OBJECT? 0= if k 1+ exit then
   v s" s" SLOT-WIDTH  v s" p" SLOT-WIDTH *  v s" o" SLOT-WIDTH *  k + ;

\ The records a `sources` array makes; another value is rejected at its place.
: RECORD-COUNT ( JSON:value -- n ) 0 swap [: +RECORDS ;] JSON:EACH ;

\ Reads the description text, which must outlive the store.
: READ-STORE ( str ^ARENA:arena -- ^store ) {: text:str arena :}
   text JSON:PARSE {: v :}
   arena ARENA:ALLOCATOR {: al:^ :}
   al [type] store ALLOC:NEW {: store:^ :}
   v READ-BYTES store STORE-LIMIT !
   al 0 [type] shape VEC:NEW store STORE-SHAPES !
   al 0 [type] key-set VEC:NEW store STORE-KEYS !
   al 0 [type] source VEC:NEW store STORE-SOURCES !
   al 0 [type] origin VEC:NEW store STORE-ORIGINS !
   v s" shapes" JSON:GET {: shapes :}
   store STORE-SHAPES @ shapes OBJECT-SIZE VEC:RESERVE
   shapes [: store arena ADD-SHAPE ;] JSON:EACH-MEMBER
   shapes [: store LINK-PART ;] JSON:EACH-MEMBER
   shapes [: store RESOLVE-PART ;] JSON:EACH-MEMBER
   v store arena READ-KEYS
   v s" sources" JSON:GET {: sources :}
   store STORE-ORIGINS @ sources ARRAY-LENGTH VEC:RESERVE
   store STORE-SOURCES @ sources RECORD-COUNT VEC:RESERVE
   0 sources [: store arena ADD-SOURCE ;] JSON:EACH drop
   store arena INDEX-RECORDS
   store ;

;package
