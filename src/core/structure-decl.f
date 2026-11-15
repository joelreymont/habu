\ structure-decl.f: the STRUCTURE front end (package STRUCTURE-DECL), grammar loop
\ and reject dispatch only. DECL-FRONT (generated-declaration.f) owns the token
\ source and the guarded run; DECL-EVENT (decl-event.f) owns every event.
\   STRUCTURE type-name[<params>] (POLICY p | DERIVE f+ | OPAQUE)*
\      (FIELD name type | BUFFER name bytes)* ;STRUCTURE
\ A reject rolls the whole declaration back to a byte-identical registry. Loads
\ after decl-event.f and structure-make.f, whose GENERATE and ARM ;STRUCTURE calls.

using SCHEMA-REG
using TFAM

package TYPE-DECL
private
package STRUCTURE-DECL
using DECL-FRONT

\ Transient parse state, not declaration state: STRUCTURE is top-level
\ interpret-only, so one declaration runs at a time.
variable FAM        \ family id being declared
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ field count in this declaration
variable SD-CELLS      \ running field cell width (next field's slot / byte offset)
variable SD-ITEMS   \ MAKE's inputs so far: one per FIELD, one per buffer cell
variable SEEN-FIELD \ a FIELD has appeared (header clauses must precede fields)
variable POINTEE    \ resolving the pointee of `^`: the declared family may be named

$100 constant SD-ARG-CAP
create SD-ARGS SD-ARG-CAP cells allot
variable SD-ARG-N

: SD-RESET ( -- )
   0 SEEN-FIELD ! ;
SD-RESET
: SD-MISSING! ( -- ) TYPE@ MISSING! ;

: SD-NAME-FIX$ ( -- ^u8 n ) s" write the family name after `STRUCTURE`, as in `STRUCTURE point`" ;
: SD-LONG-FIX$ ( -- ^u8 n ) s" shorten the name to 255 bytes or fewer" ;
: SD-TYPE-FIX$ ( -- ^u8 n )
   s" name a type declared before this declaration: `n`, `^u8`, a header parameter, a `STRUCTURE` or an `ENUM`" ;
: SD-PARAM-FIX$ ( -- ^u8 n )
   s" list the parameter in the header, as in `STRUCTURE box<a>`, before a field names it" ;
: SD-ARGS-FIX$ ( -- ^u8 n ) s" give the family its type arguments, as in `box<n>`" ;
: SD-APP-FIX$ ( -- ^u8 n )
   s" write a type application as `family<type,type>`, as in `SPAN:span<^u8>`" ;
: SD-PTR-FIX$ ( -- ^u8 n )
   s" spell a pointer `^T`, `^` joined to the type it points at, as in `FIELD next ^node`" ;
: SD-OPEN-FIX$ ( -- ^u8 n ) s" close each `<` of the field type with `>` before the next clause" ;

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks: a copy kept here drifted twice (it admitted `if`, then value-record names).
\ Case and duplicate are TFAM-DECL's.
: NAME-RESERVED? ( ^u8 n -- bool )
   2dup s" structure" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop YES EXIT THEN
   TYPE-NAME:FAMILY-RESERVED? ;

: REQUIRE-NAME ( ^u8 n -- )
   dup 0= IF 2drop s" missing name" SD-NAME-FIX$ TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ SD-LONG-FIX$ TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail"
      s" write the name as a lowercase tail with no package qualifier, as in `point`"
      E-TFAM-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name"
      s" pick another name: a built-in type such as `n` or a keyword such as `if` cannot name a family"
      TYPE-DECL:E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;

: NAME-GATE ( ^u8 n -- )         \ the packet names the family, then REQUIRE-NAME judges it
   2dup DECL-REJECT:FAMILY! REQUIRE-NAME ;

: SD-KIND$ ( -- ^u8 n ) s" structure" ;

\ A family that owns a linear value is an accepted field type: the structure owns
\ the obligation by containment (TFAM-CONCRETE-LINEAR? walks into it), the rule a
\ field naming a DEFLINEAR con already relies on.
\ The kind test falls through to "unknown field type" because the one kind it
\ excludes, TK-EVIDENCE, has no declarer source can write.
\ The declared family may be named only beneath a `^` (`^node`, `SPAN:span<^node>`);
\ by value, as in `option<node>`, it is 7109.
: FIELD-FAM? ( ^u8 n -- n bool ) {: a:^ u:n :}
   TFAM-ACTIVE-PKG$ a u TFAM-SIG-RESOLVE {: id:n sr:n :}
   sr SR-AMBIG = IF a u DECL-REJECT:TOKEN!  a u TYPE-DECL:TDECL-AMBIG throw THEN
   sr SR-FOUND <> IF 0 NO EXIT THEN
   id UNSTORED-REASON IF TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN 2drop 2drop
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM @ = POINTEE @ 0= and IF
      s" field type cannot recursively name its owner"
      s" name the structure being declared only beneath `^`, as in `FIELD next ^node`"
      TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   id YES ;

: LETTER-TYPE ( ^u8 n n -- n )
   {: want:n :}
   drop c@
   dup ASCII-N = IF drop CC-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CC-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CC-R SCHEMA-CON EXIT THEN
   TYPE-DECL:DECL-PARAM 0= IF
      drop s" unknown field type" SD-PARAM-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   dup {: idx:n :}
   want PK-SCOPE = want PK-REGION = or
   want PK-TYPE = or IF
      FAM @ idx want TFAM-PK! THEN
   SCHEMA-PARAM ;

: SD-ARG+ ( n -- )
   SD-ARG-N @ SD-ARG-CAP >= IF
      s" field type application is too deep"
      s" nest fewer than 256 type arguments in one field type"
      TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   SD-ARG-N @ cells SD-ARGS + !
   SD-ARG-N @ 1 + SD-ARG-N ! ;

: SD-BLANK? ( n -- bool ) 33 < ;
: SD-DELIM? ( n -- bool )
   dup 60 = over 44 = or over 62 = or swap SD-BLANK? or ;

\ Each parser word takes the index it starts at and answers the index after what it
\ consumed; a type spelled over tokens (SD-SPAN) holds blanks.
: SD-ATOM ( ^u8 n n -- ^u8 n n ) {: a:^ u:n start:n :}
   start
   BEGIN
      dup u < IF a over + c@ SD-DELIM? 0= ELSE NO THEN
   WHILE
      1 +
   REPEAT {: j:n :}
   j start = IF
      s" malformed field type application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   a start + j start - j ;

\ The blanks an argument may start with.
: SD-SKIP ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   i BEGIN
      dup u < IF a over + c@ SD-BLANK? ELSE NO THEN
   WHILE
      1 +
   REPEAT ;

\ Whether the atom ending at j opens a pointer argument, and the index its
\ pointee is read from: an atom `^T` at the byte after `^`. A `^` with nothing
\ joined to it, as in `span<^ n>` or `span<^>`, is malformed; an atom `ptr` not
\ followed by `<` names no type and is refused as an unknown field type.
: SD-POINTEE-AT ( ^u8 n ^u8 n n -- n bool ) {: na:^ nu:n a:^ u:n j:n :}
   na c@ 94 = IF
      nu 1 = IF
         s" malformed field type application"
         SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      j nu - 1 + YES EXIT
   THEN
   na nu s" ptr" CORE-STR=CI 0= IF 0 NO EXIT THEN
   j u < IF a j + c@ 60 = ELSE NO THEN IF 0 NO EXIT THEN
   s" unknown field type" SD-PTR-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

defer SD-PARSE-NODE ( ^u8 n n n -- n n )

: SD-APP-ARGS ( ^u8 n n n n -- n n ) {: a:^ u:n fam:n k:n i:n :}
   k fam TFAM-ARITY@ >= IF
      s" field type has too many arguments"
      s" drop the extra type arguments: give the family as many as its header declares"
      TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   a u i  fam k TFAM-PK@  SD-PARSE-NODE {: node:n e:n :}
   node SD-ARG+
   a u e SD-SKIP {: j:n :}
   j u >= IF
      s" unclosed field type application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   a j + c@ {: c:n :}
   c 44 = IF a u fam k 1 + j 1 + RECURSE EXIT THEN
   c 62 = IF k 1 + j 1 + EXIT THEN
   s" malformed field type application" SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

: SD-APP ( ^u8 n n n -- n n ) {: a:^ u:n fam:n i:n :}
   SD-ARG-N @ {: base:n :}
   a u fam 0 i SD-APP-ARGS {: k:n j:n :}
   k fam TFAM-ARITY@ <> IF
      s" field type has wrong arity"
      s" give the family as many type arguments as its header declares"
      TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   FIELD-MISS @ IF base SD-ARG-N ! 0 j EXIT THEN
   SCHEMA-ROOT-N@ {: start:n :}
   base BEGIN dup SD-ARG-N @ < WHILE
      dup cells SD-ARGS + @ SCHEMA-ROOT+ drop 1 +
   REPEAT drop
   base SD-ARG-N !
   fam start k SCHEMA-APP j ;

defer SD-LOOSE-NODE ( ^u8 n n -- n )

: SD-LOOSE-ARGS ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   a u  a u i SD-LOOSE-NODE  SD-SKIP {: j:n :}
   j u >= IF
      s" unclosed field type application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   a j + c@ {: c:n :}
   c 62 = IF j 1 + EXIT THEN
   c 44 <> IF
      s" malformed field type application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   a u j 1 + RECURSE ;

: SD-LOOSE-APP ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   i u >= IF
      s" unclosed field type application"
      SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   a i + c@ 62 = IF i 1 + EXIT THEN
   a u i SD-LOOSE-ARGS ;

: SD-LOOSE-ATOM ( ^u8 n -- ) {: a:^ u:n :}
   u 1 = IF
      a c@ dup ASCII-N = over ASCII-F = or over ASCII-R = or
      IF drop EXIT THEN
      TFAM-DECL-CHAR>PARAM IF
         drop a c@ TYPE-DECL:DECL-PARAM IF drop EXIT THEN
         drop
         s" type parameter is outside the declared arity"
         SD-PARAM-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw
      ELSE drop THEN
   THEN
   a u CON-OF dup 0 <> IF drop EXIT THEN drop
   a u FIELD-FAM? IF
      dup TFAM-ARITY@ 0 <> IF
         s" field type is parametric and needs type arguments"
         SD-ARGS-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw
      THEN drop EXIT
   THEN drop
   a u TYPE-MAY-ARRIVE-XT IF SD-MISSING! EXIT THEN
   s" unknown field type" SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

\ The pointee of a pointer argument read from k (SD-POINTEE-AT), with POINTEE
\ set as RESOLVE-TYPE sets it.
: SD-LOOSE-PTR ( ^u8 n n -- n ) {: a:^ u:n k:n :}
   POINTEE @ {: outer:n :}
   -1 POINTEE !
   a u k SD-LOOSE-NODE
   outer POINTEE ! ;

\ SD-PARSE-NODE-IMPL's grammar over the arguments of a family that may still arrive
\ (SD-LOOSE-APP), building no application; an empty `<>` passes here and SD-APP refuses it.
: SD-LOOSE-NODE-IMPL ( ^u8 n n -- n ) {: a:^ u:n i:n :}
   a u  a u i SD-SKIP  SD-ATOM {: na:^ nu:n j:n :}
   na nu a u j SD-POINTEE-AT IF {: k:n :} a u k SD-LOOSE-PTR EXIT THEN drop
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? IF
         {: fam:n :}
         a u fam j 1 + SD-APP nip EXIT
      THEN drop
      na nu TYPE-MAY-ARRIVE-XT 0= IF
         s" unknown field type" SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      SD-MISSING!
      a u j 1 + SD-LOOSE-APP EXIT
   THEN
   na nu SD-LOOSE-ATOM j ;
: SD-LOOSE-INSTALL ( -- ) [: SD-LOOSE-NODE-IMPL ;] is SD-LOOSE-NODE ;
SD-LOOSE-INSTALL

\ An argument `^T` is a pointer to T, its pointee read from k
\ (SD-POINTEE-AT) and built as RESOLVE-TYPE's pointer arm builds one; while T is
\ read the declared family may be named (FIELD-FAM?).
: SD-PTR-NODE ( ^u8 n n -- n n ) {: a:^ u:n k:n :}
   POINTEE @ {: outer:n :}
   -1 POINTEE !
   a u k PK-CELL SD-PARSE-NODE {: node:n e:n :}
   outer POINTEE !
   FIELD-MISS @ IF 0 e EXIT THEN
   node s" field type is a pointer to a linear value and cannot own it"
   DECL-EVENT:REQUIRE-POINTEE SCHEMA-PTR e ;

\ The node at i of a field type's text: a type parameter, a con, a family
\ application or a pointer argument, with the index after it.
: SD-PARSE-NODE-IMPL ( ^u8 n n n -- n n ) {: a:^ u:n i:n want:n :}
   a u  a u i SD-SKIP  SD-ATOM {: na:^ nu:n j:n :}
   na nu a u j SD-POINTEE-AT IF {: k:n :} a u k SD-PTR-NODE EXIT THEN drop
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? 0= IF
         drop
         na nu TYPE-MAY-ARRIVE-XT 0= IF
            s" unknown field type" SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
         SD-MISSING!
         0 a u j 1 + SD-LOOSE-APP EXIT
      THEN
      {: fam:n :}
      a u fam j 1 + SD-APP EXIT
   THEN
   nu 1 = IF na nu want LETTER-TYPE j EXIT THEN
   na nu CON-OF dup 0 <> IF SCHEMA-CON j EXIT THEN drop
   na nu FIELD-FAM? IF {: fam:n :}
      fam TFAM-ARITY@ 0 <> IF
         s" field type is parametric and needs type arguments"
         SD-ARGS-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      fam 0 0 SCHEMA-APP j EXIT
   THEN drop
   na nu TYPE-MAY-ARRIVE-XT IF SD-MISSING! 0 j EXIT THEN
   s" unknown field type" SD-TYPE-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw ;

: SD-PARSER-INSTALL ( -- ) [: SD-PARSE-NODE-IMPL ;] is SD-PARSE-NODE ;
SD-PARSER-INSTALL

: SD-NEST ( n n -- n ) {: depth:n c:n :}   \ `<` depth after one byte
   c 60 = IF depth 1 + EXIT THEN
   c 62 = IF depth 1 - EXIT THEN
   depth ;
: SD-DEPTH ( n ^u8 n -- n ) {: depth:n a:^ u:n :}
   depth u 0 ?do a i + c@ SD-NEST loop ;

\ One declaration's tokens stand in order in one buffer (parse-name never refills, a replay
\ reads one body); its end or `;STRUCTURE` goes back and the type is unclosed.
: SD-SPAN-REST ( ^u8 n n -- ^u8 n ) {: a:^ u:n depth:n :}
   u depth
   BEGIN dup 0 > WHILE
      NEXT dup 0= IF YES ELSE 2dup s" ;structure" CORE-STR=CI THEN IF
         UNGET  drop a swap DECL-REJECT:TOKEN!
         s" unclosed field type application"
         SD-OPEN-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      2swap nip  >r 2dup r> -rot SD-DEPTH  -rot
      swap NULL-PTR - a NULL-PTR - - +  swap
   REPEAT
   drop a swap ;

\ A type whose first token leaves a `<` open runs to the token closing it, as one span of the
\ source or replay body, which outlives the TYPE@ span SD-MISSING! keeps.
: SD-SPAN ( ^u8 n -- ^u8 n ) {: a:^ u:n :}
   0 a u SD-DEPTH {: depth:n :}
   depth 1 < IF a u EXIT THEN
   a u depth SD-SPAN-REST 2dup DECL-REJECT:TOKEN! ;

defer SD-QUOT-ELEM ( ^u8 n -- n )
: SD-QUOT-FAIL ( ^u8 n n -- ) {: wa:^ wu:n code:n :}
   wa wu
   s" write the quotation type as `[ in -- out ]`, with at most 256 types on each side"
   code DECL-REJECT:REJECT throw ;

\ The pointee text of a field type that opens a pointer: the rest of a `^T`
\ span, empty for a bare `^`. A leading `ptr` reaches SD-POINTEE-AT, which
\ refuses it.
: SD-PTR-TEXT? ( ^u8 n -- ^u8 n bool ) {: a:^ u:n :}
   u 0 > IF a c@ 94 = IF a 1 + u 1 - YES EXIT THEN THEN
   a u NO ;

\ A field type's text, from its first token (SD-SPAN); a quotation `[` reads the
\ tokens after it, a leading `^` the rest of its span. The text resolved is the
\ reject token, so a refused pointee names itself, not the `^T` span that holds it.
: RESOLVE-TYPE ( ^u8 n -- n )
   dup 0= IF 2drop s" missing field type"
      s" write a type after the field name, as in `FIELD x n`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup DECL-REJECT:TOKEN!
   SD-SPAN
   2dup TYPE!
   2dup s" [" CORE-STR= IF
      2drop [: NEXT ;] [: SD-QUOT-ELEM ;] [: SD-QUOT-FAIL ;] TYPE-DECL:PARSE-QUOT EXIT THEN
   SD-PTR-TEXT? IF
      dup 0= IF
         2drop s" pointer without a pointee"
         SD-PTR-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
      POINTEE @ {: outer:n :}
      -1 POINTEE !
      RECURSE
      outer POINTEE !
      FIELD-MISS @ IF drop 0 EXIT THEN
      s" field type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEE SCHEMA-PTR EXIT THEN
   0 SD-ARG-N !
   2dup 0 PK-CELL SD-PARSE-NODE {: node:n j:n :}
   j over <> IF
      2drop s" malformed field type application"
      SD-APP-FIX$ TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   2drop node ;
: SD-QUOT-INSTALL ( -- ) [: RESOLVE-TYPE ;] is SD-QUOT-ELEM ;
SD-QUOT-INSTALL

\ Clause drivers emit through DECL-EVENT, which owns duplicate, ordinal and
\ selector state; each mutates only the fresh family record.
: HEADER-ORDER ( -- )
   SEEN-FIELD @ IF
      s" header clause after the first field"
      s" move the `POLICY`, `DERIVE` and `OPAQUE` clauses before the first `FIELD`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   NEXT POLICY-CODE {: code:n :}
   code TL-NICHE = IF
      TYPE-DECL:TDECL-NICHE-MESSAGE$
      s" drop `POLICY niche-null`: only an `ENUM` with an empty variant, then one `^` field, can use it"
      TYPE-DECL:E-TDECL-POLICY DECL-REJECT:REJECT throw THEN
   FAM @ code TFAM-LAYOUT!
   code DECL-EVENT:POLICY ;

\ OPAQUE keeps the family's visibility and moves its generated words to the
\ private-family placement (TFAM-GEN-PUBLIC? answers placement, TFAM-PUBLIC?
\ visibility).
: OPAQUE-CLAUSE ( -- )
   HEADER-ORDER
   FAM @ DECL-EVENT:OPAQUE
   FAM @ TFAM-OPAQUE! ;

: DERIVE-FEATURE? ( ^u8 n -- bool )
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" tag" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
\ Visibility is not a condition for a derived surface: it picks the spelling and
\ the wordlist (type-family.f TF-CTOR-PRIV$, rendered by sumtype.f TDGEN-DRV-REF).
: DERIVE-CONCRETE ( -- )                \ eq/hash compare whole values: arity 0 only
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family"
      s" drop `eq` and `hash` from `DERIVE`, or declare the structure with no type parameters"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN ;
\ `addr` has no arity condition: an accessor projects the field schema at the
\ caller's instantiation, and the checker refuses the one instantiation a baked
\ offset would misdescribe (a type argument wider than one cell).
: DERIVE-ONE ( ^u8 n -- )
   2dup s" eq" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-EQ! DRV-EQ DECL-EVENT:DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-HASH! DRV-HASH DECL-EVENT:DERIVE EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-ADDR! DRV-ADDR DECL-EVENT:DERIVE EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-INIT! DRV-INIT DECL-EVENT:DERIVE EXIT THEN
   2dup s" tag" CORE-STR=CI IF
      2drop s" derive tag requires an enum"
      s" drop `tag` from `DERIVE`: only an `ENUM` derives it"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported"
      s" drop `order` from `DERIVE` and write the comparison as a word over the fields"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature"
   s" name `eq`, `hash`, `addr` or `init` after `DERIVE`"
   TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )
   HEADER-ORDER
   NEXT dup 0= IF
      2drop s" missing derive feature"
      s" name a feature after `DERIVE`: `eq`, `hash`, `addr` or `init`"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   BEGIN NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

: EMIT-FIELD ( ^u8 n n -- )
   SCHEMA-ROOT+ {: sch:n :}
   FAM @ sch SCHEMA-ROOT@ TFAM-SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!
   s" duplicate field name"
   s" give each field of this declaration its own name" E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ -rot sch
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ A field named like a generated member (MAKE, UNMAKE, F:AT, F:BYTES, F:CELLS, a
\ derived tail) would make the generator define a name twice, a die inside the
\ plan. Refused only under DERIVE addr, which header clauses have fixed by now.
: MEMBER-CLASH? ( ^u8 n -- bool )
   2dup s" make" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" unmake" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup TFAM-ADDR-FIXED-TAIL? IF 2drop YES EXIT THEN
   TFAM-DERIVED-TAIL? ;
: REQUIRE-FIELD-NAME ( ^u8 n -- )    \ consumes the copy; throws on a member clash
   FAM @ TFAM-DERIVE-ADDR? 0= IF 2drop EXIT THEN
   MEMBER-CLASH? IF
      s" field name is a generated member name"
      s" rename the field: under `DERIVE addr` the names `make`, `unmake`, `at`, `bytes`, `cells`, `eq`, `hash` and `tag` are taken"
      TYPE-DECL:E-TDECL-NAME DECL-REJECT:REJECT throw THEN ;
: FIELD-NAME ( -- ^u8 n )                 \ a FIELD or BUFFER name, judged
   NEXT dup 0= IF
      2drop s" missing field name"
      s" write a name and a type after `FIELD`, as in `FIELD x n`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   {: na:^ nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ SD-LONG-FIX$ TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw THEN
   na nu REQUIRE-FIELD-NAME
   OUTCOME-NAMES @ 0 <> IF
      na nu NAME-CHECK THEN
   na nu ;
\ Each item is at least one level of MAKE's effect, so no record of more items
\ than EFFECT-DEPTH-MAX can be made; USIG-DEPTH-FITS? measures nested types.
: ITEMS-FULL ( -- )
   s" record takes more than 4096 items to make"
   s" split the record: MAKE takes one item per `FIELD` and one per buffer cell, at most 4096"
   TYPE-DECL:E-TDECL-CAP DECL-REJECT:REJECT throw ;
: ITEM-ROOM ( -- n ) EFFECT-DEPTH-MAX SD-ITEMS @ - ;
: FIELD-CLAUSE ( -- )
   FIELD-NAME {: na:^ nu:n :}
   0 FIELD-MISS !  0 POINTEE !
   NEXT {: ta:^ tu:n :}
   ta tu RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF
      ta tu FAM @ node REQUIRE-DERIVABLE
      ITEM-ROOM 1 < IF na nu DECL-REJECT:TOKEN! ITEMS-FULL THEN
      SD-ITEMS @ 1 + SD-ITEMS !
      na nu node EMIT-FIELD THEN
   -1 SEEN-FIELD ! ;

\ `BUFFER name N`: N bytes inline from the next slot, in whole cells. The count
\ is a number literal, never a name, so the layout is fixed by the text alone.
: BUFFER-COUNT ( -- n )
   NEXT dup 0= IF
      2drop s" missing buffer byte count"
      s" write a byte count after the buffer's name, as in `BUFFER bytes 16`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   num-parse {: v:n flt:bool ok:bool :}
   ok 0= flt or v 1 < or IF
      s" buffer byte count must be a number above 0"
      s" give the byte count as a number literal above 0, as in `BUFFER bytes 16`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   v ITEM-ROOM CELL * > IF ITEMS-FULL THEN     \ before its cells are counted
   v ;
: EMIT-BUFFER ( ^u8 n n -- ) {: na:^ nu:n bytes:n :}
   bytes 1 - CELL / 1 + {: fw:n :}
   CC-U8 SCHEMA-CON SCHEMA-ROOT+ {: sch:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate field name"
   s" give each field of this declaration its own name" E-TFAM-DUP DECL-REJECT:EXPECT
   FAM @ na nu sch
   SD-CELLS @  fw  SD-CELLS @ CELL *  bytes  CELL  PF-FLAG-BUFFER
   DECL-EVENT:FIELD
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ Derived eq and hash compare and hash whole cells, and a buffer's last cell
\ may hold bytes past its count, so neither may read one. DERIVE precedes the
\ fields (HEADER-ORDER): the family's derive flags are final here.
: BUFFER-DERIVABLE ( -- )
   FAM @ TFAM-DERIVE-EQ? IF
      s" derive eq cannot compare a buffer"
      s" drop `eq` from `DERIVE`, or hold the bytes in `FIELD`s"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-DERIVE-HASH? IF
      s" derive hash cannot hash a buffer"
      s" drop `hash` from `DERIVE`, or hold the bytes in `FIELD`s"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN ;
: BUFFER-CLAUSE ( -- )
   FIELD-NAME {: na:^ nu:n :}
   BUFFER-DERIVABLE
   BUFFER-COUNT {: bytes:n :}
   bytes 1 - CELL / 1 + SD-ITEMS @ + SD-ITEMS !
   na nu bytes EMIT-BUFFER
   -1 SEEN-FIELD ! ;

: SD-REGISTER ( ^u8 n n -- )
   {: na:^ nu:n ar:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate family"
   s" rename the structure: this package already declares a family with this name"
   E-TFAM-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-PRODUCT TFAM-DECL FAM !
   TYPE-FIELD:COUNT FLDBASE !
   0 NFLD !   0 SD-CELLS !   0 SD-ITEMS ! ;

: SD-MAKEABLE? ( -- bool )
   NFLD @ 0 > ;
\ Accessors are generated at commit because each is armed with a committed field
\ id (structure-make.f). A fieldless structure has nothing to address and no
\ MAKE / UNMAKE pair, whose constructor package the public accessor spelling uses.
: SD-ADDR-ARM ( -- )
   FAM @ TFAM-DERIVE-ADDR? FAM @ TFAM-DERIVE-INIT? or 0= IF EXIT THEN
   SD-MAKEABLE? 0= IF
      s" derived fields require a family with fields"
      s" add a `FIELD` to the structure, or drop `addr` and `init` from `DERIVE`"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-DERIVE-INIT? IF
      s" derive init requires a canonical fixed-cell record"
      s" drop `init` from `DERIVE`, or give every field a fixed cell width"
      TYPE-DECL:E-TDECL-DERIVE DECL-REJECT:EXPECT THEN
   FAM @ STRUCTURE-MAKE:ARM ;
: SD-CLOSE ( -- )
   DECL-REJECT:AT-FAMILY
   NFLD @ 0 ?do
      FLDBASE @ i + PF-PENDING-SCH@ FAM @ swap PF-SCHEMA-OK? 0= IF
         s" field type parameter has incompatible kinds"
         s" use each type parameter in one kind of place: a value type, a scope or a region"
         TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw THEN
   loop
   MISSING? IF E-UNRESOLVED throw THEN
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   NFLD @ 0 ?do
      FLDBASE @ i + PF-PENDING-SCH@ SCHEMA-ROOT@
      s" field type is a pointer to a linear value and cannot own it"
      DECL-EVENT:REQUIRE-POINTEES
   loop
   FAM @ SD-CELLS @ TFAM-SLOTS!
   FAM @ TFAM-OPAQUE? SD-MAKEABLE? 0= and IF
      s" opaque requires a family with fields"
      s" add a `FIELD` to the structure, or drop `OPAQUE`"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   \ Only a structure with fields gets MAKE / UNMAKE; a fieldless one is an opaque
   \ one-cell family, as a bare NEWTYPE declares.
   SD-MAKEABLE? IF FAM @ STRUCTURE-MAKE:GENERATE THEN
   SD-ADDR-ARM ;

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;STRUCTURE"
      s" end the declaration with `;STRUCTURE` after its last field"
      TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop -1 SEEN-END ! SD-CLOSE YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2dup s" buffer" CORE-STR=CI IF 2drop BUFFER-CLAUSE NO EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" opaque" CORE-STR=CI IF 2drop OPAQUE-CLAUSE NO EXIT THEN
   2drop s" unexpected token in structure declaration"
   s" start each clause with `FIELD`, `POLICY`, `DERIVE` or `OPAQUE`, and end with `;STRUCTURE`"
   TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw ;
: CLAUSES ( -- ) BEGIN CLAUSE UNTIL ;

: DRIVE ( -- )
   NEXT 2dup DECL-REJECT:FAMILY!
   dup 0= IF 2drop s" missing name" SD-NAME-FIX$ TYPE-DECL:E-TDECL-SYNTAX DECL-REJECT:REJECT throw THEN
   TYPE-DECL:DECL-HEADER 0= IF
      drop 2drop s" invalid type parameter header"
      s" name each type parameter once in the header, as in `STRUCTURE box<a>` or `STRUCTURE pair<a,b>`"
      TYPE-DECL:E-TDECL-ARITY DECL-REJECT:REJECT throw
   THEN
   {: ar:n :}
   2dup NAME-GATE
   ar
   SD-REGISTER
   CLAUSES ;

: SD-FRONT ( -- ^u8 n ^u8 n [ -- ] )
   SD-KIND$ s" ;structure" [: SD-RESET DRIVE ;] ;

public

: DECLARE-STRUCTURE ( -- ) SD-FRONT DECL-FRONT:RUN ;

\ Registers a STRUCTURE from a body a tool lexed, by DECLARE-STRUCTURE's grammar,
\ defining no word; a body without ;STRUCTURE is rejected "missing ;STRUCTURE".
: REPLAY-STRUCTURE ( ^u8 n ^u8 n -- ^u8 n bool )
   SD-FRONT DECL-FRONT:REPLAY-OUTCOME ;

;package

public

EXPORT STRUCTURE-DECL:DECLARE-STRUCTURE
EXPORT STRUCTURE-DECL:REPLAY-STRUCTURE

;package

\ STRUCTURE is a documented global (package-first exception, like the SUMTYPE
\ opener); it parses its body to ;STRUCTURE at interpret time, so its
\ checked effect is ( -- ).
: STRUCTURE ( -- ) TYPE-DECL:DECLARE-STRUCTURE ;

;using
;using
