\ structure-decl.f: the STRUCTURE front end (package STRUCTURE-DECL), grammar loop
\ and reject dispatch only. DECL-FRONT (generated-declaration.f) owns the token
\ source and the guarded run; DECL-EVENT (decl-event.f) owns every event.
\   STRUCTURE type-name[<params>] (POLICY p | DERIVE f+ | OPAQUE)* (FIELD name type)* ;STRUCTURE
\ A reject rolls the whole declaration back to a byte-identical registry. Loads
\ after decl-event.f and structure-make.f, whose GENERATE and ARM ;STRUCTURE calls.

using SCHEMA-REG
using TFAM
using DECL-FRONT

package STRUCTURE-DECL

\ Transient parse state, not declaration state: STRUCTURE is top-level
\ interpret-only, so one declaration runs at a time.
variable FAM        \ family id being declared
variable TOK        \ declaration-event token; SD-RESET zeroes it at body start
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ field count in this declaration
variable SD-CELLS      \ running field cell width (next field's slot / byte offset)
variable SEEN-FIELD \ a FIELD has appeared (header clauses must precede fields)

$100 constant SD-ARG-CAP
create SD-ARGS SD-ARG-CAP cells allot
variable SD-ARG-N

: SD-RESET ( -- )
   0 TOK !   0 SEEN-FIELD ! ;
SD-RESET
: SD-MISSING! ( -- ) TYPE@ MISSING! ;

\ The reserved list is TYPE-NAME:FAMILY-RESERVED?, the one every family definer
\ asks: a copy kept here drifted twice (it admitted `if`, then value-record names).
\ Case and duplicate are TFAM-DECL's.
: NAME-RESERVED? ( ptr u8 n -- bool )
   2dup s" structure" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop YES EXIT THEN
   TYPE-NAME:FAMILY-RESERVED? ;

: REQUIRE-NAME ( ptr u8 n -- )
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail" E-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name" E-NAME DECL-REJECT:REJECT throw THEN ;

\ A family that owns a linear value is an accepted field type: the structure owns
\ the obligation by containment (TFAM-CONCRETE-LINEAR? walks into it), the rule a
\ field naming a DEFLINEAR con already relies on.
\ The kind test falls through to "unknown field type" because the one kind it
\ excludes, TK-EVIDENCE, has no declarer source can write.
: FIELD-FAM? ( ptr u8 n -- n bool )
   TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE 0= IF drop 0 NO EXIT THEN
   {: id:n :}
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM @ = IF
      s" field type cannot recursively name its owner"
      E-PAYLOAD DECL-REJECT:REJECT throw THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n n -- n )
   {: want:n :}
   drop c@
   dup ASCII-N = IF drop CON-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CON-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CON-R SCHEMA-CON EXIT THEN
   TYPE-DECL:DECL-PARAM 0= IF
      drop s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   dup {: idx:n :}
   want PK-SCOPE = want PK-REGION = or
   want PK-TYPE = or IF
      FAM @ idx want TFAM-PK! THEN
   SCHEMA-PARAM ;

\ A pointer is a non-owning boundary (TFCL-NODE? stops at it), so `ptr <linear>`
\ would let the containing family copy and drop a linear resource: refused here,
\ for the family and the con spelling alike.
: REQUIRE-POINTEE ( n -- n )
   dup TFCL-NODE? IF
      s" field type is a pointer to a linear value and cannot own it"
      E-PAYLOAD DECL-REJECT:REJECT throw THEN ;

: SD-ARG+ ( n -- )
   SD-ARG-N @ SD-ARG-CAP >= IF
      s" field type application is too deep" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   SD-ARG-N @ cells SD-ARGS + !
   SD-ARG-N @ 1 + SD-ARG-N ! ;

: SD-DELIM? ( n -- bool )
   dup 60 = over 44 = or swap 62 = or ;

\ The field type parser threads its byte cursor: each word takes the index it
\ starts at and answers the index after what it consumed.
: SD-ATOM ( ptr u8 n n -- ptr u8 n n ) {: a:ptr u:n start:n :}
   start
   BEGIN
      dup u < IF a over + c@ SD-DELIM? 0= ELSE NO THEN
   WHILE
      1 +
   REPEAT {: j:n :}
   j start = IF
      s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a start + j start - j ;

defer SD-PARSE-NODE ( ptr u8 n n n -- n n )

: SD-APP-ARGS ( ptr u8 n n n n -- n n ) {: a:ptr u:n fam:n k:n i:n :}
   k fam TFAM-ARITY@ >= IF
      s" field type has too many arguments" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a u i  fam k TFAM-PK@  SD-PARSE-NODE {: node:n j:n :}
   node SD-ARG+
   j u >= IF
      s" unclosed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a j + c@ {: c:n :}
   c 44 = IF a u fam k 1 + j 1 + RECURSE EXIT THEN
   c 62 = IF k 1 + j 1 + EXIT THEN
   s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SD-APP ( ptr u8 n n n -- n n ) {: a:ptr u:n fam:n i:n :}
   SD-ARG-N @ {: base:n :}
   a u fam 0 i SD-APP-ARGS {: k:n j:n :}
   k fam TFAM-ARITY@ <> IF
      s" field type has wrong arity" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   FIELD-MISS @ IF base SD-ARG-N ! 0 j EXIT THEN
   SCHEMA-ROOT-N@ {: start:n :}
   base BEGIN dup SD-ARG-N @ < WHILE
      dup cells SD-ARGS + @ SCHEMA-ROOT+ drop 1 +
   REPEAT drop
   base SD-ARG-N !
   fam start k SCHEMA-APP j ;

defer SD-LOOSE-NODE ( ptr u8 n n -- n )

: SD-LOOSE-ARGS ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   a u i SD-LOOSE-NODE {: j:n :}
   j u >= IF
      s" unclosed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a j + c@ {: c:n :}
   c 62 = IF j 1 + EXIT THEN
   c 44 <> IF
      s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a u j 1 + RECURSE ;

: SD-LOOSE-APP ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   i u >= IF
      s" unclosed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a i + c@ 62 = IF i 1 + EXIT THEN
   a u i SD-LOOSE-ARGS ;

: SD-LOOSE-ATOM ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1 = IF
      a c@ dup ASCII-N = over ASCII-F = or over ASCII-R = or
      IF drop EXIT THEN
      TFAM-DECL-CHAR>PARAM IF
         drop a c@ TYPE-DECL:DECL-PARAM IF drop EXIT THEN
         drop
         s" type parameter is outside the declared arity" E-PAYLOAD DECL-REJECT:REJECT throw
      ELSE drop THEN
   THEN
   a u CON-OF dup 0 <> IF drop EXIT THEN drop
   a u FIELD-FAM? IF
      dup TFAM-ARITY@ 0 <> IF
         s" field type is parametric and needs type arguments" E-PAYLOAD DECL-REJECT:REJECT throw
      THEN drop EXIT
   THEN drop
   a u TYPE-MAY-ARRIVE-XT IF SD-MISSING! EXIT THEN
   s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SD-LOOSE-NODE-IMPL ( ptr u8 n n -- n ) {: a:ptr u:n i:n :}
   a u i SD-ATOM {: na:ptr nu:n j:n :}
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? IF
         {: fam:n :}
         a u fam j 1 + SD-APP nip EXIT
      THEN drop
      na nu TYPE-MAY-ARRIVE-XT 0= IF
         s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      SD-MISSING!
      a u j 1 + SD-LOOSE-APP EXIT
   THEN
   na nu SD-LOOSE-ATOM j ;
: SD-LOOSE-INSTALL ( -- ) [: SD-LOOSE-NODE-IMPL ;] is SD-LOOSE-NODE ;
SD-LOOSE-INSTALL

: SD-PARSE-NODE-IMPL ( ptr u8 n n n -- n n ) {: a:ptr u:n i:n want:n :}
   a u i SD-ATOM {: na:ptr nu:n j:n :}
   j u < IF a j + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? 0= IF
         drop
         na nu TYPE-MAY-ARRIVE-XT 0= IF
            s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
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
         s" field type is parametric and needs type arguments" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      fam 0 0 SCHEMA-APP j EXIT
   THEN drop
   na nu TYPE-MAY-ARRIVE-XT IF SD-MISSING! 0 j EXIT THEN
   s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SD-PARSER-INSTALL ( -- ) [: SD-PARSE-NODE-IMPL ;] is SD-PARSE-NODE ;
SD-PARSER-INSTALL

defer SD-QUOT-ELEM ( ptr u8 n -- n )
: SD-QUOT-FAIL ( ptr u8 n n -- ) DECL-REJECT:REJECT throw ;

: RESOLVE-TYPE ( ptr u8 n -- n )
   dup 0= IF 2drop s" missing field type" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup TYPE!
   2dup s" [" CORE-STR= IF
      2drop [: NEXT ;] [: SD-QUOT-ELEM ;] [: SD-QUOT-FAIL ;] TYPE-DECL:PARSE-QUOT EXIT THEN
   2dup s" ptr" CORE-STR=CI IF
      2drop NEXT RECURSE
      FIELD-MISS @ IF drop 0 EXIT THEN
      REQUIRE-POINTEE SCHEMA-PTR EXIT THEN
   0 SD-ARG-N !
   2dup 0 PK-CELL SD-PARSE-NODE {: node:n j:n :}
   j over <> IF
      2drop s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   2drop node ;
: SD-QUOT-INSTALL ( -- ) [: RESOLVE-TYPE ;] is SD-QUOT-ELEM ;
SD-QUOT-INSTALL

\ Clause drivers emit through DECL-EVENT, which owns duplicate, ordinal and
\ selector state; each mutates only the fresh family record.
: HEADER-ORDER ( -- )
   SEEN-FIELD @ IF
      s" header clause after the first field" E-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   NEXT POLICY-CODE {: code:n :}
   FAM @ code TFAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

\ OPAQUE keeps the family's visibility and moves its generated words to the
\ private-family placement (TFAM-GEN-PUBLIC? answers placement, TFAM-PUBLIC?
\ visibility).
: OPAQUE-CLAUSE ( -- )
   HEADER-ORDER
   TOK @ FAM @ DECL-EVENT:OPAQUE TOK !
   FAM @ TFAM-OPAQUE! ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
\ Visibility is not a condition for a derived surface: it picks the spelling and
\ the wordlist (type-family.f TF-CTOR-PRIV$, rendered by sumtype.f TDGEN-DRV-REF).
: DERIVE-CONCRETE ( -- )                \ eq/hash compare whole values: arity 0 only
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family" E-DERIVE DECL-REJECT:REJECT throw THEN ;
: EMIT-DERIVE ( n n -- )
   TOK @ -rot DECL-EVENT:DERIVE TOK ! ;
\ `addr` has no arity condition: an accessor projects the field schema at the
\ caller's instantiation, and the checker refuses the one instantiation a baked
\ offset would misdescribe (a type argument wider than one cell).
: DERIVE-ONE ( ptr u8 n -- )
   2dup s" eq" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-EQ! FAM @ DRV-EQ EMIT-DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-HASH! FAM @ DRV-HASH EMIT-DERIVE EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-ADDR! FAM @ DRV-ADDR EMIT-DERIVE EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-INIT! FAM @ DRV-INIT EMIT-DERIVE EXIT THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported" E-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature" E-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )
   HEADER-ORDER
   NEXT dup 0= IF
      2drop s" missing derive feature" E-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   BEGIN NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

: EMIT-FIELD ( ptr u8 n n -- )
   SCHEMA-ROOT+ {: sch:n :}
   FAM @ sch SCHEMA-ROOT@ TFAM-SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ 2swap sch
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ A field named like a generated member (MAKE, UNMAKE, F:AT, F:BYTES, F:CELLS, a
\ derived tail) would make the generator define a name twice, a die inside the
\ plan. Refused only under DERIVE addr, which header clauses have fixed by now.
: MEMBER-CLASH? ( ptr u8 n -- bool )
   2dup s" make" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" unmake" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup TFAM-ADDR-FIXED-TAIL? IF 2drop YES EXIT THEN
   TFAM-DERIVED-TAIL? ;
: REQUIRE-FIELD-NAME ( ptr u8 n -- )    \ consumes the copy; throws on a member clash
   FAM @ TFAM-DERIVE-ADDR? 0= IF 2drop EXIT THEN
   MEMBER-CLASH? IF
      s" field name is a generated member name" E-NAME DECL-REJECT:REJECT throw THEN ;
: FIELD-CLAUSE ( -- )
   NEXT dup 0= IF
      2drop s" missing field name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   {: na:ptr nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   na nu REQUIRE-FIELD-NAME
   OUTCOME-NAMES @ 0 <> IF
      na nu s" structure field names exceed declaration window" NAME-CHECK THEN
   0 FIELD-MISS !
   NEXT RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF na nu node EMIT-FIELD THEN
   -1 SEEN-FIELD ! ;

: SD-REGISTER ( ptr u8 n n -- )
   {: na:ptr nu:n ar:n :}
   na nu DECL-REJECT:TOKEN!
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-PRODUCT TFAM-DECL FAM !
   TYPE-FIELD:COUNT FLDBASE !
   0 NFLD !   0 SD-CELLS !
   DECL-EVENT:CURRENT TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK !
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;

: SD-MAKEABLE? ( -- bool )
   NFLD @ 0 > ;
\ Accessors are generated at commit because each is armed with a committed field
\ id (structure-make.f). A fieldless structure has nothing to address and no
\ MAKE / UNMAKE pair, whose constructor package the public accessor spelling uses.
: SD-ADDR-ARM ( -- )
   FAM @ TFAM-DERIVE-ADDR? FAM @ TFAM-DERIVE-INIT? or 0= IF EXIT THEN
   SD-MAKEABLE? 0= IF
      s" derived fields require a family with fields" E-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-DERIVE-INIT? IF
      s" derive init requires a canonical fixed-cell record"
      E-DERIVE DECL-REJECT:EXPECT THEN
   FAM @ STRUCTURE-MAKE:ARM ;
: SD-CLOSE ( -- )
   DECL-REJECT:AT-FAMILY
   NFLD @ 0 ?do
      TOK @ FAM @ FLDBASE @ i + DECL-EVENT:FIELD-SCHEMA@ FAM @ swap PF-SCHEMA-OK? 0= IF
         s" field type parameter has incompatible kinds"
         E-PAYLOAD DECL-REJECT:REJECT throw THEN
   loop
   MISSING? IF E-UNRESOLVED throw THEN
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   FAM @ SD-CELLS @ TFAM-SLOTS!
   FAM @ TFAM-OPAQUE? SD-MAKEABLE? 0= and IF
      s" opaque requires a family with fields" E-SYNTAX DECL-REJECT:REJECT throw THEN
   \ Only a structure with fields gets MAKE / UNMAKE; a fieldless one is an opaque
   \ one-cell family, as a bare NEWTYPE declares. A private family's generated
   \ words carry no colon (habu2.f C-QUALIFY-DEF routes a qualified definition to
   \ the public wordlist), and their package keeps defining into its wordlist, so
   \ they are guarded by name (TFAM-CTOR-WORD?); a public family's constructor
   \ namespace is a protected wid.
   SD-MAKEABLE? IF TOK @ FAM @ STRUCTURE-MAKE:GENERATE THEN
   SD-ADDR-ARM ;

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;STRUCTURE" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop -1 SEEN-END ! SD-CLOSE YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" opaque" CORE-STR=CI IF 2drop OPAQUE-CLAUSE NO EXIT THEN
   2drop s" unexpected token in structure declaration" E-SYNTAX
   DECL-REJECT:REJECT throw ;
: CLAUSES ( -- ) BEGIN CLAUSE UNTIL ;

: DRIVE ( -- )
   NEXT 2dup DECL-REJECT:FAMILY!
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   TYPE-DECL:DECL-HEADER 0= IF
      drop 2drop s" invalid type parameter header" E-ARITY DECL-REJECT:REJECT throw
   THEN
   {: ar:n :}
   2dup DECL-REJECT:FAMILY!
   2dup REQUIRE-NAME
   ar
   SD-REGISTER
   CLAUSES ;

: SD-FRONT ( -- ptr u8 n ptr u8 n [ -- ] )
   s" structure" s" ;structure" [: SD-RESET DRIVE ;] ;

public

\ The last declaration's parse state back to zero at a file boundary or image
\ capture (generated-declaration-protection.f SNAPSHOT-RESET), so a cached load
\ and a fresh one capture the same bytes.
: SNAPSHOT-RESET ( -- )
   SD-RESET
   0 FAM !  0 FLDBASE !  0 NFLD !  0 SD-CELLS !
   SD-ARG-CAP 0 ?do 0 i cells SD-ARGS + ! loop  0 SD-ARG-N ! ;

: SD-RUN ( -- ) SD-FRONT DECL-FRONT:RUN ;

\ SD-REPLAY registers a STRUCTURE from tokens a tool already lexed
\ (tools/check-core.f, verify-source.f) with SD-RUN's grammar, validation and
\ reject packet, defining no word: MAKE / UNMAKE are registered, not rendered.
\ The body includes its ;STRUCTURE terminator. SD-REPLAY-OUTCOME answers the first
\ type the body named that may still arrive instead of raising.
: SD-REPLAY-OUTCOME ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   SD-FRONT DECL-FRONT:REPLAY-OUTCOME ;
: SD-REPLAY ( ptr u8 n ptr u8 n -- )
   SD-FRONT DECL-FRONT:REPLAY ;

;package

\ STRUCTURE is a documented global (package-first exception, like the SUMTYPE and
\ PRODUCT openers); it parses its body to ;STRUCTURE at interpret time, so its
\ checked effect is ( -- ).
: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;

;using
;using
;using
