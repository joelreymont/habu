\ structure-decl.f — the STRUCTURE typed-declaration front end (package
\ STRUCTURE-DECL). This is the FIRST consumer of the shared declaration-event
\ transaction (src/core/decl-event.f, package DECL-EVENT). It owns ONLY the
\ STRUCTURE grammar loop and its reject dispatch; it holds NO declaration state.
\ The token source, the parse cells and the guarded transaction run are
\ DECL-FRONT's (src/core/generated-declaration.f); this file hands it its kind,
\ terminator and body.
\ The declaration events (the header clauses and every field) are owned by the
\ event module, and the duplicate / reserved / case field-name gate is raised by
\ the field record through the field-event path unchanged (docs/forth.md "Structures And
\ Enums"; dot habu-structure-parse-typed-c5a01e1f).
\
\ Grammar:
\   STRUCTURE type-name[<params>] header-clause* field* ;STRUCTURE
\   header-clause = POLICY policy-name | DERIVE derive-name+ | OPAQUE
\   field         = FIELD field-name type-expr
\ A malformed, duplicate, reserved, unresolved, or mixed-legacy token rejects at
\ the exact offending token with the E-TDECL-* family (values mirror sumtype.f)
\ or the field record's own name-gate code, and the whole provisional
\ declaration rolls back to a byte-identical registry.
\
\ DECLARATION-TRANSACTION:RUN owns the declaration savepoint. The checker,
\ metadata, event, and constructor-protection participants keep their snapshots
\ live until the complete declaration body has validated and every reversible
\ commit succeeds.
\ A reject therefore retires the family, schema, layout, field, event, generated
\ dictionary, and staged protection state together.
\
\ Once SD-CLOSE has bound the family field range and width, it asks
\ STRUCTURE-MAKE:GENERATE to define the MAKE / UNMAKE constructor words from the
\ still-provisional field schemas. ONE condition gates the call:
\   - FIELDS only. A zero-field structure is an opaque one-cell family that
\     publishes no constructor (the authority-safe shape a bare
\     NEWTYPE also declares); only a declaration WITH fields is a
\     product with a MAKE/UNMAKE pair, so GENERATE is skipped when NFLD is zero.
\ VISIBILITY IS NOT A CONDITION. It decides the SPELLING and the WORDLIST, not
\ whether a structure has a construction surface: a public family publishes
\ FAMILY:MAKE / FAMILY:UNMAKE into its reserved constructor namespace, a private
\ one publishes FAMILY-MAKE / FAMILY-UNMAKE into the declaring package's private
\ wordlist, where a second package cannot resolve them; nearly every memory
\ record under src/lib/ is package-private. The OPAQUE header clause moves a
\ PUBLIC family's generated words to that private placement while the type stays
\ nameable everywhere (TFAM-GEN-PUBLIC? answers placement, TFAM-PUBLIC?
\ visibility): another package constructs only through what the declaring
\ package defines over the pair.
\
\ MEMORY RECORDS. `DERIVE addr` gives a family an address surface over this one
\ field registry; there is no second record registry. It generates one accessor
\ per field, F:f ( ptr F -- ptr T ) with body `<byte-offset> field-project`, plus
\ F:AT ( ptr F n -- ptr F ) over record-at and the F:BYTES / F:CELLS sizes. An
\ accessor yields an address, so @ ! c@ do the access. Spellings come from
\ TF-CTOR-PKG$ (public: package then tail, hyphens doubled) and TF-CTOR-PRIV$
\ (private: FAMILY-member, uppercased). A private generated word cannot wear a
\ colon: the engine routes a qualified definition to the namespace's PUBLIC
\ wordlist (habu2.f C-QUALIFY-DEF) and the checker records a colon name as a
\ public symbol (CHECKER-RECORD-SYM). The declaring package's private wordlist
\ is current at ;STRUCTURE, so the unqualified definition lands there.
\ A public family's constructor namespace is a protected wid (prot-wid-add). A
\ private family's wordlist cannot be one, since its package keeps defining into
\ it, so its guard is by name: TFAM-CTOR-WORD? recognises the TF-PRIV-MEMBER$
\ members while the package is open and the undefine guard refuses them with
\ E-CTOR-PROTECTED. Each accessor is minted through FIELD-PROJ!, a single-shot
\ window keyed on the accessor's name that disarms at its field-project token
\ even on a reject. The committed field id is the sole authority for family,
\ offset, extent, role and schema; the checker cross-checks the baked offset.
\ Outside the window field-project is exactly `+`, and record-at preserves the
\ family (both in structure-make.f).
\ Generation remains inside the shared transaction. If evaluation or checker
\ certification rejects either constructor, the runner restores every
\ participant before the error returns to the caller.
\
\ Loaded AFTER the checker hook, AFTER decl-event.f (it drives DECL-EVENT:*), and
\ AFTER structure-make.f (its ;STRUCTURE calls STRUCTURE-MAKE:GENERATE, so the
\ generator must be defined first); the STRUCTURE opener is the only executable
\ STRUCTURE declaration surface.

using SCHEMA-REG
using TFAM
using DECL-FRONT

package STRUCTURE-DECL

\ ---------------------------------------------------------------------------
\ transient parse state (parse-loop bookkeeping, not declaration state). One
\ declaration at a time: STRUCTURE is top-level interpret-only, never nested.
\ ---------------------------------------------------------------------------
variable FAM        \ family id being declared
variable TOK        \ declaration-event token; SD-RESET zeroes it at body start
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ field count in this declaration
variable SD-CELLS      \ running field cell width (next field's slot / byte offset)
variable SEEN-FIELD \ a FIELD has appeared (header clauses must precede fields)

$100 constant SD-ARG-CAP
create SD-ARGS SD-ARG-CAP cells allot
variable SD-ARG-N

: SD-RESET ( -- )                      \ grammar scalars; re-seeded at load (process-local)
   0 TOK !   0 SEEN-FIELD ! ;
SD-RESET
: SD-MISSING! ( -- ) TYPE@ MISSING! ;

\ ---------------------------------------------------------------------------
\ name gate: a reserved family name is one TYPE-NAME:FAMILY-RESERVED? lists, the
\ list every family definer asks, or a STRUCTURE opener. Case + duplicate are
\ enforced by TFAM-DECL itself. A copy of the list kept here drifted from the
\ legacy definers twice: it admitted the control words (`if`), then value
\ record names (`STRUCTURE vr` after `VALUE-RECORD vr`).
\ ---------------------------------------------------------------------------
: NAME-RESERVED? ( ptr u8 n -- bool )
   2dup s" structure" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop YES EXIT THEN
   TYPE-NAME:FAMILY-RESERVED? ;

: REQUIRE-NAME ( ptr u8 n -- )      \ validate the family name (throws; consumes the copy)
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail" E-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name" E-NAME DECL-REJECT:REJECT throw THEN ;

\ ---------------------------------------------------------------------------
\ field type resolution -> a schema node. A type application records its child
\ roots contiguously after nested applications have finished building theirs.
\ Quotation effects are resolved through TYPE-DECL:PARSE-QUOT below.
\
\ A family that owns a linear value — directly or through its own fields — IS an
\ accepted field type (dot habu-checker-enum-payload-9e1ae6cc). The structure then
\ owns that obligation by containment, which is the same rule a field naming a
\ bare DEFLINEAR con already relies on: TFAM-CONCRETE-LINEAR? walks the product's
\ field schemas, follows an application node into the family it names, and reports
\ the structure linear, so the checker counts the whole bundle as one linear unit.
\ Refusing the family spelling while accepting the con spelling blocked the name
\ and never the obligation, so it bought no soundness.
\
\ A parametric family named bare is the one resolved-but-unusable case source can
\ reach, and it now says so. Calling a registered family "unknown field type" sent
\ readers hunting for a missing declaration that was in fact right there. The
\ remaining kind test still falls through to the unknown message because the only
\ kind it excludes, TK-EVIDENCE, has no declarer any source can write.
\ ---------------------------------------------------------------------------
: FIELD-FAM? ( ptr u8 n -- n bool )
   TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE 0= IF drop 0 NO EXIT THEN
   {: id:n :}
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM @ = IF
      s" field type cannot recursively name its owner"
      E-PAYLOAD DECL-REJECT:REJECT throw THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n n -- n )       \ single-char type: param / n / f / r
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

\ A pointer is a NON-OWNING boundary: TFCL-NODE? stops at a pointer node, so a
\ field spelled `ptr <linear>` reads non-linear and the containing family would
\ copy and drop freely while a linear resource sits behind the address. The
\ family spelling and the con spelling launder identically, so both are refused
\ here, at the declaration door, with one rule.
: REQUIRE-POINTEE ( n -- n )                \ pointee node, or reject a linear owner behind the address
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

\ Argument K of FAM's application at cursor I and every one after it, each
\ pushed as an argument row; answers the argument count and the cursor past `>`.
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

\ The loose arguments at cursor I and every one after it; answers the cursor
\ past `>`.
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

: RESOLVE-TYPE ( ptr u8 n -- n )        \ type token(s) -> schema node
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

\ ---------------------------------------------------------------------------
\ clause drivers. Each emits its event through DECL-EVENT (which owns duplicate /
\ ordinal / selector state) and mutates only the fresh family record.
\ ---------------------------------------------------------------------------
: HEADER-ORDER ( -- )                   \ header clauses precede the first field
   SEEN-FIELD @ IF
      s" header clause after the first field" E-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   NEXT POLICY-CODE {: code:n :}
   FAM @ code TFAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

\ OPAQUE: the family keeps its declared visibility and its generated words take
\ the private-family placement (header above). The event validates
\ (package, at most once), then the fresh family record is marked.
: OPAQUE-CLAUSE ( -- )
   HEADER-ORDER
   TOK @ FAM @ DECL-EVENT:OPAQUE TOK !
   FAM @ TFAM-OPAQUE! ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )  \ a known/recognised derive feature token
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
\ VISIBILITY IS NOT A CONDITION, for the same reason it is not one for the
\ MAKE/UNMAKE pair (see the generation note at the head of this file): it decides
\ the spelling and the wordlist, not whether a family has a derived surface. A
\ public family's derived words go into its reserved constructor namespace, a
\ private family's into the declaring package's private wordlist as
\ FAMILY-MEMBER (src/core/type-family.f TF-CTOR-PRIV$, rendered by
\ src/core/sumtype.f TDGEN-DRV-REF). The gate that stood here refused a private
\ family outright; nearly every memory record under src/lib/ is package-private, so
\ it refused the facility to almost everything that needs it.
: DERIVE-CONCRETE ( -- )                \ eq/hash compare whole values: arity 0 only
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family" E-DERIVE DECL-REJECT:REJECT throw THEN ;
: EMIT-DERIVE ( n n -- )                \ ( fam feature-code -- ) emit the DERIVE event
   TOK @ -rot DECL-EVENT:DERIVE TOK ! ;
\ `addr` carries no arity condition: an accessor projects the field schema at the
\ caller's instantiation, which is the whole point of the projection window, and
\ the checker refuses the one instantiation a baked offset would misdescribe (a
\ type argument wider than one cell).
: DERIVE-ONE ( ptr u8 n -- )            \ apply one feature + emit its event
   2dup s" eq" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-EQ! FAM @ DRV-EQ EMIT-DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop DERIVE-CONCRETE FAM @ TFAM-DERIVE-HASH! FAM @ DRV-HASH EMIT-DERIVE EXIT THEN
   2dup s" addr" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-ADDR! FAM @ DRV-ADDR EMIT-DERIVE EXIT THEN
   2dup s" init" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-INIT! FAM @ DRV-INIT EMIT-DERIVE EXIT THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported" E-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature" E-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )                  \ DERIVE feature+ : first mandatory, rest by lookahead
   HEADER-ORDER
   NEXT dup 0= IF
      2drop s" missing derive feature" E-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   BEGIN NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

: EMIT-FIELD ( ptr u8 n n -- )          \ ( na nu node -- ) layout + drive the field event
   SCHEMA-ROOT+ {: sch:n :}                \ ( na nu )
   FAM @ sch SCHEMA-ROOT@ TFAM-SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!              \ the field name owns the field record's rejects
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ 2swap sch                \ ( tok fam na nu sch )
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
\ A generated member and a field share one namespace: with DERIVE addr the family
\ publishes F:AT, F:BYTES and F:CELLS beside one word per field, and it already
\ publishes F:MAKE / F:UNMAKE and any derived tail. A field named like one of
\ those would make the generator render a name it had already defined, which is a
\ process-killing die inside the plan. The declaration door is where it belongs,
\ at the offending token, and only when the addr surface is actually asked for —
\ header clauses precede fields, so the flag is final by the time a field is read.
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
      2drop s" missing field name" E-SYNTAX DECL-REJECT:REJECT throw THEN   \ field name
   {: na:ptr nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   na nu REQUIRE-FIELD-NAME
   OUTCOME-NAMES @ 0 <> IF
      na nu s" structure field names exceed declaration window" NAME-CHECK THEN
   0 FIELD-MISS !
   NEXT RESOLVE-TYPE {: node:n :}
   FIELD-MISS @ 0= IF na nu node EMIT-FIELD THEN
   -1 SEEN-FIELD ! ;

\ ---------------------------------------------------------------------------
\ transaction orchestration.
\ ---------------------------------------------------------------------------
: SD-REGISTER ( ptr u8 n n -- )            \ ( na nu arity -- ) register the family, open the tx
   {: na:ptr nu:n ar:n :}
   na nu DECL-REJECT:TOKEN!             \ the family name owns the registry's rejects
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-PRODUCT TFAM-DECL FAM !
   TYPE-FIELD:COUNT FLDBASE !
   0 NFLD !   0 SD-CELLS !
   DECL-EVENT:CURRENT TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK !
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;

: SD-MAKEABLE? ( -- bool )                 \ a structure WITH fields owns a MAKE/UNMAKE pair
   NFLD @ 0 > ;
\ `DERIVE addr` names the family whose address surface this transaction owns; the
\ words themselves are generated one phase later, at commit, because an accessor
\ is armed with a COMMITTED field id (src/core/structure-make.f). A
\ fieldless structure is an opaque one-cell family with nothing to address, and
\ the fields are what give it the MAKE/UNMAKE pair whose constructor package an
\ accessor's public spelling is built from.
: SD-ADDR-ARM ( -- )
   FAM @ TFAM-DERIVE-ADDR? FAM @ TFAM-DERIVE-INIT? or 0= IF EXIT THEN
   SD-MAKEABLE? 0= IF
      s" derived fields require a family with fields" E-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-DERIVE-INIT? IF
      s" derive init requires a canonical fixed-cell record"
      E-DERIVE DECL-REJECT:EXPECT THEN
   FAM @ STRUCTURE-MAKE:ARM ;
: SD-CLOSE ( -- )                          \ bind field range + width, then generate the ctors
   DECL-REJECT:AT-FAMILY                   \ close-stage faults belong to the whole declaration
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
   \ The constructor generator's rejects use the packet's code table. The
   \ initialized accessor's fixed-layout gate arms its own reason in SD-ADDR-ARM.
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
   DECL-REJECT:REJECT throw ;           \ unexpected / mixed-legacy token at the exact token
: CLAUSES ( -- ) BEGIN CLAUSE UNTIL ;

: DRIVE ( -- )                          \ family header + register + body
   NEXT 2dup DECL-REJECT:FAMILY!     \ ( na nu )  keep the span
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

: SD-FRONT ( -- ptr u8 n ptr u8 n [ -- ] )   \ kind, terminator and body for DECL-FRONT
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

\ SD-REPLAY ( name body -- ) : register a STRUCTURE from tokens a tool has
\ already lexed, defining no word. Same grammar, same validation, same registry
\ writes, same reject packet as SD-RUN — only the token source differs and the
\ MAKE/UNMAKE pair is registered without being rendered (structure-make.f
\ GENERATE). This is what lets tools/check-core.f and src/habu/verify-source.f
\ see a STRUCTURE family at all, so a later signature in the same source can
\ name it. The body buffer is the declaration body INCLUDING its ;STRUCTURE
\ terminator; a buffer without one rejects through the front end's own
\ "missing ;STRUCTURE" gate exactly as a truncated live declaration does.
\ SD-REPLAY-OUTCOME answers the first type the body named that may still
\ arrive instead of raising.
: SD-REPLAY-OUTCOME ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   SD-FRONT DECL-FRONT:REPLAY-OUTCOME ;
: SD-REPLAY ( ptr u8 n ptr u8 n -- )
   SD-FRONT DECL-FRONT:REPLAY ;

;package

\ STRUCTURE is the one executable composite-declaration keyword and therefore a
\ documented global language surface (package-first exception, like the shipped
\ SUMTYPE/PRODUCT openers): it parses its own body up to ;STRUCTURE at interpret
\ time, so its checked effect is ( -- ).
\ STRUCTURE type-name[<params>] [POLICY p] [DERIVE f+] [OPAQUE] (FIELD name type)* ;STRUCTURE
: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;

;using
;using
;using
