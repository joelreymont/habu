\ structure-decl.f — the STRUCTURE typed-declaration front end (package
\ STRUCTURE-DECL). This is the FIRST consumer of the shared declaration-event
\ transaction (src/core/decl-event.f, package DECL-EVENT). It owns ONLY the
\ STRUCTURE grammar loop and its reject dispatch; it holds NO declaration state.
\ The declaration events (the header clauses and every field) are owned by the
\ event module, and the duplicate / reserved / case field-name gate is raised by
\ the field record through the field-event path unchanged (docs/type-families.md
\ §2.2-2.5; dot habu-structure-parse-typed-c5a01e1f).
\
\ Grammar (docs §2.1):
\   STRUCTURE type-name arity header-clause* field* ;STRUCTURE
\   header-clause = POLICY policy-name | DERIVE derive-name+ | OPAQUE
\   field         = FIELD field-name type-expr
\ A malformed, duplicate, reserved, unresolved, or mixed-legacy token rejects at
\ the exact offending token with the E-TDECL-* family (values mirror sumtype.f)
\ or the field record's own name-gate code, and the whole provisional
\ declaration rolls back to a byte-identical registry.
\
\ GENERATED-DECL owns the declaration savepoint. The checker, metadata, event,
\ and constructor-protection participants keep their snapshots live until the
\ complete declaration body has validated and every reversible commit succeeds.
\ A reject therefore retires the family, schema, layout, field, event, generated
\ dictionary, and staged protection state together.
\
\ Once SD-CLOSE has bound the family field range and width, it asks
\ STRUCTURE-MAKE:GENERATE to define the MAKE / UNMAKE constructor words from the
\ still-provisional field schemas. ONE condition gates the call:
\   - FIELDS only. A zero-field structure is an opaque one-cell family that
\     publishes no constructor (docs/type-families.md §2.2 — the authority-safe
\     shape a bare NEWTYPE also declares); only a declaration WITH fields is a
\     product with a MAKE/UNMAKE pair, so GENERATE is skipped when NFLD is zero.
\ VISIBILITY IS NOT A CONDITION. It decides the SPELLING and the WORDLIST, not
\ whether a structure has a construction surface: a public family publishes
\ FAMILY:MAKE / FAMILY:UNMAKE into its reserved constructor namespace, a private
\ one publishes FAMILY-MAKE / FAMILY-UNMAKE into the declaring package's private
\ wordlist, where a second package cannot resolve them. The gate that used to
\ stand here refused a private family outright and called package-scoped private
\ generation deferred type-DSL work; it is that work (dot
\ habu-generate-a-private-80272413, docs/type-system.md §10.4), and without it
\ the declared memory record serves almost nothing, because nearly every memory
\ record under lib/ is package-private.
\ The OPAQUE header clause moves a PUBLIC family's generated words to that
\ private placement while the type stays nameable everywhere (docs/type-system.md
\ §10.4): the only construction surface another package gets is what the
\ declaring package defines over the pair.
\ Generation remains inside the shared transaction. If evaluation or checker
\ certification rejects either constructor, the coordinator restores every
\ participant before the error returns to the caller.
\
\ Loaded AFTER the checker hook, AFTER decl-event.f (it drives DECL-EVENT:*), and
\ AFTER structure-make.f (its ;STRUCTURE calls STRUCTURE-MAKE:GENERATE, so the
\ generator must be defined first); the STRUCTURE opener is the only executable
\ STRUCTURE declaration surface.

using SCHEMA-REG
using TFAM

package STRUCTURE-DECL

\ --- named reject codes: local names for the shared declaration codes, read at
\ load time from their owners, sumtype.f TYPE-DECL:E-TDECL-* and type-family.f
\ E-TFAM-*, as enum-decl.f reads them. The field record's E-PF-NAME /
\ E-TFAM-DUP / E-TFAM-CASE name-gate codes are raised by TFAM-DECL / the
\ field-event path and pass through unchanged.
TYPE-DECL:E-TDECL-SYNTAX constant E-SYNTAX    \ malformed: missing name/arity/terminator, unexpected/legacy token
TYPE-DECL:E-TDECL-ARITY constant E-ARITY      \ arity token is not a small decimal in [0, cap]
TYPE-DECL:E-TDECL-PAYLOAD constant E-PAYLOAD  \ unresolved / unknown field type
TYPE-DECL:E-TDECL-NAME constant E-NAME        \ reserved or colliding family name
TYPE-DECL:E-TDECL-POLICY constant E-POLICY    \ unknown or not-yet-supported layout policy
TYPE-DECL:E-TDECL-DERIVE constant E-DERIVE    \ unknown or not-yet-supported derive feature
TYPE-DECL:E-TDECL-CAP constant E-CAP          \ a family or field name longer than NAME-MAX
E-TFAM-CASE constant E-CASE                   \ family name is not a lowercase canonical tail
E-TFAM-DUP constant E-DUP                     \ duplicate family or field tail, raised by TFAM-DECL /
                                              \ the field-event path; named here only so a
                                              \ reason can be armed for it before those calls
7192 constant E-UNRESOLVED                    \ private replay outcome after rollback

110 constant ASCII-N
102 constant ASCII-F
114 constant ASCII-R

\ The longest family or field name, which every spelling derived from it has
\ room for (src/core/type-family.f TF-NAME-MAX, read here at top level).
TF-NAME-MAX constant NAME-MAX

\ typed boolean producers (core has no `true`/`false`).
: YES ( -- bool ) 0 0= ;
: NO ( -- bool ) 0 0= 0= ;

\ The single-letter con codes are src/core/checker.f constants created before it
\ claims the source, so a from-source boot gives them no row: they are read
\ here at top level, as the reject codes are.
CC-N constant CON-N          \ single-letter n : signed cell
CC-BOOL constant CON-BOOL    \ single-letter f : boolean/flag
CC-R constant CON-R          \ single-letter r : real/float

\ --- one-token pushback. The token bytes stay valid across a line refill (the
\ engine buffers the input source), so the pushback holds the raw span in a
\ declared pointer cell.
variable PEND-U   PTR-VARIABLE PEND-A
: PEND! ( ptr u8 n -- ) PEND-U ! PEND-A ! ;
: PEND@ ( -- ptr u8 n ) PEND-A @ PEND-U @ ;

\ ---------------------------------------------------------------------------
\ transient parse state (parse-loop bookkeeping, not declaration state). One
\ declaration at a time: STRUCTURE is top-level interpret-only, never nested.
\ ---------------------------------------------------------------------------
variable FAM        \ family id being declared
variable TOK        \ live declaration-event token (0 = no open transaction)
variable SD-ARITY      \ parsed family arity
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ field count in this declaration
variable SD-CELLS      \ running field cell width (next field's slot / byte offset)
variable SEEN-FIELD \ a FIELD has appeared (header clauses must precede fields)
variable SEEN-END   \ this declaration's ;STRUCTURE has been consumed
variable SD-SI         \ private digit-scan index
variable SD-TI         \ byte index within one field type token
variable SD-FIELD-MISS
PTR-VARIABLE SD-MISS-A
variable SD-MISS-U
PTR-VARIABLE SD-TYPE-A
variable SD-TYPE-U
$2000 constant SD-SEEN-CAP
create SD-SEEN SD-SEEN-CAP allot
variable SD-SEEN-U
variable SD-SEEN-I
variable SD-OUTCOME-NAMES

$100 constant SD-ARG-CAP
create SD-ARGS SD-ARG-CAP cells allot
variable SD-ARG-N

: SD-RESET ( -- )                      \ base state; re-seeded at load (process-local)
   NULL-PTR 0 PEND!   0 TOK !   0 SEEN-FIELD !   0 SEEN-END !
   0 SD-FIELD-MISS !  0 SD-SEEN-U !
   NULL-PTR SD-TYPE-A !  0 SD-TYPE-U ! ;
SD-RESET

: SD-MISS-CLEAR ( -- ) NULL-PTR SD-MISS-A ! 0 SD-MISS-U ! ;
SD-MISS-CLEAR
: SD-MISSING! ( -- )
   -1 SD-FIELD-MISS !
   SD-MISS-U @ 0= IF SD-TYPE-A @ SD-MISS-A ! SD-TYPE-U @ SD-MISS-U ! THEN ;

\ Tokens come from the live input source, or — when a tool is replaying a
\ declaration it has already lexed (SD-REPLAY below) — from that tool's token
\ stream. The pushback is checked first either way, so lookahead behaves
\ identically on both sources.
: SD-RAW ( -- ptr u8 n )               \ next body token (honours one pushback)
   PEND-U @ 0 > IF PEND@ 0 PEND-U ! EXIT THEN
   DECL-REPLAY:RP-ACTIVE? IF DECL-REPLAY:RP-NEXT EXIT THEN
   parse-name ;
\ Every body token is recorded as the packet's offending token as it is read, so
\ a reject raised inside the family registry, the field record, or a transaction
\ participant still names the token that provoked it without those owners
\ knowing anything about the packet.
: SD-NEXT ( -- ptr u8 n )              \ next body token, recorded for diagnostics
   SD-RAW 2dup DECL-REJECT:TOKEN! ;
: UNGET ( ptr u8 n -- ) PEND! ;

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

: NAME-LONG? ( ptr u8 n -- bool ) nip NAME-MAX > ;

: REQUIRE-NAME ( ptr u8 n -- )      \ validate the family name (throws; consumes the copy)
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup NAME-LONG? IF 2drop TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail" E-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name" E-NAME DECL-REJECT:REJECT throw THEN ;

\ --- arity token: a small decimal within the shared declaration alphabet.
: SD-DIGIT? ( n -- bool ) dup 47 > swap 58 < and ;
: SD-ALLDIG? ( ptr u8 n -- bool )
   dup 0= IF 2drop NO EXIT THEN
   {: u:n :}                        \ ( a )
   0 SD-SI !
   BEGIN SD-SI @ u < WHILE
      dup SD-SI @ + c@ SD-DIGIT? 0= IF drop NO EXIT THEN
      SD-SI @ 1 + SD-SI !
   REPEAT
   drop YES ;
: DEC ( ptr u8 n -- n )             \ decode an all-digit token
   {: u:n :}                        \ ( a )
   0                                \ ( a acc )
   0 SD-SI !
   BEGIN SD-SI @ u < WHILE
      10 * over SD-SI @ + c@ 48 - +    \ acc = acc*10 + digit
      SD-SI @ 1 + SD-SI !
   REPEAT
   nip ;                            \ drop a, keep acc
\ Same wording the legacy definer prints for a bad arity token.
: ARITY-WHY$ ( -- ptr u8 n ) s" arity must be a decimal, at most 23 parameters" ;
: PARSE-ARITY ( ptr u8 n -- n )
   dup 0= IF 2drop s" missing arity" E-ARITY DECL-REJECT:REJECT throw THEN
   2dup SD-ALLDIG? 0= IF 2drop ARITY-WHY$ E-ARITY DECL-REJECT:REJECT throw THEN
   DEC dup TFAM-DECL-PARAM-COUNT > IF
      drop ARITY-WHY$ E-ARITY DECL-REJECT:REJECT throw THEN ;

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
   TFAM-DECL-CHAR>PARAM 0= IF
      drop s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   dup SD-ARITY @ < IF
      dup {: idx:n :}
      want PK-SCOPE = want PK-REGION = or
      want PK-TYPE = or IF
         FAM @ idx want TFAM-PK! THEN
      SCHEMA-PARAM EXIT
   THEN
   drop
   s" type parameter is outside the declared arity" E-PAYLOAD DECL-REJECT:REJECT throw ;

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

: SD-ATOM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SD-TI @ {: start:n :}
   BEGIN
      SD-TI @ u < IF a SD-TI @ + c@ SD-DELIM? 0= ELSE NO THEN
   WHILE
      SD-TI @ 1 + SD-TI !
   REPEAT
   SD-TI @ start = IF
      s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a start + SD-TI @ start - ;

defer SD-PARSE-NODE ( ptr u8 n n -- n )

: SD-APP ( ptr u8 n n -- n ) {: a:ptr u:n fam:n :}
   SD-ARG-N @ {: base:n :}
   0
   BEGIN
      dup fam TFAM-ARITY@ >= IF
         s" field type has too many arguments" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      dup fam swap TFAM-PK@ a u rot SD-PARSE-NODE SD-ARG+
      1 +
      SD-TI @ u >= IF
         s" unclosed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      a SD-TI @ + c@ dup 44 = IF
         drop SD-TI @ 1 + SD-TI !
      ELSE 62 = IF
         SD-TI @ 1 + SD-TI !
         dup fam TFAM-ARITY@ <> IF
            s" field type has wrong arity" E-PAYLOAD DECL-REJECT:REJECT throw THEN
         SD-FIELD-MISS @ IF base SD-ARG-N ! drop 0 EXIT THEN
         SCHEMA-ROOT-N@ {: start:n :}
         base BEGIN dup SD-ARG-N @ < WHILE
            dup cells SD-ARGS + @ SCHEMA-ROOT+ drop 1 +
         REPEAT drop
         base SD-ARG-N !
         fam start rot SCHEMA-APP EXIT
      ELSE
         s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw
      THEN THEN
   AGAIN ;

defer SD-LOOSE-NODE ( ptr u8 n -- )
: SD-LOOSE-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   SD-TI @ u >= IF
      s" unclosed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   a SD-TI @ + c@ 62 = IF SD-TI @ 1 + SD-TI ! EXIT THEN
   BEGIN
      a u SD-LOOSE-NODE
      SD-TI @ u >= IF
         s" unclosed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      a SD-TI @ + c@ dup 62 = IF drop SD-TI @ 1 + SD-TI ! EXIT THEN
      44 <> IF
         s" malformed field type application" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      SD-TI @ 1 + SD-TI !
   AGAIN ;

: SD-LOOSE-ATOM ( ptr u8 n -- ) {: a:ptr u:n :}
   u 1 = IF
      a c@ dup ASCII-N = over ASCII-F = or over ASCII-R = or
      IF drop EXIT THEN
      TFAM-DECL-CHAR>PARAM IF
         SD-ARITY @ < IF EXIT THEN
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

: SD-LOOSE-NODE-IMPL ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SD-ATOM {: na:ptr nu:n :}
   SD-TI @ u < IF a SD-TI @ + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? IF
         {: fam:n :}
         SD-TI @ 1 + SD-TI !
         a u fam SD-APP drop EXIT
      THEN drop
      na nu TYPE-MAY-ARRIVE-XT 0= IF
         s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      SD-MISSING!
      SD-TI @ 1 + SD-TI !
      a u SD-LOOSE-APP EXIT
   THEN
   na nu SD-LOOSE-ATOM ;
: SD-LOOSE-INSTALL ( -- ) [: SD-LOOSE-NODE-IMPL ;] is SD-LOOSE-NODE ;
SD-LOOSE-INSTALL

: SD-PARSE-NODE-IMPL ( ptr u8 n n -- n ) {: a:ptr u:n want:n :}
   a u SD-ATOM {: na:ptr nu:n :}
   SD-TI @ u < IF a SD-TI @ + c@ 60 = ELSE NO THEN IF
      na nu FIELD-FAM? 0= IF
         drop
         na nu TYPE-MAY-ARRIVE-XT 0= IF
            s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
         SD-MISSING!
         SD-TI @ 1 + SD-TI !
         a u SD-LOOSE-APP 0 EXIT
      THEN
      {: fam:n :}
      SD-TI @ 1 + SD-TI !
      a u fam SD-APP EXIT
   THEN
   nu 1 = IF na nu want LETTER-TYPE EXIT THEN
   na nu CON-OF dup 0 <> IF SCHEMA-CON EXIT THEN drop
   na nu FIELD-FAM? IF {: fam:n :}
      fam TFAM-ARITY@ 0 <> IF
         s" field type is parametric and needs type arguments" E-PAYLOAD DECL-REJECT:REJECT throw THEN
      fam 0 0 SCHEMA-APP EXIT
   THEN drop
   na nu TYPE-MAY-ARRIVE-XT IF SD-MISSING! 0 EXIT THEN
   s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SD-PARSER-INSTALL ( -- ) [: SD-PARSE-NODE-IMPL ;] is SD-PARSE-NODE ;
SD-PARSER-INSTALL

defer SD-QUOT-ELEM ( ptr u8 n -- n )
: SD-QUOT-FAIL ( ptr u8 n n -- ) DECL-REJECT:REJECT throw ;

: RESOLVE-TYPE ( ptr u8 n -- n )        \ type token(s) -> schema node
   dup 0= IF 2drop s" missing field type" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup SD-TYPE-U ! SD-TYPE-A !
   2dup s" [" CORE-STR= IF
      2drop [: SD-NEXT ;] [: SD-QUOT-ELEM ;] [: SD-QUOT-FAIL ;] TYPE-DECL:PARSE-QUOT EXIT THEN
   2dup s" ptr" CORE-STR=CI IF
      2drop SD-NEXT RECURSE
      SD-FIELD-MISS @ IF drop 0 EXIT THEN
      REQUIRE-POINTEE SCHEMA-PTR EXIT THEN
   0 SD-TI ! 0 SD-ARG-N !
   2dup PK-CELL SD-PARSE-NODE {: node:n :}
   SD-TI @ over <> IF
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

: POLICY-CODE ( ptr u8 n -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop s" missing layout policy name" E-POLICY DECL-REJECT:REJECT throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop TL-STACK-CELL-TAG EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop TL-PACKED-TAG EXIT THEN
   2drop s" unknown layout policy" E-POLICY DECL-REJECT:REJECT throw ;
: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   SD-NEXT POLICY-CODE {: code:n :}
   FAM @ code TFAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

\ OPAQUE: the family keeps its declared visibility and its generated words take
\ the private-family placement (docs/type-system.md §10.4). The event validates
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
\ family outright; nearly every memory record under lib/ is package-private, so
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
   SD-NEXT dup 0= IF
      2drop s" missing derive feature" E-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
   BEGIN SD-NEXT dup 0= IF 2drop EXIT THEN
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
: SD-SEEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 SD-SEEN-I !
   BEGIN SD-SEEN-I @ SD-SEEN-U @ < WHILE
      SD-SEEN SD-SEEN-I @ + c@ {: len:n :}
      len u = IF
         SD-SEEN BYTE-VIEW SD-SEEN-I @ + 1+ len a u CORE-STR= IF YES EXIT THEN
      THEN
      SD-SEEN-I @ len 1+ + SD-SEEN-I !
   REPEAT NO ;
: SD-NAME-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DECL-REJECT:TOKEN!
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   a u PF-NAME-REQUIRE
   a u SD-SEEN? IF E-DUP throw THEN
   SD-SEEN-U @ u + 1+ SD-SEEN-CAP > IF
      s" structure field names exceed declaration window"
      E-CAP DECL-REJECT:REJECT throw THEN
   u SD-SEEN SD-SEEN-U @ + c!
   0 SD-SEEN-I !
   BEGIN SD-SEEN-I @ u < WHILE
      a SD-SEEN-I @ + c@
      SD-SEEN SD-SEEN-U @ + 1+ SD-SEEN-I @ + c!
      SD-SEEN-I @ 1+ SD-SEEN-I !
   REPEAT
   SD-SEEN-U @ u + 1+ SD-SEEN-U ! ;
: FIELD-CLAUSE ( -- )
   SD-NEXT dup 0= IF
      2drop s" missing field name" E-SYNTAX DECL-REJECT:REJECT throw THEN   \ field name
   {: na:ptr nu:n :}
   na nu NAME-LONG? IF TF-NAME-LONG$ E-CAP DECL-REJECT:REJECT throw THEN
   na nu REQUIRE-FIELD-NAME
   SD-OUTCOME-NAMES @ 0 <> IF na nu SD-NAME-CHECK THEN
   0 SD-FIELD-MISS !
   SD-NEXT RESOLVE-TYPE {: node:n :}
   SD-FIELD-MISS @ 0= IF na nu node EMIT-FIELD THEN
   -1 SEEN-FIELD ! ;

\ ---------------------------------------------------------------------------
\ transaction orchestration.
\ ---------------------------------------------------------------------------
: VIS ( -- n )                          \ declaration visibility (public at top level)
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF CHECKER-VIS-PUBLIC EXIT THEN CHECKER-AUTH-PACKAGE-MODE@ ;
: SD-REGISTER ( ptr u8 n n -- )            \ ( na nu arity -- ) register the family, open the tx
   {: na:ptr nu:n ar:n :}
   ar SD-ARITY !
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
\ is armed with a COMMITTED field id (src/core/structure-make.f, ORDER 830). A
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
   SD-MISS-U @ IF E-UNRESOLVED throw THEN
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   FAM @ SD-CELLS @ TFAM-SLOTS!
   FAM @ TFAM-OPAQUE? SD-MAKEABLE? 0= and IF
      s" opaque requires a family with fields" E-SYNTAX DECL-REJECT:REJECT throw THEN
   \ The constructor generator's rejects use the packet's code table. The
   \ initialized accessor's fixed-layout gate arms its own reason in SD-ADDR-ARM.
   SD-MAKEABLE? IF TOK @ FAM @ STRUCTURE-MAKE:GENERATE THEN
   SD-ADDR-ARM ;

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   SD-NEXT dup 0= IF 2drop
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

: DRIVE ( -- )                          \ name + arity + register + body
   SD-NEXT 2dup DECL-REJECT:FAMILY!     \ ( na nu )  keep the span
   2dup REQUIRE-NAME                    \ named before validation, so a bad name is reported
   SD-NEXT PARSE-ARITY                  \ ( na nu arity )
   SD-REGISTER
   CLAUSES ;

\ One provisional transaction: commit by persisting, roll the family + schema +
\ layout + event stream back to a byte-identical registry on any reject.
: SD-BODY ( -- )
   SD-MISS-CLEAR
   [: SD-RESET DRIVE ;] GENERATED-DECL:RUN ;

\ Resynchronize the input to the end of THIS declaration. Same contract, same
\ reasoning and same two exemptions as enum-decl.f's ED-RESYNC: it matters only
\ when a multi-error load will swallow the reject and hand the rest of the
\ declaration to the interpreter, the terminator is this declaration's own
\ boundary, and there is nothing to skip once `;STRUCTURE` has been consumed or
\ the input has ended. Measured before this existed:
\ `STRUCTURE m7sbad 0 FIELD x n FIELD x n ;STRUCTURE NEWTYPE m7scont 1`
\ reported the duplicate field, counted it, then died on
\ `E-UNDEFINED: ;STRUCTURE`.
: SD-SKIP-BODY ( -- )
   BEGIN
      SD-RAW dup 0= IF 2drop EXIT THEN
      2dup s" ;structure" CORE-STR=CI IF 2drop EXIT THEN
      2drop
   AGAIN ;
: SD-RESYNC ( -- )
   SEEN-END @ IF EXIT THEN
   MULTI-ERR? 0= IF EXIT THEN
   SD-SKIP-BODY ;

\ A reject is rendered through the shared declaration packet AFTER the
\ coordinator has rolled everything back, then rethrown with its exact code,
\ which is the same order the legacy definers use (sumtype.f TDECL-RUN:
\ restore, report, rethrow). Both drivers below share this one guarded body, so
\ a replayed declaration reports through exactly the same renderer.
: SD-DRIVE ( -- )                      \ body, then resynchronize before reporting
   [: SD-BODY ;] catch {: rc:n :}
   rc 0= IF SD-RESET EXIT THEN
   TYPE-DECL:QUOT-ROLLBACK
   SD-RESYNC
   SD-RESET
   rc E-UNRESOLVED <> IF SD-MISS-CLEAR THEN
   rc throw ;
: SD-GUARDED ( -- )
   [: SD-DRIVE ;] DECL-REJECT:GUARD ;

\ The replay stream is retired on BOTH exits. Closing only on success would
\ leave a rejected replay installed, and the next live STRUCTURE would then read
\ its tokens from a spent buffer instead of the input source.
: SD-REPLAY-END ( n -- ptr u8 n bool ) \ direct missing span after rollback
   DECL-REPLAY:RP-RELEASE
   dup E-UNRESOLVED = IF
      drop SD-MISS-A @ SD-MISS-U @
      0 DECL-REJECT:GUARD-CODE
      YES SD-MISS-CLEAR EXIT THEN
   DECL-REJECT:GUARD-CODE
   SD-MISS-CLEAR
   NULL-PTR 0 NO ;

public

: SD-RUN ( -- )
   s" structure" DECL-REJECT:OPEN
   SD-GUARDED ;

\ SD-REPLAY ( name body -- ) : register a STRUCTURE from tokens a tool has
\ already lexed, defining no word. Same grammar, same validation, same registry
\ writes, same reject packet as SD-RUN — only the token source differs and the
\ MAKE/UNMAKE pair is registered without being rendered (structure-make.f
\ GENERATE). This is what lets tools/check-core.f and src/habu/verify-source.f
\ see a STRUCTURE family at all, so a later signature in the same source can
\ name it. The body buffer is the declaration body INCLUDING its ;STRUCTURE
\ terminator; a buffer without one rejects through the front end's own
\ "missing ;STRUCTURE" gate exactly as a truncated live declaration does.
\
\ The stream is claimed BEFORE the packet is opened. Claiming can fail — another
\ replay is already installed — and that failure belongs to the caller that
\ misused the entry, not to any declaration: it must not clear the packet of the
\ declaration that owns the live stream, and it must not close that stream on the
\ way out. So it propagates as its own code with no packet of its own, and only a
\ claim that SUCCEEDED reaches the close below.
: SD-REPLAY-WITH ( ptr u8 n ptr u8 n bool -- ptr u8 n bool )
   {: na:ptr nu:n ba:ptr bu:n names:bool :}
   na nu ba bu DECL-REPLAY:RP-CLAIM
   s" structure" DECL-REJECT:OPEN
   names SD-OUTCOME-NAMES !
   [: SD-DRIVE ;] catch
   0 SD-OUTCOME-NAMES !
   SD-REPLAY-END ;
: SD-REPLAY-OUTCOME ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   YES SD-REPLAY-WITH ;
: SD-REPLAY ( ptr u8 n ptr u8 n -- )
   NO SD-REPLAY-WITH IF 2drop E-UNRESOLVED throw THEN 2drop ;

;package

\ STRUCTURE is the one executable composite-declaration keyword and therefore a
\ documented global language surface (package-first exception, like the shipped
\ SUMTYPE/PRODUCT openers): it parses its own body up to ;STRUCTURE at interpret
\ time, so its checked effect is ( -- ).
\ STRUCTURE type-name arity [POLICY p] [DERIVE f+] [OPAQUE] (FIELD name type)* ;STRUCTURE
: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;

;using
;using
