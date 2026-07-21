\ enum-decl.f — the ENUM typed-declaration front end (package ENUM-DECL). This is
\ the SECOND consumer of the shared declaration-event transaction
\ (src/core/decl-event.f, package DECL-EVENT), binding to the SAME proven pattern
\ the STRUCTURE front end proved (src/core/structure-decl.f): persist-unframed
\ with an ED-MARK/ED-RESTORE registry snapshot around DECL-EVENT OPEN..PUBLISH/
\ ROLLBACK, package-local ED-* names that never shadow globals, and the pre-hook
\ registry reached only through named TRUSTED: forwarders. It owns ONLY the ENUM
\ grammar loop and its reject dispatch; it holds NO declaration state (the events,
\ the field rows, the variant registration, and the current-variant selector all
\ live in the event module).
\
\ Grammar (docs/type-families.md §2.1, §2.3):
\   enum-decl    = ENUM type-name (full-enum | compact-enum) ;ENUM
\   full-enum    = arity header-clause* variant-block+
\   compact-enum = compact-variant+
\   header-clause = POLICY policy-name | DERIVE derive-name+
\   variant-block = VARIANT variant-name (FIELD field-name type-expr)* ;VARIANT
\   compact-variant = variant-name
\ The FIRST body token after the family name selects the mode irrevocably: a
\ decimal is the arity header of a full block declaration; any other token is the
\ first bare variant of an implicitly-arity-zero compact declaration. The modes
\ never mix — a header clause or a VARIANT/FIELD/;VARIANT token in a compact body,
\ or a bare token where a full body expects a VARIANT, rejects at the exact token.
\ Every malformed/duplicate/reserved/unresolved reject rolls the whole provisional
\ declaration back to a byte-identical registry.
\
\ ---------------------------------------------------------------------------
\ FAMILY KIND (settled). The kind is chosen by mode at registration, before the
\ variants are parsed, because TFAM-DECL fixes the kind once (there is no kind
\ mutator). A compact ENUM is all-payloadless by construction, so it registers
\ TK-ENUM exactly like the legacy compact ENUM (docs §9.3): arity 0, slots 0. A
\ full ENUM is the general named-variant sum surface, so it registers TK-SUM with
\ the declared arity — the same kind test/decl-event-suite.f drives DECL-EVENT
\ variants over, and the kind PF-OWNER-OK? requires for a named variant field
\ (fam TFAM-SUM? or TFAM-ENUM?, type-family.f). §2.3's "all payloadless => enum
\ layout" is a downstream LAYOUT selection derived from the payload slots, not the
\ coarse registry kind: TFAM-WIDTH@ already treats TK-SUM and TK-ENUM identically
\ for a zero-payload family, so a full ENUM whose variants happen to be payloadless
\ is width-identical to a compact ENUM. Full-mode fields are named FIELD clauses
\ carried by the shared field event under the open variant selector; each variant
\ sets SV.SCH-COUNT=0 (the event module's DEV-VARIANT does this) and downstream
\ discovers a variant's fields by scanning the TYPE-FIELD rows keyed (family,
\ variant-id), per the settled seam, until the sumvfields rename lands.
\
\ ---------------------------------------------------------------------------
\ COMMIT / ROLLBACK and PARSE-ONLY SCOPE.
\ ---------------------------------------------------------------------------
\ Composition is identical to structure-decl.f: a top-level ENUM runs at interpret
\ time and persists UNFRAMED. The event stream + provisional field rows commit by
\ DECL-EVENT:PUBLISH and retire by DECL-EVENT:ROLLBACK; the family row, its schema
\ roots, its layout descriptor, the family/variant-name interns, and the variant
\ rows are snapshotted at open (ED-MARK) and restored on reject (ED-RESTORE) —
\ ED-RESTORE runs AFTER DECL-EVENT:ROLLBACK so the earlier (pre-family) string-pool
\ high-water wins. This dot is PARSE only: it publishes the family + variant +
\ field metadata and drives no constructor generation. The per-variant checked
\ constructor package (MESSAGE:QUIT, docs §2.3) is enum-generate-named's later
\ stage; the compact ENUM's constructor words the legacy sumtype.f definer emits
\ today are NOT reproduced here, because this front end is registration-only.
\
\ ---------------------------------------------------------------------------
\ GLOBAL-KEYWORD BINDING (staged). The global ENUM token still belongs to the
\ legacy sumtype.f definer, whose live call sites (test/type-decl-suite.f,
\ test/type-family-suite.f, test/candidate-validation.f, maki/async-dag.f, ...)
\ depend on its exact grammar, reject codes, and generated constructors. Binding
\ the global ENUM token to THIS front end now would shadow that definer and break
\ those gates, so the front end is reached as ENUM-DECL:ED-RUN and the hard cutover
\ (docs §2.7 tombstones + call-site migration) rebinds the global token later.
\ enum-decl.f therefore defines NO global word and is a pure prelude leaf: no
\ existing prelude file calls into it.
\
\ Loaded AFTER the checker hook and AFTER decl-event.f (it drives DECL-EVENT:*),
\ in the post-hook DECL group after structure-decl.f.

package ENUM-DECL

\ --- named reject codes. Re-declared package-locally (values mirror sumtype.f
\ E-TDECL-* 7107/7108/7109/7110/7116/7119 and type-family.f E-TFAM-CASE 7101),
\ exactly as structure-decl.f and decl-event.f re-declare them, because the global
\ pre-hook constants that own them are removed by the type-DSL cutover and do not
\ survive the checked engine's fixpoint self-rebuild. E-TFAM-DUP (family/variant
\ duplicate, 7102) and the field record's own E-PF-NAME / E-TFAM-DUP / E-TFAM-CASE
\ / E-PF-SCHEMA name+schema gate codes are raised by TFAM-DECL, SUMV-ADD, and the
\ field-event path and pass through unchanged.
7107 constant E-SYNTAX      \ malformed: missing name/terminator, mixed mode, header/variant/field out of place
7108 constant E-ARITY       \ arity token is not a small decimal in [0, cap]
7109 constant E-PAYLOAD     \ unresolved / unknown field type
7110 constant E-NAME        \ reserved or colliding family name
7116 constant E-POLICY      \ unknown or not-yet-supported layout policy
7119 constant E-DERIVE      \ unknown or not-yet-supported derive feature
7101 constant E-CASE        \ family name is not a lowercase canonical tail

26 constant ARITY-CAP       \ positional params are letters a..z (docs §9.2)

\ typed boolean producers (core has no `true`/`false`).
: YES ( -- bool ) 0 0= ;
: NO ( -- bool ) 0 0= 0= ;

\ ---------------------------------------------------------------------------
\ Trusted forwarders to the pre-hook registry / schema / checker words. These are
\ raw-memory / metaprogramming boundaries the checker cannot type from a post-hook
\ checked body; the compiled reference survives the REG-PROTECT name seal exactly
\ as decl-event.f's DEV-FLD-* and structure-decl.f's FAM-* forwarders do.
\ ---------------------------------------------------------------------------
TRUSTED: FAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: FAM-LAYOUT! ( n n -- ) TFAM-LAYOUT! ;
TRUSTED: FAM-EQ! ( n -- ) TFAM-DERIVE-EQ! ;
TRUSTED: FAM-HASH! ( n -- ) TFAM-DERIVE-HASH! ;
TRUSTED: FAM-VAR-RANGE! ( n n n -- ) TFAM-VAR-RANGE! ;
TRUSTED: FAM-FLD-RANGE! ( n n n -- ) TFAM-FLD-RANGE! ;
TRUSTED: FAM-SLOTS! ( n n -- ) TFAM-SLOTS! ;
TRUSTED: FAM-PUBLIC? ( n -- bool ) TFAM-PUBLIC? ;
TRUSTED: FAM-ARITY@ ( n -- n ) TFAM-ARITY@ ;
TRUSTED: FAM-WIDTH@ ( n -- n ) TFAM-WIDTH@ ;
TRUSTED: FAM-LAYOUT? ( n -- bool ) TFAM-LAYOUT? ;
TRUSTED: FAM-CELL? ( n -- bool ) TFAM-CELL? ;
TRUSTED: FAM-LINEAR? ( n -- bool ) TFAM-CONCRETE-LINEAR? ;
TRUSTED: SIG-RESOLVE ( ptr u8 n ptr u8 n -- n bool ) TFAM-SIG-RESOLVE ;
TRUSTED: ACTIVE-PKG$ ( -- ptr u8 n ) TFAM-ACTIVE-PKG$ ;
TRUSTED: PKG-ACTIVE? ( -- bool ) CHECKER-PACKAGE-ACTIVE? ;
TRUSTED: PKG-MODE@ ( -- n ) CHECKER-PACKAGE-MODE @ ;
TRUSTED: CANON? ( ptr u8 n -- bool ) TF-CANON? ;
TRUSTED: GRAMMAR-KW? ( ptr u8 n -- bool ) TF-GRAMMAR-KEYWORD? ;
TRUSTED: CON-CODE ( ptr u8 n -- n ) CON-OF ;
TRUSTED: LOW? ( n -- bool ) LOWER? ;
TRUSTED: SUMV-N@ ( -- n ) SUMV-N @ ;    \ variant-cursor high-water (variant range start)
TRUSTED: CON-N ( -- n ) CC-N ;          \ single-letter n : signed cell
TRUSTED: CON-BOOL ( -- n ) CC-BOOL ;    \ single-letter f : boolean/flag
TRUSTED: CON-R ( -- n ) CC-R ;          \ single-letter r : real/float
TRUSTED: LT-STACK ( -- n ) TL-STACK-CELL-TAG ;   \ default layout policy code
TRUSTED: LT-PACKED ( -- n ) TL-PACKED-TAG ;      \ packed-tag layout policy code
TRUSTED: DV-EQ ( -- n ) DRV-EQ ;                 \ derive feature code: equality
TRUSTED: DV-HASH ( -- n ) DRV-HASH ;             \ derive feature code: hash
TRUSTED: PKG-PUBLIC ( -- n ) CHECKER-PACKAGE-PUBLIC ;   \ public visibility code
TRUSTED: ED-SCH-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: ED-SCH-PARAM ( n -- n ) SCHEMA-PARAM ;
TRUSTED: ED-SCH-PTR ( n -- n ) SCHEMA-PTR ;
TRUSTED: ED-SCH-APP ( n n n -- n ) SCHEMA-APP ;
TRUSTED: SCH-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: SCH-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: SCH-APP? ( n -- bool ) SCHEMA-APP? ;
TRUSTED: SCH-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: FLAGS-NONE ( -- n ) PF-FLAGS-NONE ;   \ field-record layout flag: none
TRUSTED: TK-SUM-K ( -- n ) TK-SUM ;            \ full-mode named-variant sum kind
TRUSTED: TK-ENUM-K ( -- n ) TK-ENUM ;          \ compact-mode payloadless enum kind

\ --- registry rollback snapshot. The seven cursors a declaration can grow that
\ the event module does not own (family, string pool, param pool, variant pool,
\ layout pool, schema node pool, schema-root pool). Straight cell reads/writes:
\ no locals, no control, so the trusted boundary stays minimal (docs/forth.md
\ "Keep TRUSTED: bodies syntax-simple").
variable M-TFAM   variable M-STR   variable M-PK   variable M-SUMV
variable M-LAY    variable M-SCH   variable M-ROOT
TRUSTED: ED-MARK ( -- )
   TFAM-N @ M-TFAM !   TF-STR-U @ M-STR !   TF-PK-N @ M-PK !
   SUMV-N @ M-SUMV !   LAY-N @ M-LAY !   SCH-N @ M-SCH !   SCH-ROOT-N @ M-ROOT ! ;
TRUSTED: ED-RESTORE ( -- )
   M-TFAM @ TFAM-N !   M-STR @ TF-STR-U !   M-PK @ TF-PK-N !
   M-SUMV @ SUMV-N !   M-LAY @ LAY-N !   M-SCH @ SCH-N !   M-ROOT @ SCH-ROOT-N ! ;

\ --- one-token pushback. The token bytes stay valid across a line refill (the
\ engine buffers the input source), so the pushback holds the raw span; storing a
\ ptr u8 through a plain cell and reading it back is the one place the checker
\ needs a named boundary.
variable PEND-U   variable PEND-A
TRUSTED: PEND! ( ptr u8 n -- ) PEND-U ! PEND-A ! ;
TRUSTED: PEND@ ( -- ptr u8 n ) PEND-A @ PEND-U @ ;

\ ---------------------------------------------------------------------------
\ transient parse state (parse-loop bookkeeping, not declaration state). One
\ declaration at a time: ENUM is top-level interpret-only, never nested.
\ ---------------------------------------------------------------------------
variable FAM        \ family id being declared
variable TOK        \ live declaration-event token (0 = no open transaction)
variable ED-ARITY      \ parsed family arity (0 for compact)
variable VBASE      \ variant high-water at open (variant range start)
variable NVAR       \ variant count in this declaration
variable FLDBASE    \ committed field high-water at open (field range start)
variable NFLD       \ total field count across all variants
variable VCELLS     \ running payload cell width WITHIN the open variant
variable MAXSLOTS   \ widest variant payload cell width (the sum's payload slots)
variable SEEN-VARIANT \ a VARIANT has appeared (header clauses must precede variants)
variable ED-SI         \ private digit-scan index

: ED-RESET ( -- )                      \ base state; re-seeded at load (process-local)
   0 PEND-U !   0 TOK !   0 SEEN-VARIANT ! ;
ED-RESET

: ED-NEXT ( -- ptr u8 n )              \ next body token (honours one pushback)
   PEND-U @ 0 > IF PEND@ 0 PEND-U ! EXIT THEN
   parse-name ;
: UNGET ( ptr u8 n -- ) PEND! ;

\ ---------------------------------------------------------------------------
\ name gate: a reserved family name is a grammar keyword, the ENUM openers, a
\ single-character token (would collide with a type letter / arity param), or a
\ concrete checker type name. Case + duplicate are enforced by TFAM-DECL itself.
\ ---------------------------------------------------------------------------
: NAME-RESERVED? ( ptr u8 n -- bool )
   2dup GRAMMAR-KW? IF 2drop YES EXIT THEN
   2dup s" enum" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;enum" CORE-STR=CI IF 2drop YES EXIT THEN
   dup 1 = IF 2drop YES EXIT THEN
   CON-CODE 0 <> ;

: REQUIRE-NAME ( ptr u8 n -- )      \ validate the family name (throws; consumes the copy)
   dup 0= IF 2drop E-SYNTAX throw THEN
   2dup CANON? 0= IF 2drop E-CASE throw THEN
   NAME-RESERVED? IF E-NAME throw THEN ;

\ --- mode-selection / arity token: a small decimal in [0, ARITY-CAP].
: ED-DIGIT? ( n -- bool ) dup 47 > swap 58 < and ;
: ED-ALLDIG? ( ptr u8 n -- bool )    \ true when every byte is a digit (mode selector)
   dup 0= IF 2drop NO EXIT THEN
   {: u:n :}                        \ ( a )
   0 ED-SI !
   BEGIN ED-SI @ u < WHILE
      dup ED-SI @ + c@ ED-DIGIT? 0= IF drop NO EXIT THEN
      ED-SI @ 1 + ED-SI !
   REPEAT
   drop YES ;
: DEC ( ptr u8 n -- n )             \ decode an all-digit token
   {: u:n :}                        \ ( a )
   0                                \ ( a acc )
   0 ED-SI !
   BEGIN ED-SI @ u < WHILE
      10 * over ED-SI @ + c@ 48 - +    \ acc = acc*10 + digit
      ED-SI @ 1 + ED-SI !
   REPEAT
   nip ;                            \ drop a, keep acc
: PARSE-ARITY ( ptr u8 n -- n )
   dup 0= IF 2drop E-ARITY throw THEN
   2dup ED-ALLDIG? 0= IF 2drop E-ARITY throw THEN
   DEC dup ARITY-CAP > IF drop E-ARITY throw THEN ;

\ ---------------------------------------------------------------------------
\ field type resolution -> a schema node (docs §8): concrete cell types (n/f/r +
\ multi-char con names), positional letter params within arity, ptr T, and closed
\ non-linear arity-0 layout/cell families. Everything else is unresolved. Mirrors
\ structure-decl.f's resolver; a shared resolver module is future type-DSL work.
\ ---------------------------------------------------------------------------
: FIELD-FAM? ( ptr u8 n -- n bool )     \ resolve a closed arity-0 layout/cell family
   ACTIVE-PKG$ 2swap SIG-RESOLVE 0= IF drop 0 NO EXIT THEN
   {: id:n :}
   id FAM-LAYOUT? id FAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM-ARITY@ 0 <> IF 0 NO EXIT THEN
   id FAM-LINEAR? IF 0 NO EXIT THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n -- n )         \ single-char type: param / n / f / r
   over c@ {: c:n :}                     \ ( a u )
   c LOW? 0= IF 2drop E-PAYLOAD throw THEN
   c 97 - ED-ARITY @ < IF 2drop c 97 - ED-SCH-PARAM EXIT THEN
   c [char] n = IF 2drop CON-N ED-SCH-CON EXIT THEN
   c [char] f = IF 2drop CON-BOOL ED-SCH-CON EXIT THEN
   c [char] r = IF 2drop CON-R ED-SCH-CON EXIT THEN
   2drop E-PAYLOAD throw ;

: RESOLVE-TYPE ( ptr u8 n -- n )        \ type token(s) -> schema node
   dup 0= IF 2drop E-SYNTAX throw THEN
   2dup s" ptr" CORE-STR=CI IF 2drop ED-NEXT RECURSE ED-SCH-PTR EXIT THEN
   dup 1 = IF LETTER-TYPE EXIT THEN
   2dup CON-CODE dup 0 <> IF nip nip ED-SCH-CON EXIT THEN drop
   2dup FIELD-FAM? IF nip nip 0 0 ED-SCH-APP EXIT THEN drop
   2drop E-PAYLOAD throw ;

: SCH-WIDTH ( n -- n )                  \ physical cell width of a field schema node
   dup SCH-APP? IF SCH-A@ FAM-WIDTH@ EXIT THEN drop 1 ;

\ ---------------------------------------------------------------------------
\ header clauses (full mode only). Each emits its event through DECL-EVENT (which
\ owns duplicate / ordinal state) and mutates only the fresh family record.
\ ---------------------------------------------------------------------------
: HEADER-ORDER ( -- )                   \ header clauses precede the first variant
   SEEN-VARIANT @ IF E-SYNTAX throw THEN ;

: POLICY-CODE ( ptr u8 n -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop E-POLICY throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop LT-STACK EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop LT-PACKED EXIT THEN
   2drop E-POLICY throw ;
: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   ED-NEXT POLICY-CODE {: code:n :}
   FAM @ code FAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )  \ a known/recognised derive feature token
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
: DERIVE-GUARD ( -- )                   \ derive needs a public, concrete (arity 0) family
   FAM @ FAM-PUBLIC? 0= IF E-DERIVE throw THEN
   FAM @ FAM-ARITY@ 0 <> IF E-DERIVE throw THEN ;
: EMIT-DERIVE ( n n -- )                \ ( fam feature-code -- ) emit the DERIVE event
   TOK @ -rot DECL-EVENT:DERIVE TOK ! ;
: DERIVE-ONE ( ptr u8 n -- )            \ apply one feature + emit its event
   DERIVE-GUARD
   2dup s" eq" CORE-STR=CI IF 2drop FAM @ FAM-EQ! FAM @ DV-EQ EMIT-DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop FAM @ FAM-HASH! FAM @ DV-HASH EMIT-DERIVE EXIT THEN
   2drop E-DERIVE throw ;                \ order recognised but not yet supported, plus unknown
: DERIVE-CLAUSE ( -- )                  \ DERIVE feature+ : first mandatory, rest by lookahead
   HEADER-ORDER
   ED-NEXT dup 0= IF 2drop E-DERIVE throw THEN DERIVE-ONE
   BEGIN ED-NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

\ ---------------------------------------------------------------------------
\ variant blocks (full mode). VARIANT opens a variant (DECL-EVENT:VARIANT
\ registers the SUMV row and pins the current-variant selector), its FIELD clauses
\ emit shared field events carrying that selector, ;VARIANT closes it. Each
\ variant's payload cells restart at slot 0 (VCELLS), and the family's payload
\ width is the widest variant (MAXSLOTS).
\ ---------------------------------------------------------------------------
: EMIT-FIELD ( ptr u8 n n -- )          \ ( na nu node -- ) layout + drive the field event
   SCH-ROOT+ {: sch:n :}                \ ( na nu )
   sch SCH-ROOT@ SCH-WIDTH {: fw:n :}
   TOK @ FAM @ 2swap sch                \ ( tok fam na nu sch )
   VCELLS @  fw  VCELLS @ CELL *  fw CELL *  CELL  FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw VCELLS @ + VCELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   ED-NEXT dup 0= IF 2drop E-SYNTAX throw THEN   \ field name
   2>r                                        \ r: na nu   (held; span stays valid)
   ED-NEXT RESOLVE-TYPE                           \ ( node )
   2r> rot                                     \ ( na nu node )
   EMIT-FIELD ;

: VARIANT-NAME ( -- ptr u8 n )          \ next token = variant name (must be present)
   ED-NEXT dup 0= IF 2drop E-SYNTAX throw THEN ;
: OPEN-VARIANT ( -- )
   VARIANT-NAME 2>r                     \ r: na nu
   TOK @ FAM @ 2r>                      \ ( tok fam na nu )
   DECL-EVENT:VARIANT TOK !             \ SUMV-ADD + set current-variant selector
   0 VCELLS ! ;
: CLOSE-VARIANT ( -- )
   VCELLS @ MAXSLOTS @ max MAXSLOTS !
   TOK @ FAM @ DECL-EVENT:END-VARIANT TOK !     \ clear the current-variant selector
   NVAR @ 1 + NVAR ! ;
: VARIANT-CLAUSE ( -- bool )            \ read one variant-body token; YES = ;VARIANT
   ED-NEXT dup 0= IF 2drop E-SYNTAX throw THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2drop E-SYNTAX throw ;               \ anonymous payload token / stray token
: VARIANT-BODY ( -- ) BEGIN VARIANT-CLAUSE UNTIL ;
: VARIANT-BLOCK ( -- )
   -1 SEEN-VARIANT !
   OPEN-VARIANT VARIANT-BODY CLOSE-VARIANT ;

\ ---------------------------------------------------------------------------
\ compact variants (compact mode). Each bare token is one payloadless variant:
\ open then immediately close (no fields). A full-mode grammar keyword appearing
\ here is a mixed-mode / compact-header / compact-payload reject at that token.
\ ---------------------------------------------------------------------------
: COMPACT-KW? ( ptr u8 n -- bool )      \ a full-mode keyword illegal in a compact body
   2dup s" variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop YES EXIT THEN
   s" derive" CORE-STR=CI ;
: COMPACT-VARIANT ( ptr u8 n -- )       \ register one payloadless variant
   2dup COMPACT-KW? IF 2drop E-SYNTAX throw THEN
   2>r TOK @ FAM @ 2r>                  \ ( tok fam na nu )
   DECL-EVENT:VARIANT TOK !
   TOK @ FAM @ DECL-EVENT:END-VARIANT TOK !
   NVAR @ 1 + NVAR ! ;

\ ---------------------------------------------------------------------------
\ transaction orchestration.
\ ---------------------------------------------------------------------------
: VIS ( -- n )                          \ declaration visibility (public at top level)
   PKG-ACTIVE? 0= IF PKG-PUBLIC EXIT THEN PKG-MODE@ ;
: OPEN-TX ( -- )                        \ capture bases, open the event tx, emit DECL
   SUMV-N@ VBASE !
   TYPE-FIELD:COUNT FLDBASE !
   0 NVAR !   0 NFLD !   0 MAXSLOTS !
   DECL-EVENT:OPEN TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK ! ;
: REGISTER-FULL ( ptr u8 n n -- )          \ ( na nu arity -- ) TK-SUM family, arity header event
   {: ar:n :}                           \ ( na nu )
   ar ED-ARITY !
   2>r ACTIVE-PKG$ VIS 2r>              \ ( pa pu vis na nu )
   ar TK-SUM-K FAM-DECL FAM !
   OPEN-TX
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;
: REGISTER-COMPACT ( ptr u8 n -- )         \ ( na nu -- ) TK-ENUM family, arity 0, no header event
   0 ED-ARITY !
   2>r ACTIVE-PKG$ VIS 2r>              \ ( pa pu vis na nu )
   0 TK-ENUM-K FAM-DECL FAM !
   OPEN-TX ;

: ED-CLOSE ( -- )                          \ bind variant + field ranges + width, then publish
   NVAR @ 0= IF E-SYNTAX throw THEN         \ an enum needs at least one variant
   FAM @ VBASE @ NVAR @ FAM-VAR-RANGE!
   FAM @ FLDBASE @ NFLD @ FAM-FLD-RANGE!
   FAM @ MAXSLOTS @ FAM-SLOTS!
   TOK @ DECL-EVENT:PUBLISH ;

: FULL-CLAUSE ( -- bool )               \ read + dispatch one full-body token; YES = ;ENUM
   ED-NEXT dup 0= IF 2drop E-SYNTAX throw THEN
   2dup s" ;enum" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" variant" CORE-STR=CI IF 2drop VARIANT-BLOCK NO EXIT THEN
   2drop E-SYNTAX throw ;               \ arity-then-compact / stray / mixed-legacy token
: FULL-BODY ( -- ) BEGIN FULL-CLAUSE UNTIL ;

: COMPACT-BODY ( ptr u8 n -- )          \ first variant token in hand, then loop to ;ENUM
   BEGIN
      dup 0= IF 2drop E-SYNTAX throw THEN            \ input ended before ;ENUM
      2dup s" ;enum" CORE-STR=CI IF 2drop EXIT THEN
      COMPACT-VARIANT
      ED-NEXT
   AGAIN ;

: DRIVE ( -- )                          \ name + mode select + register + body
   parse-name 2dup REQUIRE-NAME         \ ( na nu )  keep the span
   ED-NEXT                              \ ( na nu ta tu )  first body token selects the mode
   2dup ED-ALLDIG? IF
      PARSE-ARITY REGISTER-FULL FULL-BODY
   ELSE
      2>r REGISTER-COMPACT 2r> COMPACT-BODY
   THEN
   ED-CLOSE ;

public

\ One provisional transaction: commit by persisting, roll the family + schema +
\ layout + variant + event stream back to a byte-identical registry on any reject.
: ED-RUN ( -- )
   ED-RESET
   ED-MARK
   [: DRIVE ;] catch {: rc:n :}
   rc 0= IF EXIT THEN
   TOK @ 0 <> IF TOK @ DECL-EVENT:ROLLBACK THEN
   ED-RESTORE
   rc throw ;

;package
