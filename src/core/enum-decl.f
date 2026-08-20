\ enum-decl.f — the ENUM typed-declaration front end (package ENUM-DECL). This is
\ the SECOND consumer of the shared declaration-event transaction
\ (src/core/decl-event.f, package DECL-EVENT), binding to the same atomic
\ GENERATED-DECL coordinator used by STRUCTURE. It owns only the ENUM
\ grammar loop and its reject dispatch; it holds NO declaration state (the events,
\ the field rows, the variant registration, and the current-variant selector all
\ live in the event module).
\
\ Grammar (docs/type-families.md §2.1, §2.3):
\   enum-decl    = ENUM type-name (full-enum | compact-enum) ;ENUM
\   full-enum    = arity header-clause* variant-block+
\   compact-enum = header-clause* compact-variant+
\   header-clause = POLICY policy-name | DERIVE derive-name+
\   variant-block = VARIANT variant-name (FIELD field-name type-expr)* ;VARIANT
\   compact-variant = variant-name
\ The FIRST body token after the family name selects the mode irrevocably: a
\ decimal is the arity header of a full block declaration; any other token starts
\ an implicitly-arity-zero compact declaration. Compact header clauses precede
\ its first bare variant. The modes never mix — a VARIANT/FIELD/;VARIANT token
\ in a compact body, a compact header after its first variant, or a bare token
\ where a full body expects a VARIANT rejects at the exact token.
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
\ TRANSACTION SCOPE.
\ ---------------------------------------------------------------------------
\ The checker, metadata, event, constructor-generation and constructor-protection
\ participants retain one shared savepoint until the complete declaration has
\ validated. A reject restores every owned registry in reverse participant order.
\ This front end parses and publishes family, variant, and field metadata; it
\ renders no constructor itself. Once the variant range, the field range and the
\ payload width are bound, ED-CLOSE names the family to the ORDER 820 constructor
\ participant (src/core/generated-declaration.f) and the declaration transaction
\ generates the per-variant checked constructor package (MESSAGE:QUIT, docs §2.3)
\ inside its commit phase, so a failure anywhere publishes no constructor at all.
\
\ ---------------------------------------------------------------------------
\ GLOBAL-KEYWORD BINDING (landed). The global `ENUM` keyword IS this front end:
\ `: ENUM ( -- ) ENUM-DECL:ED-RUN ;` at the end of this file is the sole global
\ entry, written the way src/core/structure-decl.f writes `STRUCTURE`. The legacy
\ compact parser and generator in src/core/sumtype.f, and its metadata-only
\ CHECKER-DEFENUM entry, are deleted. ED-REPLAY below remains the checked entry
\ for callers that have already lexed a declaration.
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
7102 constant E-DUP         \ duplicate family, variant, or field tail (type-family.f
                            \ E-TFAM-DUP, raised by TFAM-DECL / SUMV-ADD / the field-event
                            \ path; named here only so a reason can be armed before those
                            \ calls)

110 constant ASCII-N
102 constant ASCII-F
114 constant ASCII-R

\ typed boolean producers (core has no `true`/`false`).
: YES ( -- bool ) 0 0= ;
: NO ( -- bool ) 0 0= 0= ;

\ ---------------------------------------------------------------------------
\ Trusted forwarders to the pre-hook registry / schema / checker words. These are
\ raw-memory / metaprogramming boundaries the checker cannot type from a post-hook
\ checked body; the compiled reference survives the REG-PROTECT name seal exactly
\ as decl-event.f's DEV-FLD-* and structure-decl.f's FAM-* forwarders do.
\ Replacement owner: TYPE-FIXES-PLAN E5-E8.
\
\ A forwarder belongs here ONLY while its target has no checker-recorded effect.
\ TFAM-PUBLIC?, TFAM-ARITY@, TFAM-WIDTH@, CHECKER-AUTH-PACKAGE-ACTIVE? and
\ CHECKER-AUTH-PACKAGE-MODE@ carry primitive axioms in src/core/checker.f, so the
\ bodies below call them by name and the checker enforces the axiom. Forwarding a
\ word whose effect the checker holds restates a signature nothing verifies
\ (dot habu-visibility-discharge-548-fab55650).
\ ---------------------------------------------------------------------------

\ --- one-token pushback. The token bytes stay valid across a line refill (the
\ engine buffers the input source), so the pushback holds the raw span; storing a
\ ptr u8 through a plain cell and reading it back is the one place the checker
\ needs a named boundary. Replacement owner: TYPE-FIXES-PLAN item 27.
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
variable SEEN-END     \ this declaration's ;ENUM has been consumed
variable ED-SI         \ private digit-scan index

: ED-RESET ( -- )                      \ base state; re-seeded at load (process-local)
   0 PEND-U !   0 TOK !   0 SEEN-VARIANT !   0 SEEN-END ! ;
ED-RESET

\ Tokens come from the live input source, or — when a tool is replaying a
\ declaration it has already lexed (ED-REPLAY below) — from that tool's token
\ stream. The pushback is checked first either way, so the compact body's
\ lookahead behaves identically on both sources.
: ED-RAW ( -- ptr u8 n )               \ next body token (honours one pushback)
   PEND-U @ 0 > IF PEND@ 0 PEND-U ! EXIT THEN
   DECL-REPLAY:RP-ACTIVE? IF DECL-REPLAY:RP-NEXT EXIT THEN
   parse-name ;
\ Every body token is recorded as the packet's offending token as it is read, so
\ a reject raised inside the family registry, the variant registry, the field
\ record, or a transaction participant still names the token that provoked it
\ without those owners knowing anything about the packet.
: ED-NEXT ( -- ptr u8 n )              \ next body token, recorded for diagnostics
   ED-RAW 2dup DECL-REJECT:TOKEN! ;
: UNGET ( ptr u8 n -- ) PEND! ;

\ ---------------------------------------------------------------------------
\ name gate: a reserved family name is a grammar keyword, a control word, the
\ ENUM openers, a single-character token (would collide with a type letter /
\ arity param), or a concrete checker type name. Case + duplicate are enforced by
\ TFAM-DECL itself. The control-word arm reads TYPE-NAME:CONTROL?, the single
\ owner of that list, which is the same list the legacy definer's
\ TDECL-RESERVED? consults: without it `ENUM-DECL:ED-RUN if red green ;ENUM` was
\ accepted here while `ENUM if red green ;ENUM` was refused 7110, so the global
\ token could not move to this front end without losing the reject.
\ ---------------------------------------------------------------------------
: NAME-RESERVED? ( ptr u8 n -- bool )
   2dup TF-GRAMMAR-KEYWORD? IF 2drop YES EXIT THEN
   2dup TYPE-NAME:CONTROL? IF 2drop YES EXIT THEN
   2dup s" enum" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;enum" CORE-STR=CI IF 2drop YES EXIT THEN
   dup 1 = IF 2drop YES EXIT THEN
   CON-OF 0 <> ;

: REQUIRE-NAME ( ptr u8 n -- )      \ validate the family name (throws; consumes the copy)
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup TF-CANON? 0= IF
      2drop s" name must be a lowercase family tail" E-CASE DECL-REJECT:REJECT throw THEN
   NAME-RESERVED? IF s" reserved name" E-NAME DECL-REJECT:REJECT throw THEN ;

\ --- mode-selection / arity token: a decimal within the shared alphabet.
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
\ Same wording the legacy definer prints for a bad arity token.
: ARITY-WHY$ ( -- ptr u8 n ) s" arity must be a decimal, at most 23 parameters" ;
: PARSE-ARITY ( ptr u8 n -- n )
   dup 0= IF 2drop s" missing arity" E-ARITY DECL-REJECT:REJECT throw THEN
   2dup ED-ALLDIG? 0= IF 2drop ARITY-WHY$ E-ARITY DECL-REJECT:REJECT throw THEN
   DEC dup TFAM-DECL-PARAM-COUNT > IF
      drop ARITY-WHY$ E-ARITY DECL-REJECT:REJECT throw THEN ;

\ ---------------------------------------------------------------------------
\ field type resolution -> a schema node (docs §8): concrete cell types (n/f/r +
\ multi-char con names), positional letter params within arity, ptr T, and closed
\ arity-0 layout/cell families. Everything else is unresolved. Mirrors
\ structure-decl.f's resolver; a shared resolver module is future type-DSL work.
\
\ A family that owns a linear value — directly or through its own fields — IS an
\ accepted payload type (dot habu-checker-enum-payload-9e1ae6cc). The enum then
\ owns that obligation by containment, which is the same rule a variant naming a
\ bare DEFLINEAR con already relies on: TFAM-CONCRETE-LINEAR? walks the variant
\ payload schemas, follows an application node into the family it names, and
\ reports the enum linear, so the checker counts the whole bundle as one linear
\ unit. Refusing the family spelling while accepting the con spelling blocked the
\ name and never the obligation, so it bought no soundness.
\
\ A parametric family named bare is the one resolved-but-unusable case source can
\ reach, and it now says so. Calling a registered family "unknown payload type"
\ sent readers hunting for a missing declaration that was in fact right there. The
\ remaining kind test still falls through to the unknown message because the only
\ kind it excludes, TK-EVIDENCE, has no declarer any source can write.
\ ---------------------------------------------------------------------------
: FIELD-FAM? ( ptr u8 n -- n bool )     \ resolve a closed arity-0 layout/cell family
   TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE 0= IF drop 0 NO EXIT THEN
   {: id:n :}
   id TFAM-LAYOUT? id TFAM-CELL? or 0= IF 0 NO EXIT THEN
   id TFAM-ARITY@ 0 <> IF
      s" payload type is parametric and needs type arguments" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n -- n )         \ single-char type: param / n / f / r
   drop c@
   dup ASCII-N = IF drop CC-N SCHEMA-CON EXIT THEN
   dup ASCII-F = IF drop CC-BOOL SCHEMA-CON EXIT THEN
   dup ASCII-R = IF drop CC-R SCHEMA-CON EXIT THEN
   TFAM-DECL-CHAR>PARAM 0= IF
      drop s" unknown payload type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   dup ED-ARITY @ < IF SCHEMA-PARAM EXIT THEN
   drop
   s" type parameter is outside the declared arity" E-PAYLOAD DECL-REJECT:REJECT throw ;

\ A pointer is a NON-OWNING boundary: TFCL-NODE? stops at a pointer node, so a
\ payload spelled `ptr <linear>` reads non-linear and the containing family would
\ copy and drop freely while a linear resource sits behind the address. The family
\ spelling and the con spelling launder identically, so both are refused here, at
\ the declaration door, with one rule — the same rule structure-decl.f applies to
\ a field, because a variant payload carries exactly the same obligation.
: REQUIRE-POINTEE ( n -- n )                \ pointee node, or reject a linear owner behind the address
   dup TFCL-NODE? IF
      s" payload type is a pointer to a linear value and cannot own it"
      E-PAYLOAD DECL-REJECT:REJECT throw THEN ;

: RESOLVE-TYPE ( ptr u8 n -- n )        \ type token(s) -> schema node
   dup 0= IF 2drop s" missing payload type" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ptr" CORE-STR=CI IF 2drop ED-NEXT RECURSE REQUIRE-POINTEE SCHEMA-PTR EXIT THEN
   dup 1 = IF LETTER-TYPE EXIT THEN
   2dup CON-OF dup 0 <> IF nip nip SCHEMA-CON EXIT THEN drop
   2dup FIELD-FAM? IF nip nip 0 0 SCHEMA-APP EXIT THEN drop
   2drop s" unknown payload type" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SCH-WIDTH ( n -- n )                  \ physical cell width of a field schema node
   dup SCHEMA-APP? IF SCHEMA-A@ TFAM-WIDTH@ EXIT THEN drop 1 ;

\ ---------------------------------------------------------------------------
\ header clauses. Each emits its event through DECL-EVENT (which owns duplicate /
\ ordinal state) and mutates only the fresh family record.
\ ---------------------------------------------------------------------------
: HEADER-ORDER ( -- )                   \ header clauses precede the first variant
   SEEN-VARIANT @ IF
      s" header clause after the first variant" E-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CODE ( ptr u8 n -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop s" missing layout policy name" E-POLICY DECL-REJECT:REJECT throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop TL-STACK-CELL-TAG EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop TL-PACKED-TAG EXIT THEN
   2drop s" unknown layout policy" E-POLICY DECL-REJECT:REJECT throw ;
: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   ED-NEXT POLICY-CODE {: code:n :}
   FAM @ code TFAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )  \ a known/recognised derive feature token
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
: DERIVE-GUARD ( -- )                   \ derive needs a public, concrete (arity 0) family
   FAM @ TFAM-PUBLIC? 0= IF
      s" derive requires a public family" E-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ TFAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family" E-DERIVE DECL-REJECT:REJECT throw THEN ;
: EMIT-DERIVE ( n n -- )                \ ( fam feature-code -- ) emit the DERIVE event
   TOK @ -rot DECL-EVENT:DERIVE TOK ! ;
: DERIVE-ONE ( ptr u8 n -- )            \ apply one feature + emit its event
   DERIVE-GUARD
   2dup s" eq" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-EQ! FAM @ DRV-EQ EMIT-DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop FAM @ TFAM-DERIVE-HASH! FAM @ DRV-HASH EMIT-DERIVE EXIT THEN
   2dup s" order" CORE-STR=CI IF
      2drop s" derive feature not yet supported" E-DERIVE DECL-REJECT:REJECT throw THEN
   2drop s" unknown derive feature" E-DERIVE DECL-REJECT:REJECT throw ;
: DERIVE-CLAUSE ( -- )                  \ DERIVE feature+ : first mandatory, rest by lookahead
   HEADER-ORDER
   ED-NEXT dup 0= IF
      2drop s" missing derive feature" E-DERIVE DECL-REJECT:REJECT throw THEN DERIVE-ONE
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
   SCHEMA-ROOT+ {: sch:n :}                \ ( na nu )
   sch SCHEMA-ROOT@ SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!              \ the field name owns the field record's rejects
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ 2swap sch                \ ( tok fam na nu sch )
   VCELLS @  fw  VCELLS @ CELL *  fw CELL *  CELL  PF-FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw VCELLS @ + VCELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   ED-NEXT dup 0= IF
      2drop s" missing field name" E-SYNTAX DECL-REJECT:REJECT throw THEN   \ field name
   {: na:ptr nu:n :}
   ED-NEXT RESOLVE-TYPE {: node:n :}
   na nu node EMIT-FIELD ;

: VARIANT-NAME ( -- ptr u8 n )          \ next token = variant name (must be present)
   ED-NEXT dup 0= IF
      2drop s" missing variant name" E-SYNTAX DECL-REJECT:REJECT throw THEN ;
: OPEN-VARIANT ( -- )
   VARIANT-NAME {: na:ptr nu:n :}
   na nu DECL-REJECT:TOKEN!             \ the variant name owns the variant registry's rejects
   s" duplicate variant" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ na nu
   DECL-EVENT:VARIANT TOK !             \ SUMV-ADD + set current-variant selector
   0 VCELLS ! ;
: CLOSE-VARIANT ( -- )
   VCELLS @ MAXSLOTS @ max MAXSLOTS !
   TOK @ FAM @ DECL-EVENT:END-VARIANT TOK !     \ clear the current-variant selector
   NVAR @ 1 + NVAR ! ;
: VARIANT-CLAUSE ( -- bool )            \ read one variant-body token; YES = ;VARIANT
   ED-NEXT dup 0= IF 2drop s" missing ;VARIANT" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2drop s" unexpected token in variant block" E-SYNTAX
   DECL-REJECT:REJECT throw ;           \ anonymous payload token / stray token
: VARIANT-BODY ( -- ) BEGIN VARIANT-CLAUSE UNTIL ;
: VARIANT-BLOCK ( -- )
   -1 SEEN-VARIANT !
   OPEN-VARIANT VARIANT-BODY CLOSE-VARIANT ;

\ ---------------------------------------------------------------------------
\ compact body. POLICY and DERIVE use the shared header owner before the first
\ payloadless variant. Full-mode block keywords always reject; a header after a
\ variant rejects through HEADER-ORDER.
\ ---------------------------------------------------------------------------
: COMPACT-KW? ( ptr u8 n -- bool )      \ a block keyword illegal in a compact body
   2dup s" variant" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;variant" CORE-STR=CI IF 2drop YES EXIT THEN
   s" field" CORE-STR=CI ;
: COMPACT-VARIANT ( ptr u8 n -- )       \ register one payloadless variant
   {: na:ptr nu:n :}
   -1 SEEN-VARIANT !
   na nu DECL-REJECT:TOKEN!             \ the variant name owns the variant registry's rejects
   s" duplicate variant" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ na nu
   DECL-EVENT:VARIANT TOK !
   TOK @ FAM @ DECL-EVENT:END-VARIANT TOK !
   NVAR @ 1 + NVAR ! ;
: COMPACT-CLAUSE ( ptr u8 n -- bool )    \ dispatch one compact token; YES = ;ENUM
   2dup DECL-REJECT:TOKEN!               \ the mode-selecting token arrives pre-read
   2dup s" ;enum" CORE-STR=CI IF 2drop -1 SEEN-END ! YES EXIT THEN
   2dup COMPACT-KW? IF
      2drop s" block keyword in a compact enum" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   COMPACT-VARIANT NO ;

\ ---------------------------------------------------------------------------
\ transaction orchestration.
\ ---------------------------------------------------------------------------
: VIS ( -- n )                          \ declaration visibility (public at top level)
   CHECKER-AUTH-PACKAGE-ACTIVE? 0= IF CHECKER-PACKAGE-PUBLIC EXIT THEN CHECKER-AUTH-PACKAGE-MODE@ ;
: OPEN-TX ( -- )                        \ capture bases, open the event tx, emit DECL
   SUMV-N@ VBASE !
   TYPE-FIELD:COUNT FLDBASE !
   0 NVAR !   0 NFLD !   0 MAXSLOTS !
   DECL-EVENT:CURRENT TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK ! ;
: REGISTER-FULL ( ptr u8 n n -- )          \ ( na nu arity -- ) TK-SUM family, arity header event
   {: na:ptr nu:n ar:n :}
   ar ED-ARITY !
   na nu DECL-REJECT:TOKEN!             \ the family name owns the registry's rejects
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   ar TK-SUM TFAM-DECL FAM !
   OPEN-TX
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;
: REGISTER-COMPACT ( ptr u8 n -- )         \ ( na nu -- ) TK-ENUM family, arity 0, no header event
   {: na:ptr nu:n :}
   0 ED-ARITY !
   na nu DECL-REJECT:TOKEN!             \ the family name owns the registry's rejects
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   TFAM-ACTIVE-PKG$ VIS na nu
   0 TK-ENUM TFAM-DECL FAM !
   OPEN-TX ;

\ ---------------------------------------------------------------------------
\ constructor generation. This front end still renders nothing itself: once the
\ variant range, the field range, and the payload width are bound, it names the
\ family to the ORDER 820 constructor participant and the declaration transaction
\ does the work in its commit phase, after DECL-EVENT has promoted this
\ declaration's TYPE-FIELD rows to the committed watermark the shared generator
\ reads. So a rendering, evaluation, or certification failure anywhere in the set
\ rolls every participant back and publishes no constructor at all.
\
\ The gate itself lives with the participant (GENERATED-DECL-CTOR:OWNS?) so that
\ one predicate covers arming here and the participant's own re-proof, and both
\ ENUM modes pass it. Both, because the global-token cutover's acceptance is that
\ every existing plain ENUM behaves identically through this front end, and the
\ legacy sumtype.f definer already publishes constructors for a compact
\ payloadless enum: measured on the parent, a legacy-declared `LGC:RED` resolves
\ while the same compact family through ED-RUN was E-UNDEFINED. Arming only the
\ full TK-SUM mode would leave compact enums a parity gap the cutover could never
\ close.
\ ---------------------------------------------------------------------------
\ The packed memory descriptor is baked HERE, at the same point the legacy
\ definer bakes it (sumtype.f CHECKER-DEFENUM-BODY, between TFAM-VAR-RANGE! /
\ TFAM-SLOTS! and the constructor publish): PACKED-DESC reads the variant count
\ and the payload slot width, so both must already be bound, and a `POLICY
\ stack-cell-tag` family bakes no row at all. LAY-ADD lands inside the checker
\ participant's savepoint (LAY-N is one of the marks TF-SAVE/TF-RESTORE carry),
\ so a later close-stage reject retires the descriptor with the family.
: ED-CLOSE ( -- )                          \ bind variant + field ranges + width, bake layout, arm generation
   DECL-REJECT:AT-FAMILY                    \ close-stage faults belong to the whole declaration
   NVAR @ 0= IF s" empty enum" E-SYNTAX DECL-REJECT:REJECT throw THEN   \ needs a variant
   FAM @ VBASE @ NVAR @ TFAM-VAR-RANGE!
   FAM @ FLDBASE @ NFLD @ TFAM-FLD-RANGE!
   FAM @ MAXSLOTS @ TFAM-SLOTS!
   FAM @ TDECL-LAYOUT-DESC
   \ No reason is armed for the constructor participant's collide check. That
   \ check runs two phases later, in the commit phase, and nothing between here
   \ and there is a token this front end holds, so an arming made here would
   \ have to cover the whole of generation and would then be inherited by any
   \ other reserved-name reject generation raises. The packet answers that class
   \ from its code table instead: less specific than sumtype.f's own wording,
   \ and true for every reserved-name reject generation can produce.
   FAM @ GENERATED-DECL-CTOR:OWNS? IF FAM @ GENERATED-DECL-CTOR:ARM THEN ;

: FULL-CLAUSE ( -- bool )               \ read + dispatch one full-body token; YES = ;ENUM
   ED-NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;ENUM" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;enum" CORE-STR=CI IF 2drop -1 SEEN-END ! YES EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2dup s" variant" CORE-STR=CI IF 2drop VARIANT-BLOCK NO EXIT THEN
   2drop s" unexpected token in enum declaration" E-SYNTAX
   DECL-REJECT:REJECT throw ;           \ arity-then-compact / stray / mixed-legacy token
: FULL-BODY ( -- ) BEGIN FULL-CLAUSE UNTIL ;

: COMPACT-BODY ( ptr u8 n -- )          \ first variant token in hand, then loop to ;ENUM
   BEGIN
      dup 0= IF 2drop
         DECL-REJECT:AT-FAMILY
         s" missing ;ENUM" E-SYNTAX DECL-REJECT:REJECT throw THEN
      COMPACT-CLAUSE IF EXIT THEN
      ED-NEXT
   AGAIN ;

: DRIVE ( -- )                          \ name + mode select + register + body
   ED-NEXT 2dup DECL-REJECT:FAMILY!     \ ( na nu )  keep the span
   2dup REQUIRE-NAME                    \ named before validation, so a bad name is reported
   {: na:ptr nu:n :}
   ED-NEXT {: ta:ptr tu:n :}            \ first body token selects the mode
   ta tu ED-ALLDIG? IF
      ta tu PARSE-ARITY {: ar:n :}
      na nu ar REGISTER-FULL
      FULL-BODY
   ELSE
      na nu REGISTER-COMPACT
      ta tu COMPACT-BODY
   THEN
   ED-CLOSE ;

\ One provisional transaction: commit by persisting, roll the family + schema +
\ layout + variant + event stream back to a byte-identical registry on any reject.
: ED-BODY ( -- )
   [: ED-RESET DRIVE ;] GENERATED-DECL:RUN ;

\ Resynchronize the input to the end of THIS declaration.
\
\ It matters only when the reject will be swallowed — a multi-error load, where
\ the interpreter carries on with whatever the stream holds next. The legacy
\ definer got this for free: it copied every token through `;ENUM` into a buffer
\ before parsing any of it, so a swallowed reject always left the stream past the
\ terminator. This front end reads as it parses and stops at the fault, so the
\ tokens between the fault and `;ENUM` are still ahead of the interpreter, which
\ would try to execute them. Measured on the commit before this one:
\ `ENUM m7bad red red blue ;ENUM NEWTYPE m7cont 1` reported the duplicate
\ variant, counted it, and then died on `E-UNDEFINED: blue` — the load aborted
\ where the legacy definer continued and declared `m7cont`.
\
\ The terminator is this declaration's own boundary, so skipping to it consumes
\ exactly what belongs to it and nothing more. Two cases need no skip and get
\ none: a reject raised after `;ENUM` was already consumed (SEEN-END — skipping
\ then would eat the NEXT declaration), and a reject raised because the input
\ ended without a terminator (the scan simply meets end of input). Tokens are
\ read through ED-RAW rather than ED-NEXT so the packet keeps naming the token
\ that actually provoked the reject; GUARD renders it after this returns.
: ED-SKIP-BODY ( -- )
   BEGIN
      ED-RAW dup 0= IF 2drop EXIT THEN
      2dup s" ;enum" CORE-STR=CI IF 2drop EXIT THEN
      2drop
   AGAIN ;
: ED-RESYNC ( -- )
   SEEN-END @ IF EXIT THEN
   DECL-REJECT:MULTI-ERROR? 0= IF EXIT THEN
   ED-SKIP-BODY ;

\ A reject is rendered through the shared declaration packet AFTER the
\ coordinator has rolled everything back, then rethrown with its exact code,
\ which is the same order the legacy definers use (sumtype.f TDECL-RUN:
\ restore, report, rethrow). Both drivers below share this one guarded body, so
\ a replayed declaration reports through exactly the same renderer.
: ED-DRIVE ( -- )                      \ body, then resynchronize before reporting
   [: ED-BODY ;] catch {: rc:n :}
   rc 0= IF EXIT THEN
   ED-RESYNC
   rc throw ;
: ED-GUARDED ( -- )
   [: ED-DRIVE ;] DECL-REJECT:GUARD ;

\ The replay stream is retired on BOTH exits. Closing only on success would
\ leave a rejected replay installed, and the next live ENUM would then read its
\ tokens from a spent buffer instead of the input source.
: ED-REPLAY-END ( n -- )               \ ( caught-code -- ) close, then re-raise unchanged
   DECL-REPLAY:RP-RELEASE
   dup 0= IF drop EXIT THEN
   throw ;

public

: ED-RUN ( -- )
   s" enum" DECL-REJECT:OPEN
   ED-GUARDED ;

\ ED-REPLAY ( name body -- ) : register an ENUM from tokens a tool has already
\ lexed, defining no word. Same grammar, same validation, same registry writes,
\ same reject packet as ED-RUN — only the token source differs and the variant
\ constructors are registered without being rendered (the ORDER 820 participant
\ in generated-declaration.f). BOTH modes replay: the first body token still
\ selects full or compact exactly as it does live, so this entry replaces the
\ legacy compact-only CHECKER-DEFENUM that tools/check-core.f and
\ src/habu/verify-source.f drive today. The body buffer is the declaration body
\ INCLUDING its ;ENUM terminator; a buffer without one rejects through the front
\ end's own "missing ;ENUM" gate exactly as a truncated live declaration does.
\
\ The stream is claimed BEFORE the packet is opened. Claiming can fail — another
\ replay is already installed — and that failure belongs to the caller that
\ misused the entry, not to any declaration: it must not clear the packet of the
\ declaration that owns the live stream, and it must not close that stream on the
\ way out. So it propagates as its own code with no packet of its own, and only a
\ claim that SUCCEEDED reaches the close below.
: ED-REPLAY ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ba:ptr bu:n :}
   na nu ba bu DECL-REPLAY:RP-CLAIM
   s" enum" DECL-REJECT:OPEN
   [: ED-GUARDED ;] catch
   ED-REPLAY-END ;

;package

\ ENUM is the global sum/enum declaration keyword and therefore a documented
\ global language surface (the same package-first exception STRUCTURE carries):
\ it parses its own body up to ;ENUM at interpret time, so its checked effect is
\ ( -- ). This is the sole global entry — there is no alias and no second parser.
\ A compact body registers the TK-ENUM family sumtype.f's definer used to
\ register; a body whose first token is an arity registers the full TK-SUM form
\ with named variants and named FIELD payloads, which the compact grammar could
\ not express at all.
\ ENUM type-name (variant)+ ;ENUM
\ ENUM type-name arity [POLICY p] [DERIVE f+] (VARIANT name (FIELD n t)* ;VARIANT)+ ;ENUM
: ENUM ( -- ) ENUM-DECL:ED-RUN ;
