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
\   header-clause = POLICY policy-name | DERIVE derive-name+
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
\ STRUCTURE-MAKE:GENERATE to define the FAMILY:MAKE / FAMILY:UNMAKE constructor
\ package from the still-provisional field schemas. Two conditions gate the call:
\   - PUBLIC only. GENERATE requires a public family. A private structure gets no
\     construction surface, matching the shipped product precedent: sumtype.f's
\     TDECL-CTOR-PUBLISH / TDECL-PROD-WORDS simply exit for a non-public family,
\     so private products publish no MAKE/UNMAKE (fail-closed, sumtype.f note
\     "private products have no construction surface"). Package-scoped private
\     generation is deferred type-DSL work, not invented here.
\   - FIELDS only. A zero-field structure is an opaque one-cell family that
\     publishes no constructor (docs/type-families.md §2.2 — the authority-safe
\     TYPEFAMILY replacement); only a declaration WITH fields is a product with a
\     MAKE/UNMAKE pair, so GENERATE is skipped when NFLD is zero.
\ Generation remains inside the shared transaction. If evaluation or checker
\ certification rejects either constructor, the coordinator restores every
\ participant before the error returns to the caller.
\
\ Loaded AFTER the checker hook, AFTER decl-event.f (it drives DECL-EVENT:*), and
\ AFTER structure-make.f (its ;STRUCTURE calls STRUCTURE-MAKE:GENERATE, so the
\ generator must be defined first); the STRUCTURE opener is the only executable
\ STRUCTURE declaration surface.

package STRUCTURE-DECL

\ --- named reject codes. Re-declared package-locally (values mirror sumtype.f
\ E-TDECL-*: 7107/7108/7109/7110/7116/7119 and type-family.f E-TFAM-CASE 7101),
\ because the global pre-hook constants that own them are removed by the type-DSL
\ cutover and do not survive the checked engine's fixpoint self-rebuild (the same
\ reason decl-event.f re-declares its E-DEV-* codes). E-TFAM-DUP (family duplicate,
\ 7102) and the field record's E-PF-NAME / E-TFAM-DUP / E-TFAM-CASE name-gate
\ codes are raised by TFAM-DECL / the field-event path and pass through unchanged.
7107 constant E-SYNTAX      \ malformed: missing name/arity/terminator, unexpected/legacy token
7108 constant E-ARITY       \ arity token is not a small decimal in [0, cap]
7109 constant E-PAYLOAD     \ unresolved / unknown field type
7110 constant E-NAME        \ reserved or colliding family name
7116 constant E-POLICY      \ unknown or not-yet-supported layout policy
7119 constant E-DERIVE      \ unknown or not-yet-supported derive feature
7101 constant E-CASE        \ family name is not a lowercase canonical tail
7102 constant E-DUP         \ duplicate family or field tail (type-family.f E-TFAM-DUP,
                            \ raised by TFAM-DECL / the field-event path; named here only
                            \ so a reason can be armed for it before those calls)

110 constant ASCII-N
102 constant ASCII-F
114 constant ASCII-R

\ typed boolean producers (core has no `true`/`false`).
: YES ( -- bool ) 0 0= ;
: NO ( -- bool ) 0 0= 0= ;

\ ---------------------------------------------------------------------------
\ Trusted forwarders to the pre-hook registry / schema / checker words. These
\ are raw-memory / metaprogramming boundaries the checker cannot type from a
\ post-hook checked body; the compiled reference survives the name seal exactly
\ as decl-event.f's DEV-FLD-* forwarders and top-row.f's effect-read boundary do.
\ ---------------------------------------------------------------------------
TRUSTED: FAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: FAM-LAYOUT! ( n n -- ) TFAM-LAYOUT! ;
TRUSTED: FAM-EQ! ( n -- ) TFAM-DERIVE-EQ! ;
TRUSTED: FAM-HASH! ( n -- ) TFAM-DERIVE-HASH! ;
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
TRUSTED: CON-N ( -- n ) CC-N ;          \ single-letter n : signed cell
TRUSTED: CON-BOOL ( -- n ) CC-BOOL ;    \ single-letter f : boolean/flag
TRUSTED: CON-R ( -- n ) CC-R ;          \ single-letter r : real/float
TRUSTED: LT-STACK ( -- n ) TL-STACK-CELL-TAG ;   \ default layout policy code
TRUSTED: LT-PACKED ( -- n ) TL-PACKED-TAG ;      \ packed-tag layout policy code
TRUSTED: DV-EQ ( -- n ) DRV-EQ ;                 \ derive feature code: equality
TRUSTED: DV-HASH ( -- n ) DRV-HASH ;             \ derive feature code: hash
TRUSTED: PKG-PUBLIC ( -- n ) CHECKER-PACKAGE-PUBLIC ;   \ public visibility code
TRUSTED: SD-SCH-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: SD-SCH-PARAM ( n -- n ) SCHEMA-PARAM ;
TRUSTED: SD-SCH-PTR ( n -- n ) SCHEMA-PTR ;
TRUSTED: SD-SCH-APP ( n n n -- n ) SCHEMA-APP ;
TRUSTED: SCH-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: SCH-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: SCH-APP? ( n -- bool ) SCHEMA-APP? ;
TRUSTED: SCH-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: FLAGS-NONE ( -- n ) PF-FLAGS-NONE ;   \ field-record layout flag: none
TRUSTED: TK-PROD ( -- n ) TK-PRODUCT ;         \ single-shape record family kind

\ --- one-token pushback. The token bytes stay valid across a line refill (the
\ engine buffers the input source), so the pushback holds the raw span; storing a
\ ptr u8 through a plain cell and reading it back is the one place the checker
\ needs a named boundary.
variable PEND-U   variable PEND-A
TRUSTED: PEND! ( ptr u8 n -- ) PEND-U ! PEND-A ! ;
TRUSTED: PEND@ ( -- ptr u8 n ) PEND-A @ PEND-U @ ;

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
variable SD-SI         \ private digit-scan index

: SD-RESET ( -- )                      \ base state; re-seeded at load (process-local)
   0 PEND-U !   0 TOK !   0 SEEN-FIELD ! ;
SD-RESET

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
\ name gate: a reserved family name is a grammar keyword, the STRUCTURE openers,
\ a single-character token (would collide with a type letter / arity param), or a
\ concrete checker type name. Case + duplicate are enforced by TFAM-DECL itself.
\ ---------------------------------------------------------------------------
: NAME-RESERVED? ( ptr u8 n -- bool )
   2dup GRAMMAR-KW? IF 2drop YES EXIT THEN
   2dup s" structure" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop YES EXIT THEN
   dup 1 = IF 2drop YES EXIT THEN
   CON-CODE 0 <> ;

: REQUIRE-NAME ( ptr u8 n -- )      \ validate the family name (throws; consumes the copy)
   dup 0= IF 2drop s" missing name" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup CANON? 0= IF
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
\ field type resolution -> a schema node (docs §8): concrete cell types (n/f/r +
\ multi-char con names), positional letter params within arity, ptr T, and closed
\ non-linear arity-0 layout/cell families. Everything else is unresolved.
\ ---------------------------------------------------------------------------
: FIELD-FAM? ( ptr u8 n -- n bool )     \ resolve a closed arity-0 layout/cell family
   ACTIVE-PKG$ 2swap SIG-RESOLVE 0= IF drop 0 NO EXIT THEN
   {: id:n :}
   id FAM-LAYOUT? id FAM-CELL? or 0= IF 0 NO EXIT THEN
   id FAM-ARITY@ 0 <> IF 0 NO EXIT THEN
   id FAM-LINEAR? IF 0 NO EXIT THEN
   id YES ;

: LETTER-TYPE ( ptr u8 n -- n )         \ single-char type: param / n / f / r
   drop c@
   dup ASCII-N = IF drop CON-N SD-SCH-CON EXIT THEN
   dup ASCII-F = IF drop CON-BOOL SD-SCH-CON EXIT THEN
   dup ASCII-R = IF drop CON-R SD-SCH-CON EXIT THEN
   TFAM-DECL-CHAR>PARAM 0= IF
      drop s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw THEN
   dup SD-ARITY @ < IF SD-SCH-PARAM EXIT THEN
   drop
   s" type parameter is outside the declared arity" E-PAYLOAD DECL-REJECT:REJECT throw ;

: RESOLVE-TYPE ( ptr u8 n -- n )        \ type token(s) -> schema node
   dup 0= IF 2drop s" missing field type" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ptr" CORE-STR=CI IF 2drop SD-NEXT RECURSE SD-SCH-PTR EXIT THEN
   dup 1 = IF LETTER-TYPE EXIT THEN
   2dup CON-CODE dup 0 <> IF nip nip SD-SCH-CON EXIT THEN drop
   2dup FIELD-FAM? IF nip nip 0 0 SD-SCH-APP EXIT THEN drop
   2drop s" unknown field type" E-PAYLOAD DECL-REJECT:REJECT throw ;

: SCH-WIDTH ( n -- n )                  \ physical cell width of a field schema node
   dup SCH-APP? IF SCH-A@ FAM-WIDTH@ EXIT THEN drop 1 ;

\ ---------------------------------------------------------------------------
\ clause drivers. Each emits its event through DECL-EVENT (which owns duplicate /
\ ordinal / selector state) and mutates only the fresh family record.
\ ---------------------------------------------------------------------------
: HEADER-ORDER ( -- )                   \ header clauses precede the first field
   SEEN-FIELD @ IF
      s" header clause after the first field" E-SYNTAX DECL-REJECT:REJECT throw THEN ;

: POLICY-CODE ( ptr u8 n -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop s" missing layout policy name" E-POLICY DECL-REJECT:REJECT throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop LT-STACK EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop LT-PACKED EXIT THEN
   2drop s" unknown layout policy" E-POLICY DECL-REJECT:REJECT throw ;
: POLICY-CLAUSE ( -- )
   HEADER-ORDER
   SD-NEXT POLICY-CODE {: code:n :}
   FAM @ code FAM-LAYOUT!
   TOK @ FAM @ code DECL-EVENT:POLICY TOK ! ;

: DERIVE-FEATURE? ( ptr u8 n -- bool )  \ a known/recognised derive feature token
   2dup s" eq" CORE-STR=CI IF 2drop YES EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop YES EXIT THEN
   s" order" CORE-STR=CI ;
: DERIVE-GUARD ( -- )                   \ derive needs a public, concrete (arity 0) family
   FAM @ FAM-PUBLIC? 0= IF
      s" derive requires a public family" E-DERIVE DECL-REJECT:REJECT throw THEN
   FAM @ FAM-ARITY@ 0 <> IF
      s" derive requires a concrete (arity 0) family" E-DERIVE DECL-REJECT:REJECT throw THEN ;
: EMIT-DERIVE ( n n -- )                \ ( fam feature-code -- ) emit the DERIVE event
   TOK @ -rot DECL-EVENT:DERIVE TOK ! ;
: DERIVE-ONE ( ptr u8 n -- )            \ apply one feature + emit its event
   DERIVE-GUARD
   2dup s" eq" CORE-STR=CI IF 2drop FAM @ FAM-EQ! FAM @ DV-EQ EMIT-DERIVE EXIT THEN
   2dup s" hash" CORE-STR=CI IF 2drop FAM @ FAM-HASH! FAM @ DV-HASH EMIT-DERIVE EXIT THEN
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
   SCH-ROOT+ {: sch:n :}                \ ( na nu )
   sch SCH-ROOT@ SCH-WIDTH {: fw:n :}
   2dup DECL-REJECT:TOKEN!              \ the field name owns the field record's rejects
   s" duplicate field name" E-DUP DECL-REJECT:EXPECT
   TOK @ FAM @ 2swap sch                \ ( tok fam na nu sch )
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   SD-NEXT dup 0= IF
      2drop s" missing field name" E-SYNTAX DECL-REJECT:REJECT throw THEN   \ field name
   {: na:ptr nu:n :}
   SD-NEXT RESOLVE-TYPE {: node:n :}
   na nu node EMIT-FIELD
   -1 SEEN-FIELD ! ;

\ ---------------------------------------------------------------------------
\ transaction orchestration.
\ ---------------------------------------------------------------------------
: VIS ( -- n )                          \ declaration visibility (public at top level)
   PKG-ACTIVE? 0= IF PKG-PUBLIC EXIT THEN PKG-MODE@ ;
: SD-REGISTER ( ptr u8 n n -- )            \ ( na nu arity -- ) register the family, open the tx
   {: na:ptr nu:n ar:n :}
   ar SD-ARITY !
   na nu DECL-REJECT:TOKEN!             \ the family name owns the registry's rejects
   s" duplicate family" E-DUP DECL-REJECT:EXPECT
   ACTIVE-PKG$ VIS na nu
   ar TK-PROD FAM-DECL FAM !
   TYPE-FIELD:COUNT FLDBASE !
   0 NFLD !   0 SD-CELLS !
   DECL-EVENT:CURRENT TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK !
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;

: SD-MAKEABLE? ( -- bool )                 \ a public structure WITH fields owns a MAKE/UNMAKE package
   FAM @ FAM-PUBLIC? NFLD @ 0 > and ;
: SD-CLOSE ( -- )                          \ bind field range + width, then generate the ctors
   DECL-REJECT:AT-FAMILY                   \ close-stage faults belong to the whole declaration
   FAM @ FLDBASE @ NFLD @ FAM-FLD-RANGE!
   FAM @ SD-CELLS @ FAM-SLOTS!
   \ No reason is armed for the generator's own rejects, for the same reason
   \ ED-CLOSE arms none: they are raised past the last token this front end
   \ holds, so an arming here would cover all of generation rather than the one
   \ fault it names. The packet answers them from its code table.
   SD-MAKEABLE? IF TOK @ FAM @ STRUCTURE-MAKE:GENERATE THEN ;

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   SD-NEXT dup 0= IF 2drop
      DECL-REJECT:AT-FAMILY
      s" missing ;STRUCTURE" E-SYNTAX DECL-REJECT:REJECT throw THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop SD-CLOSE YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
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
   [: SD-RESET DRIVE ;] GENERATED-DECL:RUN ;

\ A reject is rendered through the shared declaration packet AFTER the
\ coordinator has rolled everything back, then rethrown with its exact code,
\ which is the same order the legacy definers use (sumtype.f TDECL-RUN:
\ restore, report, rethrow). Both drivers below share this one guarded body, so
\ a replayed declaration reports through exactly the same renderer.
: SD-GUARDED ( -- )
   [: SD-BODY ;] DECL-REJECT:GUARD ;

\ The replay stream is retired on BOTH exits. Closing only on success would
\ leave a rejected replay installed, and the next live STRUCTURE would then read
\ its tokens from a spent buffer instead of the input source.
: SD-REPLAY-END ( n -- )               \ ( caught-code -- ) close, then re-raise unchanged
   DECL-REPLAY:RP-RELEASE
   dup 0= IF drop EXIT THEN
   throw ;

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
: SD-REPLAY ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ba:ptr bu:n :}
   na nu ba bu DECL-REPLAY:RP-CLAIM
   s" structure" DECL-REJECT:OPEN
   [: SD-GUARDED ;] catch
   SD-REPLAY-END ;

;package

\ STRUCTURE is the one executable composite-declaration keyword and therefore a
\ documented global language surface (package-first exception, like the shipped
\ SUMTYPE/PRODUCT openers): it parses its own body up to ;STRUCTURE at interpret
\ time, so its checked effect is ( -- ).
\ STRUCTURE type-name arity [POLICY p] [DERIVE f+] (FIELD name type)* ;STRUCTURE
: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;
