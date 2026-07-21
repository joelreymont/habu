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
\ ---------------------------------------------------------------------------
\ COMMIT / ROLLBACK COMPOSITION (settled empirically; see the report note).
\ ---------------------------------------------------------------------------
\ A top-level composite declaration runs at interpret time and persists UNFRAMED,
\ exactly like the shipped SUMTYPE/PRODUCT definers: the checker candidate frame
\ (CHECK-CANDIDATE-START/DONE) ALWAYS rolls back on close (a rejected scope AND a
\ successful candidate probe both restore — docs/type-families.md §21.1), so it
\ can never COMMIT a successful family, and wrapping the transaction in it would
\ retire the very family + field rows this front end must publish for the
\ downstream generate-field / generate-make / prove-generic lanes to read. The
\ front end therefore commits by simply persisting and rolls back only on reject:
\   - The event stream + provisional field rows are committed by DECL-EVENT:PUBLISH
\     and retired by DECL-EVENT:ROLLBACK (the event module owns that boundary,
\     including the field record's own PF transaction and the field-name interns).
\   - The family row, its schema roots, its layout descriptor, and the family-name
\     intern are NOT owned by the event module. They are snapshotted at open
\     (SD-MARK) and restored on reject (SD-RESTORE) — the same mark/restore idiom
\     the shipped sumtype.f TDECL-MARK/RESTORE uses, reached here through named
\     trusted forwarders because these registry cursors are REG-PROTECT name-sealed
\     against post-hook interpret-mode writes (compiled references from a TRUSTED:
\     body are the sanctioned boundary, exactly as decl-event.f forwards PF-*).
\ SD-RESTORE runs AFTER DECL-EVENT:ROLLBACK so the earlier (pre-family) string-pool
\ high-water wins over the event module's (post-family-name) one.
\
\ ---------------------------------------------------------------------------
\ CONSTRUCTOR GENERATION (the ;STRUCTURE -> STRUCTURE-MAKE seam).
\ ---------------------------------------------------------------------------
\ Once SD-CLOSE has bound the family field range + width and DECL-EVENT:PUBLISH
\ has committed the field rows, ;STRUCTURE hands the fully published family id to
\ STRUCTURE-MAKE:GENERATE (src/core/structure-make.f), which defines the sealed
\ FAMILY:MAKE / FAMILY:UNMAKE constructor package from the committed field
\ schemas. Two spec conditions gate the call (SD-MAKEABLE?), so GENERATE is only
\ invoked when its own contract already holds:
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
\ Under that gate GENERATE composes INFALLIBLY after PUBLISH: every reject it can
\ raise is structurally impossible for a just-published structure. The family is
\ live (SD-RESTORE only retires it on a reject, and this is the success path) and
\ product-kind (SD-REGISTER registered it TK-PROD), so E-SM-FAM cannot fire; the
\ gate guarantees at least one field, so E-SM-EMPTY cannot fire; DECL-EVENT:PUBLISH
\ committed every field row in [FLDBASE, FLDBASE+NFLD), so the committed-field
\ reader never throws E-PF-ID; and a duplicate family name already rejected at
\ TFAM-DECL inside SD-REGISTER, so this is the family's first and only generation
\ and E-SM-DUP cannot fire. The call therefore owns no rollback path: it runs
\ inside SD-RUN's catch only because SD-CLOSE does, and it cannot throw for a
\ well-formed declaration — exactly how the product ctor path relies on generation
\ being infallible once its declaration validated.
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

\ --- registry rollback snapshot. The seven cursors a declaration can grow that
\ the event module does not own (family, string pool, param pool, variant pool,
\ layout pool, schema node pool, schema-root pool). Straight cell reads/writes:
\ no locals, no control, so the trusted boundary stays minimal (docs/forth.md
\ "Keep TRUSTED: bodies syntax-simple").
variable M-TFAM   variable M-STR   variable M-PK   variable M-SUMV
variable M-LAY    variable M-SCH   variable M-ROOT
TRUSTED: SD-MARK ( -- )
   TFAM-N @ M-TFAM !   TF-STR-U @ M-STR !   TF-PK-N @ M-PK !
   SUMV-N @ M-SUMV !   LAY-N @ M-LAY !   SCH-N @ M-SCH !   SCH-ROOT-N @ M-ROOT ! ;
TRUSTED: SD-RESTORE ( -- )
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

: SD-NEXT ( -- ptr u8 n )              \ next body token (honours one pushback)
   PEND-U @ 0 > IF PEND@ 0 PEND-U ! EXIT THEN
   parse-name ;
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
   dup 0= IF 2drop E-SYNTAX throw THEN
   2dup CANON? 0= IF 2drop E-CASE throw THEN
   NAME-RESERVED? IF E-NAME throw THEN ;

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
: PARSE-ARITY ( ptr u8 n -- n )
   dup 0= IF 2drop E-ARITY throw THEN
   2dup SD-ALLDIG? 0= IF 2drop E-ARITY throw THEN
   DEC dup TFAM-DECL-PARAM-COUNT > IF drop E-ARITY throw THEN ;

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
   TFAM-DECL-CHAR>PARAM 0= IF drop E-PAYLOAD throw THEN
   dup SD-ARITY @ < IF SD-SCH-PARAM EXIT THEN
   drop
   E-PAYLOAD throw ;

: RESOLVE-TYPE ( ptr u8 n -- n )        \ type token(s) -> schema node
   dup 0= IF 2drop E-SYNTAX throw THEN
   2dup s" ptr" CORE-STR=CI IF 2drop SD-NEXT RECURSE SD-SCH-PTR EXIT THEN
   dup 1 = IF LETTER-TYPE EXIT THEN
   2dup CON-CODE dup 0 <> IF nip nip SD-SCH-CON EXIT THEN drop
   2dup FIELD-FAM? IF nip nip 0 0 SD-SCH-APP EXIT THEN drop
   2drop E-PAYLOAD throw ;

: SCH-WIDTH ( n -- n )                  \ physical cell width of a field schema node
   dup SCH-APP? IF SCH-A@ FAM-WIDTH@ EXIT THEN drop 1 ;

\ ---------------------------------------------------------------------------
\ clause drivers. Each emits its event through DECL-EVENT (which owns duplicate /
\ ordinal / selector state) and mutates only the fresh family record.
\ ---------------------------------------------------------------------------
: HEADER-ORDER ( -- )                   \ header clauses precede the first field
   SEEN-FIELD @ IF E-SYNTAX throw THEN ;

: POLICY-CODE ( ptr u8 n -- n )         \ policy name -> layout code (or reject)
   dup 0= IF 2drop E-POLICY throw THEN
   2dup s" stack-cell-tag" CORE-STR=CI IF 2drop LT-STACK EXIT THEN
   2dup s" packed-tag" CORE-STR=CI IF 2drop LT-PACKED EXIT THEN
   2drop E-POLICY throw ;
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
   SD-NEXT dup 0= IF 2drop E-DERIVE throw THEN DERIVE-ONE
   BEGIN SD-NEXT dup 0= IF 2drop EXIT THEN
      2dup DERIVE-FEATURE? IF DERIVE-ONE ELSE UNGET EXIT THEN
   AGAIN ;

: EMIT-FIELD ( ptr u8 n n -- )          \ ( na nu node -- ) layout + drive the field event
   SCH-ROOT+ {: sch:n :}                \ ( na nu )
   sch SCH-ROOT@ SCH-WIDTH {: fw:n :}
   TOK @ FAM @ 2swap sch                \ ( tok fam na nu sch )
   SD-CELLS @  fw  SD-CELLS @ CELL *  fw CELL *  CELL  FLAGS-NONE
   DECL-EVENT:FIELD TOK !
   fw SD-CELLS @ + SD-CELLS !
   NFLD @ 1 + NFLD ! ;
: FIELD-CLAUSE ( -- )
   SD-NEXT dup 0= IF 2drop E-SYNTAX throw THEN   \ field name
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
   ACTIVE-PKG$ VIS na nu
   ar TK-PROD FAM-DECL FAM !
   TYPE-FIELD:COUNT FLDBASE !
   0 NFLD !   0 SD-CELLS !
   DECL-EVENT:OPEN TOK !
   TOK @ FAM @ DECL-EVENT:DECL TOK !
   TOK @ FAM @ ar DECL-EVENT:ARITY TOK ! ;

: SD-MAKEABLE? ( -- bool )                 \ a public structure WITH fields owns a MAKE/UNMAKE package
   FAM @ FAM-PUBLIC? NFLD @ 0 > and ;
: SD-CLOSE ( -- )                          \ bind field range + width, publish, then generate the ctors
   FAM @ FLDBASE @ NFLD @ FAM-FLD-RANGE!
   FAM @ SD-CELLS @ FAM-SLOTS!
   TOK @ DECL-EVENT:PUBLISH
   SD-MAKEABLE? IF FAM @ STRUCTURE-MAKE:GENERATE THEN ;   \ infallible here (see header seam note)

: CLAUSE ( -- bool )                    \ read + dispatch one body token; YES = ;STRUCTURE
   SD-NEXT dup 0= IF 2drop E-SYNTAX throw THEN
   2dup s" ;structure" CORE-STR=CI IF 2drop SD-CLOSE YES EXIT THEN
   2dup s" field" CORE-STR=CI IF 2drop FIELD-CLAUSE NO EXIT THEN
   2dup s" policy" CORE-STR=CI IF 2drop POLICY-CLAUSE NO EXIT THEN
   2dup s" derive" CORE-STR=CI IF 2drop DERIVE-CLAUSE NO EXIT THEN
   2drop E-SYNTAX throw ;               \ unexpected / mixed-legacy token at the exact token
: CLAUSES ( -- ) BEGIN CLAUSE UNTIL ;

: DRIVE ( -- )                          \ name + arity + register + body
   parse-name 2dup REQUIRE-NAME         \ ( na nu )  keep the span
   parse-name PARSE-ARITY               \ ( na nu arity )
   SD-REGISTER
   CLAUSES ;

public

\ One provisional transaction: commit by persisting, roll the family + schema +
\ layout + event stream back to a byte-identical registry on any reject.
: SD-RUN ( -- )
   SD-RESET
   SD-MARK
   [: DRIVE ;] catch {: rc:n :}
   rc 0= IF EXIT THEN
   TOK @ 0 <> IF TOK @ DECL-EVENT:ROLLBACK THEN
   SD-RESTORE
   rc throw ;

;package

\ STRUCTURE is the one executable composite-declaration keyword and therefore a
\ documented global language surface (package-first exception, like the shipped
\ SUMTYPE/PRODUCT openers): it parses its own body up to ;STRUCTURE at interpret
\ time, so its checked effect is ( -- ).
\ STRUCTURE type-name arity [POLICY p] [DERIVE f+] (FIELD name type)* ;STRUCTURE
: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;
