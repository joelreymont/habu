\ decl-event.f — shared typed-declaration syntax-event transaction (package
\ DECL-EVENT). ONE transaction, event log, rollback/publication boundary, and
\ read-only event reflection that BOTH post-hook declarers (STRUCTURE and ENUM)
\ drive, so neither front end duplicates parser state or publication logic
\ (MODEL-CAD-V2-PLAN.md §3.1; docs/type-families.md §2.5; dot
\ habu-type-declarations-shared-14ab0e48). This file owns NO front end, no
\ parser state, no cold descriptors, no legacy syntax: the two front ends
\ (structure-decl.f / enum-decl.f, later dots) only recognise tokens and drive
\ the words below; ALL declaration state lives here.
\
\ A declaration emits an ordered stream of syntax events into one growable,
\ pointer-free record arena. Both front ends emit IDENTICAL field events: a
\ STRUCTURE product field (current variant = NO-VARIANT) and an ENUM variant
\ payload field (current variant = the open variant id) both produce one
\ DEV-K-FIELD event and one field-record row with the SAME shape; only the
\ variant selector — held here, not by the front end — differs. Downstream
\ consumers (constructors, MATCH, codecs, snapshots, AOT) walk one ordered
\ stream regardless of front end.
\
\ Ownership (settled by the chain orchestrator):
\  - Header clauses (arity, POLICY, DERIVE) are events HERE, with duplicate-clause
\    validation HERE (E-DEV-DUP-POLICY / E-DEV-DUP-DERIVE) and the arity value
\    bound HERE (E-TDECL-ARITY). Their state is the event stream itself.
\  - Variant open/close are events HERE; open enforces TYPE-NAME policy, performs
\    SUMV-ADD duplicate registration, and surfaces the returned id as the
\    CURRENT-VARIANT selector.
\  - Field name-gate (duplicate / reserved / case) is NOT here: the field record
\    raises those and this module surfaces the throw unchanged — no second gate.
\
\ Transaction shape mirrors the field record's strict last-in, first-out frames.
\ GENERATED-DECL opens this participant at every coordinator snapshot, keeps
\ events and field rows provisional through body validation, preflights
\ their contiguity, then publishes both reversible watermarks. Rollback restores
\ the event arena, published event high-water, field ordinal, variant ordinal,
\ current-variant selector, and the field registry. The checker participant owns
\ the enclosing family, variant, schema, and layout registry savepoint.
\
\ Loaded AFTER the checker hook (the sole STRUCTURE/ENUM parser loads later
\ still); state is process-local and re-seeded at load, like top-row.f, so
\ snapshot re-arm rides the later snapshot and ahead-of-time compilation work.
\
\ ---------------------------------------------------------------------------
\ FIELD-RECORD / VARIANT-REGISTRY INTEGRATION SEAM (reconcile at merge).
\ ---------------------------------------------------------------------------
\ The field record is habu-fields-add-shared-6b063c62 (landing as
\ src/core/type-field.f on the Mac); on this base its contract lives in
\ src/core/type-family.f as the pre-hook TYPE-FIELD-OWNER transaction + TYPE-FIELD
\ package, and variant registration is SUMV-ADD there too.
\
\ The field-owner lifecycle is no longer consumed through TRUSTED forwarders.
\ src/core/checker.f carries a PPRIM axiom for each public TYPE-FIELD-OWNER and
\ TYPE-FIELD word, so the calls below are ordinary checked qualified calls whose
\ argument roles the checker verifies at every site (dot
\ habu-type-field-owner-619ec6b5). Only the two words with no axiom keep a
\ forwarder: TYPE-NAME:VARIANT-REQUIRE and the still-global SUMV-ADD.
\ Consumed contract:
\   TYPE-FIELD-OWNER:OPEN      ( -- tok )                   begin a field transaction
\   TYPE-FIELD-OWNER:ADD       ( tok fam var na nu sch slot cells boff bytes al flags -- tok )
\   TYPE-FIELD-OWNER:PREPARE   ( tok -- provisional-count ) validate without mutation
\   TYPE-FIELD-OWNER:COMMIT    ( tok -- )                   publish while retaining rollback
\   TYPE-FIELD-OWNER:FINALIZE  ( tok -- )                   release a committed frame
\   TYPE-FIELD-OWNER:ROLLBACK  ( tok -- )                   retire open or committed rows
\   TYPE-FIELD-OWNER:TX-SCHEMA-FOR ( tok fam id -- sch )    provisional schema root
\   TYPE-FIELD-OWNER:TX-CELLS-FOR  ( tok fam id -- cells )  provisional cell width
\   TYPE-FIELD:COUNT ( -- n )                               committed field high-water
\   TYPE-NAME:VARIANT-REQUIRE ( na nu -- )                  reserved/collision gate
\   SUMV-ADD         ( fam na nu tag ss sc pc -- vid )      register one canonical, unique variant

package DECL-EVENT

\ --- syntax-event kinds (record tag values). No 0 tag: an unwritten cell is
\ never a valid event, so a stray read is caught by the id-range guards. Slot
\ meaning by kind (the FAM slot is always the family id):
\   DEV-K-DECL         VAR = none          FLD = none      (open one declaration)
\   DEV-K-ARITY        VAR = arity         FLD = none      (header clause)
\   DEV-K-POLICY       VAR = policy code   FLD = none      (header clause)
\   DEV-K-DERIVE       VAR = feature code  FLD = none      (header clause)
\   DEV-K-VARIANT      VAR = variant id    FLD = none      (open one enum variant)
\   DEV-K-VARIANT-END  VAR = variant id    FLD = none      (close the open variant)
\   DEV-K-FIELD        VAR = owner variant FLD = field id  (shared field event)
1 constant DEV-K-DECL
2 constant DEV-K-ARITY
3 constant DEV-K-POLICY
4 constant DEV-K-DERIVE
5 constant DEV-K-VARIANT
6 constant DEV-K-VARIANT-END
7 constant DEV-K-FIELD

-1 constant DEV-NO-VARIANT   \ VAR sentinel: no open variant (structure fields, framing)
-1 constant DEV-NO-FIELD     \ FLD sentinel for every non-field event
-1 constant DEV-NO-FAMILY    \ DEVTX.FAM sentinel: DECL has not bound this frame
-1 constant DEV-NO-OWNER     \ frame has not emitted an event

0 constant DEV-FAMILY-USE
1 constant DEV-FAMILY-BIND

\ --- named reject codes (thrown, caught by the parser/CHECK path or unit `catch`).
\ Field name-gate and variant duplicate/canonical codes (E-TFAM-DUP 7102,
\ E-PF-NAME 7125, E-TFAM-CASE 7101) are raised by the field record / SUMV and
\ pass through unchanged. TYPE-NAME raises variant syntax code 7107 or reserved
\ name code 7110 before SUMV.
7161 constant E-DEV-TX          \ stale or non-LIFO declaration-event transaction token
7162 constant E-DEV-STATE       \ field-record publication broke the field-id contiguity invariant
7163 constant E-DEV-DUP-POLICY  \ a second POLICY clause in one declaration
7164 constant E-DEV-DUP-DERIVE  \ the same DERIVE feature recorded twice in one declaration
7172 constant E-DEV-FIELD-SCOPE \ provisional field is outside the token's declaration/family
7173 constant E-DEV-FAMILY-SCOPE \ token does not own the requested declaration family

\ The shared malformed-arity code mirrors sumtype.f.  The bound itself comes
\ from type-family.f's canonical declaration alphabet through the normal
\ post-hook seam below, so front ends and the event validator cannot drift.
7108 constant E-DEV-ARITY       \ arity outside [0, cap] — the shared malformed-arity code

72 constant DEV-BUG-RC          \ internal invariant violation (bad id / oob): fail-closed die

\ ---------------------------------------------------------------------------
\ SEAM forwarders (see header). TRUSTED: only for the two pre-hook words that
\ still have no checker axiom; every field-owner call is checked and qualified.
\ ---------------------------------------------------------------------------
TRUSTED: DEV-NAME-VARIANT-REQUIRE ( ptr u8 n -- ) TYPE-NAME:VARIANT-REQUIRE ;
TRUSTED: DEV-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;

\ ---------------------------------------------------------------------------
\ event record arena (interleaved cells, pointer-free: KIND/FAM/VAR/FLD/OWNER are
\ all integers, so a grow is a plain cell copy and any later snapshot bake is
\ verbatim with no rebase). OWNER is the first event id of the frame that
\ emitted the row, so finalized nested declarations remain distinguishable from
\ their still-open parent. One growable buffer whose base rides a variable.
\ ---------------------------------------------------------------------------
0 cells constant DEV.KIND-OFF
1 cells constant DEV.FAM-OFF
2 cells constant DEV.VAR-OFF
3 cells constant DEV.FLD-OFF
4 cells constant DEV.OWNER-OFF
5 cells constant DEV-REC

: DEV.KIND ( ptr a -- ptr a ) DEV.KIND-OFF + ;
: DEV.FAM ( ptr a -- ptr a ) DEV.FAM-OFF + ;
: DEV.VAR ( ptr a -- ptr a ) DEV.VAR-OFF + ;
: DEV.FLD ( ptr a -- ptr a ) DEV.FLD-OFF + ;
: DEV.OWNER ( ptr a -- ptr a ) DEV.OWNER-OFF + ;

8 constant DEV-CAP-INIT           \ small seed; grows geometrically (doubles) on demand
variable DEV-CAP-V   DEV-CAP-INIT DEV-CAP-V !
create DEV-A-BOOT   DEV-CAP-INIT DEV-REC * allot
variable DEV-A-P    DEV-A-BOOT DEV-A-P !
: DEV-BASE ( -- ptr a ) DEV-A-P @ ;

variable DEV-N        \ provisional event high-water (arena end, in records)
variable DEV-PUB-N    \ published event count; reflection reads [0, DEV-PUB-N)
variable DEV-BASE-FLD \ field-record committed count captured at the OUTER open
variable DEV-FLD-ORD  \ running field-event ordinal within the outer transaction
variable DEV-VAR-ORD  \ running variant tag ordinal within the outer transaction
variable DEV-CUR-VAR  \ current-variant selector: open variant id, else DEV-NO-VARIANT
variable DEV-I        \ private scan index (dedup / reflection / identity)
variable DEV-FOUND    \ private dedup-scan hit marker

\ raw arena realloc is the one memory boundary the checker cannot model here
\ (relocating base held in a variable); everything above/below stays checked.
TRUSTED: DEV-REG-GROW1 ( ptr a n n -- ) REG-GROW1 ;

: DEV-GROW ( n -- ) {: need:n :}
   need DEV-CAP-V @ 2 * max {: nc:n :}
   DEV-A-P  DEV-CAP-V @ DEV-REC *  nc DEV-REC *  DEV-REG-GROW1
   nc DEV-CAP-V ! ;
: DEV-ENSURE ( -- )               \ room for the next event id (DEV-N)
   DEV-N @ DEV-CAP-V @ < IF exit THEN
   DEV-N @ 1 + DEV-GROW ;

: DEV-ROW ( n -- ptr a ) {: id:n :}     \ address of provisional event `id`
   id 0 < IF s" decl-event: bad event id" DEV-BUG-RC die THEN
   id DEV-N @ >= IF s" decl-event: bad event id" DEV-BUG-RC die THEN
   id DEV-REC * DEV-BASE + ;
: DEV-REC@ ( n -- ptr a ) {: id:n :}    \ address of PUBLISHED event `id` (reflection)
   id 0 < IF s" decl-event: bad published event id" DEV-BUG-RC die THEN
   id DEV-PUB-N @ >= IF s" decl-event: bad published event id" DEV-BUG-RC die THEN
   id DEV-REC * DEV-BASE + ;

\ ---------------------------------------------------------------------------
\ strict-LIFO transaction frames. Nested COMMIT keeps events + field rows
\ reversible; only the outer COMMIT advances DEV-PUB-N and outer-commits the
\ field record. Every frame owns the event-arena high-water, the field ordinal,
\ the variant ordinal, the current-variant selector, the field-record token, and
\ its own serial token.
\ ---------------------------------------------------------------------------
0 cells constant DEVTX.EVN-OFF      \ DEV-N at open (event-arena watermark)
1 cells constant DEVTX.FLDORD-OFF   \ DEV-FLD-ORD at open (field ordinal watermark)
2 cells constant DEVTX.VARORD-OFF   \ DEV-VAR-ORD at open (variant ordinal watermark)
3 cells constant DEVTX.CURVAR-OFF   \ DEV-CUR-VAR at open (current-variant watermark)
4 cells constant DEVTX.FLDTOK-OFF   \ field-record transaction token
5 cells constant DEVTX.TOK-OFF      \ this frame's serial token
6 cells constant DEVTX.PUBN-OFF     \ published event high-water at open
7 cells constant DEVTX.STATE-OFF    \ open / reversibly published
8 cells constant DEVTX.FAM-OFF      \ family owned by this declaration frame
9 cells constant DEVTX.OWNER-OFF    \ exact first event that owns frame rows
10 cells constant DEV-TX-REC

: DEVTX.EVN ( ptr a -- ptr a ) DEVTX.EVN-OFF + ;
: DEVTX.FLDORD ( ptr a -- ptr a ) DEVTX.FLDORD-OFF + ;
: DEVTX.VARORD ( ptr a -- ptr a ) DEVTX.VARORD-OFF + ;
: DEVTX.CURVAR ( ptr a -- ptr a ) DEVTX.CURVAR-OFF + ;
: DEVTX.FLDTOK ( ptr a -- ptr a ) DEVTX.FLDTOK-OFF + ;
: DEVTX.TOK ( ptr a -- ptr a ) DEVTX.TOK-OFF + ;
: DEVTX.PUBN ( ptr a -- ptr a ) DEVTX.PUBN-OFF + ;
: DEVTX.STATE ( ptr a -- ptr a ) DEVTX.STATE-OFF + ;
: DEVTX.FAM ( ptr a -- ptr a ) DEVTX.FAM-OFF + ;
: DEVTX.OWNER ( ptr a -- ptr a ) DEVTX.OWNER-OFF + ;

4 constant DEV-TX-CAP-INIT
variable DEV-TX-CAP-V   DEV-TX-CAP-INIT DEV-TX-CAP-V !
create DEV-TX-BOOT   DEV-TX-CAP-INIT DEV-TX-REC * allot
variable DEV-TX-P    DEV-TX-BOOT DEV-TX-P !
variable DEV-TX-DEPTH
variable DEV-TX-SERIAL

0 constant DEV-TX-OPEN
1 constant DEV-TX-PUBLISHED

: DEV-TX-BASE ( -- ptr a ) DEV-TX-P @ ;
: DEV-TX-GROW ( -- )
   DEV-TX-CAP-V @ 2 * {: nc:n :}
   DEV-TX-P  DEV-TX-CAP-V @ DEV-TX-REC *  nc DEV-TX-REC *  DEV-REG-GROW1
   nc DEV-TX-CAP-V ! ;
: DEV-TX-ENSURE ( -- )
   DEV-TX-DEPTH @ DEV-TX-CAP-V @ < IF exit THEN
   DEV-TX-GROW ;
: DEV-TX-AT ( n -- ptr a ) DEV-TX-REC * DEV-TX-BASE + ;
: DEV-TX-TOP ( -- ptr a )
   DEV-TX-DEPTH @ 0= IF E-DEV-TX throw THEN
   DEV-TX-DEPTH @ 1 - DEV-TX-AT ;
: DEV-TX-REQUIRE ( n -- ) DEV-TX-TOP DEVTX.TOK @ <> IF E-DEV-TX throw THEN ;
: DEV-FLD-PROVISIONAL-COUNT ( -- n )
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:PREPARE ;
: DEV-TX-OPEN-REQUIRE ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.STATE @ DEV-TX-OPEN <> IF E-DEV-TX throw THEN ;

: DEV-FAMILY-REQUIRE ( n n n -- )
   {: tok:n fam:n bind:n :}
   tok DEV-TX-OPEN-REQUIRE
   fam DEV-NO-FAMILY = IF E-DEV-FAMILY-SCOPE throw THEN
   DEV-TX-TOP DEVTX.FAM @ {: owner:n :}
   owner DEV-NO-FAMILY = IF
      bind 0= IF E-DEV-FAMILY-SCOPE throw THEN
      fam DEV-TX-TOP DEVTX.FAM !
      EXIT
   THEN
   owner fam <> IF E-DEV-FAMILY-SCOPE throw THEN
   bind 0= 0= IF E-DEV-STATE throw THEN ;

\ Kept unexercised for the same reason as the field owner's wrap guard (see
\ src/core/type-family.f OPEN): only a whitebox poke of DEV-TX-SERIAL could
\ reach it, that seam was retired by contract, and without the guard a wrapped
\ serial would mint a token that equality-compares against a live frame.
: DEV-OPEN ( -- n )
   DEV-TX-ENSURE
   DEV-TX-SERIAL @ 1 + dup 0 <= IF drop E-DEV-TX throw THEN
   {: tok:n :}
   \ no event state changes before the field snapshot exists
   TYPE-FIELD-OWNER:OPEN {: fldtok:n :}
   tok DEV-TX-SERIAL !
   DEV-TX-DEPTH @ 0= IF                     \ outer: pin the field base + reset ordinals/selector
      TYPE-FIELD:COUNT DEV-BASE-FLD !
      0 DEV-FLD-ORD !
      0 DEV-VAR-ORD !
      DEV-NO-VARIANT DEV-CUR-VAR !
   THEN
   DEV-TX-DEPTH @ DEV-TX-AT {: r:ptr :}
   DEV-N @ r DEVTX.EVN !
   DEV-FLD-ORD @ r DEVTX.FLDORD !
   DEV-VAR-ORD @ r DEVTX.VARORD !
   DEV-CUR-VAR @ r DEVTX.CURVAR !
   fldtok r DEVTX.FLDTOK !
   tok r DEVTX.TOK !
   DEV-PUB-N @ r DEVTX.PUBN !
   DEV-TX-OPEN r DEVTX.STATE !
   DEV-NO-FAMILY r DEVTX.FAM !
   DEV-NO-OWNER r DEVTX.OWNER !
   DEV-TX-DEPTH @ 1 + DEV-TX-DEPTH !
   tok ;

\ --- duplicate-clause scan over the current declaration's provisional events
\ (this frame's [start, DEV-N) range; one declaration per transaction frame).
: DEV-CUR-START ( -- n ) DEV-TX-TOP DEVTX.EVN @ ;
: DEV-CUR-OWNER ( -- n )
   DEV-TX-TOP DEVTX.OWNER @ dup DEV-NO-OWNER =
      IF drop E-DEV-STATE throw THEN ;
: DEV-OWNER-ENSURE ( -- )
   DEV-TX-TOP DEVTX.OWNER @ DEV-NO-OWNER =
      IF DEV-N @ DEV-TX-TOP DEVTX.OWNER ! THEN ;
: DEV-EMIT ( n n n n -- ) {: k:n fam:n var:n fld:n :}
   DEV-ENSURE
   DEV-OWNER-ENSURE
   DEV-N @ DEV-REC * DEV-BASE + {: r:ptr :}
   k r DEV.KIND !   fam r DEV.FAM !   var r DEV.VAR !   fld r DEV.FLD !
   DEV-CUR-OWNER r DEV.OWNER !
   DEV-N @ 1 + DEV-N ! ;
: DEV-PROV-KIND@ ( n -- n ) DEV-ROW DEV.KIND @ ;
: DEV-PROV-FAM@ ( n -- n ) DEV-ROW DEV.FAM @ ;
: DEV-PROV-VAR@ ( n -- n ) DEV-ROW DEV.VAR @ ;
: DEV-PROV-FLD@ ( n -- n ) DEV-ROW DEV.FLD @ ;
: DEV-PROV-OWNER@ ( n -- n ) DEV-ROW DEV.OWNER @ ;
: DEV-CUR-HAS-KIND? ( n -- bool ) {: k:n :}
   0 DEV-FOUND !
   DEV-CUR-START DEV-I !
   BEGIN DEV-I @ DEV-N @ < WHILE
      DEV-I @ DEV-PROV-KIND@ k =
      DEV-I @ DEV-PROV-OWNER@ DEV-CUR-OWNER = and
         IF -1 DEV-FOUND ! THEN
      DEV-I @ 1 + DEV-I !
   REPEAT
   DEV-FOUND @ 0 <> ;
: DEV-CUR-HAS-KIND-VAR? ( n n -- bool ) {: k:n v:n :}
   0 DEV-FOUND !
   DEV-CUR-START DEV-I !
   BEGIN DEV-I @ DEV-N @ < WHILE
      DEV-I @ DEV-PROV-KIND@ k =
      DEV-I @ DEV-PROV-VAR@ v = and
      DEV-I @ DEV-PROV-OWNER@ DEV-CUR-OWNER = and
         IF -1 DEV-FOUND ! THEN
      DEV-I @ 1 + DEV-I !
   REPEAT
   DEV-FOUND @ 0 <> ;

\ --- declaration open + header clauses.
: DEV-DECL ( n n -- n ) {: tok:n fam:n :}          \ open one declaration
   tok fam DEV-FAMILY-BIND DEV-FAMILY-REQUIRE
   DEV-K-DECL fam DEV-NO-VARIANT DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-FIELD-MATCH? ( n n n -- bool ) {: idx:n fam:n fld:n :}
   idx DEV-PROV-KIND@ DEV-K-FIELD =
   idx DEV-PROV-FAM@ fam = and
   idx DEV-PROV-FLD@ fld = and
   idx DEV-PROV-OWNER@ DEV-CUR-OWNER = and ;

: DEV-CUR-FIELD? ( n n -- bool ) {: fam:n fld:n :}
   DEV-CUR-START
   BEGIN dup DEV-N @ < WHILE
      dup fam fld DEV-FIELD-MATCH? IF drop 0 0= EXIT THEN
      1 +
   REPEAT
   drop 0 0= 0= ;

: DEV-FIELD-SCHEMA@ ( n n n -- n ) {: tok:n fam:n fld:n :}
   tok DEV-TX-OPEN-REQUIRE
   DEV-TX-TOP DEVTX.FAM @ fam <> IF E-DEV-FAMILY-SCOPE throw THEN
   fam fld DEV-CUR-FIELD? 0= IF E-DEV-FIELD-SCOPE throw THEN
   DEV-TX-TOP DEVTX.FLDTOK @ fam fld TYPE-FIELD-OWNER:TX-SCHEMA-FOR ;

: DEV-PAYLOAD-FIELD? ( n n n -- bool ) {: id:n fam:n vid:n :}
   id DEV-PROV-KIND@ DEV-K-FIELD =
   id DEV-PROV-FAM@ fam = and
   id DEV-PROV-VAR@ vid = and
   id DEV-PROV-OWNER@ DEV-CUR-OWNER = and ;

: DEV-PAYLOAD-OPEN? ( n n n -- bool ) {: id:n fam:n vid:n :}
   id DEV-PROV-KIND@ DEV-K-VARIANT =
   id DEV-PROV-FAM@ fam = and
   id DEV-PROV-VAR@ vid = and
   id DEV-PROV-OWNER@ DEV-CUR-OWNER = and ;

: DEV-PAYLOAD-END? ( n n n -- bool ) {: id:n fam:n vid:n :}
   id DEV-PROV-KIND@ DEV-K-VARIANT-END =
   id DEV-PROV-FAM@ fam = and
   id DEV-PROV-VAR@ vid = and
   id DEV-PROV-OWNER@ DEV-CUR-OWNER = and ;

: DEV-FIELD-ORDERED? ( n n -- bool ) {: id:n ord:n :}
   id DEV-PROV-FLD@ DEV-BASE-FLD @ ord + = ;

: DEV-FIELDS-VALIDATE ( -- )
   DEV-CUR-START DEV-TX-TOP DEVTX.FLDORD @
   BEGIN over DEV-N @ < WHILE
      over DEV-PROV-KIND@ DEV-K-FIELD = IF
         2dup DEV-FIELD-ORDERED? 0= IF 2drop E-DEV-FIELD-SCOPE throw THEN
         1 +
      THEN
      swap 1 + swap
   REPEAT
   nip DEV-FLD-ORD @ <> IF E-DEV-FIELD-SCOPE throw THEN ;

: DEV-PAYLOAD-START ( n n n -- n ) {: tok:n fam:n vid:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-TX-TOP DEVTX.STATE @ DEV-TX-OPEN <> IF E-DEV-TX throw THEN
   DEV-FIELDS-VALIDATE
   DEV-CUR-START
   BEGIN dup DEV-N @ < WHILE
      dup fam vid DEV-PAYLOAD-OPEN? IF 1 + EXIT THEN
      1 +
   REPEAT
   drop E-DEV-FIELD-SCOPE throw ;

: DEV-PAYLOAD-END ( n n n -- n ) {: start:n fam:n vid:n :}
   start
   BEGIN dup DEV-N @ < WHILE
      dup fam vid DEV-PAYLOAD-END? IF EXIT THEN
      dup fam vid DEV-PAYLOAD-FIELD? 0= IF drop E-DEV-FIELD-SCOPE throw THEN
      1 +
   REPEAT
   drop E-DEV-FIELD-SCOPE throw ;

: DEV-PAYLOAD-RANGE ( n n n -- n n ) {: tok:n fam:n vid:n :}
   tok fam vid DEV-PAYLOAD-START {: start:n :}
   start start fam vid DEV-PAYLOAD-END ;

: DEV-PAYLOAD-N ( n n n -- n ) DEV-PAYLOAD-RANGE swap - ;

: DEV-PAYLOAD-FIELD ( n n n n -- n ) {: tok:n fam:n vid:n wanted:n :}
   tok fam vid DEV-PAYLOAD-RANGE {: start:n end:n :}
   wanted 0 < wanted end start - >= or IF E-DEV-FIELD-SCOPE throw THEN
   start wanted + DEV-PROV-FLD@ ;

: DEV-PAYLOAD-SCHEMA@ ( n n n n -- n ) {: tok:n fam:n vid:n index:n :}
   tok fam vid index DEV-PAYLOAD-FIELD {: fld:n :}
   DEV-TX-TOP DEVTX.FLDTOK @ fam fld TYPE-FIELD-OWNER:TX-SCHEMA-FOR ;

: DEV-PAYLOAD-WIDTH@ ( n n n n -- n ) {: tok:n fam:n vid:n index:n :}
   tok fam vid index DEV-PAYLOAD-FIELD {: fld:n :}
   DEV-TX-TOP DEVTX.FLDTOK @ fam fld TYPE-FIELD-OWNER:TX-CELLS-FOR ;

: DEV-PAYLOAD-CELLS ( n n n -- n ) {: tok:n fam:n vid:n :}
   tok fam vid DEV-PAYLOAD-RANGE {: start:n end:n :}
   start
   0
   BEGIN over end < WHILE
      over DEV-PROV-FLD@ {: fld:n :}
      DEV-TX-TOP DEVTX.FLDTOK @ fam fld TYPE-FIELD-OWNER:TX-CELLS-FOR +
      swap 1 + swap
   REPEAT
   nip ;

: DEV-ARITY ( n n n -- n ) {: tok:n fam:n arity:n :}   \ header: arity value (shared bound)
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   arity 0 < arity TFAM-DECL-PARAM-COUNT > or IF E-DEV-ARITY throw THEN
   DEV-K-ARITY fam arity DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-POLICY ( n n n -- n ) {: tok:n fam:n policy:n :}   \ header: POLICY clause (at most once)
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-K-POLICY DEV-CUR-HAS-KIND? IF E-DEV-DUP-POLICY throw THEN
   DEV-K-POLICY fam policy DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-DERIVE ( n n n -- n ) {: tok:n fam:n feature:n :}   \ header: DERIVE feature (each once)
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-K-DERIVE feature DEV-CUR-HAS-KIND-VAR? IF E-DEV-DUP-DERIVE throw THEN
   DEV-K-DERIVE fam feature DEV-NO-FIELD DEV-EMIT
   tok ;

\ --- variant open/close. Open validates the variant tail before SUMV-ADD's
\ canonical/duplicate registration and pins its id as the current-variant
\ selector. Named-field variants carry no positional payload schema
\ (ss=sc=pc=0); field rows are discovered downstream by (family, variant-id).
\ Close clears the selector.
: DEV-VARIANT ( n n ptr u8 n -- n ) {: tok:n fam:n na:ptr nu:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   na nu DEV-NAME-VARIANT-REQUIRE
   fam na nu DEV-VAR-ORD @ 0 0 0 DEV-SUMV-ADD {: vid:n :}
   vid DEV-CUR-VAR !
   DEV-VAR-ORD @ 1 + DEV-VAR-ORD !
   DEV-K-VARIANT fam vid DEV-NO-FIELD DEV-EMIT
   tok ;
: DEV-END-VARIANT ( n n -- n ) {: tok:n fam:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-K-VARIANT-END fam DEV-CUR-VAR @ DEV-NO-FIELD DEV-EMIT
   DEV-NO-VARIANT DEV-CUR-VAR !
   tok ;

\ --- the shared field event. Adds the field to the field record FIRST under the
\ current-variant selector (the record validates owner/name/schema/layout and
\ throws — E-TFAM-DUP / E-PF-NAME / E-TFAM-CASE / E-PF-* — on any violation, so a
\ malformed field emits NO event and leaves the ordinal untouched), then records
\ the event with the field's eventual committed row id (base + ordinal, proven at
\ PREPARE before any participant commits). STRUCTURE (selector NO-VARIANT) and
\ ENUM (selector = open variant) produce the identical event + row shape.
: DEV-FIELD ( n n ptr u8 n n n n n n n n -- n )
   {: tok:n fam:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ {: fldtok:n :}        \ ADD wants the field-tx token, not our serial
   fldtok fam DEV-CUR-VAR @ na nu sch slot cellsn boff bytesn al flags TYPE-FIELD-OWNER:ADD drop
   DEV-K-FIELD fam DEV-CUR-VAR @  DEV-BASE-FLD @ DEV-FLD-ORD @ +  DEV-EMIT
   DEV-FLD-ORD @ 1 + DEV-FLD-ORD !
   tok ;

\ PREPARE proves the field rows are exactly the contiguous provisional range the
\ event stream names, then delegates the field owner's mutation-free preflight.
\ COMMIT advances reversible published watermarks while retaining both frames;
\ FINALIZE alone releases them after every participant has committed.
: DEV-PREPARE ( n -- ) {: tok:n :}
   tok DEV-TX-OPEN-REQUIRE
   DEV-FLD-PROVISIONAL-COUNT DEV-BASE-FLD @ DEV-FLD-ORD @ + <>
      IF E-DEV-STATE throw THEN ;

: DEV-COMMIT ( n -- ) {: tok:n :}
   tok DEV-PREPARE
   DEV-N @ DEV-TX-TOP DEVTX.EVN @ <> {: changed:bool :}
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:COMMIT
   DEV-TX-DEPTH @ 1 = changed and IF
      DEV-N @ DEV-PUB-N !
   THEN
   DEV-TX-PUBLISHED DEV-TX-TOP DEVTX.STATE ! ;

: DEV-FINALIZE ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.STATE @ DEV-TX-PUBLISHED <> IF E-DEV-TX throw THEN
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:FINALIZE
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH ! ;

: DEV-PUBLISH ( n -- ) {: tok:n :}
   tok DEV-PREPARE
   tok DEV-COMMIT
   tok DEV-FINALIZE ;

\ roll back every watermark this frame owns and delegate the field-registry
\ cursor to the field record. Both open and reversibly committed frames restore
\ the published view captured at OPEN. The family/variant/schema/layout cursors
\ are retired by the enclosing checker scope/candidate frame (see header).
\ A live frame is always either open or reversibly committed — the only two
\ states DEV-OPEN and DEV-COMMIT set — and both roll back the same way, so there
\ is no third value to reject here.
: DEV-FRAME-RESTORE ( ptr a -- ) {: r:ptr :}   \ every watermark one frame owns
   r DEVTX.EVN @ DEV-N !
   r DEVTX.FLDORD @ DEV-FLD-ORD !
   r DEVTX.VARORD @ DEV-VAR-ORD !
   r DEVTX.CURVAR @ DEV-CUR-VAR !
   r DEVTX.PUBN @ DEV-PUB-N ! ;

: DEV-FRAME-POP ( -- )            \ retire the live top frame's own watermarks
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
   DEV-TX-DEPTH @ DEV-TX-AT DEV-FRAME-RESTORE ;

: DEV-ROLLBACK ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:ROLLBACK
   DEV-FRAME-POP ;

: DEV-RESET ( -- )                \ base state; re-seeded at load (process-local)
   0 DEV-N !   0 DEV-PUB-N !
   0 DEV-TX-DEPTH !   0 DEV-TX-SERIAL !
   0 DEV-BASE-FLD !   0 DEV-FLD-ORD !   0 DEV-VAR-ORD !
   DEV-NO-VARIANT DEV-CUR-VAR ! ;
DEV-RESET

\ ---------------------------------------------------------------------------
\ read-only event reflection (published events only).
\ ---------------------------------------------------------------------------
: DEV-PUB-N@ ( -- n ) DEV-PUB-N @ ;
: DEV-KIND@ ( n -- n ) DEV-REC@ DEV.KIND @ ;
: DEV-FAM@ ( n -- n ) DEV-REC@ DEV.FAM @ ;
: DEV-VAR@ ( n -- n ) DEV-REC@ DEV.VAR @ ;
: DEV-FLD@ ( n -- n ) DEV-REC@ DEV.FLD @ ;
: DEV-OWNER@ ( n -- n ) DEV-REC@ DEV.OWNER @ ;
: DEV-DECL? ( n -- bool ) DEV-KIND@ DEV-K-DECL = ;
: DEV-ARITY? ( n -- bool ) DEV-KIND@ DEV-K-ARITY = ;
: DEV-POLICY? ( n -- bool ) DEV-KIND@ DEV-K-POLICY = ;
: DEV-DERIVE? ( n -- bool ) DEV-KIND@ DEV-K-DERIVE = ;
: DEV-VARIANT? ( n -- bool ) DEV-KIND@ DEV-K-VARIANT = ;
: DEV-VARIANT-END? ( n -- bool ) DEV-KIND@ DEV-K-VARIANT-END = ;
: DEV-FIELD? ( n -- bool ) DEV-KIND@ DEV-K-FIELD = ;

\ ---------------------------------------------------------------------------
\ deterministic snapshot identity. FNV-1a-64 fold over the published event
\ records. Records are pointer-free integers appended in declaration order, so
\ identical declarations always fold to an identical identity across rebuilds
\ and hosts (no address, no ordering nondeterminism). This is the deterministic
\ identity the event stream carries; cryptographic content-addressing of the
\ persisted snapshot rides the later AOT/snapshot dot.
\ ---------------------------------------------------------------------------
$100000001b3 constant DEV-FNV-PRIME
$cbf29ce484222325 constant DEV-FNV-OFFSET
: DEV-MIX ( n n -- n ) xor DEV-FNV-PRIME * ;
: DEV-IDENTITY ( -- n )
   DEV-FNV-OFFSET
   0 DEV-I !
   BEGIN DEV-I @ DEV-PUB-N @ < WHILE
      DEV-I @ DEV-KIND@ DEV-MIX
      DEV-I @ DEV-FAM@  DEV-MIX
      DEV-I @ DEV-VAR@  DEV-MIX
      DEV-I @ DEV-FLD@  DEV-MIX
      DEV-I @ DEV-OWNER@ DEV-MIX
      DEV-I @ 1 + DEV-I !
   REPEAT ;

\ GENERATED-DECL adapter.  Each coordinator nesting depth owns one slot holding
\ the EXACT event token and field token this participant opened there, both
\ written before the declaration body can run.  Every later phase proves and
\ drives that saved pair.  Reading the live top instead — as this adapter used
\ to — let any frame the body opened and never closed stand in for the
\ participant's own frame: prepare then validated the wrong frame, commit and
\ finalize published and released the wrong frame, and the participant's real
\ frame survived the transaction.  The checker participant finalizes last, sees
\ a field-transaction depth that no longer matches its savepoint, and throws
\ E-PF-TX from cleanup, which poisons the one production coordinator for the
\ rest of the process.
4 constant DEV-PART-CAP-INIT
2 constant DEV-PART-REC          \ cells per depth: event token, then field token
0 constant DEV-NO-TOKEN          \ slot sentinel; DEV-OPEN never mints a token <= 0
create DEV-PART-BASE-BOOT DEV-PART-CAP-INIT DEV-PART-REC * cells allot
PTR-VARIABLE DEV-PART-BASE-P   DEV-PART-BASE-BOOT DEV-PART-BASE-P !
variable DEV-PART-CAP      DEV-PART-CAP-INIT DEV-PART-CAP !

: DEV-PART-BASE ( -- ptr a ) DEV-PART-BASE-P @ ;

: DEV-PART-GROW ( -- )
   DEV-PART-CAP @ 2 * {: nc:n :}
   DEV-PART-BASE-P
      DEV-PART-CAP @ DEV-PART-REC * cells
      nc DEV-PART-REC * cells DEV-REG-GROW1
   nc DEV-PART-CAP ! ;

: DEV-PART-ENSURE ( -- )
   GENERATED-DECL:DEPTH DEV-PART-CAP @ <= IF EXIT THEN
   DEV-PART-GROW ;

: DEV-PART-SLOT ( -- ptr a )
   GENERATED-DECL:DEPTH 1 - DEV-PART-REC * cells DEV-PART-BASE + ;
: DEV-PART-TOK-SLOT ( -- ptr a ) DEV-PART-SLOT ;
: DEV-PART-FLD-SLOT ( -- ptr a ) DEV-PART-SLOT 1 cells + ;

: DEV-PART-CLEAR ( -- )
   DEV-NO-TOKEN DEV-PART-TOK-SLOT !
   DEV-NO-TOKEN DEV-PART-FLD-SLOT ! ;

: DEV-PART-BIND ( n -- ) {: tok:n :}   \ pin the frame OPEN just created
   tok DEV-PART-TOK-SLOT !
   DEV-TX-TOP DEVTX.FLDTOK @ DEV-PART-FLD-SLOT ! ;

: DEV-PART-SNAPSHOT ( n -- n )
   DEV-PART-ENSURE
   DEV-PART-CLEAR                 \ OPEN may reject before it mutates anything
   DEV-OPEN DEV-PART-BIND ;

: DEV-PART-OPEN? ( -- bool )
   GENERATED-DECL:DEPTH 0= IF 0 0= 0= EXIT THEN
   DEV-PART-TOK-SLOT @ DEV-NO-TOKEN <> ;

: DEV-PART-TOKEN ( -- n ) DEV-PART-TOK-SLOT @ ;
: DEV-PART-FIELD-TOKEN ( -- n ) DEV-PART-FLD-SLOT @ ;

\ Kept unexercised, like the DEV-OPEN serial-wrap guard above.  No test drives it
\ and none can today: DEVTX.FLDTOK is written once, by DEV-OPEN, and DEV-PART-BIND
\ copies that same cell one step later, so once the caller has proved the saved
\ event token IS the live top the two values are the same cell by construction.
\ It stays because it is a free precondition on the pairing the whole participant
\ rests on, and because DEVTX.FLDTOK gaining a second writer is exactly the change
\ that would make it fire.  The reachable form of this check is the one in
\ DEV-RETIRE-THROUGH below, which does have a negative fixture.
: DEV-PART-FIELD-REQUIRE ( -- )
   DEV-TX-TOP DEVTX.FLDTOK @ DEV-PART-FIELD-TOKEN <> IF E-DEV-TX throw THEN ;

\ Prepare is the participant's whole fallible boundary.  It proves the saved event
\ token is the live open top, that its frame's field token is the one we saved,
\ that the field frame is the live open top of the field owner's stack, and that
\ the field rows are exactly the contiguous range the event stream names.  Commit
\ and finalize then only advance and release marks this proof already covered.
\ DEV-PREPARE repeats the token proof because it is also the standalone public
\ entry point, so a mutation that guts THIS word still rejects one phase later —
\ test/decl-event-suite.f §19e therefore asserts the failing PHASE, not just the
\ code, or the participant reversibly commits before the rejection lands.
: DEV-PART-PROVE ( -- )
   DEV-PART-TOKEN DEV-TX-OPEN-REQUIRE
   DEV-PART-FIELD-REQUIRE
   DEV-PART-TOKEN DEV-PREPARE ;

\ Cleanup for a failed declaration.  Retire the saved frame and every frame
\ opened above it, strictly last-in first-out.  Everything is proved before the
\ first mutation: the event token names a live frame, that frame's own field
\ token is the one presented, and the field chain is checked inside the field
\ owner's own cleanup vector.  The pair cross-check is not redundant with the two
\ token lookups — each only proves its own stack has the token somewhere, so
\ without it a caller can hand over frame A's event token with frame B's field
\ token and retire different depths on the two stacks, leaving them desynchronised
\ with no error.  The field owner remains the sole authority for retiring field
\ frames: this never walks that stack itself.
: DEV-TX-INDEX ( n -- n ) {: tok:n :}
   DEV-TX-DEPTH @
   BEGIN dup 0 > WHILE
      1 -
      dup DEV-TX-AT DEVTX.TOK @ tok = IF EXIT THEN
   REPEAT
   drop E-DEV-TX throw ;

: DEV-FRAMES-RETIRE ( n -- ) {: keep:n :}
   BEGIN DEV-TX-DEPTH @ keep > WHILE
      DEV-FRAME-POP
   REPEAT ;

: DEV-RETIRE-THROUGH ( n n -- ) {: tok:n fldtok:n :}
   tok DEV-TX-INDEX {: keep:n :}
   keep DEV-TX-AT DEVTX.FLDTOK @ fldtok <> IF E-DEV-TX throw THEN
   fldtok TDECL-FIELD-CLEANUP-XT
   keep DEV-FRAMES-RETIRE ;

: DEV-PART-PREPARE ( n -- n )
   DEV-PART-OPEN? IF DEV-PART-PROVE THEN ;

: DEV-PART-COMMIT ( n -- n )
   DEV-PART-OPEN? IF DEV-PART-TOKEN DEV-COMMIT THEN ;

: DEV-PART-ROLLBACK ( n -- n )
   DEV-PART-OPEN? IF
      DEV-PART-TOKEN DEV-PART-FIELD-TOKEN DEV-RETIRE-THROUGH
      DEV-PART-CLEAR
   THEN ;

: DEV-PART-FINALIZE ( n -- n )
   DEV-PART-OPEN? IF DEV-PART-TOKEN DEV-FINALIZE DEV-PART-CLEAR THEN ;

2 constant DEV-PARTICIPANT

: DEV-PART-INSTALL ( -- )
   DEV-PARTICIPANT GENERATED-DECL:ORDER-EVENT
   [: DEV-PART-SNAPSHOT ;]
   [: DEV-PART-PREPARE ;]
   [: DEV-PART-COMMIT ;]
   [: DEV-PART-ROLLBACK ;]
   [: DEV-PART-FINALIZE ;]
   GENERATED-DECL-OWNER:REGISTER ;

\ ---------------------------------------------------------------------------
\ public API: the transaction both front ends drive + read-only reflection.
\ Phase constraints: CURRENT returns the frame opened by the coordinator
\ snapshot and rejects outside an active coordinator; OPEN remains the explicit
\ standalone interface. DECL/ARITY/
\ POLICY/DERIVE/VARIANT/END-VARIANT/FIELD and provisional FIELD-SCHEMA@/
\ PAYLOAD-* queries are legal only between OPEN and its
\ COMMIT or ROLLBACK, each takes and returns the same token, and each throws
\ E-DEV-TX if the token is not the innermost open frame. COMMIT retains a
\ reversible frame; FINALIZE releases it, ROLLBACK restores it, and PUBLISH
\ performs PREPARE, COMMIT, and FINALIZE in one call. Reflection + IDENTITY
\ read only published events and are legal any time.
\ ---------------------------------------------------------------------------
public

: OPEN ( -- n ) DEV-OPEN ;
: CURRENT ( -- n )
   DEV-PART-OPEN? 0= IF E-DEV-TX throw THEN
   DEV-PART-TOKEN dup DEV-TX-OPEN-REQUIRE ;
: DECL ( n n -- n ) DEV-DECL ;
: ARITY ( n n n -- n ) DEV-ARITY ;
: POLICY ( n n n -- n ) DEV-POLICY ;
: DERIVE ( n n n -- n ) DEV-DERIVE ;
: VARIANT ( n n ptr u8 n -- n ) DEV-VARIANT ;
: END-VARIANT ( n n -- n ) DEV-END-VARIANT ;
: FIELD ( n n ptr u8 n n n n n n n n -- n ) DEV-FIELD ;
: FIELD-SCHEMA@ ( n n n -- n ) DEV-FIELD-SCHEMA@ ;
: PAYLOAD-N ( n n n -- n ) DEV-PAYLOAD-N ;
: PAYLOAD-SCHEMA@ ( n n n n -- n ) DEV-PAYLOAD-SCHEMA@ ;
: PAYLOAD-WIDTH@ ( n n n n -- n ) DEV-PAYLOAD-WIDTH@ ;
: PAYLOAD-CELLS ( n n n -- n ) DEV-PAYLOAD-CELLS ;
: PUBLISH ( n -- ) DEV-PUBLISH ;
: PREPARE ( n -- ) DEV-PREPARE ;
: COMMIT ( n -- ) DEV-COMMIT ;
: FINALIZE ( n -- ) DEV-FINALIZE ;
: ROLLBACK ( n -- ) DEV-ROLLBACK ;
: RESET ( -- ) DEV-RESET ;

: CURRENT-VARIANT ( -- n ) DEV-CUR-VAR @ ;
: NO-VARIANT ( -- n ) DEV-NO-VARIANT ;

: COUNT ( -- n ) DEV-PUB-N@ ;
: DEPTH ( -- n ) DEV-TX-DEPTH @ ;
: KIND@ ( n -- n ) DEV-KIND@ ;
: FAMILY@ ( n -- n ) DEV-FAM@ ;
: VAR@ ( n -- n ) DEV-VAR@ ;
: FIELD@ ( n -- n ) DEV-FLD@ ;
: DECL? ( n -- bool ) DEV-DECL? ;
: ARITY? ( n -- bool ) DEV-ARITY? ;
: POLICY? ( n -- bool ) DEV-POLICY? ;
: DERIVE? ( n -- bool ) DEV-DERIVE? ;
: VARIANT? ( n -- bool ) DEV-VARIANT? ;
: VARIANT-END? ( n -- bool ) DEV-VARIANT-END? ;
: FIELD? ( n -- bool ) DEV-FIELD? ;
: IDENTITY ( -- n ) DEV-IDENTITY ;

private

\ The declaration front ends reach this module only through these hooks, and
\ they go through the SAME public words an outside caller uses — CURRENT carries
\ the coordinator-frame and open-state guards that the private token reader does
\ not. Installed here rather than inside DEV-PART-INSTALL because a quotation
\ binds its target at compile time and the public section is defined above.
: DEV-EVENT-HOOKS-INSTALL ( -- )
   [: CURRENT ;] is TDECL-EVENT-CURRENT-XT
   [: DECL ;] is TDECL-EVENT-DECL-XT
   [: ARITY ;] is TDECL-EVENT-ARITY-XT
   [: FIELD ;] is TDECL-EVENT-FIELD-XT
   [: 0 0= ;] is TDECL-EVENT-ARMED ;

DEV-EVENT-HOOKS-INSTALL
DEV-PART-INSTALL

;package
