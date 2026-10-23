\ decl-event.f — shared typed-declaration syntax-event transaction (package
\ DECL-EVENT). One transaction and provisional event scratch that both
\ post-hook declarers (STRUCTURE and ENUM) drive.
\ This file owns NO front end, no
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
\ consumers inspect the provisional stream while the declaration is open;
\ committed family, variant, schema, and field registries own the result.
\
\ Ownership (settled by the chain orchestrator):
\  - Header clauses (arity, POLICY, DERIVE, OPAQUE) are events HERE, with
\    duplicate-clause validation HERE (E-DEV-DUP-POLICY / E-DEV-DUP-DERIVE /
\    E-DEV-DUP-OPAQUE), OPAQUE's package requirement HERE
\    (E-DEV-OPAQUE-PACKAGE) and the arity value
\    bound HERE (E-TDECL-ARITY). Their state is the event stream itself.
\  - Variant open/close are events HERE; open enforces TYPE-NAME policy, performs
\    SUMV-ADD duplicate registration, and surfaces the returned id as the
\    CURRENT-VARIANT selector.
\  - Field name-gate (duplicate / reserved / case) is NOT here: the field record
\    raises those and this module surfaces the throw unchanged — no second gate.
\
\ Transaction shape mirrors the field record's strict last-in, first-out frames.
\ DECLARATION-TRANSACTION:RUN opens this participant at its savepoint, keeps
\ events and field rows provisional through body validation, preflights
\ their contiguity, then commits the field rows reversibly. Rollback restores
\ the event scratch, field ordinal, variant ordinal,
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
\ habu-type-field-owner-619ec6b5). The two words with no axiom,
\ TYPE-NAME:VARIANT-REQUIRE and the still-global SUMV-ADD, are called by name
\ too: the core prefix loads before src/core/internal-mark.f seals, so the
\ checker binds their declared rows.
\ Consumed contract:
\   TYPE-FIELD-OWNER:OPEN      ( -- tok )                   begin a field transaction
\   TYPE-FIELD-OWNER:ADD       ( tok fam var na nu sch slot cells boff bytes al flags -- tok )
\   TYPE-FIELD-OWNER:PREPARE   ( tok -- provisional-count ) validate without mutation
\   TYPE-FIELD-OWNER:COMMIT    ( tok -- )                   publish while retaining rollback
\   TYPE-FIELD-OWNER:FINALIZE  ( tok -- )                   release a committed frame
\   TDECL-FIELD-RELEASE-XT     ( -- )                       total release of the top frame
\   TYPE-FIELD-OWNER:ROLLBACK  ( tok -- )                   retire open or committed rows
\   TYPE-FIELD-OWNER:TX-SCHEMA-FOR ( tok fam id -- sch )    provisional schema root
\   TYPE-FIELD-OWNER:TX-CELLS-FOR  ( tok fam id -- cells )  provisional cell width
\   TYPE-FIELD:COUNT ( -- n )                               committed field high-water
\   TYPE-NAME:VARIANT-REQUIRE ( na nu -- )                  reserved/collision gate
\   SUMV-ADD         ( fam na nu tag ss sc pc -- vid )      register one canonical, unique variant

using TFAM

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
\   DEV-K-OPAQUE       VAR = none          FLD = none      (header clause)
1 constant DEV-K-DECL
2 constant DEV-K-ARITY
3 constant DEV-K-POLICY
4 constant DEV-K-DERIVE
5 constant DEV-K-VARIANT
6 constant DEV-K-VARIANT-END
7 constant DEV-K-FIELD
8 constant DEV-K-OPAQUE

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
7206 constant E-DEV-DUP-OPAQUE  \ a second OPAQUE clause in one declaration
7207 constant E-DEV-OPAQUE-PACKAGE \ OPAQUE on a family declared outside any package
\ What a declaration reject reports for each code (generated-declaration.f).
s" stale or out-of-order declaration event" E-DEV-TX DECL-REJECT:EXPLAIN
s" field publication broke field-id contiguity" E-DEV-STATE DECL-REJECT:EXPLAIN
s" a second POLICY clause in one declaration" E-DEV-DUP-POLICY DECL-REJECT:EXPLAIN
s" the same DERIVE feature twice in one declaration" E-DEV-DUP-DERIVE DECL-REJECT:EXPLAIN
s" field is outside this declaration" E-DEV-FIELD-SCOPE DECL-REJECT:EXPLAIN
s" declaration does not own this family" E-DEV-FAMILY-SCOPE DECL-REJECT:EXPLAIN
s" a second OPAQUE clause in one declaration" E-DEV-DUP-OPAQUE DECL-REJECT:EXPLAIN
s" OPAQUE requires a family declared in a package" E-DEV-OPAQUE-PACKAGE DECL-REJECT:EXPLAIN

\ The shared malformed-arity code is sumtype.f's, read at load time: a post-hook
\ checked body cannot name that pre-hook constant on a from-source build. The
\ bound itself comes from type-family.f's canonical declaration alphabet through
\ the normal post-hook seam below, so front ends and the event validator cannot
\ drift.
TYPE-DECL:E-TDECL-ARITY constant E-DEV-ARITY  \ arity outside [0, cap] — the shared malformed-arity code

72 constant DEV-BUG-RC          \ internal invariant violation (bad id / oob): fail-closed die

\ ---------------------------------------------------------------------------
\ event scratch (interleaved cells, pointer-free: KIND/FAM/VAR/FLD/OWNER are
\ all integers, so a grow is a plain cell copy). OWNER is the first event id of
\ the frame that emitted the row, so finalized nested declarations remain
\ distinguishable from their still-open parent. One growable buffer is reused
\ after the outer frame ends.
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
PERSISTED-PTR-VARIABLE DEV-A-P    DEV-A-BOOT DEV-A-P !
: DEV-BASE ( -- ptr n ) DEV-A-P @ ;

variable DEV-N        \ provisional event high-water (scratch end, in records)
variable DEV-BASE-FLD \ field-record committed count captured at the OUTER open
variable DEV-FLD-ORD  \ running field-event ordinal within the outer transaction
variable DEV-VAR-ORD  \ running variant tag ordinal within the outer transaction
variable DEV-CUR-VAR  \ current-variant selector: open variant id, else DEV-NO-VARIANT

: DEV-GROW ( n -- ) {: need:n :}
   need DEV-CAP-V @ 2 * max {: nc:n :}
   DEV-A-P  DEV-CAP-V @ DEV-REC *  nc DEV-REC *  REG-GROW1
   nc DEV-CAP-V ! ;
: DEV-ENSURE ( -- )               \ room for the next event id (DEV-N)
   DEV-N @ DEV-CAP-V @ < IF exit THEN
   DEV-N @ 1 + DEV-GROW ;

: DEV-ROW ( n -- ptr n ) {: id:n :}     \ address of provisional event `id`
   id 0 < IF s" decl-event: bad event id" DEV-BUG-RC die THEN
   id DEV-N @ >= IF s" decl-event: bad event id" DEV-BUG-RC die THEN
   id DEV-REC * DEV-BASE + ;
\ ---------------------------------------------------------------------------
\ strict-LIFO transaction frames. Nested COMMIT keeps events + field rows
\ reversible; the outer release reuses the event scratch after every participant
\ has committed. Every frame owns the event high-water, the field ordinal,
\ the variant ordinal, the current-variant selector, the field-record token, and
\ its own serial token.
\ ---------------------------------------------------------------------------
0 cells constant DEVTX.EVN-OFF      \ DEV-N at open (event-arena watermark)
1 cells constant DEVTX.FLDORD-OFF   \ DEV-FLD-ORD at open (field ordinal watermark)
2 cells constant DEVTX.VARORD-OFF   \ DEV-VAR-ORD at open (variant ordinal watermark)
3 cells constant DEVTX.CURVAR-OFF   \ DEV-CUR-VAR at open (current-variant watermark)
4 cells constant DEVTX.FLDTOK-OFF   \ field-record transaction token
5 cells constant DEVTX.TOK-OFF      \ this frame's serial token
6 cells constant DEVTX.STATE-OFF    \ open / reversibly committed
7 cells constant DEVTX.FAM-OFF      \ family owned by this declaration frame
8 cells constant DEVTX.OWNER-OFF    \ exact first event that owns frame rows
9 cells constant DEV-TX-REC

: DEVTX.EVN ( ptr a -- ptr a ) DEVTX.EVN-OFF + ;
: DEVTX.FLDORD ( ptr a -- ptr a ) DEVTX.FLDORD-OFF + ;
: DEVTX.VARORD ( ptr a -- ptr a ) DEVTX.VARORD-OFF + ;
: DEVTX.CURVAR ( ptr a -- ptr a ) DEVTX.CURVAR-OFF + ;
: DEVTX.FLDTOK ( ptr a -- ptr a ) DEVTX.FLDTOK-OFF + ;
: DEVTX.TOK ( ptr a -- ptr a ) DEVTX.TOK-OFF + ;
: DEVTX.STATE ( ptr a -- ptr a ) DEVTX.STATE-OFF + ;
: DEVTX.FAM ( ptr a -- ptr a ) DEVTX.FAM-OFF + ;
: DEVTX.OWNER ( ptr a -- ptr a ) DEVTX.OWNER-OFF + ;

4 constant DEV-TX-CAP-INIT
variable DEV-TX-CAP-V   DEV-TX-CAP-INIT DEV-TX-CAP-V !
create DEV-TX-BOOT   DEV-TX-CAP-INIT DEV-TX-REC * allot
PERSISTED-PTR-VARIABLE DEV-TX-P    DEV-TX-BOOT DEV-TX-P !
variable DEV-TX-DEPTH
variable DEV-TX-SERIAL

0 constant DEV-TX-OPEN
1 constant DEV-TX-COMMITTED

: DEV-TX-BASE ( -- ptr n ) DEV-TX-P @ ;
: DEV-TX-GROW ( -- )
   DEV-TX-CAP-V @ 2 * {: nc:n :}
   DEV-TX-P  DEV-TX-CAP-V @ DEV-TX-REC *  nc DEV-TX-REC *  REG-GROW1
   nc DEV-TX-CAP-V ! ;
: DEV-TX-ENSURE ( -- )
   DEV-TX-DEPTH @ DEV-TX-CAP-V @ < IF exit THEN
   DEV-TX-GROW ;
: DEV-TX-AT ( n -- ptr n ) DEV-TX-REC * DEV-TX-BASE + ;
: DEV-TX-TOP ( -- ptr n )
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
   0
   DEV-N @ DEV-CUR-START ?do
      i DEV-PROV-KIND@ k =
      i DEV-PROV-OWNER@ DEV-CUR-OWNER = and IF 1 + THEN
   loop
   0 <> ;
: DEV-CUR-HAS-KIND-VAR? ( n n -- bool ) {: k:n v:n :}
   0
   DEV-N @ DEV-CUR-START ?do
      i DEV-PROV-KIND@ k =
      i DEV-PROV-VAR@ v = and
      i DEV-PROV-OWNER@ DEV-CUR-OWNER = and IF 1 + THEN
   loop
   0 <> ;

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

\ OPAQUE needs a package: the generated words land in the declaring package's
\ private wordlist, which a top-level family does not have (DERIVE init asks the
\ same of its family, sumtype.f TDINIT-REQUIRE). The clause refuses that before
\ any name is checked.
: DEV-OPAQUE ( n n -- n ) {: tok:n fam:n :}   \ header: OPAQUE clause (at most once)
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   fam TFAM-PKG$ nip 0= IF E-DEV-OPAQUE-PACKAGE throw THEN
   DEV-K-OPAQUE DEV-CUR-HAS-KIND? IF E-DEV-DUP-OPAQUE throw THEN
   DEV-K-OPAQUE fam DEV-NO-VARIANT DEV-NO-FIELD DEV-EMIT
   tok ;

\ --- variant open/close. Open validates the variant tail before SUMV-ADD's
\ canonical/duplicate registration and pins its id as the current-variant
\ selector. Named-field variants carry no positional payload schema
\ (ss=sc=pc=0); field rows are discovered downstream by (family, variant-id).
\ Close clears the selector.
: DEV-VARIANT ( n n ptr u8 n -- n ) {: tok:n fam:n na:ptr nu:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   na nu TYPE-NAME:VARIANT-REQUIRE
   fam na nu DEV-VAR-ORD @ 0 0 0 SUMV-ADD {: vid:n :}
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
\ COMMIT retains both frames so a later participant failure can roll them back.
\ Releasing them afterwards is the runner's release step (PART-RELEASE
\ below); the public FINALIZE here is the standalone entry point and re-proves
\ everything, which is why the participant does not use it.
: DEV-PREPARE ( n -- ) {: tok:n :}
   tok DEV-TX-OPEN-REQUIRE
   DEV-FLD-PROVISIONAL-COUNT DEV-BASE-FLD @ DEV-FLD-ORD @ + <>
      IF E-DEV-STATE throw THEN ;

: DEV-COMMIT ( n -- ) {: tok:n :}
   tok DEV-PREPARE
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:COMMIT
   DEV-TX-COMMITTED DEV-TX-TOP DEVTX.STATE ! ;

: DEV-FINALIZE ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.STATE @ DEV-TX-COMMITTED <> IF E-DEV-TX throw THEN
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:FINALIZE
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
   DEV-TX-DEPTH @ 0= IF 0 DEV-N ! THEN ;

: DEV-PUBLISH ( n -- ) {: tok:n :}
   tok DEV-PREPARE
   tok DEV-COMMIT
   tok DEV-FINALIZE ;

\ roll back every watermark this frame owns and delegate the field-registry
\ cursor to the field record. The family/variant/schema/layout cursors
\ are retired by the enclosing checker scope/candidate frame (see header).
\ A live frame is always either open or reversibly committed — the only two
\ states DEV-OPEN and DEV-COMMIT set — and both roll back the same way, so there
\ is no third value to reject here.
: DEV-FRAME-RESTORE ( ptr n -- ) {: r:ptr :}   \ every watermark one frame owns
   r DEVTX.EVN @ DEV-N !
   r DEVTX.FLDORD @ DEV-FLD-ORD !
   r DEVTX.VARORD @ DEV-VAR-ORD !
   r DEVTX.CURVAR @ DEV-CUR-VAR ! ;

: DEV-FRAME-POP ( -- )            \ retire the live top frame's own watermarks
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
   DEV-TX-DEPTH @ DEV-TX-AT DEV-FRAME-RESTORE ;

: DEV-ROLLBACK ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:ROLLBACK
   DEV-FRAME-POP ;

: DEV-RESET ( -- )                \ base state; re-seeded at load (process-local)
   0 DEV-N !
   0 DEV-TX-DEPTH !   0 DEV-TX-SERIAL !
   0 DEV-BASE-FLD !   0 DEV-FLD-ORD !   0 DEV-VAR-ORD !
   DEV-NO-VARIANT DEV-CUR-VAR ! ;
DEV-RESET

\ Declaration-transaction participant.  Two cells hold the EXACT event token and
\ field token this participant opened, both written before the declaration body
\ can run.  Every later step proves and drives that saved pair.  Reading the live
\ top instead — as this adapter used to — let any frame the body opened and never
\ closed stand in for the participant's own frame: prepare then validated the
\ wrong frame, commit and finalize published and released the wrong frame, and
\ the participant's real frame survived the transaction.  Release cannot report
\ anything, so a defect of that shape has to be caught where it belongs: at this
\ participant's PREPARE.
0 constant DEV-NO-TOKEN          \ cell sentinel; DEV-OPEN never mints a token <= 0
variable DEV-PART-TOK
variable DEV-PART-FLD

: DEV-PART-CLEAR ( -- )
   DEV-NO-TOKEN DEV-PART-TOK !
   DEV-NO-TOKEN DEV-PART-FLD ! ;

: DEV-PART-BIND ( n -- ) {: tok:n :}   \ pin the frame OPEN just created
   tok DEV-PART-TOK !
   DEV-TX-TOP DEVTX.FLDTOK @ DEV-PART-FLD ! ;

: DEV-PART-OPEN? ( -- bool )
   DEV-PART-TOK @ DEV-NO-TOKEN <> ;

: DEV-PART-TOKEN ( -- n ) DEV-PART-TOK @ ;
: DEV-PART-FIELD-TOKEN ( -- n ) DEV-PART-FLD @ ;

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
\ and release then only advance and drop marks this proof already covered.
\ DEV-PREPARE repeats the token proof because it is also the standalone public
\ entry point, so a mutation that guts THIS word still rejects one step later,
\ in PART-COMMIT.  The runner runs every prepare before any commit, and this
\ participant commits first, so no participant has committed when that lands.
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

public

: PART-SNAPSHOT ( -- )
   DEV-PART-CLEAR                 \ OPEN may reject before it mutates anything
   DEV-OPEN DEV-PART-BIND ;

: PART-PREPARE ( -- )
   DEV-PART-OPEN? IF DEV-PART-PROVE THEN ;

: PART-COMMIT ( -- )
   DEV-PART-OPEN? IF DEV-PART-TOKEN DEV-COMMIT THEN ;

: PART-ROLLBACK ( -- )
   DEV-PART-OPEN? IF
      DEV-PART-TOKEN DEV-PART-FIELD-TOKEN DEV-RETIRE-THROUGH
      DEV-PART-CLEAR
   THEN ;

\ Release. The runner calls this only after every participant's reversible
\ commit succeeded, so it must not be able to reject and it is not caught. It is
\ therefore NOT DEV-FINALIZE: that word is the standalone public entry point and
\ re-proves the token, the frame state, and the field owner's watermarks, and
\ any of those proofs can throw. Everything they would re-prove was already
\ proved by this participant's own PREPARE (DEV-PART-PROVE) and then committed,
\ so all that is left here is to drop the two frames this participant owns — the
\ field frame through the owner's private total release vector, then our own
\ event frame — and clear the saved token pair.
: PART-RELEASE ( -- )
   DEV-PART-OPEN? IF
      TDECL-FIELD-RELEASE-XT
      DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
      DEV-TX-DEPTH @ 0= IF 0 DEV-N ! THEN
      DEV-PART-CLEAR
   THEN ;

private

\ ---------------------------------------------------------------------------
\ IMAGE-SNAPSHOT RESET — a different "snapshot" from the participant step just
\ above. PART-SNAPSHOT opens a DECLARATION savepoint; this word prepares the
\ module to be written into a persisted engine IMAGE.
\
\ Both arenas here grow through REG-GROW1, which re-allocates from the
\ host process's allocator, so a grown base is a live address of THIS process and
\ nothing else. The snapshot writer copies the DP heap verbatim, so a grown base
\ left in place is persisted as a number that means nothing in the process that
\ restores it: the restored image's first declaration reads DEV-BASE and stores
\ through it. Measured, before this word existed: a warm image stored 1 into an
\ unmapped page and died SIGSEGV/SIGBUS on its first ENUM, and on the runs where
\ that address happened to land inside a mapped region it corrupted it silently
\ instead. The three cursors go back to zero with the arenas because the boot
\ stores they point at are the small baked ones. Event scratch has no durable
\ contents; declarations publish their consequences into separate registries.
\ The boot stores clear whole as well: rows the last declaration left in them
\ would differ between a build that reuses its files and a fresh one.
\
\ It runs at depth 0 or not at all: an image captured mid-declaration would carry
\ frames whose field-owner and checker savepoints no one will ever close.
: DEV-SNAPSHOT-RESET ( -- )
   DECLARATION-TRANSACTION:ACTIVE? DEV-TX-DEPTH @ 0 <> or IF
      s" decl-event: snapshot inside declaration transaction" DEV-BUG-RC die
   THEN
   DEV-A-BOOT DEV-A-P !                DEV-CAP-INIT DEV-CAP-V !
   DEV-TX-BOOT DEV-TX-P !              DEV-TX-CAP-INIT DEV-TX-CAP-V !
   DEV-A-BOOT 0 DEV-CAP-INIT DEV-REC * CELL / ARENA-CELLS-ZERO
   DEV-TX-BOOT 0 DEV-TX-CAP-INIT DEV-TX-REC * CELL / ARENA-CELLS-ZERO
   DEV-RESET ;

\ ---------------------------------------------------------------------------
\ public API: the transaction both front ends drive.
\ Phase constraints: CURRENT returns the frame opened by the declaration
\ savepoint and rejects outside an open declaration; OPEN remains the explicit
\ standalone interface. DECL/ARITY/
\ POLICY/DERIVE/OPAQUE/VARIANT/END-VARIANT/FIELD and provisional FIELD-SCHEMA@/
\ PAYLOAD-* queries are legal only between OPEN and its
\ COMMIT or ROLLBACK, each takes and returns the same token, and each throws
\ E-DEV-TX if the token is not the innermost open frame. COMMIT retains a
\ reversible frame; FINALIZE releases it, ROLLBACK restores it, and PUBLISH
\ performs PREPARE, COMMIT, and FINALIZE in one call. FINALIZE is the
\ standalone release and re-proves the token, the frame state, and the field
\ owner's watermarks, so it is not what the runner's total release step
\ runs. The outer release or rollback discards the event scratch.
\ ---------------------------------------------------------------------------
public

\ In a native build a transaction cannot span a file load: every file boundary
\ runs CHECKER-REG:FILE-SCRATCH-RETIRE, whose chain refuses an open frame
\ (generated-declaration-protection.f SNAPSHOT-RESET throws inside RUN,
\ DEV-SNAPSHOT-RESET dies after a standalone OPEN), since resetting the event
\ stores would drop the frame's savepoints. The declaration forms parse to
\ their terminators and load nothing.
: OPEN ( -- n ) DEV-OPEN ;
: CURRENT ( -- n )
   DEV-PART-OPEN? 0= IF E-DEV-TX throw THEN
   DEV-PART-TOKEN dup DEV-TX-OPEN-REQUIRE ;
: DECL ( n n -- n ) DEV-DECL ;
: ARITY ( n n n -- n ) DEV-ARITY ;
: POLICY ( n n n -- n ) DEV-POLICY ;
: DERIVE ( n n n -- n ) DEV-DERIVE ;
: OPAQUE ( n n -- n ) DEV-OPAQUE ;
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
: SNAPSHOT-RESET ( -- ) DEV-SNAPSHOT-RESET ;

: CURRENT-VARIANT ( -- n ) DEV-CUR-VAR @ ;
: NO-VARIANT ( -- n ) DEV-NO-VARIANT ;

: DEPTH ( -- n ) DEV-TX-DEPTH @ ;

private

\ The declaration front ends reach this module only through these hooks, and
\ they go through the SAME public words an outside caller uses — CURRENT carries
\ the declaration-frame and open-state guards that the private token reader does
\ not. Installed after the public section because a quotation binds its target
\ at compile time.
: DEV-EVENT-HOOKS-INSTALL ( -- )
   [: CURRENT ;] is TYPE-DECL:TDECL-EVENT-CURRENT-XT
   [: DECL ;] is TYPE-DECL:TDECL-EVENT-DECL-XT
   [: ARITY ;] is TYPE-DECL:TDECL-EVENT-ARITY-XT
   [: FIELD ;] is TYPE-DECL:TDECL-EVENT-FIELD-XT
   [: 0 0= ;] is TYPE-DECL:TDECL-EVENT-ARMED ;

DEV-EVENT-HOOKS-INSTALL

;package

;using
