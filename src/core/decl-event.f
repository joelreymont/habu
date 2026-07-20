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
\  - Variant open/close are events HERE; open performs the variant registration
\    (SUMV-ADD) and surfaces the returned id as the CURRENT-VARIANT selector.
\  - Field name-gate (duplicate / reserved / case) is NOT here: the field record
\    raises those and this module surfaces the throw unchanged — no second gate.
\
\ Transaction shape mirrors the field record's strict-LIFO frames: nested OPEN
\ keeps events + field rows provisional; only the OUTER PUBLISH advances the
\ published high-water and outer-commits the field record. Rollback (nested or
\ outer) retires every watermark THIS module owns — event-arena high-water,
\ field ordinal, variant ordinal, current-variant selector — and delegates the
\ field-registry cursor to the field record's own PF-ROLLBACK. The family /
\ variant / schema / layout registry cursors are REG-PROTECT-sealed against
\ post-hook writes (type-family.f), so their rollback rides the enclosing
\ checker scope/candidate frame (TFAM-ROLLBACK-SAVE/RESTORE), exactly as the
\ current declaration path does (sumtype.f TDECL-MARK/RESTORE): the parse driver
\ opens that frame around OPEN…PUBLISH/ROLLBACK. See the report/contract note.
\
\ Loaded AFTER the checker hook (the sole STRUCTURE/ENUM parser loads later
\ still); state is process-local and re-seeded at load, like top-row.f, so
\ snapshot re-arm rides the later snapshot/AOT dot.
\
\ ---------------------------------------------------------------------------
\ FIELD-RECORD / VARIANT-REGISTRY INTEGRATION SEAM (reconcile at merge).
\ ---------------------------------------------------------------------------
\ The field record is habu-fields-add-shared-6b063c62 (landing as
\ src/core/type-field.f on the Mac); on this base its contract lives in
\ src/core/type-family.f as the pre-hook PF-* transaction + TYPE-FIELD package,
\ and variant registration is SUMV-ADD there too. These are pre-hook raw-memory
\ words the checker cannot model from a post-hook checked body, so this module
\ consumes them through the TRUSTED: forwarders below — the same idiom
\ test/type-family-rollback-suite.f uses for its TWX-PF-* shims and top-row.f
\ for its effect-read boundary. When type-field.f lands, the orchestrator
\ re-points these forwarders; nothing else in this file changes. Consumed
\ contract (named exactly as the field record's dot implies):
\   PF-BEGIN         ( -- tok )                             begin a field transaction
\   PF-ADD           ( tok fam var na nu sch slot cells boff bytes al flags -- tok )
\   PF-COMMIT        ( tok -- )                             commit (outer commit publishes)
\   PF-ROLLBACK      ( tok -- )                             retire provisional field rows
\   TYPE-FIELD:COUNT ( -- n )                               committed field high-water
\   SUMV-ADD         ( fam na nu tag ss sc pc -- vid )      register one variant (dup/canon gate)

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

\ --- named reject codes (thrown, caught by the parser/CHECK path or unit `catch`).
\ Field name-gate + variant dup/canon codes (E-TFAM-DUP 7102, E-PF-NAME 7125,
\ E-TFAM-CASE 7101) are raised by the field record / SUMV and pass through
\ unchanged.
7161 constant E-DEV-TX          \ stale or non-LIFO declaration-event transaction token
7162 constant E-DEV-STATE       \ field-record publication broke the field-id contiguity invariant
7163 constant E-DEV-DUP-POLICY  \ a second POLICY clause in one declaration
7164 constant E-DEV-DUP-DERIVE  \ the same DERIVE feature recorded twice in one declaration

\ The shared "malformed arity" code + cap. Values mirror sumtype.f
\ (E-TDECL-ARITY 7108, TDECL-ARITY-CAP 26, docs §9.2 positional params a..z);
\ they are re-declared here rather than referenced because those globals are
\ pre-hook and do not survive the checked engine's fixpoint self-rebuild, and
\ because the legacy sumtype.f owning them is removed by the type-DSL cutover.
\ Unify into a shared declaration-codes module when that lands (dot: chain).
7108 constant E-DEV-ARITY       \ arity outside [0, cap] — the shared malformed-arity code
26 constant DEV-ARITY-CAP       \ positional params are letters a..z (docs §9.2)

72 constant DEV-BUG-RC          \ internal invariant violation (bad id / oob): fail-closed die

\ ---------------------------------------------------------------------------
\ SEAM forwarders (see header). TRUSTED: because they call pre-hook contract
\ words the checker cannot type here.
\ ---------------------------------------------------------------------------
TRUSTED: DEV-FLD-BEGIN ( -- n ) PF-BEGIN ;
TRUSTED: DEV-FLD-ADD ( n n n ptr u8 n n n n n n n n -- n ) PF-ADD ;
TRUSTED: DEV-FLD-COMMIT ( n -- ) PF-COMMIT ;
TRUSTED: DEV-FLD-ROLLBACK ( n -- ) PF-ROLLBACK ;
TRUSTED: DEV-FLD-COUNT ( -- n ) TYPE-FIELD:COUNT ;
TRUSTED: DEV-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;

\ ---------------------------------------------------------------------------
\ event record arena (interleaved cells, pointer-free: KIND/FAM/VAR/FLD are all
\ integers, so a grow is a plain cell copy and any later snapshot bake is verbatim
\ with no rebase). One growable buffer whose base rides a variable; grow relocates.
\ ---------------------------------------------------------------------------
0 cells constant DEV.KIND-OFF
1 cells constant DEV.FAM-OFF
2 cells constant DEV.VAR-OFF
3 cells constant DEV.FLD-OFF
4 cells constant DEV-REC

: DEV.KIND ( ptr a -- ptr a ) DEV.KIND-OFF + ;
: DEV.FAM ( ptr a -- ptr a ) DEV.FAM-OFF + ;
: DEV.VAR ( ptr a -- ptr a ) DEV.VAR-OFF + ;
: DEV.FLD ( ptr a -- ptr a ) DEV.FLD-OFF + ;

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

: DEV-EMIT ( n n n n -- ) {: k:n fam:n var:n fld:n :}   \ append one provisional event
   DEV-ENSURE
   DEV-N @ DEV-REC * DEV-BASE + {: r:ptr :}
   k r DEV.KIND !   fam r DEV.FAM !   var r DEV.VAR !   fld r DEV.FLD !
   DEV-N @ 1 + DEV-N ! ;

\ ---------------------------------------------------------------------------
\ strict-LIFO transaction frames. Nested PUBLISH keeps events + field rows
\ provisional; only the outer PUBLISH advances DEV-PUB-N and outer-commits the
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
6 cells constant DEV-TX-REC

: DEVTX.EVN ( ptr a -- ptr a ) DEVTX.EVN-OFF + ;
: DEVTX.FLDORD ( ptr a -- ptr a ) DEVTX.FLDORD-OFF + ;
: DEVTX.VARORD ( ptr a -- ptr a ) DEVTX.VARORD-OFF + ;
: DEVTX.CURVAR ( ptr a -- ptr a ) DEVTX.CURVAR-OFF + ;
: DEVTX.FLDTOK ( ptr a -- ptr a ) DEVTX.FLDTOK-OFF + ;
: DEVTX.TOK ( ptr a -- ptr a ) DEVTX.TOK-OFF + ;

4 constant DEV-TX-CAP-INIT
variable DEV-TX-CAP-V   DEV-TX-CAP-INIT DEV-TX-CAP-V !
create DEV-TX-BOOT   DEV-TX-CAP-INIT DEV-TX-REC * allot
variable DEV-TX-P    DEV-TX-BOOT DEV-TX-P !
variable DEV-TX-DEPTH
variable DEV-TX-SERIAL

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

: DEV-OPEN ( -- n )
   DEV-TX-ENSURE
   DEV-TX-SERIAL @ 1 + dup 0 <= IF drop E-DEV-TX throw THEN
   dup DEV-TX-SERIAL ! {: tok:n :}
   DEV-TX-DEPTH @ 0= IF                     \ outer: pin the field base + reset ordinals/selector
      DEV-FLD-COUNT DEV-BASE-FLD !
      0 DEV-FLD-ORD !
      0 DEV-VAR-ORD !
      DEV-NO-VARIANT DEV-CUR-VAR !
   THEN
   DEV-FLD-BEGIN {: fldtok:n :}             \ open the field-record transaction (nests in lockstep)
   DEV-TX-DEPTH @ DEV-TX-AT {: r:ptr :}
   DEV-N @ r DEVTX.EVN !
   DEV-FLD-ORD @ r DEVTX.FLDORD !
   DEV-VAR-ORD @ r DEVTX.VARORD !
   DEV-CUR-VAR @ r DEVTX.CURVAR !
   fldtok r DEVTX.FLDTOK !
   tok r DEVTX.TOK !
   DEV-TX-DEPTH @ 1 + DEV-TX-DEPTH !
   tok ;

\ --- duplicate-clause scan over the current declaration's provisional events
\ (this frame's [start, DEV-N) range; one declaration per transaction frame).
: DEV-CUR-START ( -- n ) DEV-TX-TOP DEVTX.EVN @ ;
: DEV-PROV-KIND@ ( n -- n ) DEV-ROW DEV.KIND @ ;
: DEV-PROV-VAR@ ( n -- n ) DEV-ROW DEV.VAR @ ;
: DEV-CUR-HAS-KIND? ( n -- bool ) {: k:n :}
   0 DEV-FOUND !
   DEV-CUR-START DEV-I !
   BEGIN DEV-I @ DEV-N @ < WHILE
      DEV-I @ DEV-PROV-KIND@ k = IF -1 DEV-FOUND ! THEN
      DEV-I @ 1 + DEV-I !
   REPEAT
   DEV-FOUND @ 0 <> ;
: DEV-CUR-HAS-KIND-VAR? ( n n -- bool ) {: k:n v:n :}
   0 DEV-FOUND !
   DEV-CUR-START DEV-I !
   BEGIN DEV-I @ DEV-N @ < WHILE
      DEV-I @ DEV-PROV-KIND@ k =  DEV-I @ DEV-PROV-VAR@ v =  and IF -1 DEV-FOUND ! THEN
      DEV-I @ 1 + DEV-I !
   REPEAT
   DEV-FOUND @ 0 <> ;

\ --- declaration open + header clauses.
: DEV-DECL ( n n -- n ) {: tok:n fam:n :}          \ open one declaration
   tok DEV-TX-REQUIRE
   DEV-K-DECL fam DEV-NO-VARIANT DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-ARITY ( n n n -- n ) {: tok:n fam:n arity:n :}   \ header: arity value (shared bound)
   tok DEV-TX-REQUIRE
   arity 0 < arity DEV-ARITY-CAP > or IF E-DEV-ARITY throw THEN
   DEV-K-ARITY fam arity DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-POLICY ( n n n -- n ) {: tok:n fam:n policy:n :}   \ header: POLICY clause (at most once)
   tok DEV-TX-REQUIRE
   DEV-K-POLICY DEV-CUR-HAS-KIND? IF E-DEV-DUP-POLICY throw THEN
   DEV-K-POLICY fam policy DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-DERIVE ( n n n -- n ) {: tok:n fam:n feature:n :}   \ header: DERIVE feature (each once)
   tok DEV-TX-REQUIRE
   DEV-K-DERIVE feature DEV-CUR-HAS-KIND-VAR? IF E-DEV-DUP-DERIVE throw THEN
   DEV-K-DERIVE fam feature DEV-NO-FIELD DEV-EMIT
   tok ;

\ --- variant open/close. Open registers the variant (SUMV-ADD: dup / canon /
\ reserved gate passes through) and pins its id as the current-variant selector;
\ named-field variants carry no positional payload schema (ss=sc=pc=0), the field
\ rows are discovered downstream by (family, variant-id). Close clears the selector.
: DEV-VARIANT ( n n ptr u8 n -- n ) {: tok:n fam:n na:ptr nu:n :}
   tok DEV-TX-REQUIRE
   fam na nu DEV-VAR-ORD @ 0 0 0 DEV-SUMV-ADD {: vid:n :}
   vid DEV-CUR-VAR !
   DEV-VAR-ORD @ 1 + DEV-VAR-ORD !
   DEV-K-VARIANT fam vid DEV-NO-FIELD DEV-EMIT
   tok ;
: DEV-END-VARIANT ( n n -- n ) {: tok:n fam:n :}
   tok DEV-TX-REQUIRE
   DEV-K-VARIANT-END fam DEV-CUR-VAR @ DEV-NO-FIELD DEV-EMIT
   DEV-NO-VARIANT DEV-CUR-VAR !
   tok ;

\ --- the shared field event. Adds the field to the field record FIRST under the
\ current-variant selector (the record validates owner/name/schema/layout and
\ throws — E-TFAM-DUP / E-PF-NAME / E-TFAM-CASE / E-PF-* — on any violation, so a
\ malformed field emits NO event and leaves the ordinal untouched), then records
\ the event with the field's eventual committed row id (base + ordinal, proven at
\ PUBLISH). STRUCTURE (selector NO-VARIANT) and ENUM (selector = open variant)
\ produce the identical event + row shape.
: DEV-FIELD ( n n ptr u8 n n n n n n n n -- n )
   {: tok:n fam:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ {: fldtok:n :}        \ PF-ADD wants the field-tx token, not our serial
   fldtok fam DEV-CUR-VAR @ na nu sch slot cellsn boff bytesn al flags DEV-FLD-ADD drop
   DEV-K-FIELD fam DEV-CUR-VAR @  DEV-BASE-FLD @ DEV-FLD-ORD @ +  DEV-EMIT
   DEV-FLD-ORD @ 1 + DEV-FLD-ORD !
   tok ;

\ atomic publication boundary. Nested PUBLISH just closes its field-tx frame and
\ leaves events provisional; the OUTER PUBLISH outer-commits the field record and,
\ ONLY THEN, advances the published high-water — publishing every event of the
\ whole nested tree in one step. The contiguity assertion turns "field rows commit
\ as [base, base+ordinal)" from an assumption into a checked invariant.
: DEV-PUBLISH ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ DEV-FLD-COMMIT
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
   DEV-TX-DEPTH @ 0= IF
      DEV-FLD-COUNT DEV-BASE-FLD @ DEV-FLD-ORD @ + <> IF E-DEV-STATE throw THEN
      DEV-N @ DEV-PUB-N !
   THEN ;

\ roll back every watermark this frame owns and delegate the field-registry
\ cursor to the field record. DEV-PUB-N is never touched: nothing is published
\ until the outer PUBLISH, so a rejected (nested or outer) stream leaves the
\ published view exactly as it was. The family/variant/schema/layout cursors are
\ retired by the enclosing checker scope/candidate frame (see header).
: DEV-ROLLBACK ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP {: r:ptr :}
   r DEVTX.FLDTOK @ DEV-FLD-ROLLBACK
   r DEVTX.EVN @ DEV-N !
   r DEVTX.FLDORD @ DEV-FLD-ORD !
   r DEVTX.VARORD @ DEV-VAR-ORD !
   r DEVTX.CURVAR @ DEV-CUR-VAR !
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH ! ;

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
      DEV-I @ 1 + DEV-I !
   REPEAT ;

\ ---------------------------------------------------------------------------
\ public API: the transaction both front ends drive + read-only reflection.
\ Phase constraints: OPEN starts a frame and returns its token; DECL/ARITY/
\ POLICY/DERIVE/VARIANT/END-VARIANT/FIELD are legal only between OPEN and its
\ PUBLISH or ROLLBACK, each takes and returns the SAME token, and each throws
\ E-DEV-TX if the token is not the innermost open frame. PUBLISH / ROLLBACK
\ consume the token and close the frame. Reflection + IDENTITY read only
\ PUBLISHED events and are legal any time.
\ ---------------------------------------------------------------------------
public

: OPEN ( -- n ) DEV-OPEN ;
: DECL ( n n -- n ) DEV-DECL ;
: ARITY ( n n n -- n ) DEV-ARITY ;
: POLICY ( n n n -- n ) DEV-POLICY ;
: DERIVE ( n n n -- n ) DEV-DERIVE ;
: VARIANT ( n n ptr u8 n -- n ) DEV-VARIANT ;
: END-VARIANT ( n n -- n ) DEV-END-VARIANT ;
: FIELD ( n n ptr u8 n n n n n n n n -- n ) DEV-FIELD ;
: PUBLISH ( n -- ) DEV-PUBLISH ;
: ROLLBACK ( n -- ) DEV-ROLLBACK ;
: RESET ( -- ) DEV-RESET ;

: CURRENT-VARIANT ( -- n ) DEV-CUR-VAR @ ;
: NO-VARIANT ( -- n ) DEV-NO-VARIANT ;

: COUNT ( -- n ) DEV-PUB-N@ ;
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

;package
