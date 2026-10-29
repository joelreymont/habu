\ decl-event.f: the declaration-event transaction (package DECL-EVENT) holding all
\ STRUCTURE and ENUM declaration state; the front ends only recognise tokens.
\ Both emit the same DEV-K-FIELD event; only the variant selector, held here,
\ differs. The field name gate is the field record's (type-family.f TYPE-FIELD),
\ surfaced unchanged. State is process-local and re-seeded at load.
\ TYPE-FIELD-OWNER and TYPE-FIELD calls are checked through checker.f PPRIM axioms.

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

\ The field record and SUMV raise the field name-gate and variant duplicate or
\ canonical codes (E-TFAM-DUP, E-PF-NAME, E-TFAM-CASE) unchanged; TYPE-NAME raises
\ 7107 or 7110 before SUMV.
7161 constant E-DEV-TX          \ stale or non-LIFO declaration-event transaction token
7162 constant E-DEV-STATE       \ field-record publication broke the field-id contiguity invariant
7163 constant E-DEV-DUP-POLICY  \ a second POLICY clause in one declaration
7164 constant E-DEV-DUP-DERIVE  \ the same DERIVE feature recorded twice in one declaration
7172 constant E-DEV-FIELD-SCOPE \ provisional field is outside the token's declaration/family
7173 constant E-DEV-FAMILY-SCOPE \ token does not own the requested declaration family
7206 constant E-DEV-DUP-OPAQUE  \ a second OPAQUE clause in one declaration
7207 constant E-DEV-OPAQUE-PACKAGE \ OPAQUE on a family declared outside any package
s" stale or out-of-order declaration event" E-DEV-TX DECL-REJECT:EXPLAIN
s" field publication broke field-id contiguity" E-DEV-STATE DECL-REJECT:EXPLAIN
s" a second POLICY clause in one declaration" E-DEV-DUP-POLICY DECL-REJECT:EXPLAIN
s" the same DERIVE feature twice in one declaration" E-DEV-DUP-DERIVE DECL-REJECT:EXPLAIN
s" field is outside this declaration" E-DEV-FIELD-SCOPE DECL-REJECT:EXPLAIN
s" declaration does not own this family" E-DEV-FAMILY-SCOPE DECL-REJECT:EXPLAIN
s" a second OPAQUE clause in one declaration" E-DEV-DUP-OPAQUE DECL-REJECT:EXPLAIN
s" OPAQUE requires a family declared in a package" E-DEV-OPAQUE-PACKAGE DECL-REJECT:EXPLAIN

\ E-DEV-ARITY reads sumtype.f's malformed-arity code at top level: a post-hook
\ checked body cannot name that pre-hook constant on a from-source build.
TYPE-DECL:E-TDECL-ARITY constant E-DEV-ARITY  \ arity outside [0, cap] — the shared malformed-arity code

72 constant DEV-BUG-RC          \ internal invariant violation (bad id / oob): fail-closed die

\ Event scratch: interleaved integer cells, so a grow is a plain cell copy. OWNER
\ is the first event id of the frame that emitted the row, so finalized nested
\ declarations stay distinguishable from their still-open parent.
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
   DEV-A-P DEV-A-BOOT  DEV-CAP-V @ DEV-REC *  nc DEV-REC *  ARENA-BOOT-GROW
   nc DEV-CAP-V ! ;
: DEV-ENSURE ( -- )
   DEV-N @ DEV-CAP-V @ < IF exit THEN
   DEV-N @ 1 + DEV-GROW ;

: DEV-ROW ( n -- ptr n ) {: id:n :}
   id 0 < IF s" decl-event: bad event id" DEV-BUG-RC die THEN
   id DEV-N @ >= IF s" decl-event: bad event id" DEV-BUG-RC die THEN
   id DEV-REC * DEV-BASE + ;
\ Strict-LIFO transaction frames. A nested COMMIT keeps events and field rows
\ reversible; the outer release reuses the event scratch. Rollback restores the
\ event scratch, both ordinals, the selector and the field registry; the checker
\ participant owns the family, variant, schema and layout savepoint.
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
   DEV-TX-P DEV-TX-BOOT  DEV-TX-CAP-V @ DEV-TX-REC *  nc DEV-TX-REC *  ARENA-BOOT-GROW
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

\ The serial-wrap guard is kept unexercised like the field owner's (type-family.f
\ OPEN): a wrapped serial would mint a token that compares equal to a live frame.
: DEV-OPEN ( -- n )
   DEV-TX-ENSURE
   DEV-TX-SERIAL @ 1 + dup 0 <= IF drop E-DEV-TX throw THEN
   {: tok:n :}
   \ no event state changes before the field snapshot exists
   TYPE-FIELD-OWNER:OPEN {: fldtok:n :}
   tok DEV-TX-SERIAL !
   DEV-TX-DEPTH @ 0= IF
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

\ Duplicate-clause scans cover this frame's [start, DEV-N) range: one declaration
\ per frame.
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

: DEV-DECL ( n n -- n ) {: tok:n fam:n :}
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

: DEV-ARITY ( n n n -- n ) {: tok:n fam:n arity:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   arity 0 < arity TFAM-DECL-PARAM-COUNT > or IF E-DEV-ARITY throw THEN
   DEV-K-ARITY fam arity DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-POLICY ( n n n -- n ) {: tok:n fam:n policy:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-K-POLICY DEV-CUR-HAS-KIND? IF E-DEV-DUP-POLICY throw THEN
   DEV-K-POLICY fam policy DEV-NO-FIELD DEV-EMIT
   tok ;

: DEV-DERIVE ( n n n -- n ) {: tok:n fam:n feature:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-K-DERIVE feature DEV-CUR-HAS-KIND-VAR? IF E-DEV-DUP-DERIVE throw THEN
   DEV-K-DERIVE fam feature DEV-NO-FIELD DEV-EMIT
   tok ;

\ OPAQUE needs a package: the generated words land in the declaring package's
\ private wordlist, which a top-level family lacks (DERIVE init asks the same,
\ sumtype.f TDINIT-REQUIRE).
: DEV-OPAQUE ( n n -- n ) {: tok:n fam:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   fam TFAM-PKG$ nip 0= IF E-DEV-OPAQUE-PACKAGE throw THEN
   DEV-K-OPAQUE DEV-CUR-HAS-KIND? IF E-DEV-DUP-OPAQUE throw THEN
   DEV-K-OPAQUE fam DEV-NO-VARIANT DEV-NO-FIELD DEV-EMIT
   tok ;

\ Named-field variants carry no positional payload schema (ss=sc=pc=0); their
\ field rows are found downstream by (family, variant id).
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

\ The field record validates and throws first (E-TFAM-DUP, E-PF-NAME, E-TFAM-CASE,
\ E-PF-*), so a malformed field emits no event and leaves the ordinal untouched.
\ The event carries the field's eventual committed row id (base + ordinal), proven
\ at PREPARE before any participant commits.
: DEV-FIELD ( n n ptr u8 n n n n n n n n -- n )
   {: tok:n fam:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   tok fam DEV-FAMILY-USE DEV-FAMILY-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ {: fldtok:n :}        \ ADD wants the field-tx token, not our serial
   fldtok fam DEV-CUR-VAR @ na nu sch slot cellsn boff bytesn al flags TYPE-FIELD-OWNER:ADD drop
   DEV-K-FIELD fam DEV-CUR-VAR @  DEV-BASE-FLD @ DEV-FLD-ORD @ +  DEV-EMIT
   DEV-FLD-ORD @ 1 + DEV-FLD-ORD !
   tok ;

\ PREPARE proves the field rows are exactly the contiguous provisional range the
\ event stream names. COMMIT retains both frames for rollback. FINALIZE is the
\ standalone release and re-proves everything, so the participant releases
\ through PART-RELEASE instead.
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

\ The family, variant, schema and layout cursors and the name pool are retired by
\ the enclosing checker frame. Open and committed frames roll back alike.
: DEV-FRAME-RESTORE ( ptr n -- ) {: r:ptr :}
   r DEVTX.EVN @ DEV-N !
   r DEVTX.FLDORD @ DEV-FLD-ORD !
   r DEVTX.VARORD @ DEV-VAR-ORD !
   r DEVTX.CURVAR @ DEV-CUR-VAR ! ;

: DEV-FRAME-POP ( -- )
   DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
   DEV-TX-DEPTH @ DEV-TX-AT DEV-FRAME-RESTORE ;

: DEV-ROLLBACK ( n -- ) {: tok:n :}
   tok DEV-TX-REQUIRE
   DEV-TX-TOP DEVTX.FLDTOK @ TYPE-FIELD-OWNER:ROLLBACK
   DEV-FRAME-POP ;

: DEV-RESET ( -- )
   0 DEV-N !
   0 DEV-TX-DEPTH !   0 DEV-TX-SERIAL !
   0 DEV-BASE-FLD !   0 DEV-FLD-ORD !   0 DEV-VAR-ORD !
   DEV-NO-VARIANT DEV-CUR-VAR ! ;
DEV-RESET

\ The participant holds the exact event and field tokens it opened, both saved
\ before the body runs. Reading the live top instead let a frame the body opened
\ and never closed stand in for its own; release cannot report, so PREPARE
\ catches that shape.
0 constant DEV-NO-TOKEN          \ cell sentinel; DEV-OPEN never mints a token <= 0
variable DEV-PART-TOK
variable DEV-PART-FLD

: DEV-PART-CLEAR ( -- )
   DEV-NO-TOKEN DEV-PART-TOK !
   DEV-NO-TOKEN DEV-PART-FLD ! ;

: DEV-PART-BIND ( n -- ) {: tok:n :}
   tok DEV-PART-TOK !
   DEV-TX-TOP DEVTX.FLDTOK @ DEV-PART-FLD ! ;

: DEV-PART-OPEN? ( -- bool )
   DEV-PART-TOK @ DEV-NO-TOKEN <> ;

: DEV-PART-TOKEN ( -- n ) DEV-PART-TOK @ ;
: DEV-PART-FIELD-TOKEN ( -- n ) DEV-PART-FLD @ ;

\ Kept unexercised, like the DEV-OPEN serial-wrap guard: DEVTX.FLDTOK has one
\ writer, DEV-OPEN, so this fires only if it gains a second. The reachable form,
\ in DEV-RETIRE-THROUGH, has a negative fixture.
: DEV-PART-FIELD-REQUIRE ( -- )
   DEV-TX-TOP DEVTX.FLDTOK @ DEV-PART-FIELD-TOKEN <> IF E-DEV-TX throw THEN ;

\ PREPARE is the participant's whole fallible boundary; commit and release only
\ advance and drop marks it proved. DEV-PREPARE repeats the token proof as the
\ standalone entry, so gutting this word still rejects at PART-COMMIT, before any
\ participant has committed (every prepare runs first and this one commits first).
: DEV-PART-PROVE ( -- )
   DEV-PART-TOKEN DEV-TX-OPEN-REQUIRE
   DEV-PART-FIELD-REQUIRE
   DEV-PART-TOKEN DEV-PREPARE ;

\ Failed-declaration cleanup retires the saved frame and every frame above it,
\ LIFO, after proving everything. The pair cross-check is not redundant: each
\ token lookup proves only that its own stack holds the token, so frame A's event
\ token with frame B's field token would desynchronise the stacks silently. The
\ field owner alone retires field frames.
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

\ The runner calls release only after every reversible commit succeeded, so it
\ must not reject and is not caught: hence not DEV-FINALIZE, which re-proves and
\ can throw. PART-PREPARE already proved the frames; this drops the field frame
\ through the owner's total release vector, then the event frame.
: PART-RELEASE ( -- )
   DEV-PART-OPEN? IF
      TDECL-FIELD-RELEASE-XT
      DEV-TX-DEPTH @ 1 - DEV-TX-DEPTH !
      DEV-TX-DEPTH @ 0= IF 0 DEV-N ! THEN
      DEV-PART-CLEAR
   THEN ;

private

\ Image-snapshot reset, not the declaration PART-SNAPSHOT. Both arenas grow by
\ ARENA-BOOT-GROW into fresh pages of this process and the snapshot copies the DP
\ heap verbatim, so a grown base would persist as a dead address: measured, a warm
\ image stored into an unmapped page and died SIGSEGV/SIGBUS on its first ENUM.
\ The boot stores clear whole so a reusing build and a fresh one capture the same
\ bytes. Runs only at depth 0: a mid-declaration image would carry frames whose
\ savepoints no one closes.
: DEV-SNAPSHOT-RESET ( -- )
   DECLARATION-TRANSACTION:ACTIVE? DEV-TX-DEPTH @ 0 <> or IF
      s" decl-event: snapshot inside declaration transaction" DEV-BUG-RC die
   THEN
   DEV-A-P DEV-A-BOOT DEV-CAP-V @ DEV-REC * ARENA-BOOT-RESTORE
   DEV-CAP-INIT DEV-CAP-V !
   DEV-TX-P DEV-TX-BOOT DEV-TX-CAP-V @ DEV-TX-REC * ARENA-BOOT-RESTORE
   DEV-TX-CAP-INIT DEV-TX-CAP-V !
   DEV-A-BOOT 0 DEV-CAP-INIT DEV-REC * CELL / ARENA-CELLS-ZERO
   DEV-TX-BOOT 0 DEV-TX-CAP-INIT DEV-TX-REC * CELL / ARENA-CELLS-ZERO
   DEV-RESET ;

\ Public API. CURRENT answers the frame the declaration savepoint opened and
\ rejects outside one; OPEN is the standalone interface. The event words and the
\ provisional FIELD-SCHEMA@ / PAYLOAD-* queries are legal only between OPEN and
\ COMMIT or ROLLBACK, take and return the same token, and throw E-DEV-TX unless
\ it is the innermost open frame. The outer release or rollback discards the
\ event scratch.
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

\ The front ends reach this module only through these hooks, which go through the
\ public words (CURRENT carries the frame and open-state guards the private reader
\ lacks). Installed after the public section: a quotation binds its target at
\ compile time.
: DEV-EVENT-HOOKS-INSTALL ( -- )
   [: CURRENT ;] is TYPE-DECL:TDECL-EVENT-CURRENT-XT
   [: DECL ;] is TYPE-DECL:TDECL-EVENT-DECL-XT
   [: ARITY ;] is TYPE-DECL:TDECL-EVENT-ARITY-XT
   [: FIELD ;] is TYPE-DECL:TDECL-EVENT-FIELD-XT
   [: 0 0= ;] is TYPE-DECL:TDECL-EVENT-ARMED ;

DEV-EVENT-HOOKS-INSTALL

;package

;using
