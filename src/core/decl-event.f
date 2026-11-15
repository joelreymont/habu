\ decl-event.f: the event log of the one open STRUCTURE or ENUM declaration
\ (package DECL-EVENT); the front ends only recognise tokens. Both emit the same
\ DEV-K-FIELD event; only the variant selector, held here, differs. The runner
\ opens one declaration at a time: PART-SNAPSHOT starts an empty log, and the
\ checker frame's rollback rewinds the declaration's field rows.

using SCHEMA-REG
using TFAM

package TYPE-DECL
private
package DECL-EVENT

\ --- syntax-event kinds (record tag values). Slot meaning by kind (the FAM slot
\ is always the family id):
\   DEV-K-POLICY       VAR = policy code   FLD = none      (header clause)
\   DEV-K-DERIVE       VAR = feature code  FLD = none      (header clause)
\   DEV-K-VARIANT      VAR = variant id    FLD = none      (open one enum variant)
\   DEV-K-VARIANT-END  VAR = variant id    FLD = none      (close the open variant)
\   DEV-K-FIELD        VAR = owner variant FLD = field id  (shared field event)
\   DEV-K-OPAQUE       VAR = none          FLD = none      (header clause)
1 constant DEV-K-POLICY
2 constant DEV-K-DERIVE
3 constant DEV-K-VARIANT
4 constant DEV-K-VARIANT-END
5 constant DEV-K-FIELD
6 constant DEV-K-OPAQUE

-1 constant DEV-NO-VARIANT   \ VAR sentinel: no open variant (structure fields, framing)
-1 constant DEV-NO-FIELD     \ FLD sentinel for every non-field event

\ The field record and SUMV raise the field name-gate and variant duplicate or
\ canonical codes (E-TFAM-DUP, E-PF-NAME, E-TFAM-CASE) unchanged; TYPE-NAME raises
\ 7107 or 7110 before SUMV.
7163 constant E-DEV-DUP-POLICY  \ a second POLICY clause in one declaration
7164 constant E-DEV-DUP-DERIVE  \ the same DERIVE feature recorded twice in one declaration
7206 constant E-DEV-DUP-OPAQUE  \ a second OPAQUE clause in one declaration
7207 constant E-DEV-OPAQUE-PACKAGE \ OPAQUE on a family declared outside any package
s" a second POLICY clause in one declaration"
s" keep one `POLICY` clause in the declaration header"
E-DEV-DUP-POLICY DECL-REJECT:EXPLAIN
s" the same DERIVE feature twice in one declaration"
s" name each `DERIVE` feature once"
E-DEV-DUP-DERIVE DECL-REJECT:EXPLAIN
s" a second OPAQUE clause in one declaration"
s" keep one `OPAQUE` clause in the declaration header"
E-DEV-DUP-OPAQUE DECL-REJECT:EXPLAIN
s" OPAQUE requires a family declared in a package"
s" declare the `OPAQUE` structure inside a `package`, or drop `OPAQUE`"
E-DEV-OPAQUE-PACKAGE DECL-REJECT:EXPLAIN

\ Event scratch: interleaved integer cells, so a grow is a plain cell copy.
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
PERSISTED-PTR-VARIABLE DEV-A-P    DEV-A-BOOT DEV-A-P !
: DEV-BASE ( -- ptr n ) DEV-A-P @ ;

variable DEV-N        \ events logged by the open declaration
variable DEV-BASE-FLD \ field-record count when the declaration opened
variable DEV-FLD-ORD  \ fields logged by the open declaration
variable DEV-VAR-ORD  \ variants logged by the open declaration
variable DEV-CUR-VAR  \ current-variant selector: open variant id, else DEV-NO-VARIANT

: DEV-GROW ( n -- ) {: need:n :}
   need DEV-CAP-V @ 2 * max {: nc:n :}
   DEV-A-P DEV-CAP-V @ DEV-REC *  nc DEV-REC *  REG-GROW1
   nc DEV-CAP-V ! ;
: DEV-ENSURE ( -- )
   DEV-N @ DEV-CAP-V @ < IF exit THEN
   DEV-N @ 1 + DEV-GROW ;

: DEV-ROW ( n -- ptr n ) DEV-REC * DEV-BASE + ;
: DEV-EMIT ( n n n n -- ) {: k:n fam:n var:n fld:n :}
   DEV-ENSURE
   DEV-N @ DEV-ROW {: r:ptr :}
   k r DEV.KIND !   fam r DEV.FAM !   var r DEV.VAR !   fld r DEV.FLD !
   DEV-N @ 1 + DEV-N ! ;
: DEV-PROV-KIND@ ( n -- n ) DEV-ROW DEV.KIND @ ;
: DEV-PROV-FAM@ ( n -- n ) DEV-ROW DEV.FAM @ ;
: DEV-PROV-VAR@ ( n -- n ) DEV-ROW DEV.VAR @ ;
: DEV-PROV-FLD@ ( n -- n ) DEV-ROW DEV.FLD @ ;
: DEV-CUR-HAS-KIND? ( n -- bool ) {: k:n :}
   0
   DEV-N @ 0 ?do
      i DEV-PROV-KIND@ k = IF 1 + THEN
   loop
   0 <> ;
: DEV-CUR-HAS-KIND-VAR? ( n n -- bool ) {: k:n v:n :}
   0
   DEV-N @ 0 ?do
      i DEV-PROV-KIND@ k =
      i DEV-PROV-VAR@ v = and IF 1 + THEN
   loop
   0 <> ;

: DEV-PAYLOAD-OPEN? ( n n n -- bool ) {: id:n fam:n vid:n :}
   id DEV-PROV-KIND@ DEV-K-VARIANT =
   id DEV-PROV-FAM@ fam = and
   id DEV-PROV-VAR@ vid = and ;

: DEV-PAYLOAD-END? ( n n n -- bool ) {: id:n fam:n vid:n :}
   id DEV-PROV-KIND@ DEV-K-VARIANT-END =
   id DEV-PROV-FAM@ fam = and
   id DEV-PROV-VAR@ vid = and ;

\ A variant's payload is the field events between its DEV-K-VARIANT and its
\ DEV-K-VARIANT-END: start and end event ids.
: DEV-PAYLOAD-RANGE ( n n -- n n ) {: fam:n vid:n :}
   0 BEGIN dup fam vid DEV-PAYLOAD-OPEN? 0= WHILE 1 + REPEAT
   1 + dup
   BEGIN dup fam vid DEV-PAYLOAD-END? 0= WHILE 1 + REPEAT ;

public

\ The family, variant, field, schema and layout cursors and the name pool roll
\ back with the checker frame's participant, not here.
: PART-SNAPSHOT ( -- )
   0 DEV-N !
   TYPE-FIELD:COUNT DEV-BASE-FLD !
   0 DEV-FLD-ORD !   0 DEV-VAR-ORD !
   DEV-NO-VARIANT DEV-CUR-VAR ! ;

: PART-COMMIT ( -- ) PF-COMMIT ;

\ The event words and the provisional FIELD-SCHEMA@ / PAYLOAD-* queries act on
\ the open declaration. The declaration forms parse to their terminators and
\ load nothing, so no declaration spans a file load.
: FIELD-SCHEMA@ ( n -- n ) PF-ROW PF.SCH @ ;

: PAYLOAD-N ( n n -- n ) DEV-PAYLOAD-RANGE swap - ;

: PAYLOAD-SCHEMA@ ( n n n -- n ) {: fam:n vid:n index:n :}
   fam vid DEV-PAYLOAD-RANGE drop index + DEV-PROV-FLD@ {: fld:n :}
   fld FIELD-SCHEMA@ ;

\ The arity is a header check: it logs no event.
: ARITY ( n n -- ) {: fam:n arity:n :}
   arity 0 < arity TFAM-DECL-PARAM-COUNT > or IF TYPE-DECL:E-TDECL-ARITY throw THEN ;

: POLICY ( n n -- ) {: fam:n policy:n :}
   DEV-K-POLICY DEV-CUR-HAS-KIND? IF E-DEV-DUP-POLICY throw THEN
   DEV-K-POLICY fam policy DEV-NO-FIELD DEV-EMIT ;

: DERIVE ( n n -- ) {: fam:n feature:n :}
   DEV-K-DERIVE feature DEV-CUR-HAS-KIND-VAR? IF E-DEV-DUP-DERIVE throw THEN
   DEV-K-DERIVE fam feature DEV-NO-FIELD DEV-EMIT ;

\ OPAQUE needs a package: the generated words land in the declaring package's
\ private wordlist, which a top-level family lacks.
: OPAQUE ( n -- ) {: fam:n :}
   fam TFAM-PKG$ nip 0= IF E-DEV-OPAQUE-PACKAGE throw THEN
   DEV-K-OPAQUE DEV-CUR-HAS-KIND? IF E-DEV-DUP-OPAQUE throw THEN
   DEV-K-OPAQUE fam DEV-NO-VARIANT DEV-NO-FIELD DEV-EMIT ;

\ Named-field variants carry no positional payload schema (ss=sc=pc=0); their
\ field rows are found downstream by (family, variant id).
: VARIANT ( n ptr u8 n -- ) {: fam:n na:ptr nu:n :}
   na nu TYPE-NAME:VARIANT-REQUIRE
   fam na nu DEV-VAR-ORD @ 0 0 0 SUMV-ADD {: vid:n :}
   vid DEV-CUR-VAR !
   DEV-VAR-ORD @ 1 + DEV-VAR-ORD !
   DEV-K-VARIANT fam vid DEV-NO-FIELD DEV-EMIT ;
: END-VARIANT ( n -- ) {: fam:n :}
   DEV-K-VARIANT-END fam DEV-CUR-VAR @ DEV-NO-FIELD DEV-EMIT
   DEV-NO-VARIANT DEV-CUR-VAR ! ;

\ FIELD checks the schema against the family's kinds (E-PF-SCHEMA) and the
\ field record validates the rest and throws first (E-TFAM-DUP, E-PF-NAME,
\ E-TFAM-CASE), so a malformed field emits no event and leaves the ordinal untouched.
\ The event carries the field's row id: the field record numbers this
\ declaration's rows from DEV-BASE-FLD in the order they are added.
: FIELD ( n ptr u8 n n n n n n n n -- )
   {: fam:n na:ptr nu:n sch:n slot:n cellsn:n boff:n bytesn:n al:n flags:n :}
   fam sch PF-SCHEMA-OK? 0= IF E-PF-SCHEMA throw THEN
   fam DEV-CUR-VAR @ na nu sch slot cellsn boff bytesn al flags PF-ADD
   DEV-K-FIELD fam DEV-CUR-VAR @  DEV-BASE-FLD @ DEV-FLD-ORD @ +  DEV-EMIT
   DEV-FLD-ORD @ 1 + DEV-FLD-ORD ! ;

\ A pointer does not own (TFCL-NODE? stops at it), so `ptr <linear>` would let
\ its family copy and drop a linear value.
: REQUIRE-POINTEE ( n ptr u8 n -- n )
   {: node:n why:ptr whyu:n :}
   node TFCL-NODE? IF
      why whyu s" hold the linear value in the field itself, never beneath `ptr`"
      TYPE-DECL:E-TDECL-PAYLOAD DECL-REJECT:REJECT throw
   THEN
   node ;

\ REQUIRE-POINTEE on each pointee in a field type, as SCH-STORE-MASK walks it;
\ again at close, as a pointee naming the declared family was judged fieldless.
: REQUIRE-POINTEES ( n ptr u8 n -- )
   {: node:n why:ptr whyu:n :}
   node SCHEMA-PTR? IF
      node SCHEMA-A@ why whyu REQUIRE-POINTEE why whyu RECURSE EXIT THEN
   node SCHEMA-APP? IF
      node SCHEMA-B@ {: start:n :}
      node SCHEMA-C@ 0 ?do start i + SCHEMA-ROOT@ why whyu RECURSE loop EXIT
   THEN
   node SCHEMA-QUOT? IF
      SCH-QUOT-ROWS 0 ?do
         node i SCHEMA-QUOT-ROW@ {: row:n :}
         row SCHEMA-ROW-OK? IF
            row SCHEMA-ROW-COUNT@ 0 ?do row i SCHEMA-ROW-ELEM@ why whyu RECURSE loop
         THEN
      loop
   THEN ;

;package
;package

;using
;using
