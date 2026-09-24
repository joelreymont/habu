\ ir-id-schema.f - Compiler identity boundary vectors.

require lib/errors.f
require lib/memory.f

require src/compiler/ir/id.f

package COMPILER-ID-PROOF
public

32 constant LOCAL-SHIFT
$8000000000000000 constant CELL-MIN
$7FFFFFFFFFFFFFFF constant CELL-MAX
$7FFFFFFF constant SERIAL-MAX
0 constant LOCAL-MIN
$FFFFFFFF constant LOCAL-MAX
$FFFFFFFF constant LOCAL-MASK
0 constant SCALAR-MIN
$7FFFFFFFFFFFFFFF constant SCALAR-MAX

0 constant OWNER-SAME
1 constant OWNER-OTHER

private

\ ---- vector storage ----------------------------------------------------------
\ One cell arena holding three ordered tables at fixed offsets.

$10 constant PACK-CAP
2 constant PACK-FIELDS
$10 constant CHECK-CAP
4 constant CHECK-FIELDS
$10 constant SCALAR-CAP
2 constant SCALAR-FIELDS

0 constant PACK-OFF
PACK-OFF PACK-CAP PACK-FIELDS * + constant CHECK-OFF
CHECK-OFF CHECK-CAP CHECK-FIELDS * + constant SCALAR-OFF
SCALAR-OFF SCALAR-CAP SCALAR-FIELDS * + constant ARENA-CELLS

TYPED-VARIABLE ARENA-BASE ptr n
variable PACK-N
variable CHECK-N
variable SCALAR-N

: ARENA ( -- ptr n )
   ARENA-BASE @ 0= if
      ARENA-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS ARENA-BASE !
   then
   ARENA-BASE @ ;

: SLOT@ ( n -- n ) {: slot:n :}
   slot 0 < slot ARENA-CELLS >= or if E-CID-ROW throw then
   ARENA slot cells + @ ;

: SLOT! ( n n -- ) {: value:n slot:n :}
   slot 0 < slot ARENA-CELLS >= or if E-CID-ROW throw then
   value ARENA slot cells + ! ;

: PACK-SLOT ( n -- n ) {: idx:n :}
   idx 0 < idx PACK-CAP >= or if E-CID-ROW throw then
   PACK-OFF idx PACK-FIELDS * + ;

: CHECK-SLOT ( n -- n ) {: idx:n :}
   idx 0 < idx CHECK-CAP >= or if E-CID-ROW throw then
   CHECK-OFF idx CHECK-FIELDS * + ;

: SCALAR-SLOT ( n -- n ) {: idx:n :}
   idx 0 < idx SCALAR-CAP >= or if E-CID-ROW throw then
   SCALAR-OFF idx SCALAR-FIELDS * + ;

\ ---- vector table builders ---------------------------------------------------

: PACK+ ( n n -- ) {: local:n class:n :}
   PACK-N @ PACK-SLOT {: slot:n :}
   local slot SLOT!
   class slot 1+ SLOT!
   PACK-N @ 1+ PACK-N ! ;

: CHECK+ ( n n n n -- ) {: local:n bound:n owner:n class:n :}
   CHECK-N @ CHECK-SLOT {: slot:n :}
   local slot SLOT!
   bound slot 1+ SLOT!
   owner slot 2 + SLOT!
   class slot 3 + SLOT!
   CHECK-N @ 1+ CHECK-N ! ;

: SCALAR+ ( n n -- ) {: value:n class:n :}
   SCALAR-N @ SCALAR-SLOT {: slot:n :}
   value slot SLOT!
   class slot 1+ SLOT!
   SCALAR-N @ 1+ SCALAR-N ! ;

\ Local indices offered to the packer. The accepted rows walk the declared local
\ domain end to end. The rejected rows leave it by one step at each end, and
\ then by the three raw cell shapes a local index must never be confused with:
\ the largest positive cell, the most negative cell, and a cell that already
\ looks packed with bit 63 set. That last shape is the one where a logical and
\ an arithmetic right shift disagree about the owner, so it belongs in the
\ frozen vectors even though the packer rejects it long before any projection.
: PACK-VECTORS ( -- )
   0 PACK-N !
   LOCAL-MIN                    0                  PACK+
   LOCAL-MIN 1+                 0                  PACK+
   42                           0                  PACK+
   LOCAL-MAX 1-                 0                  PACK+
   LOCAL-MAX                    0                  PACK+
   LOCAL-MIN 1-                 E-IR-INDEX-RANGE   PACK+
   LOCAL-MAX 1+                 E-IR-INDEX-RANGE   PACK+
   CELL-MAX                     E-IR-INDEX-RANGE   PACK+
   CELL-MIN                     E-IR-INDEX-RANGE   PACK+
   LOCAL-MASK LOCAL-SHIFT lshift 5 or
                                E-IR-INDEX-RANGE   PACK+ ;

\ Bound and owner decisions over an identity that the packer accepted. The
\ accepted rows include a bound above bit 32 and the largest legal bound; the
\ rejected rows cover the exact bound boundary, a foreign owner, and a bound
\ that leaves the scalar domain by sign and by bit 63.
: CHECK-VECTORS ( -- )
   0 CHECK-N !
   LOCAL-MIN    LOCAL-MIN 1+   OWNER-SAME    0                   CHECK+
   LOCAL-MIN    LOCAL-MIN      OWNER-SAME    E-IR-INDEX-BOUND    CHECK+
   42           43             OWNER-SAME    0                   CHECK+
   42           42             OWNER-SAME    E-IR-INDEX-BOUND    CHECK+
   42           7              OWNER-SAME    E-IR-INDEX-BOUND    CHECK+
   LOCAL-MAX    LOCAL-MAX 1+   OWNER-SAME    0                   CHECK+
   LOCAL-MAX    LOCAL-MAX      OWNER-SAME    E-IR-INDEX-BOUND    CHECK+
   LOCAL-MIN    SCALAR-MAX     OWNER-SAME    0                   CHECK+
   42           43             OWNER-OTHER   E-IR-OWNER          CHECK+
   LOCAL-MIN    LOCAL-MIN 1+   OWNER-OTHER   E-IR-OWNER          CHECK+
   LOCAL-MAX    LOCAL-MAX 1+   OWNER-OTHER   E-IR-OWNER          CHECK+
   42           SCALAR-MIN 1-  OWNER-SAME    E-IR-SCALAR-RANGE   CHECK+
   42           CELL-MIN       OWNER-SAME    E-IR-SCALAR-RANGE   CHECK+
   42           LOCAL-MASK LOCAL-SHIFT lshift 5 or
                               OWNER-SAME    E-IR-SCALAR-RANGE   CHECK+
   42           SCALAR-MIN 1-  OWNER-OTHER   E-IR-SCALAR-RANGE   CHECK+ ;

\ Values offered to the two scalar families. Five accepted rows carry bits above
\ bit 32, and two of those are shaped exactly like a packed identity, so a
\ scalar that quietly took the owner or local projection, or picked up a module
\ serial, would not round-trip to the value it was given. The rejected rows
\ leave the scalar domain by one step, by the most negative cell, and by two
\ further cells with bit 63 set.
: SCALAR-VECTORS ( -- )
   0 SCALAR-N !
   SCALAR-MIN                                     0                   SCALAR+
   SCALAR-MIN 1+                                  0                   SCALAR+
   42                                             0                   SCALAR+
   LOCAL-MASK                                     0                   SCALAR+
   LOCAL-MASK 1+                                  0                   SCALAR+
   LOCAL-MASK 2 +                                 0                   SCALAR+
   SERIAL-MAX LOCAL-SHIFT lshift                  0                   SCALAR+
   SERIAL-MAX LOCAL-SHIFT lshift 5 or             0                   SCALAR+
   SCALAR-MAX                                     0                   SCALAR+
   SCALAR-MIN 1-                                  E-IR-SCALAR-RANGE   SCALAR+
   CELL-MIN                                       E-IR-SCALAR-RANGE   SCALAR+
   LOCAL-MASK 1+ negate                           E-IR-SCALAR-RANGE   SCALAR+
   LOCAL-MASK LOCAL-SHIFT lshift 5 or             E-IR-SCALAR-RANGE   SCALAR+ ;

: BUILD-VECTORS ( -- )
   PACK-VECTORS
   CHECK-VECTORS
   SCALAR-VECTORS ;

public

: PACK-ROWS ( -- n )   PACK-N @ ;
: CHECK-ROWS ( -- n )  CHECK-N @ ;
: SCALAR-ROWS ( -- n ) SCALAR-N @ ;

: PACK-LOCAL ( n -- n )   PACK-SLOT SLOT@ ;
: PACK-CLASS ( n -- n )   PACK-SLOT 1+ SLOT@ ;

: CHECK-LOCAL ( n -- n )  CHECK-SLOT SLOT@ ;
: CHECK-BOUND ( n -- n )  CHECK-SLOT 1+ SLOT@ ;
: CHECK-OWNER ( n -- n )  CHECK-SLOT 2 + SLOT@ ;
: CHECK-CLASS ( n -- n )  CHECK-SLOT 3 + SLOT@ ;

: SCALAR-VALUE ( n -- n ) SCALAR-SLOT SLOT@ ;
: SCALAR-CLASS ( n -- n ) SCALAR-SLOT 1+ SLOT@ ;

: BUILD ( -- )
   BUILD-VECTORS ;

private

\ ---- running one vector row through the production surface --------------------
\ Every probe below is stack-preserving so a caught quotation can carry its row
\ back out, and every one of them reaches `IR-ID` only through its public words.

: PACK-PROBE ( n -- n ) {: local:n :}
   IR-ID:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key local IR-ID:PACK-SOURCE drop
   local ;

: CHECK-SAME-PROBE ( n n -- n n ) {: local:n bound:n :}
   IR-ID:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key bound IR-ID:COUNT key local IR-ID:PACK-SOURCE IR-ID:SOURCE-CHECK drop
   local bound ;

: CHECK-OTHER-PROBE ( n n -- n n ) {: local:n bound:n :}
   IR-ID:NEW-MODULE drop {: minter:IR-ID:ir-module-key :}
   IR-ID:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   other bound IR-ID:COUNT minter local IR-ID:PACK-SOURCE IR-ID:SOURCE-CHECK drop
   local bound ;

: CHECK-PROBE ( n n n -- n n n ) {: local:n bound:n owner:n :}
   owner OWNER-OTHER = if
      local bound CHECK-OTHER-PROBE 2drop
   else
      local bound CHECK-SAME-PROBE 2drop
   then
   local bound owner ;

: COUNT-PROBE ( n -- n ) {: value:n :}
   value IR-ID:COUNT drop
   value ;

: POOL-PROBE ( n -- n ) {: value:n :}
   value IR-ID:POOL-OFF drop
   value ;

public

\ 0 means the production words accepted the row; any other value is the exact
\ throw code they raised.
: PACK-OUTCOME ( n -- n )
   [: PACK-PROBE ;] catch nip ;

: CHECK-OUTCOME ( n n n -- n )
   [: CHECK-PROBE ;] catch {: rc:n :}
   2drop drop
   rc ;

: COUNT-OUTCOME ( n -- n )
   [: COUNT-PROBE ;] catch nip ;

: POOL-OUTCOME ( n -- n )
   [: POOL-PROBE ;] catch nip ;

\ Round trips through the production surface, for rows the outcome words already
\ proved acceptable.
: PACK-LOCAL-BACK ( n -- n ) {: local:n :}
   IR-ID:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   key local IR-ID:PACK-SOURCE IR-ID:SOURCE-LOCAL ;

: PACK-OWNER-KEPT? ( n -- bool ) {: local:n :}
   IR-ID:NEW-MODULE {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
   key local IR-ID:PACK-SOURCE IR-ID:SOURCE-OWNER owner IR-ID:MODULE-SAME? ;

: COUNT-BACK ( n -- n )
   IR-ID:COUNT IR-ID:COUNT-N ;

: POOL-BACK ( n -- n )
   IR-ID:POOL-OFF IR-ID:POOL-OFF-N ;

;package
