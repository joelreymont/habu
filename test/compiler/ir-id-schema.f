\ ir-id-schema.f - the frozen canonical description of the IR-0.1 identity design.
\
\ The module lives in `package COMPILER-ID-PROOF`. Its subject is the production
\ identity authority `src/compiler/ir/id.f` (`package IR-ID`). It holds three
\ things and nothing else:
\
\   1. A canonical byte form of the identity design. It names the schema and its
\      version, the packing widths and bounds, every identity family in
\      declaration order, the two projection rules, and every named guard with
\      the error code it throws, the word that owns it, and how that guard can
\      be reached. The family rows are not transcribed: they are read back from
\      the live checker type-family registry across the one `require` of the
\      production source below, so renaming, reordering, adding, removing, or
\      changing the arity, kind, or visibility of a family cannot leave the
\      canonical bytes untouched.
\   2. The SHA-256 digest of exactly those bytes (`DIGEST-HEX$`) and the
\      committed frozen value it must equal (`EXPECTED-DIGEST$`). Changing the
\      design without consciously re-freezing the digest is a test failure.
\   3. One ordered set of valid and hostile numeric vectors covering every guard
\      that is numerically reachable through the public `IR-ID` surface, plus
\      the checked words that run one vector row through that surface. The
\      vectors are part of the digested bytes, so a hostile row cannot be
\      weakened or deleted silently either.
\
\ How the freeze actually holds. The digest alone would only freeze a
\ description; on its own a body change in `id.f` could keep the same bytes. The
\ vectors close that hole from the other side, because every declared width and
\ bound below is used to BUILD the vector rows rather than being restated by
\ them: `LOCAL-MAX` is accepted and `LOCAL-MAX 1+` is rejected by the real
\ packer, `CELL-MAX` is accepted and `-1` rejected by the real scalar
\ constructors, and `LOCAL-MAX 1+` is a legal bound. Move a constant here and
\ the running engine rejects the row; move a constant in `id.f` and the same row
\ fails. The two halves cannot drift apart while both are green.
\
\ Not everything a defensive guard checks can be reached from outside `IR-ID`.
\ A module key and a packed identity cell are unforgeable: the only producers
\ are `IR-ID:NEW-MODULE` and `IR-ID:PACK-*`, the raw casts are private, and a
\ `CAST:` into those families from another package is rejected before runtime.
\ So the two module-serial guards are declared with reachability `sealed-key`,
\ and serial exhaustion is declared `allocator` because reaching it needs the
\ whole 2^31-1 serial space. Those three are named here with their owners and
\ are proved elsewhere: the sealed-cast rejection by the static wrong-family
\ fixture named in the fixture rows below, and exhaustion by the allocator-laws
\ leaf. Only the four `vector` guards carry numeric rows, and the manifest is
\ what says which is which.
\
\ Wrong-family rejection and require replay stay out of the numeric tables on
\ purpose: the first is a static checker fact and the second is a real child
\ load, and neither is a number. Both are referenced by the fixture rows.
\
\ Consumers: the focused test `test/compiler/ir-id-manifest.f`, and later the
\ Rocq parity gate, which binds these bytes and these vectors to the model in
\ `formal/Common/`.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/memory.f

package COMPILER-ID-PROOF
private

variable FAM-BASE
variable FAM-TOP

public

\ Load protocol, not API. The registry does not expose the owning package of a
\ family, so the only sound way to name exactly the families that the production
\ source declares is to read the registry high-water mark either side of its one
\ `require`. Both words run once, at load time, immediately below.
: FAMILY-MARK-BEFORE ( -- )
   TFAM-N@ FAM-BASE ! ;

: FAMILY-MARK-AFTER ( -- )
   TFAM-N@ FAM-TOP ! ;

;package

COMPILER-ID-PROOF:FAMILY-MARK-BEFORE
require src/compiler/ir/id.f
COMPILER-ID-PROOF:FAMILY-MARK-AFTER

package COMPILER-ID-PROOF
private

\ A reference nominal cell family. The checker's own kind numbering is engine
\ state, so the manifest compares the kind every identity family reports against
\ the kind of a real arity-0 NEWTYPE declared right here instead of restating an
\ engine constant.
NEWTYPE cid-cell-probe 0

variable PROBE-FAM
TFAM-N@ 1- PROBE-FAM !

public

\ ---- frozen design constants -------------------------------------------------
\ Each of these is bound to the running implementation by the vector rows built
\ from it further down; none of them is a free-standing claim.

64 constant CELL-BITS
63 constant CELL-VALUE-BITS
31 constant SERIAL-BITS
32 constant LOCAL-BITS
32 constant LOCAL-SHIFT

$8000000000000000 constant CELL-MIN
$7FFFFFFFFFFFFFFF constant CELL-MAX

1 constant SERIAL-MIN
$7FFFFFFF constant SERIAL-MAX

0 constant LOCAL-MIN
$FFFFFFFF constant LOCAL-MAX
$FFFFFFFF constant LOCAL-MASK

0 constant SCALAR-MIN
$7FFFFFFFFFFFFFFF constant SCALAR-MAX

13 constant FAMILY-COUNT
7 constant GUARD-COUNT
2 constant FIXTURE-COUNT

\ Which owner key a check-vector row uses: the key that packed the identity, or
\ a second, different module key.
0 constant OWNER-SAME
1 constant OWNER-OTHER

private

\ Family roles. Index 0 is the minting authority, index 1 is the owner witness,
\ indices 2..10 are the packed reference families, and the last two are plain
\ scalars that must never be packed.
2 constant PACKED-START
11 constant PACKED-END

: FAMILY-RANGE ( n -- ) {: idx:n :}
   idx 0 < idx FAMILY-COUNT >= or if E-CID-FAMILY throw then ;

: FAMILY-NAME$ ( n -- ptr u8 n )
   case
      0 of s" ir-module-key" endof
      1 of s" ir-module-id" endof
      2 of s" ir-source-id" endof
      3 of s" ir-fun-id" endof
      4 of s" ir-block-id" endof
      5 of s" ir-op-id" endof
      6 of s" ir-value-id" endof
      7 of s" ir-type-id" endof
      8 of s" ir-attr-id" endof
      9 of s" ir-symbol-id" endof
      10 of s" ir-span-id" endof
      11 of s" ir-pool-offset" endof
      12 of s" ir-count" endof
      E-CID-FAMILY throw
   endcase ;

: FAMILY-ROLE$ ( n -- ptr u8 n ) {: idx:n :}
   idx FAMILY-RANGE
   idx 0 = if s" key" exit then
   idx PACKED-START < if s" owner" exit then
   idx PACKED-END < if s" packed" exit then
   s" scalar" ;

: FAMILY-ID ( n -- n ) {: idx:n :}
   idx FAMILY-RANGE
   FAM-BASE @ idx + ;

\ ---- live registry cross-check ----------------------------------------------

: FAMILY-SPAN-CHECK ( -- )
   FAM-TOP @ FAM-BASE @ - FAMILY-COUNT <> if E-CID-FAMILY throw then ;

: FAMILY-ROW-CHECK ( n -- ) {: idx:n :}
   idx FAMILY-ID {: id:n :}
   id TFAM-NAME$ idx FAMILY-NAME$ STR= 0= if E-CID-FAMILY throw then
   id TFAM-ARITY@ 0 <> if E-CID-FAMILY throw then
   id TFAM-KIND@ PROBE-FAM @ TFAM-KIND@ <> if E-CID-FAMILY throw then
   id TFAM-PUBLIC? 0= if E-CID-FAMILY throw then ;

: FAMILY-CHECK ( -- )
   FAMILY-SPAN-CHECK
   FAMILY-COUNT 0 ?do i FAMILY-ROW-CHECK loop ;

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

variable ARENA-BASE
variable PACK-N
variable CHECK-N
variable SCALAR-N

: ARENA-FIELD ( -- ptr ptr a )
   ARENA-BASE 0 ptr-field ;

: ARENA ( -- ptr a )
   ARENA-FIELD @ 0= if
      ARENA-CELLS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS ARENA-FIELD !
   then
   ARENA-FIELD @ ;

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

\ The family name the live checker registry holds at a declared index. BUILD has
\ already proved it equals the frozen name at that index.
: FAMILY-NAME-AT ( n -- ptr u8 n )
   FAMILY-ID TFAM-NAME$ ;

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

private

\ ---- guard rows --------------------------------------------------------------

\ Guards 2..5 are the ones a numeric vector row can reach. Guards 0 and 1 need a
\ forged module key and guard 6 needs an exhausted serial space, so both sit
\ outside the vector tables by construction rather than by omission.
2 constant GUARD-VECTOR-FIRST
6 constant GUARD-VECTOR-END

: GUARD-RANGE ( n -- ) {: idx:n :}
   idx 0 < idx GUARD-COUNT >= or if E-CID-ROW throw then ;

public

: GUARD-CODE-AT ( n -- n )
   case
      0 of E-IR-MODULE-ZERO endof
      1 of E-IR-MODULE-RANGE endof
      2 of E-IR-INDEX-RANGE endof
      3 of E-IR-INDEX-BOUND endof
      4 of E-IR-OWNER endof
      5 of E-IR-SCALAR-RANGE endof
      6 of E-IR-MODULE-EXHAUSTED endof
      E-CID-ROW throw
   endcase ;

\ True for the guards a numeric vector row can reach. The other three are named
\ in the manifest with their own reachability and are owned by the wrong-family
\ fixture and the allocator leaf.
: GUARD-BY-VECTOR? ( n -- bool ) {: idx:n :}
   idx GUARD-RANGE
   idx GUARD-VECTOR-FIRST >= idx GUARD-VECTOR-END < and ;

private

\ ---- fixture rows ------------------------------------------------------------

: FIXTURE-RANGE ( n -- ) {: idx:n :}
   idx 0 < idx FIXTURE-COUNT >= or if E-CID-ROW throw then ;

public

: FIXTURE-PATH$ ( n -- ptr u8 n ) {: idx:n :}
   idx FIXTURE-RANGE
   s" test/compiler/ir-id.f" ;

\ ---- canonical byte builder --------------------------------------------------
\ Every field is a printable non-space token, so single spaces separate fields
\ and one line feed ends a row with no escaping and no ambiguity. TOKEN-CHECK
\ enforces that alphabet rather than assuming it.

\ Public because a consumer that snapshots or compares BYTES$ needs to size its
\ own buffer from the same number rather than repeating it.
$2000 constant BYTES-CAP

private

$20 constant TOKEN-LOW
$7F constant TOKEN-HIGH
$20 constant BYTE-SP
$0A constant BYTE-LF

create BYTES BYTES-CAP allot
variable BYTES-U

: BYTE+ ( n -- ) {: c:n :}
   BYTES-U @ 1+ BYTES-CAP > if E-STR-CAPACITY throw then
   c BYTES BYTES-U @ + c!
   BYTES-U @ 1+ BYTES-U ! ;

: SPAN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   BYTES-U @ u + BYTES-CAP > if E-STR-CAPACITY throw then
   a BYTES BYTES-U @ + u BYTE-COPY
   BYTES-U @ u + BYTES-U ! ;

: TOKEN-BYTE-OK? ( n -- bool ) {: c:n :}
   c TOKEN-LOW > c TOKEN-HIGH < and ;

: TOKEN-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-CID-TOKEN throw then
   u 0 ?do
      a i + c@ TOKEN-BYTE-OK? 0= if E-CID-TOKEN throw then
   loop ;

: TOKEN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TOKEN-CHECK
   a u SPAN+ ;

: NUM+ ( n -- )
   SB-RESET FMT:SB-INT SB$ TOKEN+ ;

: ROW ( ptr u8 n -- )
   TOKEN+ ;

: +TEXT ( ptr u8 n -- )
   BYTE-SP BYTE+ TOKEN+ ;

: +NUM ( n -- )
   BYTE-SP BYTE+ NUM+ ;

: ;ROW ( -- )
   BYTE-LF BYTE+ ;

\ ---- canonical rows ----------------------------------------------------------

: EMIT-HEADER ( -- )
   s" schema" ROW s" habu-ir-id" +TEXT 1 +NUM ;ROW
   s" subject" ROW s" src/compiler/ir/id.f" +TEXT s" IR-ID" +TEXT ;ROW ;

: EMIT-WIDTHS ( -- )
   s" width" ROW s" cell-bits" +TEXT CELL-BITS +NUM ;ROW
   s" width" ROW s" cell-value-bits" +TEXT CELL-VALUE-BITS +NUM ;ROW
   s" width" ROW s" serial-bits" +TEXT SERIAL-BITS +NUM ;ROW
   s" width" ROW s" local-bits" +TEXT LOCAL-BITS +NUM ;ROW
   s" width" ROW s" local-shift" +TEXT LOCAL-SHIFT +NUM ;ROW ;

: EMIT-BOUNDS ( -- )
   s" bound" ROW s" cell-min" +TEXT CELL-MIN +NUM ;ROW
   s" bound" ROW s" cell-max" +TEXT CELL-MAX +NUM ;ROW
   s" bound" ROW s" serial-min" +TEXT SERIAL-MIN +NUM ;ROW
   s" bound" ROW s" serial-max" +TEXT SERIAL-MAX +NUM ;ROW
   s" bound" ROW s" local-min" +TEXT LOCAL-MIN +NUM ;ROW
   s" bound" ROW s" local-max" +TEXT LOCAL-MAX +NUM ;ROW
   s" bound" ROW s" local-mask" +TEXT LOCAL-MASK +NUM ;ROW
   s" bound" ROW s" scalar-min" +TEXT SCALAR-MIN +NUM ;ROW
   s" bound" ROW s" scalar-max" +TEXT SCALAR-MAX +NUM ;ROW ;

: EMIT-FAMILY-ROW ( n -- ) {: idx:n :}
   idx FAMILY-ID {: id:n :}
   s" family" ROW
      idx +NUM
      id TFAM-NAME$ +TEXT
      id TFAM-ARITY@ +NUM
      id TFAM-KIND@ +NUM
      id TFAM-PUBLIC? if s" public" else s" private" then +TEXT
      idx FAMILY-ROLE$ +TEXT
   ;ROW ;

: EMIT-FAMILIES ( -- )
   s" families" ROW FAMILY-COUNT +NUM ;ROW
   FAMILY-COUNT 0 ?do i EMIT-FAMILY-ROW loop ;

: EMIT-PROJECTIONS ( -- )
   s" projection" ROW s" owner" +TEXT s" shift-right-logical" +TEXT
      LOCAL-SHIFT +NUM ;ROW
   s" projection" ROW s" local" +TEXT s" mask" +TEXT LOCAL-MASK +NUM ;ROW
   s" construction" ROW s" key" +TEXT s" NEW-MODULE" +TEXT s" sealed-cast" +TEXT ;ROW
   s" construction" ROW s" packed" +TEXT s" PACK" +TEXT s" sealed-cast" +TEXT ;ROW
   s" construction" ROW s" scalar" +TEXT s" COUNT-POOL-OFF" +TEXT
      s" open-domain" +TEXT ;ROW ;

: EMIT-GUARDS ( -- )
   s" guards" ROW GUARD-COUNT +NUM ;ROW
   s" guard" ROW 0 +NUM s" module-zero" +TEXT 0 GUARD-CODE-AT +NUM
      s" KEY-SERIAL" +TEXT s" sealed-key" +TEXT ;ROW
   s" guard" ROW 1 +NUM s" module-range" +TEXT 1 GUARD-CODE-AT +NUM
      s" KEY-SERIAL" +TEXT s" sealed-key" +TEXT ;ROW
   s" guard" ROW 2 +NUM s" index-range" +TEXT 2 GUARD-CODE-AT +NUM
      s" LOCAL-CHECK" +TEXT s" vector" +TEXT ;ROW
   s" guard" ROW 3 +NUM s" index-bound" +TEXT 3 GUARD-CODE-AT +NUM
      s" CHECK-N" +TEXT s" vector" +TEXT ;ROW
   s" guard" ROW 4 +NUM s" owner" +TEXT 4 GUARD-CODE-AT +NUM
      s" CHECK-N" +TEXT s" vector" +TEXT ;ROW
   s" guard" ROW 5 +NUM s" scalar-range" +TEXT 5 GUARD-CODE-AT +NUM
      s" MINT-COUNT-MINT-POOL-OFF" +TEXT s" vector" +TEXT ;ROW
   s" guard" ROW 6 +NUM s" module-exhausted" +TEXT 6 GUARD-CODE-AT +NUM
      s" SERIAL-NEXT" +TEXT s" allocator" +TEXT ;ROW ;

: EMIT-PACK-ROW ( n -- ) {: idx:n :}
   s" pack" ROW idx +NUM idx PACK-LOCAL +NUM idx PACK-CLASS +NUM ;ROW ;

: EMIT-CHECK-OWNER ( n -- )
   OWNER-OTHER = if s" other" +TEXT exit then
   s" same" +TEXT ;

: EMIT-CHECK-ROW ( n -- ) {: idx:n :}
   s" check" ROW
      idx +NUM
      idx CHECK-LOCAL +NUM
      idx CHECK-BOUND +NUM
      idx CHECK-OWNER EMIT-CHECK-OWNER
      idx CHECK-CLASS +NUM
   ;ROW ;

: EMIT-SCALAR-ROW ( n -- ) {: idx:n :}
   s" scalar" ROW idx +NUM idx SCALAR-VALUE +NUM idx SCALAR-CLASS +NUM ;ROW ;

: EMIT-VECTORS ( -- )
   s" vectors" ROW s" pack" +TEXT PACK-ROWS +NUM ;ROW
   PACK-ROWS 0 ?do i EMIT-PACK-ROW loop
   s" vectors" ROW s" check" +TEXT CHECK-ROWS +NUM ;ROW
   CHECK-ROWS 0 ?do i EMIT-CHECK-ROW loop
   s" vectors" ROW s" scalar" +TEXT SCALAR-ROWS +NUM ;ROW
   SCALAR-ROWS 0 ?do i EMIT-SCALAR-ROW loop ;

: EMIT-FIXTURES ( -- )
   s" fixtures" ROW FIXTURE-COUNT +NUM ;ROW
   s" fixture" ROW 0 +NUM s" wrong-family" +TEXT s" static-checker" +TEXT
      0 FIXTURE-PATH$ +TEXT s" IR-ID-TEST" +TEXT
      s" WRONG-FAMILY-CASES" +TEXT ;ROW
   s" fixture" ROW 1 +NUM s" require-replay" +TEXT s" child-load" +TEXT
      1 FIXTURE-PATH$ +TEXT s" IR-ID-TEST" +TEXT
      s" RELOAD-STABLE$" +TEXT ;ROW ;

public

\ Rebuild the canonical bytes and the vector tables from live state. Running it
\ again must produce the same bytes: every input is either a frozen constant in
\ this file or a value read back from the checker registry, and nothing consults
\ the clock, the environment, an address, or an allocation order.
: BUILD ( -- )
   FAMILY-CHECK
   BUILD-VECTORS
   0 BYTES-U !
   EMIT-HEADER
   EMIT-WIDTHS
   EMIT-BOUNDS
   EMIT-FAMILIES
   EMIT-PROJECTIONS
   EMIT-GUARDS
   EMIT-VECTORS
   EMIT-FIXTURES ;

: BYTES$ ( -- ptr u8 n )
   BYTES BYTES-U @ ;

\ SHA-256 is 32 raw bytes and twice that in lowercase hex. Public for the same
\ reason as BYTES-CAP: a consumer that keeps its own copy sizes it from here.
$20 constant DIGEST-BYTES
$40 constant DIGEST-HEX-LEN

private

create DIGEST-RAW DIGEST-BYTES allot
create DIGEST-HEX DIGEST-HEX-LEN allot

public

: DIGEST-HEX$ ( -- ptr u8 n )
   BYTES$ DIGEST-RAW SHA256
   DIGEST-RAW DIGEST-HEX SHA256>HEX
   DIGEST-HEX DIGEST-HEX-LEN ;

\ The committed freeze. Any change to the design above, to a width or bound, to
\ the family list, to a guard, or to a single vector row moves this value, so
\ re-freezing it is a deliberate reviewed act.
: EXPECTED-DIGEST$ ( -- ptr u8 n )
   s" e085bfac3fce293bbf7c02708b48ab5134177efd7e9bb7ac2a6acace05a0d013" ;

;package

package COMPILER-ID-PROOF
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
