\ maki/db/artifact.f - the canonical artifact envelope codec (MODEL-CAD-V2-PLAN.md
\ § 23.9 "Canonical typed artifacts"; frozen contract
\ habu-freeze-canonical-artifact-3b6b7087; implementation
\ habu-v2-canonical-artifact-ee5121b4).
\
\ CONCERN: the wire codec for the stored artifact envelope - canonical ENCODE,
\ DECODE, and DIGEST over an in-memory `artifact<k>`, returning typed diagnostics
\ per the frozen failure taxonomy. This reopens `package ARTIFACT` (the envelope
\ owner named by the contract) as a second file; maki/artifact.f keeps the
\ content-addressed identity registry (REGISTER/KEY$/EQUAL?/COUNT and the private
\ CAD-KIND:artifact-id mint). One concern per file: identity registry there, wire
\ envelope here.
\
\ NO NEW TRUST BOUNDARY / NO RAW-N IDENTITY: the envelope handle types are checked
\ PRODUCTs whose MAKE/UNMAKE constructors are checker-native (no TRUSTED:); the
\ only id used on the wire is CAD-KIND:artifact-id, serialized through the EXISTING
\ private ARTIFACT-ID>RAW / RAW>ARTIFACT-ID refinements already blessed by
\ maki/artifact.f (visible here because we reopen the same package). The digest is
\ a real SHA-256 over the semantic bytes, reusing the baked src/core/sha256.f word
\ SHA256, never a one-cell n->digest cast: content-digest is a four-word owned
\ value, structurally distinct from every id.
\
\ KIND SEPARATION is realized as flat per-kind families (weight-artifact vs
\ kernel-artifact), the contract-blessed alternative to parametric application:
\ the two handle types never unify, so an encoder that expects one kind rejects the
\ other at compile time. A wire kind-discriminant carries the same fact for DECODE,
\ which reports kind-mismatch when the bytes name a different kind than the reader.
\
\ SCOPE (first slice): the envelope fields realized here are the ones expressible
\ with zero new trust boundary - schema-version, kind, producer-version, identity
\ (CAD-KIND:artifact-id), an ascending/duplicate-free dependency set
\ (CAD-KIND:artifact-id), the digest-excluded created-event scalar, and the content
\ digest. The remaining contract fields (schema-id, producer-id, config-id,
\ numeric-policy-id, capability-id, audit-event-id, source-revision rev-id, and
\ target-id) are nominal families with NO owner-provided constructor or wire codec
\ anywhere in the repo yet; serializing them would require a refinement the "no new
\ trust boundary" rule forbids here. Those fields await their owner constructors -
\ tracked by the implementation dot.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f          \ reopen package ARTIFACT + its blessed id refinements

-5274 constant E-ART-ENV-DEP     \ dependency-set builder over the per-envelope cap
-5275 constant E-ART-ENV-BUF     \ ENCODE output buffer smaller than the canonical bytes
-5276 constant E-ART-ENV-SEM     \ semantic prefix over the internal scratch (unreachable cap)

package ARTIFACT
public

\ ---- envelope value types (checker-native constructors, no trust boundary) ----
\ Distinct per-kind handles: weight-artifact and kernel-artifact never unify, so a
\ kind confusion is a signature mismatch. `slot` indexes the private envelope pool.
PRODUCT weight-artifact 0
   FIELD slot n
;PRODUCT

PRODUCT kernel-artifact 0
   FIELD slot n
;PRODUCT

\ The 256-bit content digest as four 64-bit words - deliberately multi-cell so no
\ one-cell id-shaped scalar can launder into it (contract § 23.9).
PRODUCT content-digest 0
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;PRODUCT

\ Typed decode/validate outcome: ok carries the validated pool slot; every other
\ variant is one member of the frozen failure taxonomy. The taxonomy variants are
\ baked into the family (the lib/cad-num-types.f numeric-result idiom) so a total
\ ok construction does not leave a free error-type variable.
SUMTYPE art-result 1
   VARIANT ok a ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT noncanonical ;VARIANT
   VARIANT bounds ;VARIANT
   VARIANT duplicate ;VARIANT
   VARIANT unknown-required ;VARIANT
   VARIANT kind-mismatch ;VARIANT
   VARIANT unsupported-migration ;VARIANT
   VARIANT digest-mismatch ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ----------------
: >WART ( n -- weight-artifact )      ARTIFACT-WEIGHT--ARTIFACT:MAKE ;
: WART> ( weight-artifact -- n )      ARTIFACT-WEIGHT--ARTIFACT:UNMAKE ;
: >KART ( n -- kernel-artifact )      ARTIFACT-KERNEL--ARTIFACT:MAKE ;
: KART> ( kernel-artifact -- n )      ARTIFACT-KERNEL--ARTIFACT:UNMAKE ;
: >DIG ( n n n n -- content-digest )  ARTIFACT-CONTENT--DIGEST:MAKE ;

: R-OK ( n -- art-result<n> )         ARTIFACT-ART--RESULT:OK ;
: R-MALFORMED ( -- art-result<n> )    ARTIFACT-ART--RESULT:MALFORMED ;
: R-NONCANON ( -- art-result<n> )     ARTIFACT-ART--RESULT:NONCANONICAL ;
: R-BOUNDS ( -- art-result<n> )       ARTIFACT-ART--RESULT:BOUNDS ;
: R-DUP ( -- art-result<n> )          ARTIFACT-ART--RESULT:DUPLICATE ;
: R-UNKNOWN-REQ ( -- art-result<n> )  ARTIFACT-ART--RESULT:UNKNOWN-REQUIRED ;
: R-KIND ( -- art-result<n> )         ARTIFACT-ART--RESULT:KIND-MISMATCH ;
: R-MIGRATION ( -- art-result<n> )    ARTIFACT-ART--RESULT:UNSUPPORTED-MIGRATION ;
: R-DIGEST ( -- art-result<n> )       ARTIFACT-ART--RESULT:DIGEST-MISMATCH ;

\ ---- protocol constants (frozen wire) -----------------------------------------
1 constant SCHEMA-VERSION            \ the only schema version this reader supports
0 constant KIND-WEIGHT               \ wire kind discriminant for weight-artifact
1 constant KIND-KERNEL               \ wire kind discriminant for kernel-artifact

\ Canonical ascending field tags. 1..5 are the digest-covered semantic fields; 6
\ and 7 are excluded from the digest; unknown tags (>= 8) are optional-or-required
\ extension fields.
1 constant TAG-VER
2 constant TAG-KIND
3 constant TAG-PVER
4 constant TAG-ID
5 constant TAG-DEPS
6 constant TAG-DIGEST                 \ stored content digest (excluded from digest)
7 constant TAG-EVENT                  \ created-event scalar (excluded from digest)
7 constant TAG-KNOWN-MAX
1 constant FLAG-REQUIRED              \ flags bit 0: an unknown field flagged required rejects
8 constant U64W                       \ fixed little-endian scalar width
4 constant U32W                       \ fixed little-endian length width
2 constant HDR-W                      \ tag byte + flags byte
32 constant DIGEST-BYTES              \ 256-bit content digest

\ taxonomy codes accumulated during decode, mapped to art-result at the boundary
1 constant D-MALFORMED
2 constant D-NONCANON
3 constant D-BOUNDS
4 constant D-DUP
5 constant D-UNKNOWN-REQ
6 constant D-KIND
7 constant D-MIGRATION
8 constant D-DIGEST

\ ---- capacities (first-slice bounded pool; a durable store is a later dot) -----
64 constant ENV-CAP                   \ live envelope slots (ring reuse)
16 constant DEP-CAP                   \ dependency identities per envelope
$1000 constant SCRATCH-CAP            \ semantic/encode scratch bytes
$2000 constant ROPAQUE-CAP            \ retained opaque optional-field arena

\ ---- envelope pool (parallel per-slot columns) --------------------------------
create P-VER   ENV-CAP cells allot
create P-KIND  ENV-CAP cells allot
create P-PVER  ENV-CAP cells allot
create P-ID    ENV-CAP cells allot            \ CAD-KIND:artifact-id raw
create P-EVENT ENV-CAP cells allot
create P-DEPN  ENV-CAP cells allot
create P-DEP   ENV-CAP DEP-CAP * cells allot   \ ascending, duplicate-free id raws
create P-ROFF  ENV-CAP cells allot             \ retained opaque byte offset
create P-RLEN  ENV-CAP cells allot             \ retained opaque byte length
variable P-NEXT                                \ ring cursor

create ROPAQUE ROPAQUE-CAP allot
variable ROPAQUE-U

\ ---- dependency-set builder scratch -------------------------------------------
create DSCR DEP-CAP cells allot
variable DSCR-N

\ ---- scratch encode buffer + cursor (semantic prefix and full envelope) --------
create EBUF SCRATCH-CAP allot
variable EO
create DGBUF DIGEST-BYTES allot                \ recomputed digest bytes
create STOREDG DIGEST-BYTES allot              \ stored on-wire digest bytes

\ ---- decode cursor state ------------------------------------------------------
PTR-VARIABLE DBASE                             \ decode input base pointer
variable DLEN
variable DPOS
variable DPREV                                 \ last tag seen (ascending check)
variable DERR                                  \ 0 = ok, else taxonomy code
variable SEEN                                  \ seen-required bitset over tags 1..7
variable SAW-DIGEST
variable DEC-PREVDEP                           \ previous dep raw (ascending check)

\ ---- per-slot column access ---------------------------------------------------
: VER@ ( n -- n )    cells P-VER + @ ;
: VER! ( n n -- )    cells P-VER + ! ;
: KIND@ ( n -- n )   cells P-KIND + @ ;
: KIND! ( n n -- )   cells P-KIND + ! ;
: PVER@ ( n -- n )   cells P-PVER + @ ;
: PVER! ( n n -- )   cells P-PVER + ! ;
: ID@ ( n -- n )     cells P-ID + @ ;
: ID! ( n n -- )     cells P-ID + ! ;
: EVENT@ ( n -- n )  cells P-EVENT + @ ;
: EVENT! ( n n -- )  cells P-EVENT + ! ;
: DEPN@ ( n -- n )   cells P-DEPN + @ ;
: DEPN! ( n n -- )   cells P-DEPN + ! ;
: ROFF@ ( n -- n )   cells P-ROFF + @ ;
: ROFF! ( n n -- )   cells P-ROFF + ! ;
: RLEN@ ( n -- n )   cells P-RLEN + @ ;
: RLEN! ( n n -- )   cells P-RLEN + ! ;

: DEP@ ( n n -- n ) {: s:n k:n :}      s DEP-CAP * k + cells P-DEP + @ ;
: DEP! ( n n n -- ) {: v:n s:n k:n :}   v  s DEP-CAP * k + cells P-DEP + ! ;

: SLOT-ALLOC ( -- n )                          \ ring reuse; first-slice pool
   P-NEXT @  dup 1+ ENV-CAP mod P-NEXT ! ;

\ ---- fixed little-endian scalar codec -----------------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

: LE-GET ( ptr u8 n -- n ) {: a:ptr w:n :}
   0  0 begin dup w < while
      dup {: k:n :}
      a k + c@ k 8 * lshift  rot or swap
      1+
   repeat drop ;

\ 32-byte digest equality (direct byte compare, not on the wire path)
: DIG-BYTES-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup DIGEST-BYTES < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- semantic-prefix emitter (shared by ENCODE, DIGEST, DECODE re-hash) --------
: E-RESET ( -- )   0 EO ! ;

: E-ROOM ( n -- ) {: k:n :}
   EO @ k + SCRATCH-CAP > if E-ART-ENV-SEM throw then ;

: E-U8 ( n -- ) {: c:n :}
   1 E-ROOM
   c EBUF EO @ + c!
   EO @ 1+ EO ! ;

: E-LE ( n n -- ) {: v:n w:n :}
   w E-ROOM
   v EBUF EO @ + w LE-PUT
   EO @ w + EO ! ;

: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u E-ROOM
   a EBUF EO @ + u BYTE-COPY
   EO @ u + EO ! ;

: E-HEAD ( n n -- ) {: tag:n flags:n :}
   tag E-U8  flags E-U8 ;

\ A length-delimited scalar field: tag, required flag, len=8, then the LE64 value.
: E-U64-FIELD ( n n -- ) {: tag:n v:n :}
   tag FLAG-REQUIRED E-HEAD
   U64W U32W E-LE
   v U64W E-LE ;

\ The dependency set field: tag, flags, len, count then count ascending id raws.
: E-DEPS-FIELD ( n -- ) {: s:n :}
   s DEPN@ {: cnt:n :}
   TAG-DEPS FLAG-REQUIRED E-HEAD
   cnt 1+ U64W * U32W E-LE                     \ payload = count word + cnt id words
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s k DEP@ U64W E-LE
      1+
   repeat drop ;

\ Emit ONLY the digest-covered semantic fields (tags 1..5) into EBUF from a slot.
: EMIT-SEMANTIC ( n -- ) {: s:n :}
   E-RESET
   TAG-VER  s VER@  E-U64-FIELD
   TAG-KIND s KIND@ E-U64-FIELD
   TAG-PVER s PVER@ E-U64-FIELD
   TAG-ID   s ID@   E-U64-FIELD
   s E-DEPS-FIELD ;

\ ---- SHA-256 content digest over the semantic prefix --------------------------
: HASH-SEMANTIC ( n -- )                       \ slot -> EBUF holds fields 1..5, DGBUF = 32 digest bytes
   EMIT-SEMANTIC
   EBUF EO @ DGBUF SHA256 ;

: DIGEST-VALUE ( n -- content-digest )         \ four LE64 words over the 32 bytes
   HASH-SEMANTIC
   DGBUF            U64W LE-GET
   DGBUF U64W +     U64W LE-GET
   DGBUF U64W 2 * + U64W LE-GET
   DGBUF U64W 3 * + U64W LE-GET
   >DIG ;

\ ---- full canonical envelope into EBUF ----------------------------------------
: EMIT-ENVELOPE ( n -- ) {: s:n :}
   s HASH-SEMANTIC                             \ EBUF = tags 1..5, DGBUF = their digest
   TAG-DIGEST FLAG-REQUIRED E-HEAD
   DIGEST-BYTES U32W E-LE
   DGBUF DIGEST-BYTES E-BYTES
   TAG-EVENT s EVENT@ E-U64-FIELD              \ excluded scalar
   s RLEN@ 0 > if                              \ retained opaque optionals (tags > 7)
      ROPAQUE s ROFF@ +  s RLEN@  E-BYTES
   then ;

\ ---- decode helpers -----------------------------------------------------------
: FAIL ( n -- )   DERR ! ;
: FAILED? ( -- bool )   DERR @ 0<> ;
: REMAIN ( -- n )   DLEN @ DPOS @ - ;

: D-U8 ( -- n )
   REMAIN 1 < if D-MALFORMED FAIL 0 exit then
   DBASE 0 ptr-field @ DPOS @ + c@
   DPOS @ 1+ DPOS ! ;

: D-LE ( n -- n ) {: w:n :}
   REMAIN w < if D-MALFORMED FAIL 0 exit then
   DBASE 0 ptr-field @ DPOS @ + w LE-GET
   DPOS @ w + DPOS ! ;

: TAKE-U64 ( n -- n ) {: declen:n :}           \ a length-8 scalar field body
   declen U64W <> if D-BOUNDS FAIL 0 exit then
   U64W D-LE ;

: TAKE-DEPS ( n n -- ) {: s:n declen:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt DEP-CAP > or if D-BOUNDS FAIL exit then
   declen cnt 1+ U64W * <> if D-BOUNDS FAIL exit then
   cnt s DEPN!
   -1 DEC-PREVDEP !
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      U64W D-LE {: d:n :}
      d DEC-PREVDEP @ = if D-DUP FAIL then
      d DEC-PREVDEP @ < if D-NONCANON FAIL then
      d s k DEP!
      d DEC-PREVDEP !
      1+
   repeat drop ;

: TAKE-DIGEST ( n -- ) {: declen:n :}
   declen DIGEST-BYTES <> if D-BOUNDS FAIL exit then
   REMAIN DIGEST-BYTES < if D-MALFORMED FAIL exit then
   DBASE 0 ptr-field @ DPOS @ + STOREDG DIGEST-BYTES BYTE-COPY
   DPOS @ DIGEST-BYTES + DPOS !
   true SAW-DIGEST ! ;

\ capture an unknown OPTIONAL field verbatim (tag>=8, always after tags 1..7) into
\ the retained arena so a forward-compatible ENCODE re-emits it byte-for-byte.
: RETAIN-FIELD ( n n n n -- ) {: s:n tag:n flags:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   HDR-W U32W + declen +  {: whole:n :}         \ tag+flags+len+payload bytes
   ROPAQUE-U @ whole + ROPAQUE-CAP > if D-BOUNDS FAIL exit then
   s RLEN@ 0= if ROPAQUE-U @ s ROFF! then        \ first retained field marks the offset
   tag ROPAQUE ROPAQUE-U @ + c!
   flags ROPAQUE ROPAQUE-U @ 1+ + c!
   declen ROPAQUE ROPAQUE-U @ HDR-W + + U32W LE-PUT
   DBASE 0 ptr-field @ DPOS @ +  ROPAQUE ROPAQUE-U @ HDR-W + U32W + +  declen BYTE-COPY
   ROPAQUE-U @ whole + ROPAQUE-U !
   s RLEN@ whole + s RLEN!
   DPOS @ declen + DPOS ! ;

: SEEN-CLEAR ( -- )   0 SEEN ! ;
: SEEN-TAG ( n -- )   1 swap lshift SEEN @ or SEEN ! ;
: SEEN? ( n -- bool )   1 swap lshift SEEN @ and 0<> ;

: KNOWN-BODY ( n n n -- ) {: s:n tag:n declen:n :}
   tag SEEN? if D-DUP FAIL exit then
   tag SEEN-TAG
   tag TAG-VER    = if declen TAKE-U64 s VER!   exit then
   tag TAG-KIND   = if declen TAKE-U64 s KIND!  exit then
   tag TAG-PVER   = if declen TAKE-U64 s PVER!  exit then
   tag TAG-ID     = if declen TAKE-U64 s ID!    exit then
   tag TAG-DEPS   = if s declen TAKE-DEPS       exit then
   tag TAG-DIGEST = if declen TAKE-DIGEST       exit then
   tag TAG-EVENT  = if declen TAKE-U64 s EVENT! exit then ;

: FIELD-STEP ( n -- ) {: s:n :}
   D-U8 {: tag:n :}   FAILED? if exit then
   D-U8 {: flags:n :} FAILED? if exit then
   U32W D-LE {: declen:n :}   FAILED? if exit then
   declen 0 < if D-BOUNDS FAIL exit then
   tag DPREV @ = if D-DUP FAIL exit then
   tag DPREV @ < if D-NONCANON FAIL exit then
   tag DPREV !
   tag TAG-KNOWN-MAX <= tag 0 > and if
      s tag declen KNOWN-BODY exit
   then
   flags FLAG-REQUIRED and 0<> if
      REMAIN declen < if D-MALFORMED FAIL exit then
      D-UNKNOWN-REQ FAIL exit
   then
   s tag flags declen RETAIN-FIELD ;

: REQUIRED-COMPLETE? ( -- bool )
   TAG-VER SEEN? TAG-KIND SEEN? and TAG-PVER SEEN? and
   TAG-ID SEEN? and TAG-DEPS SEEN? and TAG-EVENT SEEN? and
   SAW-DIGEST @ 0<> and ;

: DEC-SETUP ( ptr u8 n -- ) {: a:ptr u:n :}
   a DBASE 0 ptr-field !  u DLEN !  0 DPOS !  -1 DPREV !  0 DERR !
   SEEN-CLEAR  0 SAW-DIGEST ! ;

: DEC-SLOT-FILL ( n -- ) {: s:n :}
   0 s DEPN!  0 s ROFF!  0 s RLEN!
   begin REMAIN 0 > FAILED? 0= and while
      s FIELD-STEP
   repeat ;

: DEC-VALIDATE ( n n -- ) {: s:n expkind:n :}
   FAILED? if exit then
   REQUIRED-COMPLETE? 0= if D-MALFORMED FAIL exit then
   s KIND@ expkind <> if D-KIND FAIL exit then
   s VER@ SCHEMA-VERSION <> if D-MIGRATION FAIL exit then
   s HASH-SEMANTIC
   DGBUF STOREDG DIG-BYTES-EQ? 0= if D-DIGEST FAIL exit then ;

: DEC-RESULT ( n -- art-result<n> ) {: s:n :}
   DERR @ 0= if s R-OK exit then
   DERR @ D-MALFORMED   = if R-MALFORMED exit then
   DERR @ D-NONCANON    = if R-NONCANON exit then
   DERR @ D-BOUNDS      = if R-BOUNDS exit then
   DERR @ D-DUP         = if R-DUP exit then
   DERR @ D-UNKNOWN-REQ = if R-UNKNOWN-REQ exit then
   DERR @ D-KIND        = if R-KIND exit then
   DERR @ D-MIGRATION   = if R-MIGRATION exit then
   R-DIGEST ;

: DECODE-INTO ( ptr u8 n n -- art-result<n> ) {: a:ptr u:n expkind:n :}
   a u DEC-SETUP
   SLOT-ALLOC {: s:n :}
   s DEC-SLOT-FILL
   s expkind DEC-VALIDATE
   s DEC-RESULT ;

\ ---- dependency-set builder (canonicalises to ascending, duplicate-free) -------
: DEPS-RESET ( -- )   0 DSCR-N ! ;

: DEP+ ( CAD-KIND:artifact-id -- )
   ARTIFACT-ID>RAW {: r:n :}
   DSCR-N @ DEP-CAP >= if E-ART-ENV-DEP throw then
   r DSCR DSCR-N @ cells + !
   DSCR-N @ 1+ DSCR-N ! ;

: DSCR@ ( n -- n )   cells DSCR + @ ;
: DSCR! ( n n -- )   cells DSCR + ! ;

variable SORT-I
variable SORT-J
variable DEDUP-W

: SORT-SHIFT ( n -- ) {: v:n :}                \ shift v left into its sorted place
   SORT-I @ SORT-J !
   begin
      SORT-J @ 0 >  SORT-J @ 1- DSCR@ v >  and
   while
      SORT-J @ 1- DSCR@  SORT-J @ DSCR!
      SORT-J @ 1- SORT-J !
   repeat
   v SORT-J @ DSCR! ;

: DEPS-SORT ( -- )                              \ insertion sort DSCR[0..DSCR-N)
   1 SORT-I !
   begin SORT-I @ DSCR-N @ < while
      SORT-I @ DSCR@ SORT-SHIFT
      SORT-I @ 1+ SORT-I !
   repeat ;

: DEPS-DEDUP ( -- )                             \ compact equal neighbours after sort
   DSCR-N @ 0= if exit then
   1 DEDUP-W !
   1 SORT-I !
   begin SORT-I @ DSCR-N @ < while
      SORT-I @ DSCR@  DEDUP-W @ 1- DSCR@  <> if
         SORT-I @ DSCR@ DEDUP-W @ DSCR!
         DEDUP-W @ 1+ DEDUP-W !
      then
      SORT-I @ 1+ SORT-I !
   repeat
   DEDUP-W @ DSCR-N ! ;

: DEPS-CANON ( -- )   DEPS-SORT DEPS-DEDUP ;

: STORE-DEPS ( n -- ) {: s:n :}
   DSCR-N @ s DEPN!
   0 begin dup DSCR-N @ < while
      dup {: k:n :}
      k DSCR@ s k DEP!
      1+
   repeat drop ;

\ ---- BUILD: populate a fresh slot from typed field values ----------------------
: BUILD-SLOT ( n n CAD-KIND:artifact-id n n -- n )
   {: ver:n pver:n identity:CAD-KIND:artifact-id event:n disc:n :}
   DEPS-CANON
   SLOT-ALLOC {: s:n :}
   ver s VER!  disc s KIND!  pver s PVER!
   identity ARTIFACT-ID>RAW s ID!
   event s EVENT!
   s STORE-DEPS
   0 s ROFF!  0 s RLEN!
   s ;

\ ---- ENCODE into a caller buffer ----------------------------------------------
: ENC-SLOT ( n ptr u8 n -- n ) {: s:n out:ptr cap:n :}
   s EMIT-ENVELOPE
   EO @ cap > if E-ART-ENV-BUF throw then
   EBUF out EO @ BYTE-COPY
   EO @ ;

public

\ ---- weight-kind envelope API -------------------------------------------------
: BUILD-WEIGHT ( n n CAD-KIND:artifact-id n -- weight-artifact )
   KIND-WEIGHT BUILD-SLOT >WART ;

: DIGEST-WEIGHT ( weight-artifact -- content-digest )
   WART> DIGEST-VALUE ;

: ENCODE-WEIGHT ( weight-artifact ptr u8 n -- n )
   {: h:weight-artifact out:ptr cap:n :}
   h WART> out cap ENC-SLOT ;

: DECODE-WEIGHT ( ptr u8 n -- art-result<n> )
   KIND-WEIGHT DECODE-INTO ;

: WEIGHT-OF ( n -- weight-artifact )   >WART ;

\ ---- kernel-kind envelope API -------------------------------------------------
: BUILD-KERNEL ( n n CAD-KIND:artifact-id n -- kernel-artifact )
   KIND-KERNEL BUILD-SLOT >KART ;

: DIGEST-KERNEL ( kernel-artifact -- content-digest )
   KART> DIGEST-VALUE ;

: ENCODE-KERNEL ( kernel-artifact ptr u8 n -- n )
   {: h:kernel-artifact out:ptr cap:n :}
   h KART> out cap ENC-SLOT ;

: DECODE-KERNEL ( ptr u8 n -- art-result<n> )
   KIND-KERNEL DECODE-INTO ;

: KERNEL-OF ( n -- kernel-artifact )   >KART ;

\ ---- digest projection (total, for equality/inspection) -----------------------
: DIGEST-EQ? ( content-digest content-digest -- bool )
   ARTIFACT-CONTENT--DIGEST:UNMAKE {: y0:n y1:n y2:n y3:n :}   \ unpack the top digest
   ARTIFACT-CONTENT--DIGEST:UNMAKE {: x0:n x1:n x2:n x3:n :}   \ unpack the lower digest
   x0 y0 = x1 y1 = and x2 y2 = and x3 y3 = and ;

private

: ENV-CODEC-INIT ( -- )
   0 P-NEXT !  0 ROPAQUE-U !  DEPS-RESET ;

ENV-CODEC-INIT
;package
