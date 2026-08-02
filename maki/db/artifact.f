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
\ STRUCTUREs whose MAKE/UNMAKE constructors are checker-native (no TRUSTED:); the envelope's
\ own CAD-KIND:artifact-id is held whole in a typed column and serialized on the wire as its
\ cross-process CONTENT KEY through the EXISTING public ARTIFACT:KEY>WIRE / fail-closed
\ WIRE>KEY (blessed by maki/artifact.f, callable bare because we reopen the same package) -
\ never a raw cast. The digest is a real SHA-256 over the semantic bytes, reusing the baked
\ src/core/sha256.f word SHA256, never a one-cell n->digest cast: content-digest is a
\ four-word owned value, structurally distinct from every id.
\
\ KIND SEPARATION is realized as flat per-kind families (weight-artifact vs
\ kernel-artifact), the contract-blessed alternative to parametric application:
\ the two handle types never unify, so an encoder that expects one kind rejects the
\ other at compile time. A wire kind-discriminant carries the same fact for DECODE,
\ which reports kind-mismatch when the bytes name a different kind than the reader.
\
\ SCOPE (third slice): the last two envelope fields the checker can now express join
\ the envelope, both across the JOURNAL / REV package boundaries the txn/journal dot
\ published:
\   - created-event (TAG-EVENT) is upgraded from a raw n scalar to a proper
\     CAD-KIND:audit-event-id, serialized through JOURNAL:ID>WIRE / fail-closed
\     JOURNAL:WIRE>ID. It stays DIGEST-EXCLUDED (§ 23.9 "Digest coverage" names
\     audit-event-id as the one excluded identity): it is emitted by EMIT-ENVELOPE
\     AFTER the digest, never by EMIT-SEMANTIC, so re-recording the same artifact
\     under a new audit event does not change its digest (proven by ADT-EXCLUDED-DIGEST).
\   - source-revisions[] (TAG-SREVS) is an ascending/duplicate-free set of
\     CAD-KIND:rev-id, mirroring the artifact-id dependency set mechanics but with the
\     element obtained across the boundary via REV:KEY>WIRE and validated fail-closed
\     via REV:WIRE>KEY. It is a DIGEST-COVERED semantic field (§ 23.9 field list:
\     "source-revisions[] ... ordered source-revision identities" is semantic
\     provenance): EMIT-SEMANTIC emits it as the last digest-covered field, so
\     changing the set flips the digest (proven by ADT-SREVS-DIGEST).
\
\ CROSS-PROCESS WIRE (dot habu-wire-content-key-e5efaa74). The artifact-id (TAG-ID),
\ dependency set (TAG-DEPS), source-revision set (TAG-SREVS), and four content-addressed
\ foreign ids (schema/producer/config/target)
\ serialise as their owner's 32-byte SHA-256 CONTENT KEY (ARTIFACT:KEY>WIRE / REV:KEY>WIRE
\ / X:KEY>WIRE), never a process-local registry raw, so envelope
\ identity survives process death: a decoded content key resolves BY CONTENT against the
\ local registry (WIRE>KEY), never by registration order. numeric-policy's KEY>WIRE is
\ byte-coincident with its ID>WIRE (its content IS the dom rank, already cross-process
\ stable), and the created-event (audit-event-id) stays the 8-byte occurrence SEQUENCE
\ (JOURNAL:ID>WIRE - a sequence is occurrence-local; its cross-process replica identity is
\ the flagged journal design point, out of this dot's scope). This is the sole artifact
\ envelope wire; there is no version field, compatibility reader, or migration dispatch.
\ Every foreign id is serialized ACROSS the package boundary through its owner's total
\ KEY>WIRE and fail-closed WIRE>KEY (§ 23.9 "Foreign identity constructors and wire
\ codecs"): ARTIFACT never mints, raw-casts, or range-checks a foreign id. A WIRE>KEY
\ refusal folds into the envelope taxonomy exactly as the contract fixes: wrong-width
\ -> malformed (truncated/over-long id bytes), unknown -> bounds (out-of-range/
\ unresolved id); a duplicate rev in the set -> duplicate. Together with the
\ second-slice foreign ids (schema/producer/config/numeric-policy/target) and the
\ first-slice fields (kind, producer-version, artifact-id identity,
\ the artifact-id dependency set, and the content digest) the envelope now binds every
\ semantic identity the plan names EXCEPT the one still out of the checker's reach:
\ capabilities-used[] (CAD-KIND:capability-id, a user-gated closed vocabulary - owner
\ CAP, a product decision; its ascending tag stays reserved, see TAG-CAP-RESERVED).
\
\ VALIDATE (§ 23.9 ARTIFACT:VALIDATE, the bare tail freed by the maki/artifact.f
\ VALIDATE-ID rename) is the kind-AGNOSTIC leg: it checks owned bytes structurally
\ and verifies the recomputed digest against the stored digest WITHOUT refining to a
\ per-kind handle, returning the same art-result taxonomy as DECODE. It shares the
\ DEC-POST structural/digest core with DECODE; DECODE additionally pins the
\ exact reader kind, VALIDATE accepts any KNOWN kind.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f          \ reopen package ARTIFACT + its blessed id refinements
require maki/schema.f            \ SCHEMA:REGISTER / KEY>WIRE / WIRE>KEY (schema-id owner)
require maki/producer.f          \ PRODUCER:REGISTER / KEY>WIRE / WIRE>KEY (producer-id owner)
require maki/config.f            \ CONFIG:REGISTER / KEY>WIRE / WIRE>KEY (config-id owner)
require maki/numpolicy.f         \ NPOL:REGISTER / KEY>WIRE / WIRE>KEY (numeric-policy-id owner)
require maki/target/target.f     \ TARGET:REGISTER / KEY>WIRE / WIRE>KEY (target-id owner)
require maki/journal.f           \ JOURNAL:APPEND / ID>WIRE / WIRE>ID (audit-event-id owner)
require maki/rev.f               \ REV:COMMIT / KEY>WIRE / WIRE>KEY (rev-id owner)

-5274 constant E-ART-ENV-DEP     \ dependency-set builder over the per-envelope cap
-5275 constant E-ART-ENV-BUF     \ ENCODE output buffer smaller than the canonical bytes
-5276 constant E-ART-ENV-SEM     \ semantic prefix over the internal scratch (unreachable cap)
-5277 constant E-ART-ENV-SREV    \ source-revision-set builder over the per-envelope cap
-5278 constant E-ART-ENV-SRVW    \ REV:KEY>WIRE returned a width the fixed set element cannot hold
-5279 constant E-ART-ENV-DIDW    \ ARTIFACT:KEY>WIRE returned a width the fixed set element cannot hold

package ARTIFACT
public

\ ---- envelope value types (checker-native constructors, no trust boundary) ----
\ Distinct per-kind handles: weight-artifact and kernel-artifact never unify, so a
\ kind confusion is a signature mismatch. `slot` indexes the private envelope pool.
STRUCTURE weight-artifact 0
   FIELD slot n
;STRUCTURE

STRUCTURE kernel-artifact 0
   FIELD slot n
;STRUCTURE

\ The 256-bit content digest as four 64-bit words - deliberately multi-cell so no
\ one-cell id-shaped scalar can launder into it (contract § 23.9).
STRUCTURE content-digest 0
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;STRUCTURE

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
: R-DIGEST ( -- art-result<n> )       ARTIFACT-ART--RESULT:DIGEST-MISMATCH ;

\ ---- protocol constants (canonical wire) --------------------------------------
0 constant KIND-WEIGHT               \ wire kind discriminant for weight-artifact
1 constant KIND-KERNEL               \ wire kind discriminant for kernel-artifact

\ Canonical ascending field tags. 1..10 are the digest-covered semantic fields
\ (1..4 first-slice scalars/set + artifact-id content key; 5..9 the foreign-id families
\ whose owners publish a KEY>WIRE/WIRE>KEY cross-process pair; 10 the rev-id source-
\ revision set); 11 and 12 are excluded from the digest; unknown tags (> TAG-KNOWN-MAX)
\ are optional-or-required extension fields. The digest and event fields keep the HIGHEST
\ decoded tags because EMIT-SEMANTIC emits every digest-covered field as the ascending
\ wire prefix and the stored digest+event follow it; a semantic field with a tag above
\ them would break ascending order.
1  constant TAG-KIND
2  constant TAG-PVER
3  constant TAG-ID                     \ CAD-KIND:artifact-id       (ARTIFACT:KEY>WIRE/WIRE>KEY)
4  constant TAG-DEPS                   \ dependency set: CAD-KIND:artifact-id content keys
5  constant TAG-SCHEMA                 \ CAD-KIND:schema-id         (SCHEMA:KEY>WIRE/WIRE>KEY)
6  constant TAG-PRODUCER               \ CAD-KIND:producer-id       (PRODUCER:KEY>WIRE/WIRE>KEY)
7  constant TAG-CONFIG                 \ CAD-KIND:config-id         (CONFIG:KEY>WIRE/WIRE>KEY)
8  constant TAG-NPOL                   \ CAD-KIND:numeric-policy-id (NPOL:KEY>WIRE/WIRE>KEY, 8B)
9  constant TAG-TARGET                 \ CAD-KIND:target-id         (TARGET:KEY>WIRE/WIRE>KEY)
10 constant TAG-SREVS                  \ source-revisions[]: CAD-KIND:rev-id set (REV:KEY>WIRE/WIRE>KEY)
11 constant TAG-DIGEST                 \ stored content digest (excluded from digest)
12 constant TAG-EVENT                  \ created-event: CAD-KIND:audit-event-id (JOURNAL, excluded)
12 constant TAG-KNOWN-MAX
\ Tag reserved for the one digest-covered identity the checker cannot yet reach so a
\ future landing keeps ascending order without renumbering: capabilities-used[]
\ (CAD-KIND:capability-id, closed vocabulary, owner CAP - a user-gated product
\ decision). It stays ABOVE TAG-KNOWN-MAX (an as-yet-unhandled reserved tag routes to
\ the unknown-extension path, never a silent no-op in KNOWN-BODY); its digest-coverage
\ placement is deferred to the CAP landing dot. Documented, not yet wired here.
13 constant TAG-CAP-RESERVED          \ capabilities-used[] (reserved; not decoded here)
1 constant FLAG-REQUIRED              \ flags bit 0: an unknown field flagged required rejects
8 constant U64W                       \ fixed little-endian scalar width
4 constant U32W                       \ fixed little-endian length width
2 constant HDR-W                      \ tag byte + flags byte
32 constant DIGEST-BYTES              \ 256-bit content digest
32 constant FID-WIRE-CAP              \ scratch cap for one foreign-id wire form (owner width <= this)
\ Cross-process content-key wire width (§ 23.9 origin-class table, content-addressed
\ registry intern): the artifact-id, every dependency, and each source-revision element
\ are the owner's 32-byte SHA-256 content key (ARTIFACT:KEY>WIRE / REV:KEY>WIRE), so
\ identity survives process death - a decoded key resolves BY CONTENT against the local
\ registry, never by registration order. The dependency and source-revision SETS pack
\ FIXED-width CKW elements back to back, so the width is a protocol constant here; DEP+
\ and SREV+ guard it against the owner KEY>WIRE returned width so a future wider content
\ key fails closed (E-ART-ENV-DIDW / E-ART-ENV-SRVW) rather than truncating.
32 constant CKW

\ taxonomy codes accumulated during decode, mapped to art-result at the boundary
1 constant D-MALFORMED
2 constant D-NONCANON
3 constant D-BOUNDS
4 constant D-DUP
5 constant D-UNKNOWN-REQ
6 constant D-KIND
7 constant D-DIGEST

\ ---- capacities (first-slice bounded pool; a durable store is a later dot) -----
64 constant ENV-CAP                   \ live envelope slots (ring reuse)
16 constant DEP-CAP                   \ dependency identities per envelope
16 constant SREV-CAP                  \ source-revision identities per envelope
$1000 constant SCRATCH-CAP            \ semantic/encode scratch bytes
$2000 constant ROPAQUE-CAP            \ retained opaque optional-field arena

\ ---- envelope pool (parallel per-slot columns) --------------------------------
create P-KIND  ENV-CAP cells allot
create P-PVER  ENV-CAP cells allot
create P-DEPN  ENV-CAP cells allot
create P-DEP   ENV-CAP DEP-CAP * CKW * allot   \ ascending, duplicate-free content keys
create P-SREVN ENV-CAP cells allot             \ source-revision set count
create P-SREV  ENV-CAP SREV-CAP * CKW * allot  \ ascending, duplicate-free rev content keys
create P-ROFF  ENV-CAP cells allot             \ retained opaque byte offset
create P-RLEN  ENV-CAP cells allot             \ retained opaque byte length
variable P-NEXT                                \ ring cursor

\ ---- foreign-id columns (checker-native typed storage) ------------------------
\ Each holds the already-valid nominal id whole; ARTIFACT never sees its raw. Typed
\ per-slot buffers (the maki/target/target.f TYPED-VARIABLE/LAYOUT-BUFFER precedent)
\ so the checker keeps the family: a producer-id can never land in a schema column.
ENV-CAP TYPED-BUFFER ID-COL       CAD-KIND:artifact-id      \ the envelope's own identity
ENV-CAP TYPED-BUFFER SCHEMA-COL   CAD-KIND:schema-id
ENV-CAP TYPED-BUFFER PRODUCER-COL CAD-KIND:producer-id
ENV-CAP TYPED-BUFFER CONFIG-COL   CAD-KIND:config-id
ENV-CAP TYPED-BUFFER NPOL-COL     CAD-KIND:numeric-policy-id
ENV-CAP TYPED-BUFFER TARGET-COL   CAD-KIND:target-id
ENV-CAP TYPED-BUFFER EVENT-COL    CAD-KIND:audit-event-id   \ digest-EXCLUDED created-event

create IDWBUF FID-WIRE-CAP allot               \ scratch for one foreign-id wire form

create ROPAQUE ROPAQUE-CAP allot
variable ROPAQUE-U

\ ---- dependency-set builder scratch (32-byte content keys) --------------------
create DSCR DEP-CAP CKW * allot
variable DSCR-N
create DKEY CKW allot                          \ one-element insertion-sort scratch

\ ---- source-revision-set builder scratch (32-byte content keys) ---------------
create SSCR SREV-CAP CKW * allot
variable SSCR-N
create SKEY CKW allot                          \ one-element insertion-sort scratch

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
variable SEEN                                  \ seen-required bitset over tags 1..TAG-KNOWN-MAX
variable SAW-DIGEST
create DEC-PREVDEP CKW allot                   \ previous dep content key (ascending check)
create DEC-PREVSREV CKW allot                  \ previous source-rev content key (ascending check)

\ ---- per-slot column access ---------------------------------------------------
: KIND@ ( n -- n )   cells P-KIND + @ ;
: KIND! ( n n -- )   cells P-KIND + ! ;
: PVER@ ( n -- n )   cells P-PVER + @ ;
: PVER! ( n n -- )   cells P-PVER + ! ;
: DEPN@ ( n -- n )   cells P-DEPN + @ ;
: DEPN! ( n n -- )   cells P-DEPN + ! ;
: SREVN@ ( n -- n )  cells P-SREVN + @ ;
: SREVN! ( n n -- )  cells P-SREVN + ! ;
: ROFF@ ( n -- n )   cells P-ROFF + @ ;
: ROFF! ( n n -- )   cells P-ROFF + ! ;
: RLEN@ ( n -- n )   cells P-RLEN + @ ;
: RLEN! ( n n -- )   cells P-RLEN + ! ;

\ Content-key set elements are 32 fixed bytes each; the per-slot accessor yields the
\ element's byte address (BYTE-COPY / c@ move it), never a cell value.
: DEP-AT ( s k -- ptr u8 ) {: s:n k:n :}   s DEP-CAP * k +  CKW *  P-DEP + ;
: SREV-AT ( s k -- ptr u8 ) {: s:n k:n :}   s SREV-CAP * k +  CKW *  P-SREV + ;

\ The envelope's own identity is the whole nominal id in a typed column (like the
\ foreign ids); it serialises as ARTIFACT:KEY>WIRE / WIRE>KEY, never a raw cast.
: ID-ID@ ( n -- CAD-KIND:artifact-id )             ID-COL @ ;
: ID-ID! ( CAD-KIND:artifact-id n -- )             ID-COL ! ;

\ Foreign-id per-slot access: fetch/store the WHOLE nominal id through the typed
\ column, so the family is checker-preserved and no raw ever surfaces in ARTIFACT.
: SCHEMA-ID@ ( n -- CAD-KIND:schema-id )           SCHEMA-COL @ ;
: SCHEMA-ID! ( CAD-KIND:schema-id n -- )           SCHEMA-COL ! ;
: PRODUCER-ID@ ( n -- CAD-KIND:producer-id )       PRODUCER-COL @ ;
: PRODUCER-ID! ( CAD-KIND:producer-id n -- )       PRODUCER-COL ! ;
: CONFIG-ID@ ( n -- CAD-KIND:config-id )           CONFIG-COL @ ;
: CONFIG-ID! ( CAD-KIND:config-id n -- )           CONFIG-COL ! ;
: NPOL-ID@ ( n -- CAD-KIND:numeric-policy-id )     NPOL-COL @ ;
: NPOL-ID! ( CAD-KIND:numeric-policy-id n -- )     NPOL-COL ! ;
: TARGET-ID@ ( n -- CAD-KIND:target-id )           TARGET-COL @ ;
: TARGET-ID! ( CAD-KIND:target-id n -- )           TARGET-COL ! ;
: EVENT-ID@ ( n -- CAD-KIND:audit-event-id )       EVENT-COL @ ;
: EVENT-ID! ( CAD-KIND:audit-event-id n -- )       EVENT-COL ! ;

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

\ ---- fixed 32-byte content-key comparison (the set-canonicalisation ordering) --
\ Lexicographic over CKW bytes, the transaction.f BYTES< precedent narrowed to a fixed
\ width. Byte access on ptr locals only, so it composes with the concrete-per-buffer
\ sort words (docs/forth.md § ptr locals and cell access).
: CKEY-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: CKEY-LT? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  {: ca:n cb:n :}
      ca cb <> if drop ca cb < exit then
      1+
   repeat drop false ;

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

\ The dependency set field: tag, flags, len, count then count ascending content keys.
: E-DEPS-FIELD ( n -- ) {: s:n :}
   s DEPN@ {: cnt:n :}
   TAG-DEPS FLAG-REQUIRED E-HEAD
   U64W  cnt CKW *  +  U32W E-LE                \ payload = count word + cnt content keys
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s k DEP-AT  CKW  E-BYTES
      1+
   repeat drop ;

\ The source-revision set field: tag, flags, len, count then count ascending rev content
\ keys. Mirrors E-DEPS-FIELD but each element is a fixed CKW rev-id content key (obtained
\ via REV:KEY>WIRE at BUILD, re-emitted here as its canonical bytes).
: E-SREVS-FIELD ( n -- ) {: s:n :}
   s SREVN@ {: cnt:n :}
   TAG-SREVS FLAG-REQUIRED E-HEAD
   U64W  cnt CKW *  +  U32W E-LE                \ payload = count word + cnt rev content keys
   cnt U64W E-LE
   0 begin dup cnt < while
      dup {: k:n :}
      s k SREV-AT  CKW  E-BYTES
      1+
   repeat drop ;

\ A length-delimited opaque-bytes field: tag, required flag, len, then the bytes.
\ Used for foreign-id fields whose payload is the owner KEY>WIRE canonical form; the
\ length is the owner-returned width, never a hardcoded ARTIFACT constant.
: E-WIRE-FIELD ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   tag FLAG-REQUIRED E-HEAD
   u U32W E-LE
   a u E-BYTES ;

\ The envelope's own identity field: serialize the stored artifact-id as its 32-byte
\ cross-process content key (ARTIFACT:KEY>WIRE, bare in the reopened package), then frame
\ it. The identity is digest-covered; its content key is stable across process death.
: E-ID-FIELD ( n -- ) {: s:n :}
   s ID-ID@ IDWBUF FID-WIRE-CAP KEY>WIRE {: w:n :}
   TAG-ID IDWBUF w E-WIRE-FIELD ;

\ Foreign-id field emitters: serialize the stored nominal id ACROSS the owner package
\ boundary with X:KEY>WIRE (the cross-process content key; total for a valid id), then
\ frame it. No raw cast, no refinement in ARTIFACT. numeric-policy's KEY>WIRE is byte-
\ coincident with its ID>WIRE (its content IS the dom rank, already cross-process stable).
: E-SCHEMA-FIELD ( n -- ) {: s:n :}
   s SCHEMA-ID@ IDWBUF FID-WIRE-CAP SCHEMA:KEY>WIRE {: w:n :}
   TAG-SCHEMA IDWBUF w E-WIRE-FIELD ;
: E-PRODUCER-FIELD ( n -- ) {: s:n :}
   s PRODUCER-ID@ IDWBUF FID-WIRE-CAP PRODUCER:KEY>WIRE {: w:n :}
   TAG-PRODUCER IDWBUF w E-WIRE-FIELD ;
: E-CONFIG-FIELD ( n -- ) {: s:n :}
   s CONFIG-ID@ IDWBUF FID-WIRE-CAP CONFIG:KEY>WIRE {: w:n :}
   TAG-CONFIG IDWBUF w E-WIRE-FIELD ;
: E-NPOL-FIELD ( n -- ) {: s:n :}
   s NPOL-ID@ IDWBUF FID-WIRE-CAP NPOL:KEY>WIRE {: w:n :}
   TAG-NPOL IDWBUF w E-WIRE-FIELD ;
: E-TARGET-FIELD ( n -- ) {: s:n :}
   s TARGET-ID@ IDWBUF FID-WIRE-CAP TARGET:KEY>WIRE {: w:n :}
   TAG-TARGET IDWBUF w E-WIRE-FIELD ;

\ The digest-EXCLUDED created-event: a single audit-event-id wire field serialized
\ across the JOURNAL boundary. Emitted by EMIT-ENVELOPE (after the digest), never by
\ EMIT-SEMANTIC, so it does not enter the content digest.
: E-EVENT-FIELD ( n -- ) {: s:n :}
   s EVENT-ID@ IDWBUF FID-WIRE-CAP JOURNAL:ID>WIRE {: w:n :}
   TAG-EVENT IDWBUF w E-WIRE-FIELD ;

\ Emit the digest-covered semantic fields (tags 1..10) into EBUF from a slot, in
\ ascending tag order: kind, producer version, identity/deps, the five foreign ids, then the
\ source-revision set.
: EMIT-SEMANTIC ( n -- ) {: s:n :}
   E-RESET
   TAG-KIND s KIND@ E-U64-FIELD
   TAG-PVER s PVER@ E-U64-FIELD
   s E-ID-FIELD
   s E-DEPS-FIELD
   s E-SCHEMA-FIELD
   s E-PRODUCER-FIELD
   s E-CONFIG-FIELD
   s E-NPOL-FIELD
   s E-TARGET-FIELD
   s E-SREVS-FIELD ;

\ ---- SHA-256 content digest over the semantic prefix --------------------------
: HASH-SEMANTIC ( n -- )                       \ slot -> EBUF holds fields 1..11, DGBUF = 32 digest bytes
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
   s HASH-SEMANTIC                             \ EBUF = tags 1..10, DGBUF = their digest
   TAG-DIGEST FLAG-REQUIRED E-HEAD
   DIGEST-BYTES U32W E-LE
   DGBUF DIGEST-BYTES E-BYTES
   s E-EVENT-FIELD                             \ excluded audit-event-id (tag 13)
   s RLEN@ 0 > if                              \ retained opaque optionals (tags > TAG-KNOWN-MAX)
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

: TAKE-DIGEST ( n -- ) {: declen:n :}
   declen DIGEST-BYTES <> if D-BOUNDS FAIL exit then
   REMAIN DIGEST-BYTES < if D-MALFORMED FAIL exit then
   DBASE 0 ptr-field @ DPOS @ + STOREDG DIGEST-BYTES BYTE-COPY
   DPOS @ DIGEST-BYTES + DPOS !
   true SAW-DIGEST ! ;

\ ---- foreign-id field bodies -------------------------------------------------
\ The payload is exactly `declen` bytes at the cursor; we hand them to the owner's
\ fail-closed X:WIRE>KEY (JOURNAL:WIRE>ID for the event sequence) and fold its reject
\ arms into the envelope taxonomy per the § 23.9 contract: wrong-width (declared length
\ is not the id's canonical width) -> malformed (truncated/over-long id bytes); unknown
\ (content key does not resolve in the owner registry) -> bounds (out-of-range/unresolved
\ id). On ok we hold the refined nominal id whole in the slot's typed column and advance.
: FID-PTR ( -- ptr u8 )    DBASE 0 ptr-field @ DPOS @ + ;
: FID-ADVANCE ( n -- )     DPOS @ + DPOS ! ;

\ The envelope's own identity: resolve the 32-byte content key fail-closed through
\ ARTIFACT:WIRE>KEY (bare in the reopened package) BY CONTENT, folding the reject arms
\ exactly like a foreign id (wrong-width -> malformed, unknown -> bounds), and hold the
\ refined nominal id whole in the slot's typed column.
: TAKE-ID ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen WIRE>KEY
   MATCH id-result
      ok          OF s ID-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-SCHEMA ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen SCHEMA:WIRE>KEY
   MATCH SCHEMA:id-result
      ok          OF s SCHEMA-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-PRODUCER ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen PRODUCER:WIRE>KEY
   MATCH PRODUCER:id-result
      ok          OF s PRODUCER-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-CONFIG ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen CONFIG:WIRE>KEY
   MATCH CONFIG:id-result
      ok          OF s CONFIG-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-NPOL ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen NPOL:WIRE>KEY
   MATCH NPOL:id-result
      ok          OF s NPOL-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

: TAKE-TARGET ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen TARGET:WIRE>KEY
   MATCH TARGET:id-result
      ok          OF s TARGET-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

\ The digest-EXCLUDED created-event is a single audit-event-id wire field, folded
\ exactly like the digest-covered foreign ids (wrong-width -> malformed, unknown ->
\ bounds); the exclusion is purely emit-side (EMIT-ENVELOPE, not EMIT-SEMANTIC).
: TAKE-EVENT ( n n -- ) {: s:n declen:n :}
   REMAIN declen < if D-MALFORMED FAIL exit then
   FID-PTR declen JOURNAL:WIRE>ID
   MATCH JOURNAL:id-result
      ok          OF s EVENT-ID!  declen FID-ADVANCE ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH ;

\ One dependency set element: validate the fixed 32-byte artifact content key fail-closed
\ through ARTIFACT:WIRE>KEY (bare) BY CONTENT (unknown -> bounds, wrong-width ->
\ malformed), then enforce the ascending/duplicate-free set invariant on its content-key
\ BYTES via CMP32 (duplicate -> duplicate, descending -> noncanonical), and store the
\ canonical content key. `k > 0` gates the ascending check so the first element has no
\ predecessor to compare.
: DEP-ELEM ( n n -- ) {: s:n k:n :}
   REMAIN CKW < if D-MALFORMED FAIL exit then
   FID-PTR CKW WIRE>KEY
   MATCH id-result
      ok          OF drop ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH
   FAILED? if exit then
   k 0 > if
      FID-PTR DEC-PREVDEP CKEY-EQ? if D-DUP FAIL exit then
      FID-PTR DEC-PREVDEP CKEY-LT? if D-NONCANON FAIL exit then
   then
   FID-PTR  s k DEP-AT   CKW BYTE-COPY
   FID-PTR  DEC-PREVDEP  CKW BYTE-COPY
   CKW FID-ADVANCE ;

: TAKE-DEPS ( n n -- ) {: s:n declen:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt DEP-CAP > or if D-BOUNDS FAIL exit then
   declen  U64W cnt CKW * +  <> if D-BOUNDS FAIL exit then
   cnt s DEPN!
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      s k DEP-ELEM
      1+
   repeat drop ;

\ One source-revision set element: validate the fixed 32-byte rev content key fail-closed
\ through REV:WIRE>KEY BY CONTENT (unknown -> bounds, wrong-width -> malformed), then
\ enforce the ascending/duplicate-free set invariant on its content-key BYTES via CMP32
\ (duplicate -> duplicate, descending -> noncanonical), and store the canonical content
\ key. Mirrors DEP-ELEM across the REV boundary.
: SREV-ELEM ( n n -- ) {: s:n k:n :}
   REMAIN CKW < if D-MALFORMED FAIL exit then
   FID-PTR CKW REV:WIRE>KEY
   MATCH REV:id-result
      ok          OF drop ENDOF
      wrong-width OF D-MALFORMED FAIL ENDOF
      unknown     OF D-BOUNDS FAIL ENDOF
   ;MATCH
   FAILED? if exit then
   k 0 > if
      FID-PTR DEC-PREVSREV CKEY-EQ? if D-DUP FAIL exit then
      FID-PTR DEC-PREVSREV CKEY-LT? if D-NONCANON FAIL exit then
   then
   FID-PTR  s k SREV-AT   CKW BYTE-COPY
   FID-PTR  DEC-PREVSREV  CKW BYTE-COPY
   CKW FID-ADVANCE ;

: TAKE-SREVS ( n n -- ) {: s:n declen:n :}
   declen U64W < if D-BOUNDS FAIL exit then
   U64W D-LE {: cnt:n :}
   FAILED? if exit then
   cnt 0 < cnt SREV-CAP > or if D-BOUNDS FAIL exit then
   declen  U64W cnt CKW * +  <> if D-BOUNDS FAIL exit then
   cnt s SREVN!
   0 begin dup cnt < FAILED? 0= and while
      dup {: k:n :}
      s k SREV-ELEM
      1+
   repeat drop ;

\ capture an unknown OPTIONAL field verbatim (tag > TAG-KNOWN-MAX, always after the
\ known tags) into the retained arena so a forward-compatible ENCODE re-emits it
\ byte-for-byte.
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
   tag TAG-KIND     = if declen TAKE-U64 s KIND!  exit then
   tag TAG-PVER     = if declen TAKE-U64 s PVER!  exit then
   tag TAG-ID       = if s declen TAKE-ID         exit then
   tag TAG-DEPS     = if s declen TAKE-DEPS       exit then
   tag TAG-SCHEMA   = if s declen TAKE-SCHEMA     exit then
   tag TAG-PRODUCER = if s declen TAKE-PRODUCER   exit then
   tag TAG-CONFIG   = if s declen TAKE-CONFIG     exit then
   tag TAG-NPOL     = if s declen TAKE-NPOL       exit then
   tag TAG-TARGET   = if s declen TAKE-TARGET     exit then
   tag TAG-SREVS    = if s declen TAKE-SREVS      exit then
   tag TAG-DIGEST   = if declen TAKE-DIGEST       exit then
   tag TAG-EVENT    = if s declen TAKE-EVENT      exit then ;

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
   TAG-KIND SEEN? TAG-PVER SEEN? and
   TAG-ID SEEN? and TAG-DEPS SEEN? and
   TAG-SCHEMA SEEN? and TAG-PRODUCER SEEN? and TAG-CONFIG SEEN? and
   TAG-NPOL SEEN? and TAG-TARGET SEEN? and
   TAG-SREVS SEEN? and TAG-EVENT SEEN? and
   SAW-DIGEST @ 0<> and ;

: DEC-SETUP ( ptr u8 n -- ) {: a:ptr u:n :}
   a DBASE 0 ptr-field !  u DLEN !  0 DPOS !  -1 DPREV !  0 DERR !
   SEEN-CLEAR  0 SAW-DIGEST ! ;

: DEC-SLOT-FILL ( n -- ) {: s:n :}
   0 s DEPN!  0 s SREVN!  0 s ROFF!  0 s RLEN!
   begin REMAIN 0 > FAILED? 0= and while
      s FIELD-STEP
   repeat ;

: KNOWN-KIND? ( n -- bool )   dup KIND-WEIGHT = swap KIND-KERNEL = or ;

\ Structural core shared by DECODE (kind-pinned) and VALIDATE (kind-agnostic):
\ recompute the digest and compare it with the stored value.
: DEC-POST ( n -- ) {: s:n :}
   s HASH-SEMANTIC
   DGBUF STOREDG DIG-BYTES-EQ? 0= if D-DIGEST FAIL exit then ;

: DEC-VALIDATE ( n n -- ) {: s:n expkind:n :}   \ DECODE: pin the exact reader kind
   FAILED? if exit then
   REQUIRED-COMPLETE? 0= if D-MALFORMED FAIL exit then
   s KIND@ expkind <> if D-KIND FAIL exit then
   s DEC-POST ;

: DEC-VALIDATE-ANY ( n -- ) {: s:n :}           \ VALIDATE: accept any KNOWN kind
   FAILED? if exit then
   REQUIRED-COMPLETE? 0= if D-MALFORMED FAIL exit then
   s KIND@ KNOWN-KIND? 0= if D-KIND FAIL exit then
   s DEC-POST ;

: DEC-RESULT ( n -- art-result<n> ) {: s:n :}
   DERR @ 0= if s R-OK exit then
   DERR @ D-MALFORMED   = if R-MALFORMED exit then
   DERR @ D-NONCANON    = if R-NONCANON exit then
   DERR @ D-BOUNDS      = if R-BOUNDS exit then
   DERR @ D-DUP         = if R-DUP exit then
   DERR @ D-UNKNOWN-REQ = if R-UNKNOWN-REQ exit then
   DERR @ D-KIND        = if R-KIND exit then
   R-DIGEST ;

: DECODE-INTO ( ptr u8 n n -- art-result<n> ) {: a:ptr u:n expkind:n :}
   a u DEC-SETUP
   SLOT-ALLOC {: s:n :}
   s DEC-SLOT-FILL
   s expkind DEC-VALIDATE
   s DEC-RESULT ;

\ ---- dependency-set builder (canonicalises to ascending, duplicate-free) -------
\ DEPS-RESET / DEP+ are the public build protocol BUILD-WEIGHT documents (the caller
\ populates the pending dependency set before BUILD); the scratch address helper stays
\ private.
: DSCR-AT ( k -- ptr u8 )   CKW * DSCR + ;         \ element k byte address in DSCR

public
: DEPS-RESET ( -- )   0 DSCR-N ! ;

: DEP+ ( CAD-KIND:artifact-id -- ) {: id:CAD-KIND:artifact-id :}
   id IDWBUF FID-WIRE-CAP KEY>WIRE {: w:n :}    \ 32-byte cross-process content key
   w CKW <> if E-ART-ENV-DIDW throw then
   DSCR-N @ DEP-CAP >= if E-ART-ENV-DEP throw then
   IDWBUF  DSCR-N @ DSCR-AT  CKW BYTE-COPY
   DSCR-N @ 1+ DSCR-N ! ;
private

\ Ascending, duplicate-free content-key-set canonicalisation. deps and source-revisions
\ run the SAME insertion sort + neighbour dedup, but the concrete-per-buffer rule
\ (docs/forth.md § ptr locals and cell access) forbids a shared sort over a `ptr` buffer
\ base, so - per the maki/db/transaction.f precedent - each set canonicalises over its
\ own DSCR / SSCR buffer, sharing only the SORT cursors and the CKEY-* byte comparators.
\ The element being inserted is copied to the DKEY / SKEY scratch because it is 32 bytes,
\ not a stack cell.
variable SORT-I
variable SORT-J
variable DEDUP-W

: DEPS-SORT ( -- )                              \ insertion sort DSCR[0..DSCR-N)
   1 SORT-I !
   begin SORT-I @ DSCR-N @ < while
      SORT-I @ DSCR-AT  DKEY  CKW BYTE-COPY      \ DKEY = DSCR[i]
      SORT-I @ SORT-J !
      begin
         SORT-J @ 0 > if  DKEY  SORT-J @ 1- DSCR-AT  CKEY-LT?  else false then
      while
         SORT-J @ 1- DSCR-AT  SORT-J @ DSCR-AT  CKW BYTE-COPY   \ DSCR[j] = DSCR[j-1]
         SORT-J @ 1- SORT-J !
      repeat
      DKEY  SORT-J @ DSCR-AT  CKW BYTE-COPY      \ DSCR[j] = DKEY
      SORT-I @ 1+ SORT-I !
   repeat ;

: DEPS-DEDUP ( -- )                             \ compact equal neighbours after sort
   DSCR-N @ 0= if exit then
   1 DEDUP-W !
   1 SORT-I !
   begin SORT-I @ DSCR-N @ < while
      SORT-I @ DSCR-AT  DEDUP-W @ 1- DSCR-AT  CKEY-EQ? 0= if
         SORT-I @ DSCR-AT  DEDUP-W @ DSCR-AT  CKW BYTE-COPY
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
      k DSCR-AT  s k DEP-AT  CKW BYTE-COPY
      1+
   repeat drop ;

\ ---- source-revision-set builder (canonicalises to ascending, duplicate-free) --
\ The same set discipline as the dependency builder over the SSCR buffer, but the
\ element is the rev-id content key obtained ACROSS the REV boundary via REV:KEY>WIRE
\ (never a raw cast); the fixed set width is guarded fail-closed against a future wider
\ rev content key.
\ SREVS-RESET / SREV+ are the public source-revision build protocol (mirror DEPS-RESET /
\ DEP+); the scratch address helper stays private.
: SSCR-AT ( k -- ptr u8 )   CKW * SSCR + ;         \ element k byte address in SSCR

public
: SREVS-RESET ( -- )   0 SSCR-N ! ;

: SREV+ ( CAD-KIND:rev-id -- ) {: id:CAD-KIND:rev-id :}
   id IDWBUF FID-WIRE-CAP REV:KEY>WIRE {: w:n :}
   w CKW <> if E-ART-ENV-SRVW throw then
   SSCR-N @ SREV-CAP >= if E-ART-ENV-SREV throw then
   IDWBUF  SSCR-N @ SSCR-AT  CKW BYTE-COPY
   SSCR-N @ 1+ SSCR-N ! ;
private

: SREVS-SORT ( -- )                             \ insertion sort SSCR[0..SSCR-N)
   1 SORT-I !
   begin SORT-I @ SSCR-N @ < while
      SORT-I @ SSCR-AT  SKEY  CKW BYTE-COPY      \ SKEY = SSCR[i]
      SORT-I @ SORT-J !
      begin
         SORT-J @ 0 > if  SKEY  SORT-J @ 1- SSCR-AT  CKEY-LT?  else false then
      while
         SORT-J @ 1- SSCR-AT  SORT-J @ SSCR-AT  CKW BYTE-COPY   \ SSCR[j] = SSCR[j-1]
         SORT-J @ 1- SORT-J !
      repeat
      SKEY  SORT-J @ SSCR-AT  CKW BYTE-COPY      \ SSCR[j] = SKEY
      SORT-I @ 1+ SORT-I !
   repeat ;

: SREVS-DEDUP ( -- )                            \ compact equal neighbours after sort
   SSCR-N @ 0= if exit then
   1 DEDUP-W !
   1 SORT-I !
   begin SORT-I @ SSCR-N @ < while
      SORT-I @ SSCR-AT  DEDUP-W @ 1- SSCR-AT  CKEY-EQ? 0= if
         SORT-I @ SSCR-AT  DEDUP-W @ SSCR-AT  CKW BYTE-COPY
         DEDUP-W @ 1+ DEDUP-W !
      then
      SORT-I @ 1+ SORT-I !
   repeat
   DEDUP-W @ SSCR-N ! ;

: SREVS-CANON ( -- )   SREVS-SORT SREVS-DEDUP ;

: STORE-SREVS ( n -- ) {: s:n :}
   SSCR-N @ s SREVN!
   0 begin dup SSCR-N @ < while
      dup {: k:n :}
      k SSCR-AT  s k SREV-AT  CKW BYTE-COPY
      1+
   repeat drop ;

\ ---- BUILD: populate a fresh slot from typed field values ----------------------
\ The foreign ids (including the audit-event-id created-event) are already-valid
\ nominals from their owner constructors; BUILD holds each whole in its typed column
\ (no raw, no refinement here). The pending DEPS-* / SREVS-* sets become the canonical
\ dependency and source-revision columns. `disc` is the wire kind discriminant
\ supplied by the per-kind public builder.
: BUILD-SLOT ( n CAD-KIND:artifact-id CAD-KIND:schema-id CAD-KIND:producer-id CAD-KIND:config-id CAD-KIND:numeric-policy-id CAD-KIND:target-id CAD-KIND:audit-event-id n -- n )
   {: pver:n identity:CAD-KIND:artifact-id
      schema:CAD-KIND:schema-id  producer:CAD-KIND:producer-id
      config:CAD-KIND:config-id  npol:CAD-KIND:numeric-policy-id
      target:CAD-KIND:target-id  event:CAD-KIND:audit-event-id  disc:n :}
   DEPS-CANON  SREVS-CANON
   SLOT-ALLOC {: s:n :}
   disc s KIND!  pver s PVER!
   identity s ID-ID!
   schema s SCHEMA-ID!  producer s PRODUCER-ID!  config s CONFIG-ID!
   npol s NPOL-ID!  target s TARGET-ID!
   event s EVENT-ID!
   s STORE-DEPS  s STORE-SREVS
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
\ ( producer-version artifact-id schema-id producer-id config-id
\   numeric-policy-id target-id created-event -- weight-artifact ); created-event is a
\ CAD-KIND:audit-event-id. The pending DEPS-RESET/DEP+ and SREVS-RESET/SREV+ sets
\ supply the dependency and source-revision columns.
: BUILD-WEIGHT ( n CAD-KIND:artifact-id CAD-KIND:schema-id CAD-KIND:producer-id CAD-KIND:config-id CAD-KIND:numeric-policy-id CAD-KIND:target-id CAD-KIND:audit-event-id -- weight-artifact )
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
\ Same field order as BUILD-WEIGHT.
: BUILD-KERNEL ( n CAD-KIND:artifact-id CAD-KIND:schema-id CAD-KIND:producer-id CAD-KIND:config-id CAD-KIND:numeric-policy-id CAD-KIND:target-id CAD-KIND:audit-event-id -- kernel-artifact )
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

\ ---- envelope VALIDATE (§ 23.9 ARTIFACT:VALIDATE) -----------------------------
\ Full structural validation + digest verification over owned bytes WITHOUT pinning
\ or refining a per-kind handle: it accepts any KNOWN kind (weight or kernel) and
\ folds the recomputed-vs-stored digest check in, returning the same art-result
\ taxonomy as DECODE. The ok arm carries the validated pool slot (a single cell, per
\ the multi-cell realization rule that forbids the four-word content-digest as a sum
\ payload); the recomputed digest equals the stored digest on ok and is recoverable
\ by decoding to a kind handle and calling DIGEST-WEIGHT / DIGEST-KERNEL. No raw id
\ or trust boundary is exposed.
: VALIDATE ( ptr u8 n -- art-result<n> ) {: a:ptr u:n :}
   a u DEC-SETUP
   SLOT-ALLOC {: s:n :}
   s DEC-SLOT-FILL
   s DEC-VALIDATE-ANY
   s DEC-RESULT ;

private

: ENV-CODEC-INIT ( -- )
   0 P-NEXT !  0 ROPAQUE-U !  DEPS-RESET  SREVS-RESET ;

ENV-CODEC-INIT
;package
