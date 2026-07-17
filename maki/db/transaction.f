\ maki/db/transaction.f - the deterministic transaction data model (MODEL-CAD-V2-PLAN.md
\ § 23 "Deterministic transactions" + § 23.1 transactional action surface; dot
\ habu-v2-txn-journal-d0bc644f).
\
\ CONCERN: the immutable transaction VALUE - snapshot-isolation read/write sets,
\ dependency edges, capability set, budget ledger, obligations, and the derived
\ commit proposal - plus its canonical serialisation, validation, idempotency key,
\ and proposed revision id. Package TX. This is a DIFFERENT concern from the rev-id
\ IDENTITY registry (maki/rev.f, package REV, which this file consumes): the revision
\ CONTENT canonical form lives here (§ 23.9 rev-id NEEDS-DECISION), the rev-id
\ identity there.
\
\ FIELDS (plan § 23 "Transactions use snapshot isolation with explicit read and write
\ sets"):
\   - transaction id : the checked immutable TX:txn handle. The pool is append-only
\       and columns are written once at BUILD, so the handle names an immutable value;
\       it can never be forged (checker-native PRODUCT, no trust boundary).
\   - base revision  : CAD-KIND:rev-id (the snapshot the action reads).
\   - read set       : (CAD-KIND:artifact-id, polarity) entries; polarity PRESENT or
\       ABSENT, so a NEGATIVE LOOKUP (object absent from the base revision) is a
\       first-class recorded read (§ 23 "every read dependency and negative lookup is
\       recorded"). Canonically ordered by object identity, one entry per object.
\   - write object set : CAD-KIND:artifact-id (the immutable candidate objects the
\       action produces). Canonically ordered; a DUPLICATE object (two conflicting
\       writes to one object) is a VALIDATE reject, never silently merged.
\   - dependency edges : CAD-KIND:artifact-id input objects the outputs depend on.
\       Every edge object MUST appear in the read set, else VALIDATE rejects
\       (omitted-read) - "you cannot depend on what you did not read".
\   - capability set : closed-vocabulary capability CODES (n). CONSERVATIVE READING:
\       CAD-KIND:capability-id's owner package CAP is a product decision out of this
\       dot's scope (§ 23.9 capability-id row), so a capability is held as its stable
\       vocabulary code (the plan's capability WIRE form) until CAP lands; migrate to
\       the nominal when it does.
\   - budget ledger  : (dimension code, amount) entries, one per dimension.
\   - obligations    : CAD-KIND:obligation-id (the interned proof-obligation identity;
\       maki/db/obligation.f OBLIG:INTERN owns the model subject/relation/domain/policy/
\       verifier/environment). Canonically ordered by the obligation CONTENT KEY (32-byte
\       OBLIG:KEY>WIRE), one entry per obligation, so the idempotency key / Merkle chain
\       stays cross-process-deterministic. The nominal cannot be forged (checker-native).
\   - commit proposal : TX:PROPOSE derives the proposed revision id by REV:COMMIT over
\       a digest of (base revision + write set) - the § 23.9 "content-addressed by the
\       canonical revision content (parent + write set)" form - so deterministic replay
\       of the same transaction reproduces the same rev-id.
\
\ IDEMPOTENCY KEY (§ 23 "Idempotency keys make retries return the original result").
\ TX:IDEMPOTENCY-KEY is a SHA-256 digest over the FULL canonical transaction content
\ (base + every set). CONSERVATIVE READING (plan under-specifies the coverage): the key
\ covers the ENTIRE canonical action, so it is stable across retries of the same
\ logical transaction (any add order canonicalises identically -> equal key) and never
\ collides two DIFFERENT actions. The transaction id (handle) and idempotency key are
\ distinct: two builds of one logical action yield two handles (instances) but ONE
\ idempotency key.
\
\ CANONICAL FORM. Sets are canonicalised at BUILD (sorted by identity, deduplicated -
\ writes are sorted but NOT deduplicated so a duplicate is detectable), so encoding is
\ insertion-order-independent. ENCODE emits the canonical bytes; DECODE parses and
\ rehydrates (REGISTER object keys, REV:WIRE>KEY the base, OBLIG:WIRE>KEY the obligations)
\ back through the same builder, so decode->re-encode is byte-identical. Every id wire form
\ is a CROSS-PROCESS content key (dot habu-wire-content-key-e5efaa74): object identity on
\ the wire is the artifact store KEY (ARTIFACT:KEY$, content-addressed), the base revision
\ is its 32-byte SHA-256 content key (REV:KEY>WIRE / WIRE>KEY), and each obligation is its
\ 32-byte content key (OBLIG:KEY>WIRE / WIRE>KEY) - all resolved BY CONTENT, so the derived
\ PROPOSE revision chain is a cross-process-deterministic Merkle chain that survives process death.
\
\ Typed domain outcomes are the bespoke tx-result sum family (the § 23.9 art-result
\ idiom, never result<a,b>); throws are reserved for capacity boundaries.
\ maki -> habu only; transaction owns -5350..-5353.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f           \ CAD-KIND:artifact-id: REGISTER / KEY$ / EQUAL? / VALIDATE-ID
require maki/rev.f                \ CAD-KIND:rev-id: COMMIT / ID>WIRE / WIRE>ID / VALIDATE
require maki/db/obligation.f      \ CAD-KIND:obligation-id: INTERN / KEY>WIRE / WIRE>KEY / ID-EQUAL? / ID-VALIDATE

-5350 constant E-TX-SET-CAP      \ a read/write/dep/cap/budget/obligation set over its per-txn cap
-5351 constant E-TX-ENC-BUF      \ canonical encode over the internal scratch or the caller buffer
-5352 constant E-TX-POLARITY     \ a read polarity that is neither PRESENT nor ABSENT
-5353 constant E-TX-NO-BASE      \ BUILD with no base revision (OPEN was not called)

package TX
public

\ Read polarity: a present read observed the object in the base revision; an absent
\ read is a NEGATIVE LOOKUP (the object was absent). A named closed vocabulary.
1 constant PRESENT
0 constant ABSENT

\ The checked immutable transaction id: a handle over the append-only pool slot. Two
\ builds of one logical action yield two DISTINCT handles (instances).
PRODUCT txn 0
   FIELD slot n
;PRODUCT

\ The idempotency key: a 256-bit digest as four 64-bit words (multi-cell so no
\ one-cell scalar can launder into it), content-addressing the full canonical action.
PRODUCT idem-key 0
   FIELD w0 n
   FIELD w1 n
   FIELD w2 n
   FIELD w3 n
;PRODUCT

\ Typed transaction outcome (the § 23.9 art-result custom-sum idiom): `ok` carries the
\ validated pool slot; the reject arms are the plan's validation refusals plus the
\ structural decode refusals. A bespoke per-package sum, not result<a,b>, so a total
\ ok construction leaves no free error variable.
SUMTYPE tx-result 1
   VARIANT ok a ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT bounds ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ----------------
: >TXN ( n -- txn )              TX-TXN:MAKE ;
: TXN> ( txn -- n )              TX-TXN:UNMAKE ;
: >IDEM ( n n n n -- idem-key )  TX-IDEM--KEY:MAKE ;

: R-OK ( n -- tx-result<n> )       TX-TX--RESULT:OK ;
: R-DUP-WRITE ( -- tx-result<n> )  TX-TX--RESULT:DUPLICATE-WRITE ;
: R-OMITTED ( -- tx-result<n> )    TX-TX--RESULT:OMITTED-READ ;
: R-MALFORMED ( -- tx-result<n> )  TX-TX--RESULT:MALFORMED ;
: R-BOUNDS ( -- tx-result<n> )     TX-TX--RESULT:BOUNDS ;

\ ---- capacities + protocol constants ------------------------------------------
8 constant TX-CAP                     \ live transaction slots (ring reuse)
16 constant SET-CAP                    \ max elements per set
256 constant KEY-MAX                    \ max object store-key bytes (matches ARTIFACT ART-KEY-MAX)
8 constant U64W                         \ fixed little-endian scalar width
4 constant U32W                         \ fixed little-endian length width
32 constant DIGEST-BYTES                \ SHA-256 output
32 constant REV-WIRE-W                   \ base-rev cross-process content-key wire width (REV:KEY>WIRE)
32 constant OBL-WIRE-W                    \ obligation cross-process content-key wire width (OBLIG:KEY>WIRE)
32 constant WIRE-SCRATCH                \ one base-rev content-key wire form (owner width <= this)
$8000 constant ENC-CAP                   \ canonical-bytes scratch

\ Canonical ascending field tags.
1 constant TAG-BASE
2 constant TAG-READ
3 constant TAG-WRITE
4 constant TAG-DEP
5 constant TAG-CAP
6 constant TAG-BUDGET
7 constant TAG-OBLIG

\ Stored per-slot validity codes.
0 constant V-OK
1 constant V-DUP-WRITE
2 constant V-OMITTED-READ

\ Decode error codes.
1 constant D-MALFORMED
2 constant D-BOUNDS

\ ---- builder scratch (one transaction under construction) ---------------------
1 TYPED-BUFFER B-BASE CAD-KIND:rev-id
variable B-BASE-SET
SET-CAP TYPED-BUFFER B-READ-ID CAD-KIND:artifact-id
create B-READ-POL SET-CAP cells allot
variable B-READ-N
SET-CAP TYPED-BUFFER B-WRITE-ID CAD-KIND:artifact-id
variable B-WRITE-N
SET-CAP TYPED-BUFFER B-DEP-ID CAD-KIND:artifact-id
variable B-DEP-N
create B-CAP SET-CAP cells allot
variable B-CAP-N
create B-BUD-DIM SET-CAP cells allot
create B-BUD-AMT SET-CAP cells allot
variable B-BUD-N
SET-CAP TYPED-BUFFER B-OBL CAD-KIND:obligation-id
variable B-OBL-N

\ ---- per-slot pool columns (parallel; a durable store is a later dot) ----------
TX-CAP TYPED-BUFFER S-BASE CAD-KIND:rev-id
TX-CAP SET-CAP * TYPED-BUFFER S-READ-ID CAD-KIND:artifact-id
create S-READ-POL TX-CAP SET-CAP * cells allot
create S-READ-N TX-CAP cells allot
TX-CAP SET-CAP * TYPED-BUFFER S-WRITE-ID CAD-KIND:artifact-id
create S-WRITE-N TX-CAP cells allot
TX-CAP SET-CAP * TYPED-BUFFER S-DEP-ID CAD-KIND:artifact-id
create S-DEP-N TX-CAP cells allot
create S-CAP TX-CAP SET-CAP * cells allot
create S-CAP-N TX-CAP cells allot
create S-BUD-DIM TX-CAP SET-CAP * cells allot
create S-BUD-AMT TX-CAP SET-CAP * cells allot
create S-BUD-N TX-CAP cells allot
TX-CAP SET-CAP * TYPED-BUFFER S-OBL CAD-KIND:obligation-id
create S-OBL-N TX-CAP cells allot
create S-VALID TX-CAP cells allot
variable S-NEXT

\ ---- encode + digest scratch --------------------------------------------------
create EBUF ENC-CAP allot
variable EO
create WSC WIRE-SCRATCH allot
create DGBUF DIGEST-BYTES allot
create OWA OBL-WIRE-W allot               \ obligation content-key scratch (emit + sort compare a)
create OWB OBL-WIRE-W allot               \ obligation content-key scratch (sort compare b)

\ ---- decode cursor ------------------------------------------------------------
PTR-VARIABLE DBASE
variable DLEN
variable DPOS
variable DERR
variable DI

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

\ ---- object ordering by store-key identity (public KEY$, no private raw) --------
variable BLI
: BYTES< ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   au bu min {: m:n :}
   0 BLI !
   begin BLI @ m < while
      a BLI @ + c@  b BLI @ + c@  {: ca:n cb:n :}
      ca cb <> if ca cb < exit then
      BLI @ 1+ BLI !
   repeat
   au bu < ;

: KEY< ( CAD-KIND:artifact-id CAD-KIND:artifact-id -- bool )
   {: x:CAD-KIND:artifact-id y:CAD-KIND:artifact-id :}
   x ARTIFACT:KEY$ y ARTIFACT:KEY$ BYTES< ;

\ ---- obligation ordering by 32-byte content key (OBLIG:KEY>WIRE, no private raw) ---
: OBL-KEY< ( CAD-KIND:obligation-id CAD-KIND:obligation-id -- bool )
   {: x:CAD-KIND:obligation-id y:CAD-KIND:obligation-id :}
   x OWA OBL-WIRE-W OBLIG:KEY>WIRE drop
   y OWB OBL-WIRE-W OBLIG:KEY>WIRE drop
   OWA OBL-WIRE-W OWB OBL-WIRE-W BYTES< ;

\ ---- per-slot column access ---------------------------------------------------
: BASE@ ( n -- CAD-KIND:rev-id )   S-BASE @ ;
: READ-N@ ( n -- n )   cells S-READ-N + @ ;
: READ-ID@ ( n n -- CAD-KIND:artifact-id ) {: s:n k:n :}   s SET-CAP * k + S-READ-ID @ ;
: READ-POL@ ( n n -- n ) {: s:n k:n :}     s SET-CAP * k + cells S-READ-POL + @ ;
: WRITE-N@ ( n -- n )   cells S-WRITE-N + @ ;
: WRITE-ID@ ( n n -- CAD-KIND:artifact-id ) {: s:n k:n :}   s SET-CAP * k + S-WRITE-ID @ ;
: DEP-N@ ( n -- n )   cells S-DEP-N + @ ;
: DEP-ID@ ( n n -- CAD-KIND:artifact-id ) {: s:n k:n :}   s SET-CAP * k + S-DEP-ID @ ;
: CAP-N@ ( n -- n )   cells S-CAP-N + @ ;
: CAP@ ( n n -- n ) {: s:n k:n :}   s SET-CAP * k + cells S-CAP + @ ;
: BUD-N@ ( n -- n )   cells S-BUD-N + @ ;
: BUD-DIM@ ( n n -- n ) {: s:n k:n :}   s SET-CAP * k + cells S-BUD-DIM + @ ;
: BUD-AMT@ ( n n -- n ) {: s:n k:n :}   s SET-CAP * k + cells S-BUD-AMT + @ ;
: OBL-N@ ( n -- n )   cells S-OBL-N + @ ;
: OBL@ ( n n -- CAD-KIND:obligation-id ) {: s:n k:n :}   s SET-CAP * k + S-OBL @ ;
: VALID! ( n n -- )   cells S-VALID + ! ;
: VALID@ ( n -- n )   cells S-VALID + @ ;

: SLOT-ALLOC ( -- n )   S-NEXT @  dup 1+ TX-CAP mod S-NEXT ! ;

\ ---- builder canonicalisation: sort by identity, then deduplicate --------------
variable OSI variable OSJ
variable NSI variable NSJ
variable DW

: READ-SORT ( -- )
   1 OSI !
   begin OSI @ B-READ-N @ < while
      OSI @ B-READ-ID @ {: kid:CAD-KIND:artifact-id :}
      OSI @ cells B-READ-POL + @ {: kpol:n :}
      OSI @ OSJ !
      begin OSJ @ 0 > if kid OSJ @ 1- B-READ-ID @ KEY< else false then while
         OSJ @ 1- B-READ-ID @  OSJ @ B-READ-ID !
         OSJ @ 1- cells B-READ-POL + @  OSJ @ cells B-READ-POL + !
         OSJ @ 1- OSJ !
      repeat
      kid  OSJ @ B-READ-ID !
      kpol OSJ @ cells B-READ-POL + !
      OSI @ 1+ OSI !
   repeat ;

: READ-DEDUP ( -- )                             \ one entry per object (keep first)
   B-READ-N @ 0= if exit then
   1 DW !  1 OSI !
   begin OSI @ B-READ-N @ < while
      OSI @ B-READ-ID @  DW @ 1- B-READ-ID @  ARTIFACT:EQUAL? 0= if
         OSI @ B-READ-ID @  DW @ B-READ-ID !
         OSI @ cells B-READ-POL + @  DW @ cells B-READ-POL + !
         DW @ 1+ DW !
      then
      OSI @ 1+ OSI !
   repeat
   DW @ B-READ-N ! ;

: WRITE-SORT ( -- )                             \ sort only; duplicates stay for VALIDATE
   1 OSI !
   begin OSI @ B-WRITE-N @ < while
      OSI @ B-WRITE-ID @ {: kid:CAD-KIND:artifact-id :}
      OSI @ OSJ !
      begin OSJ @ 0 > if kid OSJ @ 1- B-WRITE-ID @ KEY< else false then while
         OSJ @ 1- B-WRITE-ID @  OSJ @ B-WRITE-ID !
         OSJ @ 1- OSJ !
      repeat
      kid OSJ @ B-WRITE-ID !
      OSI @ 1+ OSI !
   repeat ;

: DEP-SORT ( -- )
   1 OSI !
   begin OSI @ B-DEP-N @ < while
      OSI @ B-DEP-ID @ {: kid:CAD-KIND:artifact-id :}
      OSI @ OSJ !
      begin OSJ @ 0 > if kid OSJ @ 1- B-DEP-ID @ KEY< else false then while
         OSJ @ 1- B-DEP-ID @  OSJ @ B-DEP-ID !
         OSJ @ 1- OSJ !
      repeat
      kid OSJ @ B-DEP-ID !
      OSI @ 1+ OSI !
   repeat ;

: DEP-DEDUP ( -- )
   B-DEP-N @ 0= if exit then
   1 DW !  1 OSI !
   begin OSI @ B-DEP-N @ < while
      OSI @ B-DEP-ID @  DW @ 1- B-DEP-ID @  ARTIFACT:EQUAL? 0= if
         OSI @ B-DEP-ID @  DW @ B-DEP-ID !
         DW @ 1+ DW !
      then
      OSI @ 1+ OSI !
   repeat
   DW @ B-DEP-N ! ;

: CAP-SORT ( -- )                               \ ascending numeric insertion sort of B-CAP
   1 NSI !
   begin NSI @ B-CAP-N @ < while
      NSI @ cells B-CAP + @ {: key:n :}
      NSI @ NSJ !
      begin NSJ @ 0 > if NSJ @ 1- cells B-CAP + @ key > else false then while
         NSJ @ 1- cells B-CAP + @  NSJ @ cells B-CAP + !
         NSJ @ 1- NSJ !
      repeat
      key NSJ @ cells B-CAP + !
      NSI @ 1+ NSI !
   repeat ;

: CAP-DEDUP ( -- )
   B-CAP-N @ 0= if exit then
   1 DW !  1 NSI !
   begin NSI @ B-CAP-N @ < while
      NSI @ cells B-CAP + @  DW @ 1- cells B-CAP + @  <> if
         NSI @ cells B-CAP + @  DW @ cells B-CAP + !
         DW @ 1+ DW !
      then
      NSI @ 1+ NSI !
   repeat
   DW @ B-CAP-N ! ;

: OBL-SORT ( -- )                               \ insertion sort of B-OBL by content-key bytes
   1 OSI !
   begin OSI @ B-OBL-N @ < while
      OSI @ B-OBL @ {: kid:CAD-KIND:obligation-id :}
      OSI @ OSJ !
      begin OSJ @ 0 > if kid OSJ @ 1- B-OBL @ OBL-KEY< else false then while
         OSJ @ 1- B-OBL @  OSJ @ B-OBL !
         OSJ @ 1- OSJ !
      repeat
      kid OSJ @ B-OBL !
      OSI @ 1+ OSI !
   repeat ;

: OBL-DEDUP ( -- )                              \ one entry per obligation (keep first)
   B-OBL-N @ 0= if exit then
   1 DW !  1 OSI !
   begin OSI @ B-OBL-N @ < while
      OSI @ B-OBL @  DW @ 1- B-OBL @  OBLIG:ID-EQUAL? 0= if
         OSI @ B-OBL @  DW @ B-OBL !
         DW @ 1+ DW !
      then
      OSI @ 1+ OSI !
   repeat
   DW @ B-OBL-N ! ;

: BUD-SORT ( -- )                               \ sort (dim,amt) parallel by dim
   1 NSI !
   begin NSI @ B-BUD-N @ < while
      NSI @ cells B-BUD-DIM + @ {: kd:n :}
      NSI @ cells B-BUD-AMT + @ {: ka:n :}
      NSI @ NSJ !
      begin NSJ @ 0 > if NSJ @ 1- cells B-BUD-DIM + @ kd > else false then while
         NSJ @ 1- cells B-BUD-DIM + @  NSJ @ cells B-BUD-DIM + !
         NSJ @ 1- cells B-BUD-AMT + @  NSJ @ cells B-BUD-AMT + !
         NSJ @ 1- NSJ !
      repeat
      kd NSJ @ cells B-BUD-DIM + !
      ka NSJ @ cells B-BUD-AMT + !
      NSI @ 1+ NSI !
   repeat ;

: BUD-DEDUP ( -- )                              \ one entry per dimension (keep first)
   B-BUD-N @ 0= if exit then
   1 DW !  1 NSI !
   begin NSI @ B-BUD-N @ < while
      NSI @ cells B-BUD-DIM + @  DW @ 1- cells B-BUD-DIM + @  <> if
         NSI @ cells B-BUD-DIM + @  DW @ cells B-BUD-DIM + !
         NSI @ cells B-BUD-AMT + @  DW @ cells B-BUD-AMT + !
         DW @ 1+ DW !
      then
      NSI @ 1+ NSI !
   repeat
   DW @ B-BUD-N ! ;

: CANON ( -- )
   READ-SORT READ-DEDUP
   WRITE-SORT
   DEP-SORT DEP-DEDUP
   CAP-SORT CAP-DEDUP
   BUD-SORT BUD-DEDUP
   OBL-SORT OBL-DEDUP ;

\ ---- copy canonical builder into a fresh slot ---------------------------------
: STORE-BASE ( n -- )   0 B-BASE @  swap S-BASE ! ;

: STORE-READ ( n -- ) {: s:n :}
   B-READ-N @ s cells S-READ-N + !
   0 begin dup B-READ-N @ < while
      dup {: k:n :}
      k B-READ-ID @  s SET-CAP * k + S-READ-ID !
      k cells B-READ-POL + @  s SET-CAP * k + cells S-READ-POL + !
      1+
   repeat drop ;

: STORE-WRITE ( n -- ) {: s:n :}
   B-WRITE-N @ s cells S-WRITE-N + !
   0 begin dup B-WRITE-N @ < while
      dup {: k:n :}
      k B-WRITE-ID @  s SET-CAP * k + S-WRITE-ID !
      1+
   repeat drop ;

: STORE-DEP ( n -- ) {: s:n :}
   B-DEP-N @ s cells S-DEP-N + !
   0 begin dup B-DEP-N @ < while
      dup {: k:n :}
      k B-DEP-ID @  s SET-CAP * k + S-DEP-ID !
      1+
   repeat drop ;

: STORE-CAP ( n -- ) {: s:n :}
   B-CAP-N @ s cells S-CAP-N + !
   0 begin dup B-CAP-N @ < while
      dup {: k:n :}
      k cells B-CAP + @  s SET-CAP * k + cells S-CAP + !
      1+
   repeat drop ;

: STORE-BUDGET ( n -- ) {: s:n :}
   B-BUD-N @ s cells S-BUD-N + !
   0 begin dup B-BUD-N @ < while
      dup {: k:n :}
      k cells B-BUD-DIM + @  s SET-CAP * k + cells S-BUD-DIM + !
      k cells B-BUD-AMT + @  s SET-CAP * k + cells S-BUD-AMT + !
      1+
   repeat drop ;

: STORE-OBLIG ( n -- ) {: s:n :}
   B-OBL-N @ s cells S-OBL-N + !
   0 begin dup B-OBL-N @ < while
      dup {: k:n :}
      k B-OBL @  s SET-CAP * k + S-OBL !
      1+
   repeat drop ;

: STORE-ALL ( n -- ) {: s:n :}
   s STORE-BASE  s STORE-READ  s STORE-WRITE  s STORE-DEP
   s STORE-CAP   s STORE-BUDGET  s STORE-OBLIG ;

\ ---- validity: duplicate write, then omitted read dependency -------------------
: DUP-WRITE? ( n -- bool ) {: s:n :}
   s WRITE-N@ 1 <= if false exit then
   1 begin dup s WRITE-N@ < while
      dup {: k:n :}
      s k WRITE-ID@  s k 1- WRITE-ID@  ARTIFACT:EQUAL? if drop true exit then
      1+
   repeat drop false ;

: DEP-IN-READ? ( n CAD-KIND:artifact-id -- bool ) {: s:n d:CAD-KIND:artifact-id :}
   0 begin dup s READ-N@ < while
      dup {: k:n :}
      d  s k READ-ID@  ARTIFACT:EQUAL? if drop true exit then
      1+
   repeat drop false ;

: OMITTED-READ? ( n -- bool ) {: s:n :}
   0 begin dup s DEP-N@ < while
      dup {: k:n :}
      s  s k DEP-ID@  DEP-IN-READ? 0= if drop true exit then
      1+
   repeat drop false ;

: COMPUTE-VALID ( n -- ) {: s:n :}
   s DUP-WRITE? if V-DUP-WRITE s VALID! exit then
   s OMITTED-READ? if V-OMITTED-READ s VALID! exit then
   V-OK s VALID! ;

\ ---- canonical byte emit (into EBUF) ------------------------------------------
: E-RESET ( -- )   0 EO ! ;
: E-ROOM ( n -- ) {: k:n :}   EO @ k + ENC-CAP > if E-TX-ENC-BUF throw then ;
: E-U8 ( n -- ) {: c:n :}   1 E-ROOM  c EBUF EO @ + c!  EO @ 1+ EO ! ;
: E-LE ( n n -- ) {: v:n w:n :}   w E-ROOM  v EBUF EO @ + w LE-PUT  EO @ w + EO ! ;
: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}   u E-ROOM  a EBUF EO @ + u BYTE-COPY  EO @ u + EO ! ;

: E-OBJ ( CAD-KIND:artifact-id -- )             \ keylen u32 + store-key bytes
   ARTIFACT:KEY$ {: a:ptr u:n :}
   u U32W E-LE
   a u E-BYTES ;

: EMIT-BASE ( n -- ) {: s:n :}
   TAG-BASE E-U8
   s BASE@ WSC WIRE-SCRATCH REV:KEY>WIRE {: w:n :}   \ 32-byte cross-process content key
   WSC w E-BYTES ;

: EMIT-READ ( n -- ) {: s:n :}
   TAG-READ E-U8
   s READ-N@ U64W E-LE
   0 begin dup s READ-N@ < while
      dup {: k:n :}
      s k READ-ID@ E-OBJ
      s k READ-POL@ E-U8
      1+
   repeat drop ;

: EMIT-WRITE ( n -- ) {: s:n :}
   TAG-WRITE E-U8
   s WRITE-N@ U64W E-LE
   0 begin dup s WRITE-N@ < while
      dup {: k:n :}
      s k WRITE-ID@ E-OBJ
      1+
   repeat drop ;

: EMIT-DEP ( n -- ) {: s:n :}
   TAG-DEP E-U8
   s DEP-N@ U64W E-LE
   0 begin dup s DEP-N@ < while
      dup {: k:n :}
      s k DEP-ID@ E-OBJ
      1+
   repeat drop ;

: EMIT-CAP ( n -- ) {: s:n :}
   TAG-CAP E-U8
   s CAP-N@ U64W E-LE
   0 begin dup s CAP-N@ < while
      dup {: k:n :}
      s k CAP@ U64W E-LE
      1+
   repeat drop ;

: EMIT-BUDGET ( n -- ) {: s:n :}
   TAG-BUDGET E-U8
   s BUD-N@ U64W E-LE
   0 begin dup s BUD-N@ < while
      dup {: k:n :}
      s k BUD-DIM@ U64W E-LE
      s k BUD-AMT@ U64W E-LE
      1+
   repeat drop ;

: EMIT-OBLIG ( n -- ) {: s:n :}                 \ each obligation as its 32-byte content key
   TAG-OBLIG E-U8
   s OBL-N@ U64W E-LE
   0 begin dup s OBL-N@ < while
      dup {: k:n :}
      s k OBL@  OWA OBL-WIRE-W OBLIG:KEY>WIRE {: w:n :}
      OWA w E-BYTES
      1+
   repeat drop ;

: EMIT-CANON ( n -- ) {: s:n :}
   E-RESET
   s EMIT-BASE  s EMIT-READ  s EMIT-WRITE  s EMIT-DEP
   s EMIT-CAP   s EMIT-BUDGET  s EMIT-OBLIG ;

: EMIT-REV ( n -- ) {: s:n :}                \ base + write set only (the revision content)
   E-RESET
   s EMIT-BASE  s EMIT-WRITE ;

\ ---- decode cursor helpers ----------------------------------------------------
: DFAIL ( n -- )   DERR ! ;
: DFAILED? ( -- bool )   DERR @ 0<> ;
: REMAIN ( -- n )   DLEN @ DPOS @ - ;
: DPTR ( -- ptr u8 )   DBASE 0 ptr-field @ DPOS @ + ;
: DADV ( n -- )   DPOS @ + DPOS ! ;

: D-U8 ( -- n )
   REMAIN 1 < if D-MALFORMED DFAIL 0 exit then
   DPTR c@  1 DADV ;

: D-LE ( n -- n ) {: w:n :}
   REMAIN w < if D-MALFORMED DFAIL 0 exit then
   DPTR w LE-GET  w DADV ;

: DEC-TAG ( n -- ) {: want:n :}                 \ consume and require an exact tag
   D-U8 {: tag:n :} DFAILED? if exit then
   tag want <> if D-MALFORMED DFAIL then ;

: DEC-KEY ( -- ptr u8 n )                        \ keylen u32 + key bytes (into input buffer)
   U32W D-LE {: kl:n :} DFAILED? if DPTR 0 exit then
   kl 0 < kl KEY-MAX > or if D-BOUNDS DFAIL DPTR 0 exit then
   REMAIN kl < if D-MALFORMED DFAIL DPTR 0 exit then
   DPTR kl  kl DADV ;

: DEC-COUNT ( -- n )                             \ set count u64, bounded by SET-CAP
   U64W D-LE {: cnt:n :} DFAILED? if 0 exit then
   cnt 0 < cnt SET-CAP > or if D-BOUNDS DFAIL 0 exit then
   cnt ;

\ ---- builder API --------------------------------------------------------------
public

\ OPEN begins a transaction under construction on the given base revision (validated
\ fail-closed), clearing every set.
: OPEN ( CAD-KIND:rev-id -- )
   REV:VALIDATE 0 B-BASE !
   true B-BASE-SET !
   0 B-READ-N !  0 B-WRITE-N !  0 B-DEP-N !  0 B-CAP-N !  0 B-BUD-N !  0 B-OBL-N ! ;

\ READ+ records a read of an object at the base revision: polarity PRESENT (observed)
\ or ABSENT (a negative lookup). Fail-closed on an unknown polarity or a full set.
: READ+ ( CAD-KIND:artifact-id n -- ) {: id:CAD-KIND:artifact-id pol:n :}
   pol PRESENT <> pol ABSENT <> and if E-TX-POLARITY throw then
   B-READ-N @ SET-CAP >= if E-TX-SET-CAP throw then
   id ARTIFACT:VALIDATE-ID  B-READ-N @ B-READ-ID !
   pol B-READ-N @ cells B-READ-POL + !
   B-READ-N @ 1+ B-READ-N ! ;

: WRITE+ ( CAD-KIND:artifact-id -- ) {: id:CAD-KIND:artifact-id :}
   B-WRITE-N @ SET-CAP >= if E-TX-SET-CAP throw then
   id ARTIFACT:VALIDATE-ID  B-WRITE-N @ B-WRITE-ID !
   B-WRITE-N @ 1+ B-WRITE-N ! ;

: DEP+ ( CAD-KIND:artifact-id -- ) {: id:CAD-KIND:artifact-id :}
   B-DEP-N @ SET-CAP >= if E-TX-SET-CAP throw then
   id ARTIFACT:VALIDATE-ID  B-DEP-N @ B-DEP-ID !
   B-DEP-N @ 1+ B-DEP-N ! ;

: CAP+ ( n -- ) {: c:n :}                        \ closed-vocabulary capability code
   B-CAP-N @ SET-CAP >= if E-TX-SET-CAP throw then
   c B-CAP-N @ cells B-CAP + !
   B-CAP-N @ 1+ B-CAP-N ! ;

: BUDGET+ ( n n -- ) {: dim:n amt:n :}           \ budget-ledger dimension + amount
   B-BUD-N @ SET-CAP >= if E-TX-SET-CAP throw then
   dim B-BUD-N @ cells B-BUD-DIM + !
   amt B-BUD-N @ cells B-BUD-AMT + !
   B-BUD-N @ 1+ B-BUD-N ! ;

: OBLIG+ ( CAD-KIND:obligation-id -- ) {: o:CAD-KIND:obligation-id :}   \ interned proof-obligation id
   B-OBL-N @ SET-CAP >= if E-TX-SET-CAP throw then
   o OBLIG:ID-VALIDATE  B-OBL-N @ B-OBL !
   B-OBL-N @ 1+ B-OBL-N ! ;

\ BUILD canonicalises the pending sets, stores an immutable transaction into a fresh
\ pool slot, computes its validity, and returns the checked handle. Throws only at
\ capacity / missing base.
: BUILD ( -- txn )
   B-BASE-SET @ 0= if E-TX-NO-BASE throw then
   CANON
   SLOT-ALLOC {: s:n :}
   s STORE-ALL
   s COMPUTE-VALID
   s >TXN ;

\ VALIDATE returns the plan's typed transaction outcome: duplicate/conflicting writes
\ and an omitted read dependency are REJECTS; otherwise ok carries the validated slot.
: VALIDATE ( txn -- tx-result<n> ) {: t:txn :}
   t TXN> {: s:n :}
   s VALID@ {: v:n :}
   v V-DUP-WRITE = if R-DUP-WRITE exit then
   v V-OMITTED-READ = if R-OMITTED exit then
   s R-OK ;

\ IDEMPOTENCY-KEY content-addresses the FULL canonical action; retrying the same
\ logical transaction (any add order) reproduces the same key.
: IDEMPOTENCY-KEY ( txn -- idem-key ) {: t:txn :}
   t TXN> EMIT-CANON
   EBUF EO @ DGBUF SHA256
   DGBUF U64W LE-GET
   DGBUF U64W + U64W LE-GET
   DGBUF U64W 2 * + U64W LE-GET
   DGBUF U64W 3 * + U64W LE-GET
   >IDEM ;

: KEY-EQ? ( idem-key idem-key -- bool )
   TX-IDEM--KEY:UNMAKE {: y0:n y1:n y2:n y3:n :}
   TX-IDEM--KEY:UNMAKE {: x0:n x1:n x2:n x3:n :}
   x0 y0 = x1 y1 = and x2 y2 = and x3 y3 = and ;

\ PROPOSE is the commit proposal: the proposed revision id, minted by REV:COMMIT over a
\ digest of (base revision + write set) - the § 23.9 content-addressed revision content -
\ so deterministic replay of the same transaction reproduces the same rev-id.
: PROPOSE ( txn -- CAD-KIND:rev-id ) {: t:txn :}
   t TXN> EMIT-REV
   EBUF EO @ DGBUF SHA256
   DGBUF DIGEST-BYTES REV:COMMIT ;

: BASE-REV ( txn -- CAD-KIND:rev-id )   TXN> BASE@ ;

\ ENCODE writes the canonical transaction bytes into a caller buffer and returns the
\ length (E-TX-ENC-BUF if the buffer cannot hold them).
: ENCODE ( txn ptr u8 n -- n ) {: t:txn out:ptr cap:n :}
   t TXN> EMIT-CANON
   EO @ cap > if E-TX-ENC-BUF throw then
   EBUF out EO @ BYTE-COPY
   EO @ ;

\ ENCODE-REV writes the canonical REVISION CONTENT (base revision + write set only, the
\ § 23.9 content-addressed revision form) into a caller buffer and returns the length.
\ This is exactly the byte string PROPOSE digests through REV:COMMIT, so a durable
\ content-addressed store can persist a revision under REV:KEY>WIRE(PROPOSE) and read it
\ back with a matching SHA-256 (E-TX-ENC-BUF if the buffer cannot hold it).
: ENCODE-REV ( txn ptr u8 n -- n ) {: t:txn out:ptr cap:n :}
   t TXN> EMIT-REV
   EO @ cap > if E-TX-ENC-BUF throw then
   EBUF out EO @ BYTE-COPY
   EO @ ;

\ IDEM-KEY>WIRE writes the 32-byte idempotency DIGEST (SHA-256 over the full canonical
\ action, the raw preimage of IDEMPOTENCY-KEY's four words) into a caller buffer and
\ returns DIGEST-BYTES. A durable store keys an idempotency record by these bytes so a
\ retry of the same logical transaction resolves the original result across process death
\ (E-TX-ENC-BUF if the buffer cannot hold the fixed width).
: IDEM-KEY>WIRE ( txn ptr u8 n -- n ) {: t:txn out:ptr cap:n :}
   cap DIGEST-BYTES < if E-TX-ENC-BUF throw then
   t TXN> EMIT-CANON
   EBUF EO @ out SHA256
   DIGEST-BYTES ;

private

: DEC-READ ( -- )
   TAG-READ DEC-TAG  DFAILED? if exit then
   DEC-COUNT {: cnt:n :}  DFAILED? if exit then
   0 DI !
   begin DI @ cnt < DFAILED? 0= and while
      DEC-KEY {: a:ptr u:n :}
      DFAILED? 0= if
         a u ARTIFACT:REGISTER {: id:CAD-KIND:artifact-id :}
         D-U8 {: pol:n :}
         DFAILED? 0= if id pol READ+ then
      then
      DI @ 1+ DI !
   repeat ;

: DEC-WRITE ( -- )
   TAG-WRITE DEC-TAG  DFAILED? if exit then
   DEC-COUNT {: cnt:n :}  DFAILED? if exit then
   0 DI !
   begin DI @ cnt < DFAILED? 0= and while
      DEC-KEY {: a:ptr u:n :}
      DFAILED? 0= if a u ARTIFACT:REGISTER WRITE+ then
      DI @ 1+ DI !
   repeat ;

: DEC-DEP ( -- )
   TAG-DEP DEC-TAG  DFAILED? if exit then
   DEC-COUNT {: cnt:n :}  DFAILED? if exit then
   0 DI !
   begin DI @ cnt < DFAILED? 0= and while
      DEC-KEY {: a:ptr u:n :}
      DFAILED? 0= if a u ARTIFACT:REGISTER DEP+ then
      DI @ 1+ DI !
   repeat ;

: DEC-CAP ( -- )
   TAG-CAP DEC-TAG  DFAILED? if exit then
   DEC-COUNT {: cnt:n :}  DFAILED? if exit then
   0 DI !
   begin DI @ cnt < DFAILED? 0= and while
      U64W D-LE {: c:n :}
      DFAILED? 0= if c CAP+ then
      DI @ 1+ DI !
   repeat ;

: DEC-BUDGET ( -- )
   TAG-BUDGET DEC-TAG  DFAILED? if exit then
   DEC-COUNT {: cnt:n :}  DFAILED? if exit then
   0 DI !
   begin DI @ cnt < DFAILED? 0= and while
      U64W D-LE {: dim:n :}
      U64W D-LE {: amt:n :}
      DFAILED? 0= if dim amt BUDGET+ then
      DI @ 1+ DI !
   repeat ;

: DEC-OBL-ONE ( -- )                            \ 32-byte content key -> obligation-id, fail-closed
   REMAIN OBL-WIRE-W < if D-MALFORMED DFAIL exit then
   DPTR OBL-WIRE-W OBLIG:WIRE>KEY
   MATCH OBLIG:id-result
      ok          OF OBL-WIRE-W DADV  OBLIG+ ENDOF
      wrong-width OF D-MALFORMED DFAIL ENDOF
      unknown     OF D-BOUNDS DFAIL ENDOF
   ;MATCH ;

: DEC-OBLIG ( -- )
   TAG-OBLIG DEC-TAG  DFAILED? if exit then
   DEC-COUNT {: cnt:n :}  DFAILED? if exit then
   0 DI !
   begin DI @ cnt < DFAILED? 0= and while
      DEC-OBL-ONE
      DI @ 1+ DI !
   repeat ;

: DEC-BASE ( -- bool )                          \ true = opened a builder on the base rev
   TAG-BASE DEC-TAG  DFAILED? if false exit then
   REMAIN REV-WIRE-W < if D-MALFORMED DFAIL false exit then
   DPTR REV-WIRE-W REV:WIRE>KEY                  \ resolve the base content key BY CONTENT
   MATCH REV:id-result
      ok          OF REV-WIRE-W DADV  OPEN true ENDOF
      wrong-width OF D-MALFORMED DFAIL false ENDOF
      unknown     OF D-BOUNDS DFAIL false ENDOF
   ;MATCH ;

: DEC-RESULT ( -- tx-result<n> )
   DERR @ D-MALFORMED = if R-MALFORMED exit then
   R-BOUNDS ;

public

\ DECODE parses canonical transaction bytes, rehydrating object identities through
\ ARTIFACT:REGISTER and the base revision through REV:WIRE>ID, and rebuilds the
\ transaction through the same canonicalising builder; ok carries the fresh slot, so
\ decode -> re-ENCODE is byte-identical. Structural failures fold into malformed /
\ bounds (REV:WIRE>ID wrong-width -> malformed, unknown -> bounds).
: DECODE ( ptr u8 n -- tx-result<n> ) {: a:ptr u:n :}
   a DBASE 0 ptr-field !  u DLEN !  0 DPOS !  0 DERR !
   DEC-BASE 0= if DEC-RESULT exit then
   DEC-READ DEC-WRITE DEC-DEP DEC-CAP DEC-BUDGET DEC-OBLIG
   DFAILED? if DEC-RESULT exit then
   BUILD TXN> R-OK ;

\ TXN-OF rebuilds a transaction handle from a validated ok slot (e.g. a DECODE / VALIDATE
\ ok payload), the maki/db/artifact.f WEIGHT-OF precedent.
: TXN-OF ( n -- txn )   >TXN ;

\ ---- set-count projections (inspection) ---------------------------------------
: READ-COUNT ( txn -- n )     TXN> READ-N@ ;
: WRITE-COUNT ( txn -- n )    TXN> WRITE-N@ ;
: DEP-COUNT ( txn -- n )      TXN> DEP-N@ ;
: CAP-COUNT ( txn -- n )      TXN> CAP-N@ ;
: BUDGET-COUNT ( txn -- n )   TXN> BUD-N@ ;
: OBLIG-COUNT ( txn -- n )    TXN> OBL-N@ ;

private

: TX-INIT ( -- )   0 S-NEXT !  0 B-BASE-SET ! ;
TX-INIT

;package
