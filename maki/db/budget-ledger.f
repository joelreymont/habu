\ maki/db/budget-ledger.f - the monotonic budget LEDGER (MODEL-CAD-V2-PLAN.md § 23 autonomy
\ boundary "experiment, compute, storage, and wall-time budgets", plan:3205-3211; § 23.1
\ "resource budgets prevent stale or duplicated agent actions", "a failed transaction publishes
\ nothing", "exhausted budget ... returns a structured diagnostic without partial state",
\ plan:3234-3241; dot habu-v2-capability-and-0970a96d). Aligned with habu-v2-types-finite-18bb1b35.
\
\ CONCERN: a monotonic resource ACCOUNTING value over the shared BUDGET dimensions. A ledger
\ carries an initial LIMIT vector and a REMAINING vector (monotonic non-increasing); RESERVE
\ tests whether a request fits the remaining budget; CHARGE deducts a request atomically and
\ IDEMPOTENTLY (keyed by an opaque idempotency key, so a retry never double charges); DIGEST
\ content-addresses the canonical ledger state so REPLAY reproduces identical charges. This is a
\ DIFFERENT concern from the capability GRANT (package CAPTOK, the authority CEILING) and from
\ the commit store (package CSTORE, which COMPOSES reserve+charge around the publish boundary):
\ this file is pure accounting, decoupled from both (it shares only the BUDGET vocabulary).
\
\ ---- ATOMICITY + IDEMPOTENCY (compose with the commit boundary) -----------------------
\ RESERVE is a pure fit check (no mutation): it returns `exhausted` naming the FIRST dimension
\ whose request exceeds the remaining budget, else `ok`. CHARGE is keyed by a 32-byte idempotency
\ key: if that key was already charged on this ledger, CHARGE is a NO-OP `ok` (a retry reproduces
\ the original result and never double charges); otherwise it re-checks fit, deducts the request
\ from REMAINING for every dimension, and records the key. So a caller that RESERVEs before an
\ effect and CHARGEs (with the effect's idempotency key) only on success gets: a failed effect
\ charges nothing, a succeeded effect charges exactly once, and any retry charges nothing more.
\ EXHAUSTION is a typed result (a `budget-result` naming the exhausted dimension - the resource
\ diagnostic axis, plan § 23.2 / diagnostic.f DIAG:class resource), never a throw.
\
\ ---- REPLAY (deterministic ledger digest, order-independent) ---------------------------
\ DIGEST is SHA-256 over the CANONICAL ledger state: the limit vector, the remaining vector, and
\ the charged-key SET emitted in sorted (byte-ascending) order. Deduction commutes and the key
\ set is order-free, so replaying the same charges in ANY order yields an identical digest (the
\ maki/db/action.f DIGEST / registry replay precedent). maki -> habu only; budget-ledger owns
\ -5385..-5388.

require lib/prelude.f
require maki/db/budget-dim.f      \ BUDGET:dim / DIM>N / N>DIM / BUDGET:DIM-COUNT: the shared budget-dimension vocabulary

-5385 constant E-LEDGER-CAP       \ ledger pool exhausted (first-slice bounded ring)
-5386 constant E-LEDGER-KEYS      \ charged idempotency-key set over its per-ledger cap
-5387 constant E-LEDGER-RANGE     \ a ledger handle outside the allocated slot range (fail-closed)
-5388 constant E-LEDGER-BUF       \ DIGEST output buffer smaller than the fixed digest width

package LEDGER
public

\ The ledger handle: a pooled-slot handle (the maki/db/transaction.f `txn` precedent - the
\ accounting authority lives in the slot columns, not the handle).
STRUCTURE ledger 0
   FIELD slot n
;STRUCTURE

\ Typed budget outcome (never value+flag): `ok` (fit / idempotent no-op); `exhausted` names the
\ first dimension whose request exceeds the remaining budget (the resource-diagnostic axis).
SUMTYPE budget-result 0
   VARIANT ok ;VARIANT
   VARIANT exhausted BUDGET:dim ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ------------------
: >LEDGER ( n -- ledger )   LEDGER-LEDGER:MAKE ;
: LEDGER> ( ledger -- n )   LEDGER-LEDGER:UNMAKE ;

: BR-OK ( -- budget-result )               LEDGER-BUDGET--RESULT:OK ;
: BR-EXHAUSTED ( BUDGET:dim -- budget-result )   LEDGER-BUDGET--RESULT:EXHAUSTED ;

\ ---- capacities + fixed widths -------------------------------------------------
32 constant LEDGER-CAP            \ max live ledgers (v1 first slice; a durable store is a later dot)
64 constant MAX-KEYS             \ max charged idempotency keys per ledger
32 constant KEY-W                \ idempotency-key width (matches TX:IDEM-KEY>WIRE 32-byte digest)
8  constant U64W                 \ fixed little-endian scalar width
32 constant DIGEST-BYTES         \ SHA-256 output width
$4000 constant ENC-CAP            \ canonical-state scratch

\ ---- ledger pool (parallel per-slot columns; the accounting lives HERE) ---------
create L-LIMIT  LEDGER-CAP BUDGET:DIM-COUNT * cells allot   \ per-slot initial limit vector
create L-REMAIN LEDGER-CAP BUDGET:DIM-COUNT * cells allot   \ per-slot remaining vector (monotonic non-increasing)
create L-KEYS   LEDGER-CAP MAX-KEYS * KEY-W * allot   \ per-slot charged idempotency keys (32 bytes each)
create L-KEY-N  LEDGER-CAP cells allot                 \ per-slot charged-key count
variable L-N                                            \ allocated ledger count (append-only)

\ ---- request scratch (one reserve/charge request; single-writer, like the codec scratch) ---
create REQ BUDGET:DIM-COUNT cells allot

\ ---- digest scratch ------------------------------------------------------------
create EBUF ENC-CAP allot
variable EO
create DGBUF DIGEST-BYTES allot
create KORD MAX-KEYS cells allot                       \ sorted key-index permutation

\ ---- slot access ---------------------------------------------------------------
: LIMIT-AT ( n n -- ptr a ) {: s:n d:n :}   s BUDGET:DIM-COUNT * d + cells L-LIMIT + ;
: REMAIN-AT ( n n -- ptr a ) {: s:n d:n :}   s BUDGET:DIM-COUNT * d + cells L-REMAIN + ;
: KEY-AT ( n n -- ptr u8 ) {: s:n k:n :}   s MAX-KEYS * k + KEY-W * L-KEYS + ;
: KEY-N@ ( n -- n )   cells L-KEY-N + @ ;
: KEY-N! ( n n -- ) {: v:n s:n :}   v s cells L-KEY-N + ! ;

: SLOT-ALLOC ( -- n )
   L-N @ LEDGER-CAP >= if E-LEDGER-CAP throw then
   L-N @  dup 1+ L-N ! ;

: RANGE-CK ( ledger -- n )                             \ refine a ledger to its validated raw slot
   LEDGER> dup 0 < over L-N @ >= or if E-LEDGER-RANGE throw then ;

\ ---- fixed little-endian scalar codec + byte compare ---------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

variable BLI
: KEY< ( ptr u8 ptr u8 -- bool ) {: a:ptr b:ptr :}     \ 32-byte lexicographic less-than
   0 BLI !
   begin BLI @ KEY-W < while
      a BLI @ + c@  b BLI @ + c@  {: ca:n cb:n :}
      ca cb <> if ca cb < exit then
      BLI @ 1+ BLI !
   repeat false ;

: KEY= ( ptr u8 ptr u8 -- bool ) {: a:ptr b:ptr :}
   0 begin dup KEY-W < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ index of the charged key equal to the 32 bytes at p, or -1
: KEY-FIND ( n ptr u8 -- n ) {: s:n p:ptr :}
   s KEY-N@ 0 ?do
      s i KEY-AT p KEY= if i unloop exit then
   loop -1 ;

public

\ ---- ledger lifecycle ----------------------------------------------------------
\ OPEN allocates a fresh ledger with all limits + remaining zero and no charged keys.
: OPEN ( -- ledger )
   SLOT-ALLOC {: s:n :}
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      0 s d LIMIT-AT !
      0 s d REMAIN-AT !
      1+
   repeat drop
   0 s KEY-N!
   s >LEDGER ;

\ LIMIT! sets a dimension's initial limit AND its remaining (a fresh, unspent ceiling). Building
\ a ledger from a capability grant's ceiling reads CAPTOK:BUDGET@ into these per-dimension limits.
: LIMIT! ( ledger BUDGET:dim n -- ) {: l:ledger dim:BUDGET:dim v:n :}
   l RANGE-CK dim BUDGET:DIM>N {: s:n d:n :}
   v s d LIMIT-AT !
   v s d REMAIN-AT ! ;

: LIMIT@ ( ledger BUDGET:dim -- n ) {: l:ledger dim:BUDGET:dim :}
   l RANGE-CK dim BUDGET:DIM>N LIMIT-AT @ ;
: REMAINING@ ( ledger BUDGET:dim -- n ) {: l:ledger dim:BUDGET:dim :}
   l RANGE-CK dim BUDGET:DIM>N REMAIN-AT @ ;
: KEY-COUNT ( ledger -- n )   RANGE-CK KEY-N@ ;

\ ---- the reserve/charge request scratch ----------------------------------------
\ REQ-CLEAR resets the pending request; REQ+ adds an amount to a dimension. A commit composer
\ folds a transaction's declared budget into this scratch, then RESERVEs and CHARGEs against it.
: REQ-CLEAR ( -- )
   0 begin dup BUDGET:DIM-COUNT < while
      dup cells REQ + 0 swap !
      1+
   repeat drop ;
: REQ+ ( BUDGET:dim n -- ) {: dim:BUDGET:dim v:n :}
   dim BUDGET:DIM>N cells REQ +  dup @ v + swap ! ;
: REQ@ ( BUDGET:dim -- n )   BUDGET:DIM>N cells REQ + @ ;

private

\ First dimension whose pending request exceeds the ledger's remaining, or -1 if all fit.
: FIT-ESCAPE-DIM ( n -- n ) {: s:n :}
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      d cells REQ + @  s d REMAIN-AT @  > if drop d exit then
      1+
   repeat drop -1 ;

\ Deduct the pending request from every dimension's remaining (assumes a prior fit check).
: APPLY-REQ ( n -- ) {: s:n :}
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}
      s d REMAIN-AT  dup @  d cells REQ + @  -  swap !
      1+
   repeat drop ;

\ Record the 32-byte key at p into the ledger's charged-key set (fail-closed at capacity).
: KEY-RECORD ( n ptr u8 -- ) {: s:n p:ptr :}
   s KEY-N@ {: k:n :}
   k MAX-KEYS >= if E-LEDGER-KEYS throw then
   p  s k KEY-AT  KEY-W BYTE-COPY
   k 1+ s KEY-N! ;

public

\ RESERVE is a pure fit check: `ok` if the pending request fits the remaining budget on every
\ dimension, else `exhausted` naming the first dimension that does not fit (no mutation).
: RESERVE ( ledger -- budget-result )
   RANGE-CK FIT-ESCAPE-DIM {: bad:n :}
   bad 0 >= if bad BUDGET:N>DIM BR-EXHAUSTED exit then
   BR-OK ;

\ CHARGE deducts the pending request atomically and IDEMPOTENTLY, keyed by the 32 bytes at `p`.
\ An already-charged key is a no-op `ok` (a retry never double charges). A fresh key re-checks
\ fit (`exhausted` naming the dimension if it no longer fits), then deducts and records the key.
: CHARGE ( ledger ptr u8 -- budget-result ) {: l:ledger p:ptr :}
   l RANGE-CK {: s:n :}
   s p KEY-FIND 0 >= if BR-OK exit then          \ already charged -> idempotent no-op
   s FIT-ESCAPE-DIM {: bad:n :}
   bad 0 >= if bad BUDGET:N>DIM BR-EXHAUSTED exit then
   s APPLY-REQ
   s p KEY-RECORD
   BR-OK ;

\ CHARGED? reports whether the 32 bytes at `p` have already been charged on this ledger.
: CHARGED? ( ledger ptr u8 -- bool ) {: l:ledger p:ptr :}
   l RANGE-CK p KEY-FIND 0 >= ;

private

\ ---- canonical state emit (into EBUF) ------------------------------------------
: E-RESET ( -- )   0 EO ! ;
: E-ROOM ( n -- ) {: k:n :}   EO @ k + ENC-CAP > if E-LEDGER-BUF throw then ;
: E-LE ( n n -- ) {: v:n w:n :}   w E-ROOM  v EBUF EO @ + w LE-PUT  EO @ w + EO ! ;
: E-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}   u E-ROOM  a EBUF EO @ + u BYTE-COPY  EO @ u + EO ! ;

\ Sort KORD (0..key-n) so KORD[i] indexes charged keys in byte-ascending order (insertion sort).
variable KSI variable KSJ variable KSK
: KORD-BUILD ( n -- ) {: s:n :}
   s KEY-N@ {: n:n :}
   0 begin dup n < while dup dup cells KORD + ! 1+ repeat drop
   1 KSI !
   begin KSI @ n < while
      KSI @ cells KORD + @ KSK !
      KSI @ KSJ !
      begin KSJ @ 0 > if s KSK @ KEY-AT  s KSJ @ 1- cells KORD + @ KEY-AT  KEY< else false then while
         KSJ @ 1- cells KORD + @  KSJ @ cells KORD + !
         KSJ @ 1- KSJ !
      repeat
      KSK @ KSJ @ cells KORD + !
      KSI @ 1+ KSI !
   repeat ;

: EMIT-STATE ( n -- ) {: s:n :}                        \ limit vector, remaining vector, sorted key set
   E-RESET
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}   s d LIMIT-AT @ U64W E-LE   1+
   repeat drop
   0 begin dup BUDGET:DIM-COUNT < while
      dup {: d:n :}   s d REMAIN-AT @ U64W E-LE   1+
   repeat drop
   s KEY-N@ U64W E-LE
   s KORD-BUILD
   0 begin dup s KEY-N@ < while
      dup {: i:n :}
      s  i cells KORD + @  KEY-AT KEY-W E-BYTES
      1+
   repeat drop ;

public

\ DIGEST writes the 32-byte SHA-256 over the canonical ledger state into the caller buffer and
\ returns DIGEST-BYTES. Equal state (any charge ORDER) digests identically - the replay property.
: DIGEST ( ledger ptr u8 n -- n ) {: l:ledger out:ptr cap:n :}
   cap DIGEST-BYTES < if E-LEDGER-BUF throw then
   l RANGE-CK EMIT-STATE
   EBUF EO @ out SHA256
   DIGEST-BYTES ;

: COUNT ( -- n )   L-N @ ;

\ RESET clears the whole ledger pool (a durable store is a later dot; used by the acceptance tests).
: RESET ( -- )   0 L-N ! ;

;package
