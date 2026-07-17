\ maki/db/commit-store.f - the crash-safe transaction commit slice over a minimal
\ file-backed object store (MODEL-CAD-V2-PLAN.md § 23 "Deterministic transactions":
\ "Commit atomically publishes objects, dependency edges, revision, and audit event.
\ Crash injection before the commit marker publishes nothing; recovery either observes
\ the old revision or the complete new revision"; V2-2 exit "crash injection cannot
\ publish partial state"; dot habu-v2-atomic-txn-a3c26066).
\
\ CONCERN: durable, crash-safe PUBLICATION of a validated transaction as a revision. This
\ is a DIFFERENT concern from maki/db/transaction.f (the immutable transaction VALUE +
\ canonical codec) and from maki/store.f (the schedule/evidence row store keyed by the
\ section 7.4 schedule key): the general V2 content-addressed object store does not exist
\ yet, so this file lands the MINIMAL file-backed store the commit slice needs - one file
\ per revision + one head marker + one idempotency record - and nothing more. Package CSTORE.
\
\ ON-DISK LAYOUT (under the store root; regenerable, gitignored, never committed):
\   <root>/revs/<revhex>       the canonical revision content bytes (base + write set,
\                              TX:ENCODE-REV), content-addressed: <revhex> is the 64-hex
\                              rendering of the 32-byte REV:KEY>WIRE content key, so
\                              SHA-256(file) == the key. Written temp+rename (idempotent).
\   <root>/HEAD                32 raw bytes: the current head revision content key. Absent
\                              == genesis. This is the COMMIT MARKER: the single atomic
\                              linearization point (temp+rename), so a reader ever sees the
\                              OLD key or the COMPLETE NEW key, never a torn value.
\   <root>/commits/<idemhex>   32 raw bytes: the rev key an idempotency key produced.
\                              Written temp+rename AFTER HEAD, so a retry resolves the
\                              original result across process death.
\
\ COMMIT PROTOCOL (single-writer snapshot isolation). The publish is three ordered
\ temp+rename boundaries: STAGE-REV (write revs/<revhex>), ADVANCE-HEAD (rename HEAD -
\ the marker), WRITE-IDEM (write commits/<idemhex>). Because every object under HEAD is
\ content-addressed and fsynced-by-rename BEFORE HEAD advances, and HEAD advances by one
\ atomic rename, a crash at ANY boundary leaves the store recoverable to exactly the old
\ or the complete new revision:
\   - crash before ADVANCE-HEAD  -> HEAD still names the old rev; any staged revs/<revhex>
\       is a harmless orphan (re-created byte-identically on retry, content-addressed).
\   - crash at/after ADVANCE-HEAD -> HEAD names the new rev, whose revs/<revhex> is already
\       complete (staged first). WRITE-IDEM not yet run is covered by the head==newrev
\       idempotency check, so a retry still returns the original result.
\
\ RECOVER-OLD-OR-COMPLETE-NEW is therefore IMPLICIT: reading HEAD is the recovery. HEAD-IS?
\ / REV-COMPLETE? let a fresh process assert the invariant after a simulated crash.
\
\ COMMIT ORDER OF CHECKS: validate the transaction (compose TX:VALIDATE - duplicate/omitted
\ reject); an existing idempotency record OR head==proposed returns the original result
\ (idempotent retry); a head that is neither the proposed rev nor the transaction's base is
\ a stale-head CONFLICT; otherwise publish.
\
\ FAILPOINT / CRASH-INJECTION SURFACE: the three publish steps STAGE-REV / ADVANCE-HEAD /
\ WRITE-IDEM are PUBLIC and are the injection surface. A crash-test child runs a PREFIX of
\ the real production steps and lets the process die at that boundary (the honest spawn-a-
\ child crash) - there is NO failpoint branch inside COMMIT, so the production commit path
\ is byte-wise knob-free. maki/db/commit-store-crash-test.f drives the cross-process case.
\
\ DURABILITY BOUNDARY (missing capability, dotted): checked Habu exposes atomic rename
\ (RENAME-FILE / ATOMIC-WRITE-FILE) but NO fsync/fdatasync primitive. The rename gives
\ ATOMICITY and correct PROCESS-crash recovery (the page cache survives process death - the
\ exact model the spawn acceptance exercises); it does NOT guarantee power-loss / kernel-
\ crash durability. A native fsync/dir-sync capability is the remaining work (see the dot).
\
\ Typed domain outcomes are the bespoke commit-result sum; throws are reserved for capacity
\ / impossible-state boundaries. maki -> habu only; commit-store owns -5371..-5373.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/rev.f                \ CAD-KIND:rev-id: KEY>WIRE (32-byte content key)
require maki/db/transaction.f     \ TX: VALIDATE / PROPOSE / BASE-REV / ENCODE-REV / IDEM-KEY>WIRE / CAP-MASK@ / BUDGET-AT@
require maki/db/capability.f      \ CAPTOK:grant / AUTHORIZES?: the granted-authority capability gate
require maki/db/budget-ledger.f   \ LEDGER:ledger / REQ+ / RESERVE / CHARGE / CHARGED?: the budget gate
require maki/db/budget-dim.f      \ BUDGET:dim / N>DIM: the shared budget-dimension vocabulary

-5371 constant E-CSTORE-ROOT     \ resolved store root path empty or over the path cap
-5372 constant E-CSTORE-BUF      \ a revision-content blob over the store scratch capacity
-5373 constant E-CSTORE-HEAD     \ durable HEAD present but not the fixed 32-byte marker width

package CSTORE
public

\ Typed commit outcome: `committed` carries the resulting revision id; `conflict` is the
\ stale base/head precondition; the reject arms compose TX:VALIDATE's refusals. A bespoke
\ per-package sum (not result<a,b>).
SUMTYPE commit-result 1
   VARIANT committed a ;VARIANT
   VARIANT conflict ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
;SUMTYPE

\ Typed AUTHORIZED-commit outcome: COMMIT-AUTHORIZED composes the capability + budget gates around
\ COMMIT (dot habu-v2-capability-and-0970a96d, the deferred § 23 "capability and resource budgets
\ cover all effects" legs, plan:3726). `committed` carries the revision id; `conflict`/`duplicate-
\ write`/`omitted-read` mirror COMMIT's refusals (no charge on any of them); `unauthorized` is the
\ granted-authority ⊉ declared-capabilities reject (the ACTION:DISPATCH precedent); `exhausted`
\ names the first budget dimension whose declared reserve exceeds the ledger's remaining. The two
\ new reject arms fire BEFORE any publish, so an unauthorized or exhausted commit publishes nothing.
SUMTYPE auth-result 1
   VARIANT committed a ;VARIANT
   VARIANT conflict ;VARIANT
   VARIANT duplicate-write ;VARIANT
   VARIANT omitted-read ;VARIANT
   VARIANT unauthorized ;VARIANT
   VARIANT exhausted BUDGET:dim ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings ----------------
: R-COMMITTED ( CAD-KIND:rev-id -- commit-result<CAD-KIND:rev-id> )  CSTORE-COMMIT--RESULT:COMMITTED ;
: R-CONFLICT ( -- commit-result<CAD-KIND:rev-id> )                   CSTORE-COMMIT--RESULT:CONFLICT ;
: R-DUP-WRITE ( -- commit-result<CAD-KIND:rev-id> )                  CSTORE-COMMIT--RESULT:DUPLICATE-WRITE ;
: R-OMITTED ( -- commit-result<CAD-KIND:rev-id> )                    CSTORE-COMMIT--RESULT:OMITTED-READ ;

: A-COMMITTED ( CAD-KIND:rev-id -- auth-result<CAD-KIND:rev-id> )  CSTORE-AUTH--RESULT:COMMITTED ;
: A-CONFLICT ( -- auth-result<a> )        CSTORE-AUTH--RESULT:CONFLICT ;
: A-DUP-WRITE ( -- auth-result<a> )       CSTORE-AUTH--RESULT:DUPLICATE-WRITE ;
: A-OMITTED ( -- auth-result<a> )         CSTORE-AUTH--RESULT:OMITTED-READ ;
: A-UNAUTHORIZED ( -- auth-result<a> )    CSTORE-AUTH--RESULT:UNAUTHORIZED ;
: A-EXHAUSTED ( BUDGET:dim -- auth-result<a> )   CSTORE-AUTH--RESULT:EXHAUSTED ;

\ ---- capacities + fixed widths -------------------------------------------------
32 constant KEY-W                        \ rev / idem content-key width (REV:KEY>WIRE / SHA-256)
64 constant HEX-W                        \ hex rendering of a 32-byte key (SHA256>HEX)
$4000 constant REV-CAP                    \ one revision-content blob (base + write set)

\ ---- store buffers (all fixed; only lengths live in variables) -----------------
create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create PATH-BUF FS-PATH-CAP allot   variable PATH-U
create SUB-BUF  FS-PATH-CAP allot   variable SUB-U
create HEXBUF   HEX-W allot
create KEYBUF   KEY-W allot                \ scratch: a proposed-rev content key
create BASEBUF  KEY-W allot                \ scratch: a base-rev content key
create HEADBUF  KEY-W allot                \ durable HEAD bytes, read back
create IDEMBUF  KEY-W allot                \ idem digest / content-hash scratch
create AKEY     KEY-W allot                \ authorized-commit idempotency key (CHARGED? / CHARGE key)
create REVBUF   REV-CAP allot              \ revision content blob
variable ROOT-SET                          \ -1 once the root is pinned/resolved

\ ---- store root: HABU_CAD_TXN_STORE or a private default -----------------------
: DEFAULT$ ( -- ptr u8 n )   s" tmp/cad-txn-store" ;

: ENV$ ( -- ptr u8 n )
   s" HABU_CAD_TXN_STORE" GETENV dup 0 > if exit then
   2drop DEFAULT$ ;

: ROOT-SET! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u FS-PATH-CAP > or if E-CSTORE-ROOT throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !  true ROOT-SET ! ;

: ROOT-RESOLVE ( -- )
   ROOT-SET @ if exit then
   ENV$ ROOT-SET! ;

public
\ ROOT! pins the store root (a test / crash-child driver points it at a private dir).
: ROOT! ( ptr u8 n -- )   ROOT-SET! ;

\ ROOT$ resolves (env or default, once) and returns the store root path.
: ROOT$ ( -- ptr u8 n )   ROOT-RESOLVE ROOT-BUF ROOT-U @ ;

private
\ ---- path builders (distinct buffers so one path survives building the next) ---
: SUB$ ( ptr u8 n -- ptr u8 n )                  \ <root>/<name> -> SUB-BUF
   {: a:ptr u:n :}
   ROOT$ a u SUB-BUF JOIN-PATH SUB-U !
   SUB-BUF SUB-U @ ;

: HEAD-PATH$ ( -- ptr u8 n )   s" HEAD" SUB$ ;

: KEY-PATH$ ( ptr u8 n ptr u8 -- ptr u8 n )      \ <root>/<dir>/<hex(key)> -> PATH-BUF
   {: da:ptr du:n kp:ptr :}
   kp HEXBUF SHA256>HEX                           \ 32-byte key -> 64 hex chars
   da du HEXBUF HEX-W PATH-BUF JOIN-PATH PATH-U !
   PATH-BUF PATH-U @ ;

: REV-PATH$ ( ptr u8 -- ptr u8 n )               \ <root>/revs/<revhex>  (key at ptr)
   {: kp:ptr :}   s" revs" SUB$ kp KEY-PATH$ ;

: IDEM-PATH$ ( ptr u8 -- ptr u8 n )              \ <root>/commits/<idemhex>  (key at ptr)
   {: kp:ptr :}   s" commits" SUB$ kp KEY-PATH$ ;

: ENSURE-DIRS ( -- )
   ROOT$ MAKE-DIRS
   s" revs" SUB$ MAKE-DIRS
   s" commits" SUB$ MAKE-DIRS ;

\ ---- fixed 32-byte content-key helpers -----------------------------------------
: REV-KEY! ( CAD-KIND:rev-id ptr u8 -- ) {: r:CAD-KIND:rev-id dst:ptr :}
   r dst KEY-W REV:KEY>WIRE drop ;

: KEY= ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup KEY-W < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- durable HEAD read ---------------------------------------------------------
: HEAD-READ ( -- bool )                          \ read 32B into HEADBUF; true if present
   HEAD-PATH$ {: pa:ptr pu:n :}
   pa pu FILE? 0= if false exit then
   pa pu FILE-SIZE KEY-W <> if E-CSTORE-HEAD throw then
   pa pu HEADBUF KEY-W READ-ALL KEY-W <> if E-CSTORE-HEAD throw then
   true ;

public

\ ---- test-only root lifecycle --------------------------------------------------
\ ROOT+ resolves and CREATES the store root + sub-dirs, returning the root path.
: ROOT+ ( -- ptr u8 n )   ENSURE-DIRS ROOT$ ;

\ RESET removes the whole store tree (test-only); a missing store is a no-op.
: RESET ( -- )
   ROOT$ {: a:ptr u:n :}
   u 0 > if a u EXISTS? if a u REMOVE-TREE then then ;

\ HEAD? is true once a durable head marker exists (past genesis).
: HEAD? ( -- bool )   HEAD-PATH$ FILE? ;

\ ---- the three ordered publish steps (also the crash-injection surface) ---------
\ STAGE-REV writes the revision content to revs/<revhex> atomically and returns the
\ proposed revision id. Content-addressed, so re-running it after a crash is idempotent.
: STAGE-REV ( txn -- CAD-KIND:rev-id ) {: t:txn :}
   ENSURE-DIRS
   t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   r KEYBUF REV-KEY!
   KEYBUF REV-PATH$ {: pa:ptr pu:n :}
   t REVBUF REV-CAP TX:ENCODE-REV {: len:n :}
   pa pu REVBUF len ATOMIC-WRITE-FILE
   r ;

\ ADVANCE-HEAD renames HEAD to the revision's content key: the single atomic commit-marker
\ linearization point. INIT-HEAD is the same operation naming the genesis/base revision.
: ADVANCE-HEAD ( CAD-KIND:rev-id -- ) {: r:CAD-KIND:rev-id :}
   ENSURE-DIRS
   r KEYBUF REV-KEY!
   HEAD-PATH$ KEYBUF KEY-W ATOMIC-WRITE-FILE ;

: INIT-HEAD ( CAD-KIND:rev-id -- )   ADVANCE-HEAD ;

\ WRITE-IDEM records commits/<idemhex> -> rev key (temp+rename), AFTER the head advance.
: WRITE-IDEM ( txn CAD-KIND:rev-id -- ) {: t:txn r:CAD-KIND:rev-id :}
   ENSURE-DIRS
   r KEYBUF REV-KEY!
   t IDEMBUF KEY-W TX:IDEM-KEY>WIRE drop
   IDEMBUF IDEM-PATH$ {: pa:ptr pu:n :}
   pa pu KEYBUF KEY-W ATOMIC-WRITE-FILE ;

\ ---- recovery inspection (a fresh process asserts old-or-complete-new) ----------
\ HEAD-IS? is true when the durable head marker equals this revision's content key.
: HEAD-IS? ( CAD-KIND:rev-id -- bool ) {: r:CAD-KIND:rev-id :}
   HEAD-READ 0= if false exit then
   r BASEBUF REV-KEY!
   HEADBUF BASEBUF KEY= ;

\ REV-PRESENT? is true when revs/<revhex> exists on disk.
: REV-PRESENT? ( CAD-KIND:rev-id -- bool ) {: r:CAD-KIND:rev-id :}
   r KEYBUF REV-KEY!
   KEYBUF REV-PATH$ FILE? ;

\ REV-COMPLETE? is true when revs/<revhex> exists AND its bytes hash back to the key: a
\ complete, integral revision object (never a half-written body under a live head). The
\ content key is REV:KEY>WIRE = SHA-256(SHA-256(revision content)) because TX:PROPOSE
\ interns the revision by the DIGEST of its content (§ 23.9), so integrity re-derives the
\ key by hashing the file's bytes (-> the content digest) and hashing that (-> the key).
: REV-COMPLETE? ( CAD-KIND:rev-id -- bool ) {: r:CAD-KIND:rev-id :}
   r REV-PRESENT? 0= if false exit then
   r KEYBUF REV-KEY!                             \ rev content key -> KEYBUF (also the path)
   KEYBUF REV-PATH$ {: pa:ptr pu:n :}
   pa pu FILE-SIZE REV-CAP > if E-CSTORE-BUF throw then
   pa pu REVBUF REV-CAP READ-ALL {: got:n :}
   REVBUF got IDEMBUF SHA256                      \ IDEMBUF = SHA-256(content)  (content digest)
   IDEMBUF KEY-W BASEBUF SHA256                   \ BASEBUF = SHA-256(digest)   (the content key)
   KEYBUF BASEBUF KEY= ;

private
\ ---- transaction validity as an ordinal (0 ok / 1 dup / 2 omitted) --------------
: V-CODE ( txn -- n )
   TX:VALIDATE MATCH TX:tx-result
      ok OF drop 0 ENDOF
      duplicate-write OF 1 ENDOF
      omitted-read OF 2 ENDOF
      malformed OF 3 ENDOF
      bounds OF 4 ENDOF
   ;MATCH ;

public

\ COMMIT is the crash-safe publish: validate, honour idempotency (durable record or an
\ already-equal head), reject a stale base/head, else stage the revision, advance the head
\ marker, and record the idempotency key - returning the typed commit outcome.
: COMMIT ( txn -- commit-result<CAD-KIND:rev-id> ) {: t:txn :}
   t V-CODE {: v:n :}
   v 1 = if R-DUP-WRITE exit then
   v 2 = if R-OMITTED exit then
   t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   r KEYBUF REV-KEY!                              \ proposed-rev key -> KEYBUF
   t IDEMBUF KEY-W TX:IDEM-KEY>WIRE drop
   IDEMBUF IDEM-PATH$ FILE? if r R-COMMITTED exit then   \ durable idempotent retry
   HEAD-READ if
      HEADBUF KEYBUF KEY= if r R-COMMITTED exit then     \ already head (content idempotent)
      t TX:BASE-REV BASEBUF REV-KEY!
      HEADBUF BASEBUF KEY= 0= if R-CONFLICT exit then    \ stale head
   then
   t STAGE-REV drop
   r ADVANCE-HEAD
   t r WRITE-IDEM
   r R-COMMITTED ;

private

\ ---- capability + budget composition helpers (the deferred COMMIT gate legs) -----
\ FOLD-BUDGET folds a transaction's declared budget-ledger entries into the LEDGER request
\ scratch: each declared (dimension code, amount) resolves to its BUDGET:dim (fail-closed on an
\ out-of-vocabulary code) and is added, so the ledger request equals the transaction's reserve.
: FOLD-BUDGET ( txn -- ) {: t:txn :}
   LEDGER:REQ-CLEAR
   0 begin dup t TX:BUDGET-COUNT < while
      dup {: k:n :}
      t k TX:BUDGET-AT@ {: dimcode:n amt:n :}
      dimcode BUDGET:N>DIM amt LEDGER:REQ+
      1+
   repeat drop ;

\ RESERVE-DIM: -1 if the pending request fits the ledger, else the exhausted dimension ordinal.
: RESERVE-DIM ( LEDGER:ledger -- n )
   LEDGER:RESERVE MATCH LEDGER:budget-result
      ok        OF -1 ENDOF
      exhausted OF BUDGET:DIM>N ENDOF
   ;MATCH ;

public

\ COMMIT-AUTHORIZED is the CAPABILITY + BUDGET gated commit (the § 23 "capability and resource
\ budgets cover all effects" commit legs, plan:3726; dot habu-v2-capability-and-0970a96d). It
\ (1) rejects `unauthorized` when the granted authority does not contain the transaction's declared
\ capability SET (mask containment - the ACTION:DISPATCH precedent); (2) rejects `exhausted`, naming
\ the dimension, when the declared budget reserve does not fit the ledger's remaining - BOTH gates
\ fire BEFORE any publish, so a rejected authorized commit leaves HEAD unchanged and charges nothing
\ (composes with the crash-injection acceptance: an exhausted commit is state-identical to a crash
\ before the marker). It then delegates to the crash-safe COMMIT and, on a FRESH publish, CHARGES
\ the ledger exactly once keyed by the transaction's idempotency key; a retry (COMMIT resolves it
\ idempotently WITHOUT republishing, and the key is already charged) charges nothing more, and a
\ stale-head / duplicate-write / omitted-read COMMIT reject charges nothing. Obligation-discharge
\ authority stays DEFERRED: no verifier-authority model (who may discharge which obligation) is
\ landed, so this gate enforces capability + budget only (see the dot's report + follow-on dot).
: COMMIT-AUTHORIZED ( txn CAPTOK:grant LEDGER:ledger -- auth-result<CAD-KIND:rev-id> )
   {: t:txn g:CAPTOK:grant l:LEDGER:ledger :}
   g t TX:CAP-MASK@ CAPTOK:AUTHORIZES? 0= if A-UNAUTHORIZED exit then
   t FOLD-BUDGET
   t AKEY KEY-W TX:IDEM-KEY>WIRE drop
   l AKEY LEDGER:CHARGED? {: retry:bool :}
   retry 0= if
      l RESERVE-DIM {: bad:n :}
      bad 0 >= if bad BUDGET:N>DIM A-EXHAUSTED exit then
   then
   t COMMIT MATCH commit-result
      committed OF                                   \ rev on the stack; charge once on a fresh publish
                   retry 0= if l AKEY LEDGER:CHARGE drop then
                   A-COMMITTED ENDOF
      conflict        OF A-CONFLICT ENDOF
      duplicate-write OF A-DUP-WRITE ENDOF
      omitted-read    OF A-OMITTED ENDOF
   ;MATCH ;

;package
