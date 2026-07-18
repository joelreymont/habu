\ maki/db/audit-log.f - the canonical, content-chained audit EVENT log + deterministic
\ replay (MODEL-CAD-V2-PLAN.md § 23.9 "append-only audit records and deterministic
\ replay"; dot habu-v2-deterministic-audit-428d27c2).
\
\ CONCERN: an append-only log of TYPED audit EVENTS, each wired to the landed identity
\ families by their CROSS-PROCESS content keys, CONTENT-CHAINED so tamper / reorder /
\ omission fall out of one chain verification, plus a BYTE-STABLE replay that reproduces
\ the recorded revision / artifact / evidence / state digests WITHOUT any chooser/LLM.
\ Package AUDIT. This is a DIFFERENT concern from maki/journal.f (package JOURNAL, the raw
\ append-only audit-event-id OCCURRENCE journal that promotion.f uses for its closure
\ descriptors): a promotion closure is a lone occurrence record, whereas this log is a
\ HASH-CHAINED sequence whose integrity depends on an uninterleaved order, so it owns its
\ own self-contained store rather than sharing JOURNAL's (which promotion also appends to).
\
\ ---- THE EVENT (a fixed-width canonical record) --------------------------------------
\ Eight typed event KINDS (§ 23.9): action-request, action-result, txn-commit, verifier-run,
\ evidence-decision, promotion, activation, rollback. Every event is one fixed REC-W-byte
\ record so the wire form is length-free and byte-identical across processes:
\   kind(1) det(1) aux(1) k1(32) k2(32) kcap(32) prev(32)
\ where k1/k2 are the event's primary/secondary landed-identity CONTENT KEYS (REV:KEY>WIRE,
\ TX:IDEM-KEY>WIRE, ARTIFACT:KEY>WIRE, EVIDENCE:KEY>WIRE, PRODUCER:KEY>WIRE - all cross-
\ process content keys, never process-local raws), kcap is the CAPTURED-OUTPUT evidence key
\ of a NONDETERMINISTIC event (zero for a deterministic one), aux is a small ordinal (an
\ evidence-decision outcome), det marks a nondeterministic event, and prev is the SHA-256
\ hash of the PREVIOUS event's record (32 zero bytes at genesis).
\
\ ---- CONTENT CHAIN (acceptance a: omission / reorder / tamper each REJECT) ------------
\ Each event embeds prev = hash(previous record); the serialized log ends with the HEAD =
\ hash(last record). VERIFY-LOG walks the records recomputing running = hash(record i) and
\ checking record[i].prev == running-so-far, then running == HEAD at the end. Any single-
\ event TAMPER changes hash(i) so record[i+1].prev mismatches (or, for the last event, the
\ HEAD mismatches); a REORDER breaks the prev links; an OMISSION leaves the following event
\ pointing at a hash no longer produced. All three are typed rejects (broken-chain i /
\ bad-head), never a silent accept.
\
\ ---- NONDETERMINISM (acceptance b: marked + captured, the diff-runner precedent) -------
\ A nondeterministic action (an external-process verifier run, the maki/db/diff-runner.f
\ captured-evidence precedent) MUST be MARKED (det=1) and carry a captured-output evidence
\ key (kcap != 0). The typed API enforces the pairing: the ONLY marked-event constructor,
\ RECORD-VERIFIER-RUN-CAPTURED, demands the captured evidence-id as a mandatory argument, so a
\ marked event cannot be recorded without a real capture; and VERIFY-LOG independently rejects
\ a marked record whose kcap was stripped to zero (bad-nondeterministic). REPLAY folds the
\ CAPTURED key for a nondeterministic
\ event and the primary key for a deterministic one, so a marked event REPLAYS from its
\ captured output, never by re-execution - a re-run that produced a different live output does
\ not change the replayed state digest.
\
\ ---- REPLAY (acceptance: reproduce digests WITHOUT a chooser; byte-stable across processes)
\ STATE-DIGEST is a PURE FUNCTION of the serialized log bytes: a rolling SHA-256 folding each
\ event's (kind, replayed content key, secondary key). It invokes NO chooser/LLM - it folds
\ the recorded cross-process content keys - so replaying the same log in ANY process (even one
\ with a decoy-shifted registry) reproduces the identical state digest. Composed with the
\ commit store's determinism (re-committing a recorded txn reproduces the same rev-id) and the
\ promotion closure's re-verification, the log's txn-commit / promotion events reproduce the
\ live run's revision / evidence / state digests from an empty store.
\
\ Typed domain outcomes are the bespoke verify-result sum (never value+flag); throws are
\ reserved for capacity / buffer boundaries. maki -> habu only; audit-log owns -5614..-5615.

require lib/prelude.f
require lib/string.f
require maki/cad-kinds.f
require maki/artifact.f            \ CAD-KIND:artifact-id KEY>WIRE (subject content key)
require maki/producer.f           \ CAD-KIND:producer-id KEY>WIRE (verifier content key)
require maki/rev.f                 \ CAD-KIND:rev-id KEY>WIRE (revision content key)
require maki/db/evidence.f        \ CAD-KIND:evidence-id KEY>WIRE (evidence / captured-output content key)
require maki/db/transaction.f     \ TX:txn IDEM-KEY>WIRE (transaction content key)

-5614 constant E-AUDIT-CAP        \ audit log over its per-run event cap / an event index out of range
-5615 constant E-AUDIT-BUF        \ ENCODE-LOG output buffer / STATE-DIGEST input smaller than required

package AUDIT
public

\ ---- the eight typed event kinds (a closed, variant-exhaustive ENUM) -------------
ENUM event-kind
   action-request action-result txn-commit verifier-run
   evidence-decision promotion activation rollback
;ENUM

\ ---- chain verification outcome (the § 23.9 art-result custom-sum idiom) ----------
\ ok = the whole chain + head verify; malformed = a wrong-sized or over-cap serialized log;
\ broken-chain carries the first index whose prev-hash mismatched (tamper/reorder/omission);
\ bad-head = the last record's hash != the declared head; bad-nondeterministic carries the
\ index of a marked event missing its captured-output key.
SUMTYPE verify-result 0
   VARIANT ok ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT broken-chain n ;VARIANT
   VARIANT bad-head ;VARIANT
   VARIANT bad-nondeterministic n ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings -------------------
: VR-OK ( -- verify-result )              AUDIT-VERIFY--RESULT:OK ;
: VR-MALFORMED ( -- verify-result )       AUDIT-VERIFY--RESULT:MALFORMED ;
: VR-BROKEN ( n -- verify-result )        AUDIT-VERIFY--RESULT:BROKEN-CHAIN ;
: VR-BAD-HEAD ( -- verify-result )        AUDIT-VERIFY--RESULT:BAD-HEAD ;
: VR-BAD-NONDET ( n -- verify-result )    AUDIT-VERIFY--RESULT:BAD-NONDETERMINISTIC ;

\ ---- event-kind <-> byte ordinal (stable wire; the inverse fails closed) ----------
: KIND>N ( event-kind -- n )
   MATCH event-kind
      action-request    OF 0 ENDOF
      action-result     OF 1 ENDOF
      txn-commit        OF 2 ENDOF
      verifier-run      OF 3 ENDOF
      evidence-decision OF 4 ENDOF
      promotion         OF 5 ENDOF
      activation        OF 6 ENDOF
      rollback          OF 7 ENDOF
   ;MATCH ;

: N>KIND ( n -- event-kind )
   case
      0 of AUDIT-EVENT--KIND:ACTION-REQUEST    endof
      1 of AUDIT-EVENT--KIND:ACTION-RESULT     endof
      2 of AUDIT-EVENT--KIND:TXN-COMMIT        endof
      3 of AUDIT-EVENT--KIND:VERIFIER-RUN      endof
      4 of AUDIT-EVENT--KIND:EVIDENCE-DECISION endof
      5 of AUDIT-EVENT--KIND:PROMOTION         endof
      6 of AUDIT-EVENT--KIND:ACTIVATION        endof
      7 of AUDIT-EVENT--KIND:ROLLBACK          endof
      E-AUDIT-CAP throw
   endcase ;

\ ---- kind byte constants (the record's stored ordinal) ----------------------------
0 constant K-ACTION-REQUEST
1 constant K-ACTION-RESULT
2 constant K-TXN-COMMIT
3 constant K-VERIFIER-RUN
4 constant K-EVIDENCE-DECISION
5 constant K-PROMOTION
6 constant K-ACTIVATION
7 constant K-ROLLBACK

\ ---- fixed record layout ----------------------------------------------------------
32 constant HASH-W                 \ SHA-256 hash / content-key width
4  constant U32W                    \ serialized count width
0  constant OFF-KIND
1  constant OFF-DET
2  constant OFF-AUX
3  constant OFF-K1
35 constant OFF-K2
67 constant OFF-KCAP
99 constant OFF-PREV
131 constant REC-W                   \ 3 header bytes + four 32-byte fields

256 constant LOG-CAP                 \ events per run (first-slice bounded)

\ ---- self-contained chained store -------------------------------------------------
create LOG-BYTES LOG-CAP REC-W * allot     \ the fixed records, back to back
create LOG-HASH  LOG-CAP HASH-W * allot     \ per-event record hash (the chain link)
variable LOG-N                               \ appended event count
create ZKEY   HASH-W allot                    \ 32 zero bytes (genesis prev + deterministic kcap)
create KB1    HASH-W allot                    \ scratch: primary content key
create KB2    HASH-W allot                    \ scratch: secondary content key
create KBCAP  HASH-W allot                    \ scratch: captured-output content key

\ ---- fixed little-endian scalar codec (the maki/journal.f pattern) ----------------
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

\ ---- fixed 32-byte helpers --------------------------------------------------------
: BYTES=? ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: ZERO-KEY? ( ptr u8 -- bool )   ZKEY HASH-W BYTES=? ;

: ZERO-FILL ( ptr u8 n -- ) {: a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      0 a k + c!
      1+
   repeat drop ;

: FLAG>BYTE ( bool -- n )   if 1 else 0 then ;

\ ---- record / hash slot addressing ------------------------------------------------
: REC-AT ( n -- ptr u8 )   REC-W * LOG-BYTES + ;
: HASH-AT ( n -- ptr u8 )  HASH-W * LOG-HASH + ;

\ PREV-FILL copies the previous event's chain hash (or the genesis zeros) into dst.
: PREV-FILL ( ptr u8 -- ) {: dst:ptr :}
   LOG-N @ 0= if ZKEY dst HASH-W BYTE-COPY exit then
   LOG-N @ 1- HASH-AT  dst  HASH-W BYTE-COPY ;

\ ---- the append core: write one record, link + hash it ----------------------------
\ det marks a nondeterministic event, which MUST carry a captured-output key (kcap != 0);
\ a deterministic event passes ZKEY as kcap. k1/k2/kcap point at 32-byte content-key buffers.
: APPEND-EVENT ( n bool n ptr u8 ptr u8 ptr u8 -- )
   {: kindb:n det:bool aux:n k1:ptr k2:ptr kcap:ptr :}
   LOG-N @ LOG-CAP >= if E-AUDIT-CAP throw then
   LOG-N @ REC-AT {: r:ptr :}
   kindb          r OFF-KIND +  c!
   det FLAG>BYTE  r OFF-DET +   c!
   aux            r OFF-AUX +   c!
   k1    r OFF-K1 +    HASH-W BYTE-COPY
   k2    r OFF-K2 +    HASH-W BYTE-COPY
   kcap  r OFF-KCAP +  HASH-W BYTE-COPY
   r OFF-PREV +  PREV-FILL
   r REC-W  LOG-N @ HASH-AT  SHA256
   LOG-N @ 1+ LOG-N ! ;

public

\ ---- the typed RECORD surface (wired to the landed identity families) -------------
\ Each helper renders its identities to cross-process content keys and appends a
\ deterministic event, except the nondeterministic verifier run which carries captured output.

: RECORD-ACTION-REQUEST ( TX:txn -- ) {: t:TX:txn :}
   t KB1 HASH-W TX:IDEM-KEY>WIRE drop
   K-ACTION-REQUEST false 0 KB1 ZKEY ZKEY APPEND-EVENT ;

: RECORD-ACTION-RESULT ( TX:txn CAD-KIND:rev-id -- ) {: t:TX:txn r:CAD-KIND:rev-id :}
   t KB1 HASH-W TX:IDEM-KEY>WIRE drop
   r KB2 HASH-W REV:KEY>WIRE drop
   K-ACTION-RESULT false 0 KB1 KB2 ZKEY APPEND-EVENT ;

: RECORD-COMMIT ( TX:txn CAD-KIND:rev-id -- ) {: t:TX:txn r:CAD-KIND:rev-id :}
   r KB1 HASH-W REV:KEY>WIRE drop
   t KB2 HASH-W TX:IDEM-KEY>WIRE drop
   K-TXN-COMMIT false 0 KB1 KB2 ZKEY APPEND-EVENT ;

: RECORD-VERIFIER-RUN ( CAD-KIND:evidence-id -- ) {: ev:CAD-KIND:evidence-id :}
   ev KB1 HASH-W EVIDENCE:KEY>WIRE drop
   K-VERIFIER-RUN false 0 KB1 ZKEY ZKEY APPEND-EVENT ;

\ A NONDETERMINISTIC verifier run: the run evidence + the CAPTURED-OUTPUT evidence
\ (E-AUDIT-UNMARKED at record time if the captured key resolves to zero).
: RECORD-VERIFIER-RUN-CAPTURED ( CAD-KIND:evidence-id CAD-KIND:evidence-id -- )
   {: ev:CAD-KIND:evidence-id cap:CAD-KIND:evidence-id :}
   ev  KB1   HASH-W EVIDENCE:KEY>WIRE drop
   cap KBCAP HASH-W EVIDENCE:KEY>WIRE drop
   K-VERIFIER-RUN true 0 KB1 ZKEY KBCAP APPEND-EVENT ;

\ An evidence discharge decision: the subject + verifier identities + an outcome ordinal.
: RECORD-EVIDENCE-DECISION ( CAD-KIND:artifact-id CAD-KIND:producer-id n -- )
   {: subj:CAD-KIND:artifact-id verif:CAD-KIND:producer-id outcome:n :}
   subj  KB1 HASH-W ARTIFACT:KEY>WIRE drop
   verif KB2 HASH-W PRODUCER:KEY>WIRE drop
   K-EVIDENCE-DECISION false outcome KB1 KB2 ZKEY APPEND-EVENT ;

: RECORD-PROMOTION ( CAD-KIND:artifact-id CAD-KIND:evidence-id -- )
   {: model:CAD-KIND:artifact-id ev:CAD-KIND:evidence-id :}
   model KB1 HASH-W ARTIFACT:KEY>WIRE drop
   ev    KB2 HASH-W EVIDENCE:KEY>WIRE drop
   K-PROMOTION false 0 KB1 KB2 ZKEY APPEND-EVENT ;

: RECORD-ACTIVATION ( CAD-KIND:rev-id -- ) {: r:CAD-KIND:rev-id :}
   r KB1 HASH-W REV:KEY>WIRE drop
   K-ACTIVATION false 0 KB1 ZKEY ZKEY APPEND-EVENT ;

: RECORD-ROLLBACK ( CAD-KIND:rev-id CAD-KIND:rev-id -- ) {: to:CAD-KIND:rev-id from:CAD-KIND:rev-id :}
   to   KB1 HASH-W REV:KEY>WIRE drop
   from KB2 HASH-W REV:KEY>WIRE drop
   K-ROLLBACK false 0 KB1 KB2 ZKEY APPEND-EVENT ;

\ ---- read surface (in-process inspection of the internal log) ---------------------
: COUNT ( -- n )   LOG-N @ ;

: EVENT-KIND@ ( n -- event-kind ) {: k:n :}
   k 0 < k LOG-N @ >= or if E-AUDIT-CAP throw then
   k REC-AT OFF-KIND + c@ N>KIND ;

: EVENT-DET@ ( n -- bool ) {: k:n :}
   k 0 < k LOG-N @ >= or if E-AUDIT-CAP throw then
   k REC-AT OFF-DET + c@ 0<> ;

: EVENT-AUX@ ( n -- n ) {: k:n :}
   k 0 < k LOG-N @ >= or if E-AUDIT-CAP throw then
   k REC-AT OFF-AUX + c@ ;

: RESET ( -- )   0 LOG-N ! ;

private

\ ---- serialized log framing: [u32 count][records...][32-byte head] ----------------
: FRAMED-LEN ( n -- n )   REC-W * U32W + HASH-W + ;   \ bytes for `cnt` events
: REC-OFF ( n -- n )      REC-W * U32W + ;             \ byte offset of record k in the frame

public

\ ENCODE-LOG serializes the whole internal log (count, every record, then the HEAD hash of
\ the last record) into a caller buffer and returns the byte length (E-AUDIT-BUF if it cannot
\ hold the frame). The records already embed their prev-links, so the frame is self-verifying.
: ENCODE-LOG ( ptr u8 n -- n ) {: out:ptr cap:n :}
   LOG-N @ {: cnt:n :}
   cnt FRAMED-LEN {: need:n :}
   need cap > if E-AUDIT-BUF throw then
   cnt out U32W LE-PUT
   0 begin dup cnt < while
      dup {: k:n :}
      k REC-AT  out k REC-OFF +  REC-W BYTE-COPY
      1+
   repeat drop
   cnt 0= if ZKEY else cnt 1- HASH-AT then  out cnt REC-OFF +  HASH-W BYTE-COPY
   need ;

private

\ ---- chain-verification walk (over a serialized frame) ----------------------------
create RUNBUF HASH-W allot           \ rolling hash of records seen so far
variable V-ERR                        \ 0 ok / codes
variable V-IDX                        \ broken-chain / bad-nondeterministic index
0 constant VE-OK
1 constant VE-BROKEN
2 constant VE-BAD-NONDET

: VERIFY-STEP ( ptr u8 n -- ) {: a:ptr k:n :}          \ (frame k -- ) fold record k, set V-ERR on failure
   a k REC-OFF + {: rp:ptr :}
   rp OFF-DET + c@ 0<>  rp OFF-KCAP + ZERO-KEY? and if VE-BAD-NONDET V-ERR ! k V-IDX ! exit then
   rp OFF-PREV +  RUNBUF HASH-W BYTES=? 0= if VE-BROKEN V-ERR ! k V-IDX ! exit then
   rp REC-W  RUNBUF  SHA256 ;

: VERIFY-WALK ( ptr u8 n -- ) {: a:ptr cnt:n :}         \ (frame cnt -- )
   ZKEY RUNBUF HASH-W BYTE-COPY
   VE-OK V-ERR !  -1 V-IDX !
   0 begin dup cnt < V-ERR @ VE-OK = and while
      dup {: k:n :}
      a k VERIFY-STEP
      1+
   repeat drop ;

public

\ VERIFY-LOG verifies a serialized log frame: every record's prev-hash links to the running
\ chain, every marked (nondeterministic) record carries a captured-output key, and the last
\ record's hash equals the declared HEAD. Tamper, reorder, and omission each surface as a
\ typed broken-chain / bad-head; a stripped captured key as bad-nondeterministic.
: VERIFY-LOG ( ptr u8 n -- verify-result ) {: a:ptr u:n :}
   u U32W HASH-W + < if VR-MALFORMED exit then
   a U32W LE-GET {: cnt:n :}
   cnt 0 < cnt LOG-CAP > or if VR-MALFORMED exit then
   cnt FRAMED-LEN u <> if VR-MALFORMED exit then
   a cnt VERIFY-WALK
   V-ERR @ VE-BAD-NONDET = if V-IDX @ VR-BAD-NONDET exit then
   V-ERR @ VE-BROKEN     = if V-IDX @ VR-BROKEN exit then
   a cnt REC-OFF +  RUNBUF HASH-W BYTES=? 0= if VR-BAD-HEAD exit then
   VR-OK ;

private

\ ---- replay fold: state[i] = SHA-256( state[i-1] || kind || contrib || k2 ) --------
\ contrib is the CAPTURED key for a nondeterministic event (replay from captured output) and
\ the PRIMARY content key for a deterministic one - so a marked event replays from its capture.
97 constant SD-BLK-W                  \ prev-state(32) || kind(1) || contrib(32) || k2(32)
create SD-RUN HASH-W allot
create SD-BLK SD-BLK-W allot

: SD-CONTRIB ( ptr u8 -- ptr u8 ) {: rp:ptr :}
   rp OFF-DET + c@ 0<> if rp OFF-KCAP + else rp OFF-K1 + then ;

: SD-STEP ( ptr u8 n -- ) {: a:ptr k:n :}              \ (frame k -- ) fold record k into SD-RUN
   a k REC-OFF + {: rp:ptr :}
   SD-RUN            SD-BLK               HASH-W BYTE-COPY
   rp OFF-KIND + c@  SD-BLK HASH-W +      c!
   rp SD-CONTRIB     SD-BLK HASH-W + 1 +           HASH-W BYTE-COPY
   rp OFF-K2 +       SD-BLK HASH-W + 1 + HASH-W +  HASH-W BYTE-COPY
   SD-BLK SD-BLK-W  SD-RUN  SHA256 ;

public

\ STATE-DIGEST is the byte-stable replay: it folds a serialized log frame into a 32-byte state
\ digest, using the CAPTURED key for nondeterministic events and the primary content key for
\ deterministic ones. It invokes no chooser and reads no registry, so the digest is identical
\ in every process that sees the same frame (E-AUDIT-BUF on a truncated frame).
: STATE-DIGEST ( ptr u8 n ptr u8 -- n ) {: a:ptr u:n out:ptr :}
   u U32W HASH-W + < if E-AUDIT-BUF throw then
   a U32W LE-GET {: cnt:n :}
   cnt 0 < cnt LOG-CAP > or if E-AUDIT-BUF throw then
   cnt FRAMED-LEN u <> if E-AUDIT-BUF throw then
   ZKEY SD-RUN HASH-W BYTE-COPY
   0 begin dup cnt < while
      dup {: k:n :}
      a k SD-STEP
      1+
   repeat drop
   SD-RUN out HASH-W BYTE-COPY
   HASH-W ;

private

: AUDIT-INIT ( -- )
   ZKEY HASH-W ZERO-FILL
   0 LOG-N ! ;

AUDIT-INIT
;package
