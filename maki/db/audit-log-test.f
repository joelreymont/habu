\ maki/db/audit-log-test.f - checked acceptance for the canonical content-chained audit
\ event log + deterministic replay (maki/db/audit-log.f, dot habu-v2-deterministic-audit-428d27c2).
\ Proves each dot-acceptance property against the REAL landed identity / commit-store substrate,
\ each by a named test:
\
\   the log + chain (acceptance a: omission / reorder / tamper each REJECT):
\     AU-VERIFY-OK     : a well-formed 6-event log verifies (ok)
\     AU-TAMPER-MID/-IX: flipping a middle record's payload -> broken-chain at the NEXT index
\     AU-TAMPER-LAST   : flipping the last record's payload -> bad-head
\     AU-REORDER/-IX   : swapping two records -> broken-chain at the first divergent index
\     AU-OMIT/-IX      : dropping a middle record -> broken-chain at the gap index
\     AU-MALFORMED     : a wrong-sized frame -> malformed
\   (b) a nondeterministic action must be MARKED + carry captured output:
\     AU-ND-OK / AU-ND-DET : a marked captured verifier run verifies; EVENT-DET@ reports it marked
\     AU-ND-STRIP/-IX  : a marked record whose captured key is zeroed -> bad-nondeterministic
\     AU-ND-CAPTURED   : STATE-DIGEST folds the CAPTURED key - changing the live output key does NOT
\                        change the digest, changing the captured key DOES (replay from capture)
\     AU-CAP-OK/-BAD   : STATIC - a marked verifier run cannot be recorded without a captured id
\   replay reproduces digests WITHOUT a chooser (composing commit-store determinism):
\     AU-REPLAY-STABLE : re-recording the scenario against a fresh store reproduces the same digest
\     AU-ENCODE-STABLE : encoding the same log twice is byte-identical
\   read surface + fail-closed:
\     AU-KIND / AU-AUX : EVENT-KIND@ / EVENT-AUX@ project the recorded fields
\     AU-CAP-OVERFLOW  : one past the event cap -> E-AUDIT-CAP
\     AU-BUF-SMALL     : an ENCODE-LOG buffer smaller than the frame -> E-AUDIT-BUF

require lib/test.f
require lib/fs.f
require test/checker-assert.f
require maki/db/audit-log.f
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/artifact.f
require maki/producer.f
require maki/db/evidence.f
require maki/rev.f

package AUDIT-LOG-TEST

\ One private store dir for the whole suite; the scenario RESETs it first.
s" hb-audit-test" TMPDIR-MKDIR CSTORE:ROOT!

\ ---- wire-layout mirror (the test crafts tampered frames; commit-store-test's KEY-W precedent) ---
4   constant TU32W
131 constant TREC-W
32  constant THASH-W
1   constant TOFF-DET
3   constant TOFF-K1
67  constant TOFF-KCAP
256 constant TLOG-CAP
: REC-OFF ( n -- n )   TREC-W * TU32W + ;      \ byte offset of record k in a frame

create LOGBUF $4000 allot
create TMPBUF $4000 allot
create SW     TREC-W allot                       \ record-swap scratch
create SMALLBUF 8 allot
create DG1 THASH-W allot   create DG2 THASH-W allot   create DG3 THASH-W allot

\ ---- identity fixtures (REGISTER interns by content -> stable ids) --------------
: OBJ-A ( -- CAD-KIND:artifact-id )   s" audit-test/obj-a" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" audit-test/obj-c" ARTIFACT:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )           s" audit-test/genesis" REV:COMMIT ;
: SUBJ ( -- CAD-KIND:artifact-id )    s" audit-test/subj" ARTIFACT:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id )   s" audit-test/verifier" PRODUCER:REGISTER ;
: MODEL ( -- CAD-KIND:artifact-id )   s" audit-test/model" ARTIFACT:REGISTER ;
: EV1 ( -- CAD-KIND:evidence-id )     s" audit-test/ev-run-1" EVIDENCE:REGISTER ;
: EV2 ( -- CAD-KIND:evidence-id )     s" audit-test/ev-policy" EVIDENCE:REGISTER ;

: MK-TXN ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;

: CREV ( commit-result<CAD-KIND:rev-id> -- CAD-KIND:rev-id )
   MATCH CSTORE:commit-result
      committed       OF ENDOF
      conflict        OF G0 ENDOF
      duplicate-write OF G0 ENDOF
      omitted-read    OF G0 ENDOF
   ;MATCH ;

\ ---- the canonical 6-event scenario (records against a REAL committed revision) --
\ 0 action-request | 1 txn-commit | 2 verifier-run | 3 evidence-decision | 4 promotion | 5 activation
: SCENARIO ( -- )
   AUDIT:RESET
   CSTORE:RESET  G0 CSTORE:INIT-HEAD
   MK-TXN {: t:txn :}
   t AUDIT:RECORD-ACTION-REQUEST
   t CSTORE:COMMIT CREV {: r:CAD-KIND:rev-id :}
   t r AUDIT:RECORD-COMMIT
   EV1 AUDIT:RECORD-VERIFIER-RUN
   SUBJ VERIF 0 AUDIT:RECORD-EVIDENCE-DECISION
   MODEL EV2 AUDIT:RECORD-PROMOTION
   r AUDIT:RECORD-ACTIVATION ;

\ A log whose event 1 is a NONDETERMINISTIC captured verifier run (captured = EV2).
: SCENARIO-ND ( -- )
   AUDIT:RESET
   MK-TXN AUDIT:RECORD-ACTION-REQUEST
   EV1 EV2 AUDIT:RECORD-VERIFIER-RUN-CAPTURED
   SUBJ VERIF 0 AUDIT:RECORD-EVIDENCE-DECISION ;

\ ---- verify-result decoders ----------------------------------------------------
: VCODE ( AUDIT:verify-result -- n )   \ 0 ok / 1 malformed / 2 broken / 3 bad-head / 4 bad-nondet
   MATCH AUDIT:verify-result
      ok                   OF 0 ENDOF
      malformed            OF 1 ENDOF
      broken-chain         OF drop 2 ENDOF
      bad-head             OF 3 ENDOF
      bad-nondeterministic OF drop 4 ENDOF
   ;MATCH ;
: VIDX ( AUDIT:verify-result -- n )    \ the broken / bad-nondet index, else -1
   MATCH AUDIT:verify-result
      ok                   OF -1 ENDOF
      malformed            OF -1 ENDOF
      broken-chain         OF ENDOF
      bad-head             OF -1 ENDOF
      bad-nondeterministic OF ENDOF
   ;MATCH ;
: EKN ( AUDIT:event-kind -- n )
   MATCH AUDIT:event-kind
      action-request    OF 0 ENDOF
      action-result     OF 1 ENDOF
      txn-commit        OF 2 ENDOF
      verifier-run      OF 3 ENDOF
      evidence-decision OF 4 ENDOF
      promotion         OF 5 ENDOF
      activation        OF 6 ENDOF
      rollback          OF 7 ENDOF
   ;MATCH ;

\ ---- frame-surgery helpers (byte access on scratch copies) ----------------------
: ENCODE ( -- n )   LOGBUF $4000 AUDIT:ENCODE-LOG ;      \ encode the live log, return frame len
: COPY-FRAME ( n -- )   {: len:n :}   LOGBUF TMPBUF len BYTE-COPY ;
: FLIP ( ptr u8 n -- ) {: a:ptr off:n :}   a off + c@ $FF xor  a off + c! ;
: ZERO-FIELD ( ptr u8 n n -- ) {: a:ptr off:n w:n :}
   0 begin dup w < while  dup {: k:n :}  0 a off + k + c!  1+  repeat drop ;
: TLE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while  dup {: k:n :}  v k 8 * rshift $FF and a k + c!  1+  repeat drop ;
: KEY-EQ? ( ptr u8 ptr u8 -- bool ) {: a:ptr b:ptr :}
   0 begin dup THASH-W < while  dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then  1+  repeat drop true ;
: SAME-BYTES? ( ptr u8 ptr u8 n -- bool ) {: a:ptr b:ptr len:n :}
   0 begin dup len < while  dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then  1+  repeat drop true ;

\ Swap the two REC-W records at positions ai, bi inside TMPBUF (frame already copied there).
: SWAP-RECS ( n n -- ) {: ai:n bi:n :}
   TMPBUF ai REC-OFF +  SW               TREC-W BYTE-COPY
   TMPBUF bi REC-OFF +  TMPBUF ai REC-OFF +  TREC-W BYTE-COPY
   SW  TMPBUF bi REC-OFF +               TREC-W BYTE-COPY ;

\ Build into TMPBUF a frame that DROPS record index 1 (count-1); returns the new frame length.
: OMIT-MID ( n -- n ) {: cnt:n :}
   cnt 1-  TMPBUF TU32W TLE-PUT                                   \ new count
   LOGBUF TU32W +  TMPBUF TU32W +  TREC-W BYTE-COPY               \ rec0 -> pos0
   2 begin dup cnt < while  dup {: k:n :}
      LOGBUF k REC-OFF +  TMPBUF k 1- REC-OFF +  TREC-W BYTE-COPY
      1+  repeat drop
   LOGBUF cnt REC-OFF +  TMPBUF cnt 1- REC-OFF +  THASH-W BYTE-COPY  \ original head
   cnt 1- REC-OFF THASH-W + ;

\ ================================================================================
\ (a) the log verifies; tamper / reorder / omission each REJECT
\ ================================================================================
: AU-VERIFY-OK ( -- n )   SCENARIO  ENCODE {: len:n :}  LOGBUF len AUDIT:VERIFY-LOG VCODE ;

: AU-TAMPER-MID ( -- n )
   SCENARIO  ENCODE {: len:n :}  len COPY-FRAME
   TMPBUF  1 REC-OFF TOFF-K1 +  FLIP                 \ corrupt record 1's primary key
   TMPBUF len AUDIT:VERIFY-LOG VCODE ;
: AU-TAMPER-MID-IX ( -- n )
   SCENARIO  ENCODE {: len:n :}  len COPY-FRAME
   TMPBUF  1 REC-OFF TOFF-K1 +  FLIP
   TMPBUF len AUDIT:VERIFY-LOG VIDX ;                \ record 1 corrupt -> break surfaces at index 2

: AU-TAMPER-LAST ( -- n )
   SCENARIO  ENCODE {: len:n :}  len COPY-FRAME
   TMPBUF  5 REC-OFF TOFF-K1 +  FLIP                 \ corrupt the LAST record
   TMPBUF len AUDIT:VERIFY-LOG VCODE ;               \ head no longer matches -> bad-head

: AU-REORDER ( -- n )
   SCENARIO  ENCODE {: len:n :}  len COPY-FRAME
   0 1 SWAP-RECS
   TMPBUF len AUDIT:VERIFY-LOG VCODE ;
: AU-REORDER-IX ( -- n )
   SCENARIO  ENCODE {: len:n :}  len COPY-FRAME
   0 1 SWAP-RECS
   TMPBUF len AUDIT:VERIFY-LOG VIDX ;                \ first divergence at index 0

: AU-OMIT ( -- n )
   SCENARIO  ENCODE drop
   AUDIT:COUNT OMIT-MID {: nlen:n :}
   TMPBUF nlen AUDIT:VERIFY-LOG VCODE ;
: AU-OMIT-IX ( -- n )
   SCENARIO  ENCODE drop
   AUDIT:COUNT OMIT-MID {: nlen:n :}
   TMPBUF nlen AUDIT:VERIFY-LOG VIDX ;               \ gap surfaces at index 1

: AU-MALFORMED ( -- n )                              \ a truncated frame is malformed
   SCENARIO  ENCODE {: len:n :}
   LOGBUF len 1-  AUDIT:VERIFY-LOG VCODE ;

\ ================================================================================
\ (b) nondeterministic action: marked + captured, replay from captured output
\ ================================================================================
: AU-ND-OK ( -- n )    SCENARIO-ND  ENCODE {: len:n :}  LOGBUF len AUDIT:VERIFY-LOG VCODE ;
: AU-ND-DET ( -- bool )   SCENARIO-ND  1 AUDIT:EVENT-DET@ ;    \ event 1 is marked nondeterministic

: AU-ND-STRIP ( -- n )
   SCENARIO-ND  ENCODE {: len:n :}  len COPY-FRAME
   TMPBUF  1 REC-OFF TOFF-KCAP +  THASH-W  ZERO-FIELD          \ strip the captured key
   TMPBUF len AUDIT:VERIFY-LOG VCODE ;
: AU-ND-STRIP-IX ( -- n )
   SCENARIO-ND  ENCODE {: len:n :}  len COPY-FRAME
   TMPBUF  1 REC-OFF TOFF-KCAP +  THASH-W  ZERO-FIELD
   TMPBUF len AUDIT:VERIFY-LOG VIDX ;

\ STATE-DIGEST folds the CAPTURED key for the nondeterministic event: changing the live-output
\ key (k1) leaves the replay digest UNCHANGED; changing the captured key (kcap) CHANGES it.
: AU-ND-CAPTURED ( -- bool )
   SCENARIO-ND  ENCODE {: len:n :}
   LOGBUF len DG1 AUDIT:STATE-DIGEST drop                      \ baseline replay digest
   len COPY-FRAME  TMPBUF 1 REC-OFF TOFF-K1 + FLIP             \ perturb the LIVE output key
   TMPBUF len DG2 AUDIT:STATE-DIGEST drop
   len COPY-FRAME  TMPBUF 1 REC-OFF TOFF-KCAP + FLIP           \ perturb the CAPTURED key
   TMPBUF len DG3 AUDIT:STATE-DIGEST drop
   DG1 DG2 KEY-EQ?                                             \ live-output change -> same digest
   DG1 DG3 KEY-EQ? 0= and ;                                   \ captured change -> different digest

\ ---- STATIC: a marked verifier run cannot be recorded without a captured id ------
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- )    CHECK-QUIET-CANDIDATE! 0 T= ;

\ ================================================================================
\ replay reproduces the digests WITHOUT a chooser (composing commit-store determinism)
\ ================================================================================
: AU-REPLAY-STABLE ( -- bool )                       \ re-record against a FRESH store -> same digest
   SCENARIO  ENCODE {: l1:n :}  LOGBUF l1 DG1 AUDIT:STATE-DIGEST drop
   SCENARIO  ENCODE {: l2:n :}  LOGBUF l2 DG2 AUDIT:STATE-DIGEST drop
   l1 l2 =  DG1 DG2 KEY-EQ? and ;
: AU-ENCODE-STABLE ( -- bool )                       \ encoding one log twice is byte-identical
   SCENARIO  ENCODE {: l1:n :}  l1 COPY-FRAME         \ first encode -> TMPBUF via LOGBUF copy
   ENCODE {: l2:n :}                                  \ second encode of the same live log
   l1 l2 =  LOGBUF TMPBUF l1 SAME-BYTES? and ;

\ ---- read surface + fail-closed -------------------------------------------------
: AU-KIND ( -- n )   SCENARIO  1 AUDIT:EVENT-KIND@ EKN ;       \ event 1 == txn-commit (2)
: AU-AUX ( -- n )    SCENARIO  3 AUDIT:EVENT-AUX@ ;            \ evidence-decision outcome 0

: AU-CAP-OVERFLOW ( -- )
   AUDIT:RESET
   TLOG-CAP 1+ 0 ?do  G0 AUDIT:RECORD-ACTIVATION  loop ;
: AU-BUF-SMALL ( -- )   SCENARIO  SMALLBUF 8 AUDIT:ENCODE-LOG drop ;

T-RESET

\ (a) chain integrity
AU-VERIFY-OK 0 T=
AU-TAMPER-MID 2 T=
AU-TAMPER-MID-IX 2 T=
AU-TAMPER-LAST 3 T=
AU-REORDER 2 T=
AU-REORDER-IX 0 T=
AU-OMIT 2 T=
AU-OMIT-IX 1 T=
AU-MALFORMED 1 T=

\ (b) nondeterministic marking + captured-output replay
AU-ND-OK 0 T=
AU-ND-DET TTRUE
AU-ND-STRIP 4 T=
AU-ND-STRIP-IX 1 T=
AU-ND-CAPTURED TTRUE
s" AU-CAP-OK ( -- ) EV1 EV2 AUDIT:RECORD-VERIFIER-RUN-CAPTURED" YES
s" AU-CAP-BAD ( -- ) EV1 AUDIT:RECORD-VERIFIER-RUN-CAPTURED" NO

\ replay reproduces digests without a chooser
AU-REPLAY-STABLE TTRUE
AU-ENCODE-STABLE TTRUE

\ read surface + fail-closed
AU-KIND 2 T=
AU-AUX 0 T=
' AU-CAP-OVERFLOW E-AUDIT-CAP TTHROWS
' AU-BUF-SMALL E-AUDIT-BUF TTHROWS

CSTORE:RESET

T-REPORT

;package
