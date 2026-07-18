\ maki/db/audit-log-xproc-child.f - the FRESH-PROCESS replay side of the decisive
\ byte-stability test (maki/db/audit-log.f, dot habu-v2-deterministic-audit-428d27c2;
\ the maki/db/keywire-xproc-child.f cross-process precedent).
\
\ CONCERN: the "replay in a fresh process from an EMPTY store" half of the audit-log
\ byte-stability acceptance. The parent (maki/db/audit-log-xproc-test.f) builds the
\ canonical event log from content-addressed identities, serializes it with ENCODE-LOG,
\ writes the frame to a file, and spawns a FRESH bin/hb that loads THIS file and calls
\ RUN-CHILD on the frame path. Package AUDIT-XPROC.
\
\ The point it proves: the log's events reference their identities by CROSS-PROCESS content
\ key (REV:KEY>WIRE / TX:IDEM-KEY>WIRE / ARTIFACT:KEY>WIRE / EVIDENCE:KEY>WIRE / PRODUCER:
\ KEY>WIRE), never a process-local registry raw. SHIFT deliberately registers decoys FIRST so
\ the child's real ids get raw indices that DIFFER from the parent's; the child then rebuilds
\ the SAME logical log from an empty audit store and its serialized frame is BYTE-IDENTICAL to
\ the parent's - so a fresh process reproduces the identical events, chain, and state digest
\ despite a shifted registry. If the wire form were a raw index the rebuilt frame would diverge.
\ The shared descriptor words keep parent and child content identical (equal content -> equal
\ content key), and RUN-CHILD prints XPROC-OK only if the parent frame verifies AND the rebuild
\ matches byte-for-byte.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require maki/artifact.f
require maki/producer.f
require maki/rev.f
require maki/db/evidence.f
require maki/db/transaction.f
require maki/db/audit-log.f

package AUDIT-XPROC
public

\ ---- shared content-addressed identities (identical content -> identical content key) ---
: OBJ-A ( -- CAD-KIND:artifact-id )   s" ax-obj-a" ARTIFACT:REGISTER ;
: OBJ-C ( -- CAD-KIND:artifact-id )   s" ax-obj-c" ARTIFACT:REGISTER ;
: SUBJ ( -- CAD-KIND:artifact-id )    s" ax-subj" ARTIFACT:REGISTER ;
: MODEL ( -- CAD-KIND:artifact-id )   s" ax-model" ARTIFACT:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id )   s" ax-verifier" PRODUCER:REGISTER ;
: EV1 ( -- CAD-KIND:evidence-id )     s" ax/ev-run" EVIDENCE:REGISTER ;
: EV2 ( -- CAD-KIND:evidence-id )     s" ax/ev-policy" EVIDENCE:REGISTER ;
: G0 ( -- CAD-KIND:rev-id )           s" ax-genesis" REV:COMMIT ;

: MK-TXN ( -- txn )
   G0 TX:OPEN  OBJ-A TX:PRESENT TX:READ+  OBJ-C TX:WRITE+  OBJ-A TX:DEP+  TX:BUILD ;

\ BUILD-LOG records the canonical event sequence using ONLY content-addressed identities and
\ TX:PROPOSE (no file store), so parent and child produce byte-identical frames. It includes a
\ NONDETERMINISTIC captured verifier run so the byte-stability covers the capture-fold path.
: BUILD-LOG ( -- )
   AUDIT:RESET
   MK-TXN {: t:txn :}
   t AUDIT:RECORD-ACTION-REQUEST
   t TX:PROPOSE {: r:CAD-KIND:rev-id :}
   t r AUDIT:RECORD-COMMIT
   EV1 EV2 AUDIT:RECORD-VERIFIER-RUN-CAPTURED
   SUBJ VERIF 0 AUDIT:RECORD-EVIDENCE-DECISION
   MODEL EV2 AUDIT:RECORD-PROMOTION
   r AUDIT:RECORD-ACTIVATION ;

private

create PBUF $4000 allot                \ parent frame read from file
create CBUF $4000 allot                \ child's independent rebuild

: SAME-BYTES? ( ptr u8 ptr u8 n -- bool ) {: a:ptr b:ptr len:n :}
   0 begin dup len < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: VOK? ( AUDIT:verify-result -- bool )
   MATCH AUDIT:verify-result
      ok                   OF true ENDOF
      malformed            OF false ENDOF
      broken-chain         OF drop false ENDOF
      bad-head             OF false ENDOF
      bad-nondeterministic OF drop false ENDOF
   ;MATCH ;

\ SHIFT registers decoys first so the child's real ids land at raws that DIFFER from the parent's.
: SHIFT ( -- )
   s" ax-decoy-a" ARTIFACT:REGISTER drop
   s" ax-decoy-b" ARTIFACT:REGISTER drop
   s" ax-dp" PRODUCER:REGISTER drop
   s" ax/de" EVIDENCE:REGISTER drop
   s" ax-dr" REV:COMMIT drop ;

public

\ RUN-CHILD reads the parent's serialized frame, verifies its chain, then INDEPENDENTLY rebuilds
\ the same logical log from an empty audit store under a decoy-shifted registry, and prints
\ XPROC-OK iff the parent frame verifies AND the rebuild is byte-identical (identity survives
\ process death because the events are content-keyed).
: RUN-CHILD ( ptr u8 n -- ) {: p:ptr u:n :}
   p u PBUF $4000 READ-ALL {: got:n :}
   PBUF got AUDIT:VERIFY-LOG VOK? {: pv:bool :}
   SHIFT
   BUILD-LOG
   CBUF $4000 AUDIT:ENCODE-LOG {: clen:n :}
   pv  clen got =  and  PBUF CBUF got SAME-BYTES?  and
   if s" XPROC-OK" else s" XPROC-FAIL" then type ;

;package
