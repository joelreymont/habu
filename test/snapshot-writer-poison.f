\ snapshot-writer-poison.f - plant return-stack canaries before a snapshot.
\
\ Builder-only fixture: the snapshot suite injects this source ahead of the snap
\ driver (BF-EMIT-SNAP-RUN-SOURCE-WITH), so it loads while the live data region is
\ still the one SNAP:PERSIST copies. It writes a fixed non-zero canary into the low
\ high ends of the return-stack window inside the DATA region. SNAP-CANON-DATA's
\ SND-ZERO-RSTK must zero that whole window in the persisted copy, so the built
\ image reads back all zeros there. The paired assertion lives in
\ test/snapshot-writer.f POISON-CASE (RSTK-NONZERO 0 T=): a surviving canary
\ would prove a stale return-stack frame leaked into the snapshot.

package SNAP-WRITER-POISON

$5253544B4C4F0001 constant LO-CANARY   \ "RSTKLO" + 1
$5253544B48490002 constant HI-CANARY   \ "RSTKHI" + 2

: PLANT ( -- )
   LO-CANARY data-base RSTK-OFF + !
   HI-CANARY data-base RSTK-END 8 - + ! ;

: PROVE-PLANTED ( -- )
   data-base RSTK-OFF + @ LO-CANARY <> if
      s" snapshot writer low return-stack poison failed" 70 die
   then
   data-base RSTK-END 8 - + @ HI-CANARY <> if
      s" snapshot writer high return-stack poison failed" 70 die
   then ;

: POISON ( -- )
   PLANT
   PROVE-PLANTED ;

POISON

;package
