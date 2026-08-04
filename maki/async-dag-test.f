\ maki/async-dag-test.f - checked tests for the typed async execution DAG.
\ Static negatives: raw n cannot forge a stream / event / node identity and the
\ roles never collapse into each other (checker rejects, with resolving positive
\ controls). Runtime: construction, kind-gated payloads, sealed-order replay
\ determinism (byte-identical render across two builds of the same DAG), and
\ every named fail-closed path: use-before-ready, cross-stream missing wait,
\ event double-destroy / use-after-destroy, dependency cycle, sealed mutation,
\ unsealed replay, wrong-kind payload, stale handles, capacity.

require lib/test.f
require test/checker-assert.f
require maki/async-dag.f
require maki/model-ir.f

package MAKI

: ADT-YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: ADT-NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- fixtures ----------------------------------------------------------------
\ s0: memset ; record e0 ; s1: wait e0 ; memset  (cross-stream sync via event)
: ADT-BUILD-SYNC ( -- )
   ADAG-RESET
   ADAG-STREAM+ {: s0:ADAG:stream-id :}
   ADAG-STREAM+ {: s1:ADAG:stream-id :}
   ADAG-EVENT+ {: e0:ADAG:event-id :}
   1 0 16 s0 ADAG-MEMSET+ drop
   e0 s0 ADAG-RECORD+ drop
   e0 s1 ADAG-WAIT+ drop
   2 0 16 s1 ADAG-MEMSET+ drop
   ADAG-SEAL ;

\ one 2x2 gelu model-IR node (the kernel-payload fixture)
: ADT-IR1 ( -- )
   MIR-RESET
   2 2 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

\ one copy node + one kernel node on a single stream
: ADT-BUILD-CK ( -- )
   ADT-IR1
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   7 3 32 s ADAG-COPY+ drop
   0 MIR-NODE-ID s ADAG-KERNEL+ drop
   ADAG-SEAL ;

\ ---- fail-closed probes (top level cannot push quotations) ---------------------
: ADT-T-UNREADY ( -- )                  \ wait on a never-recorded event
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   ADAG-EVENT+ s ADAG-WAIT+ drop ;

: ADT-T-XSTREAM ( -- )                  \ raw cross-stream dep without an event wait
   ADAG-RESET
   ADAG-STREAM+ {: s0:ADAG:stream-id :}
   ADAG-STREAM+ {: s1:ADAG:stream-id :}
   1 0 8 s0 ADAG-MEMSET+ {: a:ADAG:node-id :}
   2 0 8 s1 ADAG-MEMSET+ {: b:ADAG:node-id :}
   a b ADAG-DEP+ ;

: ADT-T-EDESTROY ( -- )                 \ event destroyed twice
   ADAG-RESET
   ADAG-EVENT+ {: e:ADAG:event-id :}
   e ADAG-EVENT-DESTROY
   e ADAG-EVENT-DESTROY ;

: ADT-T-EUSE ( -- )                     \ record on a destroyed event
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   ADAG-EVENT+ {: e:ADAG:event-id :}
   e ADAG-EVENT-DESTROY
   e s ADAG-RECORD+ drop ;

: ADT-T-CYCLE ( -- )                    \ declared dep contradicts stream order
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   1 0 8 s ADAG-MEMSET+ {: a:ADAG:node-id :}
   2 0 8 s ADAG-MEMSET+ {: b:ADAG:node-id :}
   b a ADAG-DEP+
   ADAG-SEAL ;

: ADT-T-SEALED ( -- )                   \ node append after seal
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   1 0 8 s ADAG-MEMSET+ drop
   ADAG-SEAL
   3 0 8 s ADAG-MEMSET+ drop ;

: ADT-T-RESEAL ( -- )
   ADAG-RESET ADAG-SEAL ADAG-SEAL ;

: ADT-T-UNSEALED ( -- )                 \ replay order before seal
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   1 0 8 s ADAG-MEMSET+ drop
   0 ADAG-ORDER@ drop ;

: ADT-T-KIND-EV ( -- )                  \ event payload of a memset node
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   1 0 8 s ADAG-MEMSET+ ADAG-EVENT@ drop ;

: ADT-T-KIND-KIR ( -- )                 \ kernel payload of a memset node
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   1 0 8 s ADAG-MEMSET+ ADAG-KERNEL-IR@ drop ;

: ADT-T-IDX ( -- )                      \ refinement of an out-of-range index
   ADAG-RESET
   0 ADAG-NODE-ID drop ;

: ADT-T-STALE ( -- )                    \ stream handle held across ADAG-RESET
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   ADAG-RESET
   1 0 8 s ADAG-MEMSET+ drop ;

: ADT-T-NCAP ( -- )                     \ node table capacity
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   ADAG-NCAP 1+ 0 ?do  1 0 8 s ADAG-MEMSET+ drop  loop ;

\ the stats product destructures to the sync fixture's counts (checked word:
\ a layout value cannot ride the interpret-mode stack)
: ADT-STATS-CK ( -- )
   ADAG-STATS@ ADAG-STATS:UNMAKE {: nn:n sn:n en:n dn:n :}
   nn 4 T=
   sn 2 T=
   en 1 T=
   dn 3 T= ;

\ ---- render snapshot (byte-identical determinism comparison) -------------------
create ADT-SNAP $1000 allot
variable ADT-SNAP-U

: ADT-SNAP! ( ptr u8 n -- ) {: a:ptr u:n :}
   u $1000 > if E-ADAG-CAP throw then
   a ADT-SNAP u BYTE-COPY
   u ADT-SNAP-U ! ;

: ADT-SNAP$ ( -- ptr u8 n )  ADT-SNAP ADT-SNAP-U @ ;

T-RESET

\ ---- static: nominal identities cannot be forged or role-swapped --------------
s" ADT-C1 ( ADAG:stream-id -- ADAG:stream-id )" ADT-YES
s" ADT-C2 ( ADAG:event-id ADAG:stream-id -- ADAG:node-id ) MAKI:ADAG-WAIT+" ADT-YES
s" ADT-F1 ( n -- ADAG:stream-id )" ADT-NO
s" ADT-F2 ( n -- ADAG:event-id )" ADT-NO
s" ADT-F3 ( n -- ADAG:node-id )" ADT-NO
s" ADT-F4 ( ADAG:stream-id -- ADAG:event-id )" ADT-NO
s" ADT-F5 ( ADAG:node-id -- CAD-KIND:node-id )" ADT-NO
s" ADT-F6 ( ADAG:event-id -- n )" ADT-NO
s" ADT-F7 ( ADAG:stream-id ADAG:event-id -- ADAG:node-id ) MAKI:ADAG-WAIT+" ADT-NO

\ ---- construction + sealed replay order (event-synced two-stream DAG) ---------
ADT-BUILD-SYNC
ADAG-SEALED? TTRUE
ADAG-N@ 4 T=
ADAG-STREAMS@ 2 T=
ADAG-EVENTS@ 1 T=
ADAG-EDGES@ 3 T=                        \ prog s0 n0->n1, record->wait n1->n2, prog s1 n2->n3
0 ADAG-ORDER@ 0 ADAG-NODE-ID ADAG-NODE= TTRUE
1 ADAG-ORDER@ 1 ADAG-NODE-ID ADAG-NODE= TTRUE
2 ADAG-ORDER@ 2 ADAG-NODE-ID ADAG-NODE= TTRUE
3 ADAG-ORDER@ 3 ADAG-NODE-ID ADAG-NODE= TTRUE

\ node kinds + stream ownership + payloads survive the seal
0 ADAG-NODE-ID ADAG-KIND@ MAKI-AKIND:MEMSET MAKI-AKIND:EQ TTRUE
1 ADAG-NODE-ID ADAG-KIND@ MAKI-AKIND:EVENT-RECORD MAKI-AKIND:EQ TTRUE
2 ADAG-NODE-ID ADAG-KIND@ MAKI-AKIND:EVENT-WAIT MAKI-AKIND:EQ TTRUE
0 ADAG-NODE-ID ADAG-NODE-STREAM@ 0 ADAG-STREAM-ID ADAG-STREAM= TTRUE
3 ADAG-NODE-ID ADAG-NODE-STREAM@ 1 ADAG-STREAM-ID ADAG-STREAM= TTRUE
1 ADAG-NODE-ID ADAG-EVENT@ 0 ADAG-EVENT-ID ADAG-EVENT= TTRUE
2 ADAG-NODE-ID ADAG-EVENT@ 0 ADAG-EVENT-ID ADAG-EVENT= TTRUE
0 ADAG-NODE-ID ADAG-DST@ 1 T=
0 ADAG-NODE-ID ADAG-VAL@ 0 T=
0 ADAG-NODE-ID ADAG-BYTES@ 16 T=

\ the stats product destructures to the same counts
ADT-STATS-CK

\ event destruction after seal is lifecycle bookkeeping, not graph mutation
0 ADAG-EVENT-ID ADAG-EVENT-LIVE? TTRUE
0 ADAG-EVENT-ID ADAG-EVENT-DESTROY
0 ADAG-EVENT-ID ADAG-EVENT-LIVE? TFALSE

\ ---- determinism: same DAG -> byte-identical render (incl. replay line) -------
ADT-BUILD-SYNC ADAG-RENDER ADT-SNAP!
ADT-BUILD-SYNC ADAG-RENDER ADT-SNAP$ T$=
s" replay: n0 n1 n2 n3" ADT-SNAP$ 2swap CONTAINS? TTRUE

\ ---- copy + kernel payloads ----------------------------------------------------
ADT-BUILD-CK
0 ADAG-NODE-ID ADAG-KIND@ MAKI-AKIND:COPY MAKI-AKIND:EQ TTRUE
0 ADAG-NODE-ID ADAG-DST@ 7 T=
0 ADAG-NODE-ID ADAG-SRC@ 3 T=
0 ADAG-NODE-ID ADAG-BYTES@ 32 T=
1 ADAG-NODE-ID ADAG-KIND@ MAKI-AKIND:KERNEL MAKI-AKIND:EQ TTRUE
1 ADAG-NODE-ID ADAG-KERNEL-IR@ 0 MIR-NODE-ID MIR-NODE= TTRUE

\ empty DAG seals to an empty replay
ADAG-RESET ADAG-SEAL
ADAG-SEALED? TTRUE
ADAG-N@ 0 T=

\ ---- fail closed ----------------------------------------------------------------
' ADT-T-UNREADY  E-ADAG-UNREADY  TTHROWS
' ADT-T-XSTREAM  E-ADAG-XSTREAM  TTHROWS
' ADT-T-EDESTROY E-ADAG-EDESTROY TTHROWS
' ADT-T-EUSE     E-ADAG-EUSE     TTHROWS
' ADT-T-CYCLE    E-ADAG-CYCLE    TTHROWS
' ADT-T-SEALED   E-ADAG-SEALED   TTHROWS
' ADT-T-RESEAL   E-ADAG-SEALED   TTHROWS
' ADT-T-UNSEALED E-ADAG-UNSEALED TTHROWS
' ADT-T-KIND-EV  E-ADAG-KIND     TTHROWS
' ADT-T-KIND-KIR E-ADAG-KIND     TTHROWS
' ADT-T-IDX      E-ADAG-IDX      TTHROWS
' ADT-T-STALE    E-ADAG-IDX      TTHROWS
' ADT-T-NCAP     E-ADAG-CAP      TTHROWS

T-REPORT

;package
