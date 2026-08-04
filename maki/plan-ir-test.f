\ maki/plan-ir-test.f - checked tests for the plan-IR -> execution handoff.
\ Builds the executor-test matmul+gelu chain, lowers it through PIR-BUILD into
\ the typed async DAG, replays it with PIR-RUN, and checks: the numeric outputs
\ match the hand-computed EX-RUN values, the sealed replay order is the IR topo
\ order, kernel payloads point at their IR nodes, the render is byte-identical
\ across two builds of the same IR (determinism), an event-synced two-stream
\ DAG replays the same kernels with host no-op event nodes, and the fail-closed
\ paths: empty IR, unsealed replay, host-unsupported transfer nodes.

require lib/test.f
require lib/float.f
require maki/plan-ir.f

package MAKI

create PIT-X 8 cells allot
create PIT-W 8 cells allot

: PIT->I ( ptr a n -- n )   T-GET 0.5 f+ f>s ;            \ read cell as nearest int
: PIT->M ( ptr a n -- n )   T-GET 1000.0 f* 0.5 f+ f>s ;  \ read cell as milliunits

\ n0 = MATMUL(x,w) ; n1 = GELU(n0) - x = identity, w = [[1,2],[3,4]]
: PIT-CHAIN ( -- )
   MIR-RESET
   2 2 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   2 2 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:MATMUL MIR-OP-BEGIN
      0 MIR-SLOT-ID MIR-IN-REF MIR-IN+  1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
      2 2 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN
      0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
      2 2 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

: PIT-BIND ( -- )
   EX-RESET
   PIT-X 0 MIR-SLOT-ID EX-BIND
   PIT-W 1 MIR-SLOT-ID EX-BIND ;

: PIT-OUTS-CK ( -- )                    \ n0 = w rows; n1 = gelu(n0)
   \ ALL four cells per output: cells 1/2 are exactly where a transposed
   \ replay would differ from EX-RUN (same-type-swap blind spot).
   0 MIR-NODE-ID EX-OUT@ 0 PIT->I 1 T=
   0 MIR-NODE-ID EX-OUT@ 1 PIT->I 2 T=
   0 MIR-NODE-ID EX-OUT@ 2 PIT->I 3 T=
   0 MIR-NODE-ID EX-OUT@ 3 PIT->I 4 T=
   1 MIR-NODE-ID EX-OUT@ 0 PIT->M 841 T=
   1 MIR-NODE-ID EX-OUT@ 1 PIT->M 2.0 GELU-F 1000.0 f* 0.5 f+ f>s T=
   1 MIR-NODE-ID EX-OUT@ 2 PIT->M 3.0 GELU-F 1000.0 f* 0.5 f+ f>s T=
   1 MIR-NODE-ID EX-OUT@ 3 PIT->M 4.0 GELU-F 1000.0 f* 0.5 f+ f>s T= ;

\ a hand DAG for the same IR: kernels on s0, event sync into s1 (host no-ops)
: PIT-DAG-EVENTS ( -- )
   ADAG-RESET
   ADAG-STREAM+ {: s0:ADAG:stream-id :}
   ADAG-STREAM+ {: s1:ADAG:stream-id :}
   ADAG-EVENT+ {: e:ADAG:event-id :}
   0 MIR-NODE-ID s0 ADAG-KERNEL+ drop
   1 MIR-NODE-ID s0 ADAG-KERNEL+ drop
   e s0 ADAG-RECORD+ drop
   e s1 ADAG-WAIT+ drop
   ADAG-SEAL ;

\ ---- fail-closed probes (top level cannot push quotations) ---------------------
: PIT-T-EMPTY ( -- )
   MIR-RESET
   PIR-BUILD ;

: PIT-T-STATE ( -- )                    \ replay without a sealed DAG
   PIT-CHAIN PIT-BIND
   ADAG-RESET
   PIR-RUN ;

: PIT-T-UNSUP ( -- )                    \ transfers have no host execution yet
   PIT-CHAIN PIT-BIND
   ADAG-RESET
   ADAG-STREAM+ {: s:ADAG:stream-id :}
   1 0 8 s ADAG-MEMSET+ drop
   ADAG-SEAL
   PIR-RUN ;

\ ---- render snapshot (byte-identical determinism comparison) -------------------
create PIT-SNAP $1000 allot
variable PIT-SNAP-U

: PIT-SNAP! ( ptr u8 n -- ) {: a:ptr u:n :}
   u $1000 > if E-ADAG-CAP throw then
   a PIT-SNAP u BYTE-COPY
   u PIT-SNAP-U ! ;

: PIT-SNAP$ ( -- ptr u8 n )  PIT-SNAP PIT-SNAP-U @ ;

T-RESET

\ ---- the typed handoff: IR -> sealed DAG -> deterministic replay ---------------
PIT-CHAIN
1.0 PIT-X 0 T-SET 0.0 PIT-X 1 T-SET 0.0 PIT-X 2 T-SET 1.0 PIT-X 3 T-SET
1.0 PIT-W 0 T-SET 2.0 PIT-W 1 T-SET 3.0 PIT-W 2 T-SET 4.0 PIT-W 3 T-SET
PIT-BIND
PIR-BUILD
ADAG-SEALED? TTRUE
ADAG-N@ 2 T=
ADAG-STREAMS@ 1 T=
ADAG-EDGES@ 2 T=                        \ program order n0->n1 + data edge n0->n1
0 ADAG-ORDER@ 0 ADAG-NODE-ID ADAG-NODE= TTRUE
1 ADAG-ORDER@ 1 ADAG-NODE-ID ADAG-NODE= TTRUE
0 ADAG-NODE-ID ADAG-KERNEL-IR@ 0 MIR-NODE-ID MIR-NODE= TTRUE
1 ADAG-NODE-ID ADAG-KERNEL-IR@ 1 MIR-NODE-ID MIR-NODE= TTRUE
PIR-RUN
PIT-OUTS-CK

\ the DAG replay computes exactly what the hidden index order computed
PIT-BIND EX-RUN
PIT-OUTS-CK

\ determinism: rebuilding the DAG from the same IR renders byte-identically
PIR-BUILD ADAG-RENDER PIT-SNAP!
PIR-BUILD ADAG-RENDER PIT-SNAP$ T$=

\ an event-synced two-stream DAG replays the same kernels (events host no-op)
PIT-BIND
PIT-DAG-EVENTS
PIR-RUN
PIT-OUTS-CK

\ ---- fail closed ----------------------------------------------------------------
' PIT-T-EMPTY E-PIR-EMPTY TTHROWS
' PIT-T-STATE E-PIR-STATE TTHROWS
' PIT-T-UNSUP E-PIR-UNSUP TTHROWS

T-REPORT

;package
