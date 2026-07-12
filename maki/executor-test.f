\ maki/executor-test.f - checked tests for the full-tensor host executor (cad-7a).
\ Builds tiny model-IR node tables by hand, binds host input buffers, runs EX-RUN,
\ and checks node outputs against HAND-COMPUTED values for every dispatch family:
\ matmul/linear, elementwise (unary + broadcast binary), row words, movement,
\ reduce/scatter backward, rope, plus a multi-node topo-order chain, the
\ EX-OFF!/EX-STEP checkpoint hooks (hand buffer plan + single-node runs), and
\ every fail-closed path (cast, unbound input, bad slot / node / offset).

require lib/test.f
require lib/float.f
require maki/executor.f

package MAKI

\ ---- scratch input buffers -------------------------------------------------
create XB 8 cells allot   create WB 8 cells allot   create BB 4 cells allot
create SB 8 cells allot   create CB 8 cells allot   create IB 4 cells allot

: >I ( ptr a n -- n )   T-GET 0.5 f+ f>s ;            \ read cell as nearest int
: >M ( ptr a n -- n )   T-GET 1000.0 f* 0.5 f+ f>s ;  \ read cell as milliunits (>=0)

: SEQ ( r ptr a n -- ) {: v0:r base:ptr n:n :}       \ base[i] = v0 + i
   n 0 ?do  v0 i s>f f+  base i T-SET  loop ;

\ ---- fail-closed probes (top level cannot push quotations) -----------------
: TRY-CAST ( -- )
   MIR-RESET  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:CAST MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN ;
: TRY-UNBOUND ( -- )
   MIR-RESET  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   EX-RESET  EX-RUN ;
: TRY-NODE ( -- )
   MIR-RESET  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN  5 MIR-NODE-ID EX-OUT@ drop ;
: TRY-BINDSLOT ( -- )
   MIR-RESET  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  XB 7 MIR-SLOT-ID EX-BIND ;

\ one 2x2 gelu node (the EX-OFF!/EX-STEP fail-closed fixture)
: EXH-ONE ( -- )
   MIR-RESET  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;
: TRY-OFF-ND  ( -- )  EXH-ONE  0 5 MIR-NODE-ID EX-OFF! ;
: TRY-STEP-ND ( -- )  EXH-ONE  5 MIR-NODE-ID EX-STEP ;
: TRY-OFF-NEG ( -- )  EXH-ONE  -1 0 MIR-NODE-ID EX-OFF! ;
: TRY-OFF-CAP ( -- )  EXH-ONE  EX-ARENA-CELLS 0 MIR-NODE-ID EX-OFF! ;

\ stale nominal ids: RAW>NODE has no generation guard (unlike tensor handles),
\ so a node-id/input-slot held across MIR-RESET still passes typing - the
\ executor's own bound checks are the last line of defense and stay covered
\ (E-EX-NODE / E-EX-SLOT). The stale ids are constructed honestly: allocated
\ from a live IR, then the IR is reset and the old typed id is reused.
: EXH-STALE-NODE ( -- CAD-KIND:node-id )
   EXH-ONE
   0 MIR-NODE-ID {: nd:CAD-KIND:node-id :}
   MIR-RESET
   nd ;
: EXH-STALE-SLOT ( -- MIR:input-slot )
   EXH-ONE
   0 MIR-SLOT-ID {: s:MIR:input-slot :}
   MIR-RESET
   s ;
: TRY-STALE-OUT  ( -- )  EXH-STALE-NODE EX-OUT@ drop ;
: TRY-STALE-OFF  ( -- )  0 EXH-STALE-NODE EX-OFF! ;
: TRY-STALE-STEP ( -- )  EXH-STALE-NODE EX-STEP ;
: TRY-STALE-BIND ( -- )  EXH-STALE-SLOT XB swap EX-BIND ;

T-RESET

\ ---- MATMUL: X=[[1,2],[3,4]] W=[[5,6],[7,8]] -> Y=[[19,22],[43,50]] ----------
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ slot0 x
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ slot1 w
MAKI-OPKIND:MATMUL MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 0 T-SET 2.0 XB 1 T-SET 3.0 XB 2 T-SET 4.0 XB 3 T-SET
5.0 WB 0 T-SET 6.0 WB 1 T-SET 7.0 WB 2 T-SET 8.0 WB 3 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 19 T=
0 MIR-NODE-ID EX-OUT@ 1 >I 22 T=
0 MIR-NODE-ID EX-OUT@ 2 >I 43 T=
0 MIR-NODE-ID EX-OUT@ 3 >I 50 T=

\ ---- LINEAR: Y = X.W + b, b=[10,20] -> [[29,42],[53,70]] -------------------
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ x
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ w
1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ b (1x2)
MAKI-OPKIND:LINEAR MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 0 T-SET 2.0 XB 1 T-SET 3.0 XB 2 T-SET 4.0 XB 3 T-SET
5.0 WB 0 T-SET 6.0 WB 1 T-SET 7.0 WB 2 T-SET 8.0 WB 3 T-SET
10.0 BB 0 T-SET 20.0 BB 1 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND  BB 2 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 29 T=
0 MIR-NODE-ID EX-OUT@ 1 >I 42 T=
0 MIR-NODE-ID EX-OUT@ 2 >I 53 T=
0 MIR-NODE-ID EX-OUT@ 3 >I 70 T=

\ ---- GELU unary: x=[1,2,-1,0] -> gelu (milliunits 841,1955,-159,0) ----------
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 0 T-SET 2.0 XB 1 T-SET -1.0 XB 2 T-SET 0.0 XB 3 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >M 841 T=
0 MIR-NODE-ID EX-OUT@ 1 >M 1955 T=
0 MIR-NODE-ID EX-OUT@ 3 >M 0 T=
0 MIR-NODE-ID EX-OUT@ 2 T-GET 1000.0 f* 0.5 f- f>s -159 T=

\ ---- BIAS broadcast: x 2x2=[1,2,3,4], b 1x2=[10,20] -> [[11,22],[13,24]] ----
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:BIAS MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 4 SEQ  10.0 BB 0 T-SET 20.0 BB 1 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  BB 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 11 T=
0 MIR-NODE-ID EX-OUT@ 1 >I 22 T=
0 MIR-NODE-ID EX-OUT@ 2 >I 13 T=
0 MIR-NODE-ID EX-OUT@ 3 >I 24 T=

\ ---- MUL: a=[1,2,3,4], b=[2,3,4,5] -> [2,6,12,20] --------------------------
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:MUL MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 4 SEQ  2.0 WB 4 SEQ
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 2 T=
0 MIR-NODE-ID EX-OUT@ 3 >I 20 T=

\ ---- LAYERNORM row: x 1x3=[1,2,3] -> [-1225,0,1225] milli -------------------
MIR-RESET
1 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:LAYERNORM MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 0 T-SET 2.0 XB 1 T-SET 3.0 XB 2 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 T-GET 1000.0 f* 0.5 f- f>s -1225 T=
0 MIR-NODE-ID EX-OUT@ 1 >M 0 T=
0 MIR-NODE-ID EX-OUT@ 2 >M 1225 T=

\ ---- SOFTMAX row: x 1x2=[0,0] -> [0.5,0.5] ---------------------------------
MIR-RESET
1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:SOFTMAX-ROW MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
0.0 XB 0 T-SET 0.0 XB 1 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >M 500 T=
0 MIR-NODE-ID EX-OUT@ 1 >M 500 T=

\ ---- TRANSPOSE: [[1,2],[3,4]] -> [[1,3],[2,4]] -----------------------------
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:TRANSPOSE MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-TRANSPOSE MV-TRANSPOSE-VERDICT 0 0 MV-PACK  1  MIR-OP+ drop
1.0 XB 4 SEQ
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 1 T=
0 MIR-NODE-ID EX-OUT@ 1 >I 3 T=
0 MIR-NODE-ID EX-OUT@ 2 >I 2 T=
0 MIR-NODE-ID EX-OUT@ 3 >I 4 T=

\ ---- SLICE rows [1,3) of a 4x2 buffer (r0=1,r1=3) --------------------------
MIR-RESET
4 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:SLICE MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-SLICE MVV-FREE 1 3 MV-PACK  1  MIR-OP+ drop
10.0 XB 8 SEQ                                        \ 10..17 row-major (4x2)
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 12 T=                                 \ row 1 = {12,13}
0 MIR-NODE-ID EX-OUT@ 3 >I 15 T=                                 \ row 2 = {14,15}

\ ---- GATHER rows {2,0} of a 3x2 buffer -------------------------------------
MIR-RESET
3 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ data 3x2
2 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ idx 2x1
MAKI-OPKIND:GATHER MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-GATHER MV-GATHER-VERDICT 0 0 MV-PACK  1  MIR-OP+ drop
7.0 XB 6 SEQ                                         \ 7..12 (3x2)
2.0 IB 0 T-SET  0.0 IB 1 T-SET                        \ idx = {2,0}
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  IB 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 11 T=                                 \ row 2 = {11,12}
0 MIR-NODE-ID EX-OUT@ 2 >I 7 T=                                  \ row 0 = {7,8}

\ ---- ROWSUM-BWD: ct 2x2=[[1,2],[3,4]] -> colsum [4,6] ----------------------
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:ROWSUM-BWD MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 4 SEQ
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 4 T=
0 MIR-NODE-ID EX-OUT@ 1 >I 6 T=

\ ---- SCATTER-ADD: ct 2x2=[[1,2],[3,4]] idx={0,0} -> row0=[4,6], row1=[0,0] --
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ ct 2x2
2 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ idx 2x1
MAKI-OPKIND:SCATTER-ADD MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-GATHER MVV-MATERIALIZE 0 0 MV-PACK  1  MIR-OP+ drop
1.0 XB 4 SEQ  0.0 IB 0 T-SET 0.0 IB 1 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  IB 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 4 T=                                  \ ct[0,0]+ct[1,0] = 1+3
0 MIR-NODE-ID EX-OUT@ 1 >I 6 T=                                  \ ct[0,1]+ct[1,1] = 2+4
0 MIR-NODE-ID EX-OUT@ 2 >I 0 T=                                  \ row 1 untouched -> 0

\ ---- ROPE forward: x 1x2=[1,0], cos=[0,0], sin=[1,0] -> (0*cos... ) ---------
\ pair (x0=1,x1=0), c=cos[0]=0, s=sin[0]=1 -> re=1*0-0*1=0, im=1*1+0*0=1.
MIR-RESET
1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ x
1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ cos
1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                   \ sin
MAKI-OPKIND:ROPE MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   1 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 0 T-SET 0.0 XB 1 T-SET
0.0 WB 0 T-SET 0.0 WB 1 T-SET                         \ cos
1.0 BB 0 T-SET 0.0 BB 1 T-SET                         \ sin
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND  BB 2 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 0 T=                                  \ re = 0
0 MIR-NODE-ID EX-OUT@ 1 >I 1 T=                                  \ im = 1

\ ---- multi-node chain (topo order): n0 = MATMUL(x,w) ; n1 = GELU(n0) --------
\ x=[[1,0],[0,1]] w=[[1,2],[3,4]] -> n0 = w rows = [[1,2],[3,4]] ; gelu each.
MIR-RESET
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:MATMUL MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+ 2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
1.0 XB 0 T-SET 0.0 XB 1 T-SET 0.0 XB 2 T-SET 1.0 XB 3 T-SET
1.0 WB 0 T-SET 2.0 WB 1 T-SET 3.0 WB 2 T-SET 4.0 WB 3 T-SET
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 >I 1 T=                                  \ n0[0,0] = 1 (identity @ w)
0 MIR-NODE-ID EX-OUT@ 3 >I 4 T=                                  \ n0[1,1] = 4
1 MIR-NODE-ID EX-OUT@ 0 >M 841 T=                                \ gelu(1) = 0.841
1 MIR-NODE-ID EX-OUT@ 3 >M   4.0 GELU-F 1000.0 f* 0.5 f+ f>s T=  \ gelu(4) matches the reference

\ ---- EX-RUN-N runs a node prefix (forward slice only) ----------------------
\ same chain: EX-RUN-N 1 computes only n0 (the matmul), leaving n1 unwritten.
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND  1 EX-RUN-N
0 MIR-NODE-ID EX-OUT@ 0 >I 1 T=

\ ---- EX-OFF! / EX-STEP: external buffer plan + single-node execution --------
\ same chain under a hand plan (n0 placed ABOVE n1, the reverse of EX-PLAN):
\ each EX-STEP runs one node under the current offsets; values match EX-RUN's.
EX-RESET  XB 0 MIR-SLOT-ID EX-BIND  WB 1 MIR-SLOT-ID EX-BIND
8 0 MIR-NODE-ID EX-OFF!  0 1 MIR-NODE-ID EX-OFF!
0 MIR-NODE-ID EX-STEP  1 MIR-NODE-ID EX-STEP
0 MIR-NODE-ID EX-OUT@ 0 >I 1 T=                                  \ n0[0,0] = 1 (at offset 8)
0 MIR-NODE-ID EX-OUT@ 3 >I 4 T=                                  \ n0[1,1] = 4
1 MIR-NODE-ID EX-OUT@ 0 >M 841 T=                                \ n1 = gelu(n0) read n0's new home

\ ---- membership predicate --------------------------------------------------
OP-GELU   EX-OP-OK? TTRUE
OP-MATMUL EX-OP-OK? TTRUE
OP-CAST   EX-OP-OK? TFALSE

\ ---- fail closed -----------------------------------------------------------
' TRY-CAST     E-EX-UNSUP   TTHROWS
' TRY-UNBOUND  E-EX-UNBOUND TTHROWS
' TRY-NODE     E-MIR-IDX    TTHROWS
' TRY-BINDSLOT E-MIR-INSLOT TTHROWS
' TRY-OFF-ND   E-MIR-IDX    TTHROWS
' TRY-STEP-ND  E-MIR-IDX    TTHROWS
' TRY-OFF-NEG  E-EX-CAP     TTHROWS
' TRY-OFF-CAP  E-EX-CAP     TTHROWS
' TRY-STALE-OUT  E-EX-NODE  TTHROWS
' TRY-STALE-OFF  E-EX-NODE  TTHROWS
' TRY-STALE-STEP E-EX-NODE  TTHROWS
' TRY-STALE-BIND E-EX-SLOT  TTHROWS

T-REPORT

end-package
