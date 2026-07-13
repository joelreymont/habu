\ maki/mem-plan-test.f - checked tests for the cad-3 memory coalescing planner.
\ Vector-width rules, the CO-* access classifier, the recorded slot-alignment fact,
\ and the per-hot MEMORY rows (coalesced-v4 / unaligned+warning / strided / masked
\ tail / broadcast / gathered) over hand-built model IR so alignment is controllable.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/move-facts.f
require maki/fusion-plan.f
require maki/mem-plan.f

package MAKI

\ ---- render containment helper ---------------------------------------------
variable PT-VA  variable PT-VU
: PT-SAVE ( ptr u8 n -- )  PT-VU ! PT-VA ! ;
: PT-IN ( ptr u8 n -- )  PT-VA @ PT-VU @ 2swap CONTAINS? TTRUE ;
: PT-NOTIN ( ptr u8 n -- )  PT-VA @ PT-VU @ 2swap CONTAINS? TFALSE ;

\ ---- IR builders (a single elementwise chain over one input slot) ----------
\ the input layout arrives as a family (cannot bind into a local): the dtype
\ swaps under it for MIR-INPUT+, and the extents are read back from the slot
: MP-EW ( n n layout -- )                          \ x:rows x cols (input layout) -> GELU RELU
   MIR-RESET
   >r {: r:n c:n :}                                \ layout rides the return stack
   r c SHAPE  MAKI-DTYPE:DF32  r>  MIR-INPUT+ drop
   0 MIR-SLOT-ID MIR-SLOT-ROWS@ 0 MIR-SLOT-ID MIR-SLOT-COLS@
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MAKI-OPKIND:RELU MIR-OP-BEGIN 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
   rows cols MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

T-RESET

\ ---- vector-width rule (align esize extent -- w) ---------------------------
MAKI-ALIGN:A16     4 4 MP-W 4 T=                            \ 16B / f32 / >=4 -> v4
MAKI-ALIGN:A16     4 3 MP-W 2 T=                            \ extent 3 < 4 -> v2
MAKI-ALIGN:A8      4 4 MP-W 2 T=                            \ 8B -> at most v2
MAKI-ALIGN:A4      4 4 MP-W 1 T=                            \ 4B -> scalar
MAKI-ALIGN:UNKNOWN 4 8 MP-W 1 T=                            \ unknown -> scalar

\ ---- classifier (align esize extent layout -- status) ----------------------
MAKI-ALIGN:A16     4 4 MAKI-LAYOUT:ROW MP-CLASSIFY CO-COALESCED-V4 T=
MAKI-ALIGN:A16     4 2 MAKI-LAYOUT:ROW MP-CLASSIFY CO-COALESCED    T=   \ v2 folds into "coalesced"
MAKI-ALIGN:A8      4 4 MAKI-LAYOUT:ROW MP-CLASSIFY CO-COALESCED    T=
MAKI-ALIGN:UNKNOWN 4 4 MAKI-LAYOUT:ROW MP-CLASSIFY CO-UNALIGNED    T=
MAKI-ALIGN:A16     4 4 MAKI-LAYOUT:COL MP-CLASSIFY CO-STRIDED      T=

\ a raw n where the classifier expects an align family is a CHECKER reject
\ (replaces the old AL-N -> E-MP-ALIGN runtime throw, now unrepresentable)
s" MPT-AL-NIN ( n n n layout -- n ) MP-CLASSIFY" CHECK-QUIET-CANDIDATE! 0 T=
s" MPT-LAY-NIN ( align n n n -- n ) MP-CLASSIFY" CHECK-QUIET-CANDIDATE! 0 T=

\ ---- slot-alignment fact: default AL-UNKNOWN, setter records, fails closed ---
MIR-RESET
2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
1 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
0 MIR-SLOT-ID MIR-SLOT-AL@ ALIGN>N AL-UNKNOWN T=            \ unrecorded default
0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!
0 MIR-SLOT-ID MIR-SLOT-AL@ ALIGN>N AL-16 T=                 \ recorded
1 MIR-SLOT-ID MIR-SLOT-AL@ ALIGN>N AL-UNKNOWN T=            \ untouched slot unchanged
\ a raw n where the setter expects an align family is a CHECKER reject
\ (replaces the old AL-N -> E-MIR-ALIGN runtime throw, now unrepresentable)
s" MPT-AL-BAD ( MIR:input-slot n -- ) MIR-SLOT-AL!" CHECK-QUIET-CANDIDATE! 0 T=
: TRY-AL-SLOT ( -- )  MIR-RESET 5 MIR-SLOT-ID MIR-SLOT-AL@ ALIGN>N drop ;
' TRY-AL-SLOT E-MIR-INSLOT TTHROWS

\ ---- typed no-slot cursor (option<MIR:input-slot>) --------------------------
\ the old raw -1 sentinel is unrepresentable: a raw n, an unwrapped slot, and a
\ raw fetch are CHECKER rejects; only the some/none constructors cross the cell
s" MPT-SLOT-RAW  ( n -- ) MP-SLOT-AT !"              CHECK-QUIET-CANDIDATE! 0 T=
s" MPT-SLOT-BARE ( MIR:input-slot -- ) MP-SLOT-AT !" CHECK-QUIET-CANDIDATE! 0 T=
s" MPT-SLOT-NFT  ( -- n ) MP-SLOT-AT @"              CHECK-QUIET-CANDIDATE! 0 T=
s" MPT-SLOT-OK   ( MIR:input-slot -- ) MP-SLOT!"     CHECK-QUIET-CANDIDATE! -1 T=
\ a staged node write has NO slot: the align-warning reader fails closed on the
\ structural none instead of rendering a stale slot number
: TRY-MP-NOSLOT ( -- )  MP-SLOT@ SLOT>RAW drop ;
2 4 MAKI-LAYOUT:ROW MP-EW
1 MIR-NODE-ID MP-SET-NODE
' TRY-MP-NOSLOT E-MP-NOSLOT TTHROWS
0 MIR-SLOT-ID MP-SET-SLOT                            \ a slot read stages some
MP-SLOT@ SLOT>RAW 0 T=

\ ---- (1) 2x4 f32 chain, 16B RECORDED input -> coalesced-v4 -----------------
2 4 MAKI-LAYOUT:ROW MP-EW  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup REPORT:HOT-COUNT 2 T=
dup 0 REPORT:HOT-NAME@   s" i0" T$=
dup 0 REPORT:HOT-STATUS@ CO-COALESCED-V4 T=            \ input read
dup 1 REPORT:HOT-NAME@   s" n1" T$=
dup 1 REPORT:HOT-STATUS@ CO-COALESCED-V4 T=            \ materialized output write
REPORT:RENDER PT-SAVE
s" memory.align" PT-NOTIN
s" memory.tail"  PT-NOTIN

\ ---- (2) same chain, unrecorded input (AL-UNKNOWN) -> scalar + warning ------
2 4 MAKI-LAYOUT:ROW MP-EW  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-STATUS@ CO-UNALIGNED    T=            \ input falls back to scalar
dup 1 REPORT:HOT-STATUS@ CO-COALESCED-V4 T=            \ compiler-allocated output still v4
REPORT:RENDER PT-SAVE
s" coalesce.status.0: unaligned"                      PT-IN
s" memory.align: input 0 unknown alignment -> scalar" PT-IN

\ ---- (3) column-major input -> strided -------------------------------------
2 4 MAKI-LAYOUT:COL MP-EW  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-STATUS@ CO-STRIDED T=
REPORT:RENDER PT-SAVE  s" coalesce.status.0: strided" PT-IN

\ ---- (4) extent 2x5 -> masked-tail row (5 mod 4 = 1) -----------------------
2 5 MAKI-LAYOUT:ROW MP-EW  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-STATUS@ CO-COALESCED-V4 T=            \ v4 with a masked tail
REPORT:RENDER PT-SAVE  s" memory.tail: i0 5 mod 4 = 1" PT-IN

\ ---- (5) 1xC bias into a 2D op -> broadcast-register -----------------------
MIR-RESET
2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!    \ slot0 x  (2x4)
1 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  1 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!    \ slot1 b  (1x4)
MAKI-OPKIND:ADD MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 1 REPORT:HOT-NAME@   s" i1" T$=
dup 1 REPORT:HOT-STATUS@ CO-BROADCAST T=
drop

\ ---- (6) gather data read -> gathered --------------------------------------
MIR-RESET
4 8 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  0 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!    \ slot0 x   (data)
3 1 SHAPE MAKI-DTYPE:DI32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  1 MIR-SLOT-ID MAKI-ALIGN:A16 MIR-SLOT-AL!    \ slot1 idx
MAKI-OPKIND:GATHER MIR-OP-BEGIN 0 MIR-SLOT-ID MIR-IN-REF MIR-IN+ 1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   3 8 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-GATHER MVV-GATHERED 0 0 MV-PACK  1  MIR-OP+ drop
FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-NAME@   s" i0" T$=
dup 0 REPORT:HOT-STATUS@ CO-GATHERED T=
drop

T-REPORT

;package
