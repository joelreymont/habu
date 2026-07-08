\ maki/mem-plan-test.f - checked tests for the cad-3 memory coalescing planner.
\ Vector-width rules, the CO-* access classifier, the recorded slot-alignment fact,
\ and the per-hot MEMORY rows (coalesced-v4 / unaligned+warning / strided / masked
\ tail / broadcast / gathered) over hand-built model IR so alignment is controllable.

require lib/test.f
require lib/string.f
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
: MP-EW ( n n n -- )  {: rows:n cols:n lay:n :}    \ x:rows x cols (lay) -> GELU RELU
   MIR-RESET
   rows cols DT-F32 lay MIR-INPUT+ drop
   OP-GELU MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ rows cols DT-F32 LAY-ROW 0 1 MIR-OP+ drop
   OP-RELU MIR-OP-BEGIN 0 MIR-IN+        rows cols DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;

T-RESET

\ ---- vector-width rule (align esize extent -- w) ---------------------------
AL-16      4 4 MP-W 4 T=                            \ 16B / f32 / >=4 -> v4
AL-16      4 3 MP-W 2 T=                            \ extent 3 < 4 -> v2
AL-8       4 4 MP-W 2 T=                            \ 8B -> at most v2
AL-4       4 4 MP-W 1 T=                            \ 4B -> scalar
AL-UNKNOWN 4 8 MP-W 1 T=                            \ unknown -> scalar

\ ---- classifier (align esize extent layout -- status) ----------------------
AL-16      4 4 LAY-ROW MP-CLASSIFY CO-COALESCED-V4 T=
AL-16      4 2 LAY-ROW MP-CLASSIFY CO-COALESCED    T=   \ v2 folds into "coalesced"
AL-8       4 4 LAY-ROW MP-CLASSIFY CO-COALESCED    T=
AL-UNKNOWN 4 4 LAY-ROW MP-CLASSIFY CO-UNALIGNED    T=
AL-16      4 4 LAY-COL MP-CLASSIFY CO-STRIDED      T=

: TRY-MP-ALIGN ( -- )  AL-N 4 4 LAY-ROW MP-CLASSIFY drop ;   \ bad align class fails closed
' TRY-MP-ALIGN E-MP-ALIGN TTHROWS

\ ---- slot-alignment fact: default AL-UNKNOWN, setter records, fails closed ---
MIR-RESET
2 4 DT-F32 LAY-ROW MIR-INPUT+ drop
1 1 DT-F32 LAY-ROW MIR-INPUT+ drop
0 MIR-SLOT-AL@ AL-UNKNOWN T=                        \ unrecorded default
0 AL-16 MIR-SLOT-AL!
0 MIR-SLOT-AL@ AL-16 T=                             \ recorded
1 MIR-SLOT-AL@ AL-UNKNOWN T=                        \ untouched slot unchanged
: TRY-AL-BAD  ( -- )  MIR-RESET 1 1 DT-F32 LAY-ROW MIR-INPUT+ drop  0 AL-N MIR-SLOT-AL! ;
: TRY-AL-SLOT ( -- )  MIR-RESET 5 MIR-SLOT-AL@ drop ;
' TRY-AL-BAD  E-MIR-ALIGN  TTHROWS
' TRY-AL-SLOT E-MIR-INSLOT TTHROWS

\ ---- (1) 2x4 f32 chain, 16B RECORDED input -> coalesced-v4 -----------------
2 4 LAY-ROW MP-EW  0 AL-16 MIR-SLOT-AL!  FP-BUILD
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
2 4 LAY-ROW MP-EW  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-STATUS@ CO-UNALIGNED    T=            \ input falls back to scalar
dup 1 REPORT:HOT-STATUS@ CO-COALESCED-V4 T=            \ compiler-allocated output still v4
REPORT:RENDER PT-SAVE
s" coalesce.status.0: unaligned"                      PT-IN
s" memory.align: input 0 unknown alignment -> scalar" PT-IN

\ ---- (3) column-major input -> strided -------------------------------------
2 4 LAY-COL MP-EW  0 AL-16 MIR-SLOT-AL!  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-STATUS@ CO-STRIDED T=
REPORT:RENDER PT-SAVE  s" coalesce.status.0: strided" PT-IN

\ ---- (4) extent 2x5 -> masked-tail row (5 mod 4 = 1) -----------------------
2 5 LAY-ROW MP-EW  0 AL-16 MIR-SLOT-AL!  FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-STATUS@ CO-COALESCED-V4 T=            \ v4 with a masked tail
REPORT:RENDER PT-SAVE  s" memory.tail: i0 5 mod 4 = 1" PT-IN

\ ---- (5) 1xC bias into a 2D op -> broadcast-register -----------------------
MIR-RESET
2 4 DT-F32 LAY-ROW MIR-INPUT+ drop  0 AL-16 MIR-SLOT-AL!    \ slot0 x  (2x4)
1 4 DT-F32 LAY-ROW MIR-INPUT+ drop  1 AL-16 MIR-SLOT-AL!    \ slot1 b  (1x4)
OP-ADD MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ 1 MIR-IN-REF MIR-IN+ 2 4 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 1 REPORT:HOT-NAME@   s" i1" T$=
dup 1 REPORT:HOT-STATUS@ CO-BROADCAST T=
drop

\ ---- (6) gather data read -> gathered --------------------------------------
MIR-RESET
4 8 DT-F32 LAY-ROW MIR-INPUT+ drop  0 AL-16 MIR-SLOT-AL!    \ slot0 x   (data)
3 1 DT-I32 LAY-ROW MIR-INPUT+ drop  1 AL-16 MIR-SLOT-AL!    \ slot1 idx
OP-GATHER MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ 1 MIR-IN-REF MIR-IN+
   3 8 DT-F32 LAY-ROW  MV-GATHER MVV-GATHERED 0 0 MV-PACK  1  MIR-OP+ drop
FP-BUILD
REPORT:NEW MEM-PLAN-INTO
dup 0 REPORT:HOT-NAME@   s" i0" T$=
dup 0 REPORT:HOT-STATUS@ CO-GATHERED T=
drop

T-REPORT

end-package
