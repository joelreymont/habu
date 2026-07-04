\ maki/schedule-test.f - checked tests for the cad-4 schedule families.
\ Family metadata + selection, the mixed-radix index<->parameter-tuple enumeration,
\ the rendered candidate rows, the closed-form section 7.2 defaults, the pow2/ceil
\ math, the `sched` value-record, and the fail-closed family/index throws.

require lib/test.f
require lib/string.f
require maki/schedule.f

package MAKI

T-RESET

\ ---- family metadata --------------------------------------------------------
FAM-N 5 T=
FAM-ELEMENTWISE FAM-NAME s" elementwise-v1" T$=
FAM-ROW-REDUCE  FAM-NAME s" row-reduce-v1"  T$=
FAM-SOFTMAX-ROW FAM-NAME s" softmax-row-v1" T$=
FAM-GEMM-TF32   FAM-NAME s" gemm-tf32-v1"   T$=
FAM-DECODE      FAM-NAME s" decode-v1"      T$=

FAM-ELEMENTWISE FAM-SPACE 18 T=
FAM-ROW-REDUCE  FAM-SPACE 36 T=
FAM-SOFTMAX-ROW FAM-SPACE 72 T=
FAM-GEMM-TF32   FAM-SPACE 32 T=
FAM-DECODE      FAM-SPACE 4  T=

\ ---- family selection from a class bitmask + softmax-op flag ----------------
: MIX ( n -- n )  1 swap lshift ;                  \ class -> its bitmask bit
CLASS-MATMUL MIX false FAM-SELECT FAM-GEMM-TF32 T=
CLASS-ROW-REDUCE MIX true  FAM-SELECT FAM-SOFTMAX-ROW T=
CLASS-ROW-REDUCE MIX false FAM-SELECT FAM-ROW-REDUCE  T=
CLASS-DECODE MIX false FAM-SELECT FAM-DECODE T=
CLASS-EW MIX false FAM-SELECT FAM-ELEMENTWISE T=
CLASS-MATMUL MIX CLASS-EW MIX or false FAM-SELECT FAM-GEMM-TF32 T=  \ matmul wins first

\ ---- enumeration: index -> parameter tuple (checked at the space corners) ----
0  EW-DECODE 0 T= 1 T= 128 T=                       \ gs=0 vec=1 block=128
17 EW-DECODE 1 T= 4 T= 512 T=                       \ gs=1 vec=4 block=512
0  RR-DECODE 1 T= 1 T= 32  T=                        \ vec=1 rows=1 lanes=32
35 RR-DECODE 4 T= 4 T= 256 T=                        \ vec=4 rows=4 lanes=256
0  SM-DECODE 0 T= 1 T= 1 T= 32 T=                    \ online=0 vec=1 rows=1 lanes=32
71 SM-DECODE 1 T= 4 T= 4 T= 256 T=                   \ online=1 vec=4 rows=4 lanes=256
0  GEMM-DECODE 1 T= 4 T= 32 T= 64 T= 64 T=           \ stages warps bk bn bm
31 GEMM-DECODE 2 T= 8 T= 64 T= 128 T= 128 T=
0  DEC-DECODE 0 T= 0 T=
3  DEC-DECODE 1 T= 1 T=

\ ---- rendered candidate rows ("family k=.. k=.." style) ---------------------
FAM-ELEMENTWISE 11 CAND$ s" elementwise-v1 block=256 vec=4 grid-stride=y" T$=
FAM-ROW-REDUCE  18 CAND$ s" row-reduce-v1 lanes=128 rows=1 vec=1" T$=
FAM-SOFTMAX-ROW 36 CAND$ s" softmax-row-v1 lanes=128 rows=1 vec=1 online=n" T$=
FAM-GEMM-TF32    0 CAND$ s" gemm-tf32-v1 bm=64 bn=64 bk=32 warps=4 stages=1" T$=
FAM-DECODE       3 CAND$ s" decode-v1 block-row=y ballot=y" T$=

\ ---- generic pow2 / ceil-div math ------------------------------------------
0   NEXT-POW2 1   T=
1   NEXT-POW2 1   T=
2   NEXT-POW2 2   T=
100 NEXT-POW2 128 T=
128 NEXT-POW2 128 T=
128 POW2? TTRUE
100 POW2? TFALSE
1   POW2? TTRUE
0   POW2? TFALSE
1024 8 CEIL-DIV 128 T=
1023 8 CEIL-DIV 128 T=
1025 8 CEIL-DIV 129 T=

\ ---- closed-form defaults (section 7.2) ------------------------------------
4 EW-DEFAULT 11 T=                                   \ block=256 vec=4 grid-stride=y
2 EW-DEFAULT 9  T=
1 EW-DEFAULT 7  T=
1024 RR-DEFAULT 18 T=                                \ lanes=128 rows=1 vec=1
8    RR-DEFAULT 0  T=                                 \ clamps up to lanes=32
2048 RR-DEFAULT 27 T=                                 \ lanes=256
1024 SM-DEFAULT 36 T=                                 \ row-reduce default, online off
GEMM-DEFAULT 0 T=
DEC-DEFAULT  3 T=
FAM-ELEMENTWISE 0 4 FAM-DEFAULT 11 T=                 \ rowlen ignored, maxvec=4
FAM-ROW-REDUCE  1024 1 FAM-DEFAULT 18 T=
FAM-SOFTMAX-ROW 1024 1 FAM-DEFAULT 36 T=
FAM-GEMM-TF32   0 0 FAM-DEFAULT 0 T=
FAM-DECODE      0 0 FAM-DEFAULT 3 T=

\ ---- default candidate must render inside its family space (no throw) -------
FAM-ELEMENTWISE  FAM-ELEMENTWISE 0 4 FAM-DEFAULT  CAND$ s" elementwise-v1 block=256 vec=4 grid-stride=y" T$=

\ ---- schedule instance value-record ----------------------------------------
7 FAM-GEMM-TF32 5 >SCHED SCHED-REGION 7 T=
7 FAM-GEMM-TF32 5 >SCHED SCHED-FAM    FAM-GEMM-TF32 T=
7 FAM-GEMM-TF32 5 >SCHED SCHED-CAND   5 T=
7 FAM-GEMM-TF32 0 SCHED-ROW$ s" region=7 gemm-tf32-v1 bm=64 bn=64 bk=32 warps=4 stages=1" T$=

\ ---- fail-closed throws -----------------------------------------------------
: BAD-NAME  ( -- )  FAM-N FAM-NAME 2drop ;
: BAD-SPACE ( -- )  FAM-N FAM-SPACE drop ;
: BAD-CAND  ( -- )  FAM-ELEMENTWISE EW-SPACE CAND$ 2drop ;
: BAD-DEF   ( -- )  FAM-N 0 0 FAM-DEFAULT drop ;
: BAD-AXIS  ( -- )  9 BLOCK-AXIS drop ;
' BAD-NAME  E-SCHED-FAM TTHROWS
' BAD-SPACE E-SCHED-FAM TTHROWS
' BAD-CAND  E-SCHED-IDX TTHROWS
' BAD-DEF   E-SCHED-FAM TTHROWS
' BAD-AXIS  E-SCHED-IDX TTHROWS

T-REPORT

end-package
