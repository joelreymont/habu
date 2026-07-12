\ maki/schedule.f - schedule families, parameter spaces + closed-form defaults (cad-4).
\
\ CAD-PLAN section 7.1/7.2. A schedule never free-forms: it instantiates one of five
\ FAMILIES over a bounded, enumerable parameter space, which is what makes tuning
\ tractable and replay exact. This file is the family data + the pure enumeration and
\ default-selection math; the cache key (region signature / shape class / replay table)
\ is maki/sched-key.f, and the TILE/TUNE report wiring is maki/cad.f. One concern:
\ family selection, index<->parameter-tuple enumeration, and the section 7.2 defaults.
\
\ Families (v1) and their spaces (index -> parameter tuple, mixed-radix decode):
\   elementwise-v1  block{128,256,512} x vec{1,2,4} x grid-stride{n,y}        = 18
\   row-reduce-v1   lanes{32,64,128,256} x rows/block{1,2,4} x vec{1,2,4}     = 36
\   softmax-row-v1  row-reduce-v1 x online-softmax{n,y}                       = 72
\   gemm-tf32-v1    bm{64,128} x bn{64,128} x bk{32,64} x warps{4,8} x        = 32
\                   stages{1,2}   (epilogue is FIXED by the fusion plan, never searched)
\   decode-v1       block/row{n,y} x ballot-compaction{n,y}                   = 4
\
\ Defaults are closed-form BEFORE any tuning (section 7.2): elementwise 256 threads,
\ max legal vec (caller-supplied from mem-plan facts, else 1), grid-stride on;
\ row-reduce lanes/row = min(256, next-pow2(ceil(rowlen/8))) clamped to the axis, one
\ row/block, vec 1 (two-pass); softmax = row-reduce with online-softmax off; gemm the
\ smallest tile (64x64x32, 4 warps, 1 stage) - there is no occupancy model yet (cad-6);
\ decode block/row on, ballot on. All deterministic and tested.
\
\ A schedule instance is a `sched` value-record = region id + family id + candidate
\ index (the index recovers the parameter tuple via the family decode). The section 7.1
\ identity facts target/shape-class/dtype/layout live in the key (maki/sched-key.f), and
\ expected registers/smem + measurement history are the cad-5/cad-6 seams, not fields yet.
\
\ Fail closed: family id and candidate index out of range are named throws. maki -> habu
\ only; schedule owns -5080..-5081.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-registry.f

-5080 constant E-SCHED-FAM     \ family id out of range
-5081 constant E-SCHED-IDX     \ candidate index out of range (per-family space)

VALUE-RECORD sched rgn n fam n cnd n END-VALUE-RECORD

package MAKI
public

\ ---- family ids (selected from a region's class mix) -----------------------
0 constant FAM-ELEMENTWISE     \ elementwise-v1
1 constant FAM-ROW-REDUCE      \ row-reduce-v1
2 constant FAM-SOFTMAX-ROW     \ softmax-row-v1
3 constant FAM-GEMM-TF32       \ gemm-tf32-v1
4 constant FAM-DECODE          \ decode-v1
5 constant FAM-N               \ range bound

\ ---- per-family candidate-space sizes (product of the axis cardinalities) ---
18 constant EW-SPACE           \ 3 block x 3 vec x 2 grid-stride
36 constant RR-SPACE           \ 4 lanes x 3 rows x 3 vec
72 constant SM-SPACE           \ RR-SPACE x 2 online
32 constant GEMM-SPACE         \ 2 bm x 2 bn x 2 bk x 2 warps x 2 stages
4  constant DEC-SPACE          \ 2 block/row x 2 ballot

\ ---- generic integer math (shared with maki/sched-key.f shape bucketing) ----
: NEXT-POW2 ( n -- n ) {: v:n :}          \ smallest power of two >= v (v<2 -> 1)
   v 2 < if 1 exit then
   1 begin dup v < while 1 lshift repeat ;

: POW2? ( n -- bool ) {: v:n :}
   v 1 < if false exit then
   v NEXT-POW2 v = ;

: CEIL-DIV ( n n -- n ) {: a:n b:n :}  a b 1- + b / ;
private

\ ---- axis index -> concrete parameter value (fail closed on a bad index) -----
: BLOCK-AXIS ( n -- n )
   case  0 of 128 endof  1 of 256 endof  2 of 512 endof  E-SCHED-IDX throw  endcase ;
: VEC-AXIS ( n -- n )
   case  0 of 1 endof  1 of 2 endof  2 of 4 endof  E-SCHED-IDX throw  endcase ;
: LANES-AXIS ( n -- n )
   case 0 of 32 endof 1 of 64 endof 2 of 128 endof 3 of 256 endof E-SCHED-IDX throw endcase ;
: ROWS-AXIS ( n -- n )
   case  0 of 1 endof  1 of 2 endof  2 of 4 endof  E-SCHED-IDX throw  endcase ;
: TDIM-AXIS ( n -- n )  0= if 64 else 128 then ;   \ gemm bm / bn tile dim
: BK-AXIS ( n -- n )    0= if 32 else 64 then ;
: WARP-AXIS ( n -- n )  0= if 4 else 8 then ;
: STAGE-AXIS ( n -- n ) 0= if 1 else 2 then ;

\ ---- concrete value -> axis index (default-selection direction) -------------
: VEC>I ( n -- n ) {: vec:n :}            \ clamp a vector width to its axis index
   vec 4 >= if 2 exit then
   vec 2 = if 1 else 0 then ;

: LANES>I ( n -- n ) {: lanes:n :}        \ clamped lanes {32,64,128,256} -> {0..3}
   0  32 begin dup lanes < while  1 lshift swap 1+ swap  repeat drop ;

: YN+ ( n -- )                            \ append a boolean-axis token (1 -> y, else n)
   1 = if s" y" else s" n" then SB-APPEND ;

public

\ ---- family metadata --------------------------------------------------------
: FAM-NAME ( n -- ptr u8 n )
   case
      FAM-ELEMENTWISE of s" elementwise-v1" endof
      FAM-ROW-REDUCE  of s" row-reduce-v1"  endof
      FAM-SOFTMAX-ROW of s" softmax-row-v1" endof
      FAM-GEMM-TF32   of s" gemm-tf32-v1"   endof
      FAM-DECODE      of s" decode-v1"      endof
      E-SCHED-FAM throw
   endcase ;

: FAM-SPACE ( n -- n )
   case
      FAM-ELEMENTWISE of EW-SPACE   endof
      FAM-ROW-REDUCE  of RR-SPACE   endof
      FAM-SOFTMAX-ROW of SM-SPACE   endof
      FAM-GEMM-TF32   of GEMM-SPACE endof
      FAM-DECODE      of DEC-SPACE  endof
      E-SCHED-FAM throw
   endcase ;

\ ---- family selection from a region's class bitmask (FP-REGION-CLASSMIX) -----
\ softmax? is true when the region carries a softmax-row op (two row reductions);
\ matmul wins first (a region never mixes a contraction with a reduction).
: CLASS-IN? ( n n -- bool ) {: mix:n c:n :}  1 c lshift mix and 0= 0= ;

: FAM-SELECT ( n bool -- n ) {: mix:n sm:bool :}
   mix CLASS-MATMUL CLASS-IN? if FAM-GEMM-TF32 exit then
   mix CLASS-ROW-REDUCE CLASS-IN? if
      sm if FAM-SOFTMAX-ROW else FAM-ROW-REDUCE then exit then
   mix CLASS-DECODE CLASS-IN? if FAM-DECODE exit then
   FAM-ELEMENTWISE ;

\ ---- candidate enumeration: index -> parameter tuple (mixed-radix decode) ----
: EW-DECODE ( n -- n n n ) {: idx:n :}    \ idx -> block vec grid-stride(0/1)
   idx 2 mod {: gs:n :}
   idx 2 /   {: i2:n :}
   i2 3 mod VEC-AXIS {: vec:n :}
   i2 3 /   BLOCK-AXIS {: block:n :}
   block vec gs ;

: RR-DECODE ( n -- n n n ) {: idx:n :}    \ idx -> lanes rows vec
   idx 3 mod VEC-AXIS {: vec:n :}
   idx 3 /   {: i2:n :}
   i2 3 mod ROWS-AXIS {: rows:n :}
   i2 3 /   LANES-AXIS {: lanes:n :}
   lanes rows vec ;

: SM-DECODE ( n -- n n n n ) {: idx:n :}  \ idx -> lanes rows vec online(0/1)
   idx 2 mod {: online:n :}
   idx 2 /   RR-DECODE {: lanes:n rows:n vec:n :}
   lanes rows vec online ;

: GEMM-DECODE ( n -- n n n n n ) {: idx:n :}   \ idx -> bm bn bk warps stages
   idx 2 mod STAGE-AXIS {: stages:n :}
   idx 2 /   {: i2:n :}
   i2 2 mod WARP-AXIS {: warps:n :}
   i2 2 /    {: i3:n :}
   i3 2 mod BK-AXIS {: bk:n :}
   i3 2 /    {: i4:n :}
   i4 2 mod TDIM-AXIS {: bn:n :}
   i4 2 /    TDIM-AXIS {: bm:n :}
   bm bn bk warps stages ;

: DEC-DECODE ( n -- n n ) {: idx:n :}     \ idx -> block/row(0/1) ballot(0/1)
   idx 2 mod {: ballot:n :}
   idx 2 / 2 mod {: br:n :}
   br ballot ;

private

\ ---- rendered candidate rows (append into the shared string builder) --------
: EW-CAND+ ( n -- ) {: idx:n :}
   idx EW-DECODE {: block:n vec:n gs:n :}
   s" elementwise-v1 block=" SB-APPEND block SB-INT
   s"  vec=" SB-APPEND vec SB-INT
   s"  grid-stride=" SB-APPEND gs YN+ ;

: RR-CAND+ ( n -- ) {: idx:n :}
   idx RR-DECODE {: lanes:n rows:n vec:n :}
   s" row-reduce-v1 lanes=" SB-APPEND lanes SB-INT
   s"  rows=" SB-APPEND rows SB-INT
   s"  vec=" SB-APPEND vec SB-INT ;

: SM-CAND+ ( n -- ) {: idx:n :}
   idx SM-DECODE {: lanes:n rows:n vec:n online:n :}
   s" softmax-row-v1 lanes=" SB-APPEND lanes SB-INT
   s"  rows=" SB-APPEND rows SB-INT
   s"  vec=" SB-APPEND vec SB-INT
   s"  online=" SB-APPEND online YN+ ;

: GEMM-CAND+ ( n -- ) {: idx:n :}
   idx GEMM-DECODE {: bm:n bn:n bk:n warps:n stages:n :}
   s" gemm-tf32-v1 bm=" SB-APPEND bm SB-INT
   s"  bn=" SB-APPEND bn SB-INT
   s"  bk=" SB-APPEND bk SB-INT
   s"  warps=" SB-APPEND warps SB-INT
   s"  stages=" SB-APPEND stages SB-INT ;

: DEC-CAND+ ( n -- ) {: idx:n :}
   idx DEC-DECODE {: br:n ballot:n :}
   s" decode-v1 block-row=" SB-APPEND br YN+
   s"  ballot=" SB-APPEND ballot YN+ ;

public

\ append the rendered "family k=.. k=.." row for one candidate to the builder
: CAND+ ( n n -- ) {: fam:n idx:n :}
   idx 0 < idx fam FAM-SPACE >= or if E-SCHED-IDX throw then
   fam case
      FAM-ELEMENTWISE of idx EW-CAND+   endof
      FAM-ROW-REDUCE  of idx RR-CAND+   endof
      FAM-SOFTMAX-ROW of idx SM-CAND+   endof
      FAM-GEMM-TF32   of idx GEMM-CAND+ endof
      FAM-DECODE      of idx DEC-CAND+  endof
      E-SCHED-FAM throw
   endcase ;

: CAND$ ( n n -- ptr u8 n )  SB-RESET CAND+ SB$ ;   \ fam idx -> rendered row

\ ---- closed-form default selection (section 7.2), returns a candidate index --
: EW-DEFAULT ( n -- n ) {: maxvec:n :}    \ block=256, vec=max-legal, grid-stride=y
   1 3 *  maxvec VEC>I +  2 *  1 + ;

: RR-LANES ( n -- n ) {: rowlen:n :}      \ min(256, next-pow2(ceil(rowlen/8))), >=32
   rowlen 8 CEIL-DIV NEXT-POW2
   dup 32 < if drop 32 then
   dup 256 > if drop 256 then ;

: RR-DEFAULT ( n -- n ) {: rowlen:n :}    \ lanes=formula, rows/block=1, vec=1
   rowlen RR-LANES LANES>I 3 * 0 + 3 * 0 + ;

: SM-DEFAULT ( n -- n )  RR-DEFAULT 2 * ; \ row-reduce default, online-softmax off

: GEMM-DEFAULT ( -- n )  0 ;              \ 64x64x32, 4 warps, 1 stage (no occupancy model)

: DEC-DEFAULT ( -- n )   3 ;              \ block/row on, ballot on

\ fam + context (rowlen, max legal vec) -> default candidate index
: FAM-DEFAULT ( n n n -- n ) {: fam:n rowlen:n maxvec:n :}
   fam case
      FAM-ELEMENTWISE of maxvec EW-DEFAULT endof
      FAM-ROW-REDUCE  of rowlen RR-DEFAULT endof
      FAM-SOFTMAX-ROW of rowlen SM-DEFAULT endof
      FAM-GEMM-TF32   of GEMM-DEFAULT      endof
      FAM-DECODE      of DEC-DEFAULT       endof
      E-SCHED-FAM throw
   endcase ;

\ ---- schedule instance (region id + family id + candidate index) ------------
: >SCHED ( n n n -- sched ) ;             \ region fam cand -> sched (zero-cost)
: SCHED-REGION ( sched -- n )  2drop ;
: SCHED-FAM    ( sched -- n )  drop nip ;
: SCHED-CAND   ( sched -- n )  nip nip ;

\ render "region=<r> <candidate row>" for a schedule's fields
: SCHED-ROW$ ( n n n -- ptr u8 n ) {: region:n fam:n cand:n :}
   SB-RESET  s" region=" SB-APPEND  region SB-INT  $20 SB-APPEND-C
   fam cand CAND+  SB$ ;

;package
