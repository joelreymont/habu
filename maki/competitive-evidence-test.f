\ maki/competitive-evidence-test.f - acceptance for the checked per-side competitive
\ EVIDENCE row + metric UNITS vocabulary (maki/competitive-evidence.f, dot
\ habu-v2-competitive-evidence-5d07d471).
\
\ Pins the dot's acceptance:
\   - the migrated flagship rows render byte-stably (the GEMM 3026.6 GFLOP/s =
\     1.60x Triton row and the SAXPY-V4 64.209 GB/s = Triton 63.0 row), each an
\     exact versioned golden; a rebuild + re-render is byte-identical (replay);
\   - every metric UNIT (ns/ms/gflops/gbps/bytes/count/watts) renders;
\   - a comparison ROW refuses a mismatched numeric DOMAIN (the historical
\     Habu-FP32-exact vs Triton-TF32-relative confusion) - runtime verdict 0 AND
\     the E-CEVID-INCOMPARABLE throw - each with a resolving positive;
\   - cache-state is an explicit field: cold vs warm alters the rendered key;
\   - a reading whose unit is outside its field category is the E-CEVID-UNIT throw;
\   - over-capacity / oversized-row throws are the named E-CEVID codes;
\   - the numeric-policy / raw-n / identity-slot confusions are STATIC checker
\     rejects (CHECK-QUIET-CANDIDATE! 0), each with a resolving positive.
\
\ Fixture numbers are the real device measurements: MMM-WIDE-B-M4-S1 3026.577
\ GFLOP/s vs Triton 1890.5 (tools/ptx/perf-rows.tsv, orin-nx-25w-918mhz, 2048^3,
\ both TF32/relative); SAXPY-V4 64.209 GB/s vs Triton 63.0 (both FP32/exact);
\ Triton GEMM time_ms=272.6 (docs/eval-triton.md); GEMM smem 50176 B; SAXPY bytes
\ 200*3*2^20*4 = 2516582400; ns figure the step-1 blocked 2048^3 gpu_elapsed_ns.
\ Rate units and ms are milli-scaled (x1000); ns/bytes/count/watts are natural.

require lib/test.f
require lib/string.f
require test/checker-assert.f       \ CHECK-QUIET-CANDIDATE! for the static-reject negatives
require maki/evidence/schema.f      \ EVID:prec-class for the numeric-policy-confusion negative
require maki/competitive-evidence.f

package CEVID

\ ---- stable copy (the shared SB render is overwritten by the next render) --------
1024 constant CT-CAP
create CT-BUF CT-CAP allot  variable CT-BU
: CT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a CT-BUF u BYTE-COPY  u CT-BU ! ;
: CT-BUF$ ( -- ptr u8 n )  CT-BUF CT-BU @ ;

\ ==== the migrated flagship evidence rows (real device numbers, milli/natural) ====
\ GEMM subject: Habu MMM-WIDE-B-M4-S1 TF32 tensor-core tile, the 3026.6 = 1.60x flagship.
: GEMM-HABU ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MMM-WIDE-B BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   3026577 >GFLOPS THR!
   30 >LAUNCHES LAUNCH!
   50176 >BYTES MEM!
   25 >WATTS ENERGY! ;
\ GEMM baseline: Triton autotuned TF32 tl.dot, 1890.5 GFLOP/s, time_ms=272.6.
: GEMM-TRITON ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:TRITON-TUNED BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:TRITON-351 BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   272600 >MS LAT!
   1890500 >GFLOPS THR!
   30 >LAUNCHES LAUNCH!
   25 >WATTS ENERGY! ;
\ SAXPY subject: Habu SAXPY-V4 64.209 GB/s (FP32 exact).
: SAXPY-HABU ( -- evidence )
   BENCH-WORKLOAD:SAXPY CEVID-REVISION:SAXPY-V4 BENCH-SHAPE:N1M NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON ROW
   64209 >GBPS THR!
   2516582400 >BYTES BYT!
   200 >LAUNCHES LAUNCH!
   25 >WATTS ENERGY! ;
\ SAXPY baseline: Triton JIT 63.0 GB/s (FP32 exact).
: SAXPY-TRITON ( -- evidence )
   BENCH-WORKLOAD:SAXPY CEVID-REVISION:TRITON-JIT BENCH-SHAPE:N1M NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:TRITON-351 BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON ROW
   63000 >GBPS THR!
   2516582400 >BYTES BYT!
   200 >LAUNCHES LAUNCH!
   25 >WATTS ENERGY! ;

\ the SAME GEMM subject but cache=cold: one exact-key field changed.
: GEMM-HABU-COLD ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MMM-WIDE-B BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:COLD
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   3026577 >GFLOPS THR!  30 >LAUNCHES LAUNCH!  50176 >BYTES MEM!  25 >WATTS ENERGY! ;

\ the historical Habu blocked FP32 tile (exact) - incomparable-by-policy vs Triton TF32.
: GEMM-HABU-FP32 ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MM-BLOCKED BENCH-SHAPE:SQ2048 NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   441800 >GFLOPS THR!  30 >LAUNCHES LAUNCH!  25 >WATTS ENERGY! ;

\ a row exercising the ns unit + the all-na default readings (real corpus figure).
: NS-ROW ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MM-BLOCKED BENCH-SHAPE:SQ2048 NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   1354517944 >NS LAT! ;

\ ---- comparability probes ----------------------------------------------------
: GEMM-CMP ( -- bool )  GEMM-HABU GEMM-TRITON COMPARABLE? ;      \ both relative -> comparable
: SAXPY-CMP ( -- bool )  SAXPY-HABU SAXPY-TRITON COMPARABLE? ;   \ both exact    -> comparable
: BAD-CMP ( -- bool )  GEMM-HABU-FP32 GEMM-TRITON COMPARABLE? ;  \ exact vs relative -> NOT comparable

\ ---- fail-closed triggers ----------------------------------------------------
: TRY-INCOMPARABLE ( -- )  GEMM-HABU-FP32 GEMM-TRITON RENDER-PAIR 2drop ;  \ incomparable pair as a row
: TRY-LAT-RATE ( -- )  SAXPY-HABU 63000 >GBPS LAT! drop ;   \ a rate reading in the latency (duration) slot
: TRY-LAT-NS-OK ( -- )  SAXPY-HABU 100 >NS LAT! drop ;      \ a duration reading in the latency slot: OK
: TRY-CAP-HI ( -- )  1000000000000001 >GFLOPS drop ;        \ over the value capacity
: TRY-CAP-NEG ( -- ) -1 >GBPS drop ;                        \ negative reading value

\ ---- named-payload decoders + round-trip through the production producers --------
\ The reading is a full-mode payload ENUM: the present arm carries two named cells,
\ `value` and `unit`, and the absent arm carries `reason`. A reading is three cells
\ wide (two payload plus a tag), so it never crosses an interpret boundary; every
\ round-trip below stays inside a colon word and reports a scalar.
: RD-VALUE ( reading -- n )                     \ the reading's value, -1 when absent
   MATCH reading
      rd-at OF drop ENDOF
      rd-na OF drop -1 ENDOF
   ;MATCH ;
: RD-UNIT-GBPS? ( reading -- bool )             \ the recovered unit is exactly u-gbps
   MATCH reading
      rd-at OF nip CEVID-UNIT:U-GBPS CEVID-UNIT:EQ ENDOF
      rd-na OF drop false ENDOF
   ;MATCH ;
: RD-UNIT-NS? ( reading -- bool )               \ ...and is NOT u-ns, the ordinal-0 unit
   MATCH reading
      rd-at OF nip CEVID-UNIT:U-NS CEVID-UNIT:EQ ENDOF
      rd-na OF drop false ENDOF
   ;MATCH ;
: RD-REASON$ ( reading -- ptr u8 n )            \ the named absence reason, else a name no arm renders
   MATCH reading
      rd-at OF 2drop s" <present>" ENDOF
      rd-na OF ABS$ ENDOF
   ;MATCH ;

\ Both cells under test are deliberately non-zero: the value is 7, and the unit is
\ u-gbps rather than u-ns, whose ordinal is 0. A dropped or zeroed payload cell reads
\ back as 0, which for the value is a plain 0 and for the unit is u-ns - so each cell
\ is checked BOTH for the value it should carry and against the value a zeroed cell
\ would produce.
7 constant RD-T-VALUE
: RD-T-AT ( -- reading )        RD-T-VALUE >GBPS ;
: RD-T-NA ( -- reading )        BENCH-ABSENCE:DEVICE-GATED CEVID-READING:RD-NA ;
: RD-RT-VALUE ( -- n )          RD-T-AT RD-VALUE ;
: RD-RT-GBPS? ( -- bool )       RD-T-AT RD-UNIT-GBPS? ;
: RD-RT-NS? ( -- bool )         RD-T-AT RD-UNIT-NS? ;
: RD-NA-VALUE ( -- n )          RD-T-NA RD-VALUE ;
: RD-NA-GBPS? ( -- bool )       RD-T-NA RD-UNIT-GBPS? ;
: RD-RT-REASON$ ( -- ptr u8 n ) RD-T-NA RD-REASON$ ;
: RD-AT-REASON$ ( -- ptr u8 n ) RD-T-AT RD-REASON$ ;

\ the identities these pins are about, named once (tail + constructor package: the
\ (package, tail) pair that owns family identity, per REFLECT's R7 key)
: RD$ ( -- ptr u8 n ptr u8 n )   s" reading" s" CEVID-READING" ;
: EV$ ( -- ptr u8 n ptr u8 n )   s" evidence" s" CEVID-EVIDENCE" ;
create BIGROW 600 allot
: TRY-ROW ( -- )  BIGROW 600 ROW-STABLE 2drop ;             \ 600 > EV-ROW-CAP (512)

T-RESET

\ ==== byte goldens: the migrated flagship rows (commit these) ==================
GEMM-HABU RENDER
   s" cevid/v1|wl=gemm|rev=mmm-wide-b-m4-s1|sh=sq2048|pol=rel|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=na:not-measured|thr=3026577:gflops|byt=na:not-measured|launch=30:count|mem=50176:bytes|energy=25:watts"
   T$=
GEMM-TRITON RENDER
   s" cevid/v1|wl=gemm|rev=triton-autotuned|sh=sq2048|pol=rel|tgt=sm_87|comp=triton-3.5.1|cache=warm|pr=cuda-events|bl=triton|lat=272600:ms|thr=1890500:gflops|byt=na:not-measured|launch=30:count|mem=na:not-measured|energy=25:watts"
   T$=
SAXPY-HABU RENDER
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts"
   T$=
SAXPY-TRITON RENDER
   s" cevid/v1|wl=saxpy|rev=triton-jit|sh=n1m|pol=exact|tgt=sm_87|comp=triton-3.5.1|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=63000:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts"
   T$=
\ the ns unit + all-na default readings.
NS-ROW RENDER
   s" cevid/v1|wl=gemm|rev=mm-cp-async-blocked|sh=sq2048|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=1354517944:ns|thr=na:not-measured|byt=na:not-measured|launch=na:not-measured|mem=na:not-measured|energy=na:not-measured"
   T$=

\ ==== replay is byte-stable: rebuild + re-render is byte-identical =============
GEMM-HABU RENDER CT-COPY
GEMM-HABU RENDER CT-BUF$ T$=
SAXPY-HABU RENDER CT-COPY
SAXPY-HABU RENDER CT-BUF$ T$=

\ ==== the comparison ROW renders byte-stably (comparable pair) =================
GEMM-HABU GEMM-TRITON RENDER-PAIR
   s" cevid-cmp/v1|comparable=yes|subj=cevid/v1|wl=gemm|rev=mmm-wide-b-m4-s1|sh=sq2048|pol=rel|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=na:not-measured|thr=3026577:gflops|byt=na:not-measured|launch=30:count|mem=50176:bytes|energy=25:watts|base=cevid/v1|wl=gemm|rev=triton-autotuned|sh=sq2048|pol=rel|tgt=sm_87|comp=triton-3.5.1|cache=warm|pr=cuda-events|bl=triton|lat=272600:ms|thr=1890500:gflops|byt=na:not-measured|launch=30:count|mem=na:not-measured|energy=25:watts"
   T$=
GEMM-HABU GEMM-TRITON RENDER-PAIR CT-COPY
GEMM-HABU GEMM-TRITON RENDER-PAIR CT-BUF$ T$=

\ ==== cache-state is an explicit field: cold vs warm alters the key ============
GEMM-HABU RENDER CT-COPY
GEMM-HABU-COLD RENDER CT-BUF$ T$<>

\ ==== the numeric-policy DOMAIN pairing verdict ===============================
GEMM-CMP  TTRUE      \ both relative (Habu TF32 vs Triton TF32) -> comparable
SAXPY-CMP TTRUE      \ both exact (FP32) -> comparable
BAD-CMP   TFALSE     \ exact FP32 vs relative TF32 (the historical confusion) -> verdict 0

\ ==== named fail-closed throws ================================================
' TRY-INCOMPARABLE E-CEVID-INCOMPARABLE TTHROWS  \ a mismatched-domain pair cannot form a comparison row
' TRY-LAT-RATE     E-CEVID-UNIT         TTHROWS  \ a rate reading in the latency slot: rejected
' TRY-LAT-NS-OK    0                     TTHROWS  \ a duration reading in the latency slot: accepted
' TRY-CAP-HI       E-CEVID-CAP          TTHROWS  \ over the value capacity
' TRY-CAP-NEG      E-CEVID-CAP          TTHROWS  \ negative reading value

\ ==== the reading as a full-mode payload ENUM ===================================
\ The generated constructors, by exact spelling and exact effect. A -1 means the
\ checker resolved EXACTLY this name (it answers 1 for a name it cannot resolve), so
\ these also prove the constructor package did not drift.
s" RD-P-AT ( n CEVID:unit -- CEVID:reading ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" RD-P-NA ( BENCH:absence -- CEVID:reading ) CEVID-READING:RD-NA"
   CHECK-QUIET-CANDIDATE! -1 T=
\ the present arm takes BOTH cells in declaration order and nothing else: the two
\ cells are mandatory, they cannot be exchanged (their types differ), a raw cell
\ cannot stand in for the sealed unit, and the reading is not a bare scalar. These
\ are the FIELD-removal kills the declaration has to keep failing.
s" RD-F-AT-NOUNIT ( n -- CEVID:reading ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-F-AT-NOVAL ( CEVID:unit -- CEVID:reading ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-F-AT-SWAP ( CEVID:unit n -- CEVID:reading ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-F-AT-RAWUNIT ( n n -- CEVID:reading ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-F-AT-BARE ( n CEVID:unit -- n ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the absent arm takes exactly its reason: no value/unit pair, no raw cell, and a
\ foreign closed vocabulary cannot stand in for the absence reason.
s" RD-F-NA-PAIR ( n CEVID:unit -- CEVID:reading ) CEVID-READING:RD-NA"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-F-NA-RAW ( n -- CEVID:reading ) CEVID-READING:RD-NA"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-F-NA-FGN ( CEVID:unit -- CEVID:reading ) CEVID-READING:RD-NA"
   CHECK-QUIET-CANDIDATE! 0 T=
\ MATCH bindings are per-arm and typed: the present arm binds two cells in order and
\ the absent arm binds one, and exchanging the two cells' roles rejects.
s" RD-M-OK ( CEVID:reading -- n ) MATCH CEVID:reading rd-at OF {: v:n u:CEVID:unit :} 1 ENDOF rd-na OF {: r:BENCH:absence :} 2 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! -1 T=
s" RD-M-SWAP ( CEVID:reading -- n ) MATCH CEVID:reading rd-at OF {: u:CEVID:unit v:n :} 1 ENDOF rd-na OF {: r:BENCH:absence :} 2 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-M-UNDERBIND ( CEVID:reading -- n ) MATCH CEVID:reading rd-at OF {: v:n :} 1 ENDOF rd-na OF {: r:BENCH:absence :} 2 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- the declared shape, through the shared REFLECT reflection set --------------
\ Keyed per R7: tail plus constructor package, with FAMS as the uniqueness assertion
\ (1 is the only healthy answer - 0 means the identity resolves nothing, more than 1
\ means it is ambiguous and no pin under it could be trusted).
RD$ REFLECT:FAMS 1 T=
RD$ REFLECT:KIND TK-SUM T=
RD$ REFLECT:ARITY 0 T=
RD$ REFLECT:WIDTH 3 T=                  \ two payload cells plus one tag cell
RD$ REFLECT:VIS 1 T=                    \ public: the store and the report both read it
RD$ REFLECT:VARS 2 T=
RD$ 0 REFLECT:ARM$ s" rd-at" T$=
RD$ 1 REFLECT:ARM$ s" rd-na" T$=
RD$ 0 REFLECT:ARM-CTOR$ s" CEVID-READING" T$=
RD$ 1 REFLECT:ARM-CTOR$ s" CEVID-READING" T$=
RD$ 0 REFLECT:ARM-FLDS 2 T=             \ the present arm carries exactly two named cells
RD$ 1 REFLECT:ARM-FLDS 1 T=
RD$ 0 s" value" REFLECT:ARM-SLOT 0 T=
RD$ 0 s" unit" REFLECT:ARM-SLOT 1 T=
RD$ 1 s" reason" REFLECT:ARM-SLOT 0 T=
RD$ 0 s" value" REFLECT:ARM-CELLS 1 T=
RD$ 0 s" unit" REFLECT:ARM-CELLS 1 T=
\ every payload name is per-arm, so none answers on another arm, and an undeclared
\ name answers nothing anywhere.
RD$ 1 s" value" REFLECT:ARM-SLOT -1 T=
RD$ 1 s" unit" REFLECT:ARM-SLOT -1 T=
RD$ 0 s" reason" REFLECT:ARM-SLOT -1 T=
RD$ 0 s" absence" REFLECT:ARM-SLOT -1 T=
RD$ 2 s" value" REFLECT:ARM-SLOT -1 T=  \ and no third arm exists

\ ---- the evidence handle as a STRUCTURE -----------------------------------------
s" EV-P-MK ( n -- CEVID:evidence ) CEVID-EVIDENCE:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-P-UN ( CEVID:evidence -- n ) CEVID-EVIDENCE:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
\ the handle is a nominal single-cell record, not an int: it never unmakes into
\ another record's shape and a reading cannot stand in for it.
s" EV-F-LONG ( n n -- CEVID:evidence ) CEVID-EVIDENCE:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-F-RD ( CEVID:reading -- CEVID:evidence ) "
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-F-BARE ( CEVID:evidence -- CEVID:reading ) "
   CHECK-QUIET-CANDIDATE! 0 T=
EV$ REFLECT:FAMS 1 T=
EV$ REFLECT:KIND TK-PRODUCT T=
EV$ REFLECT:WIDTH 1 T=
EV$ REFLECT:FLDS 1 T=                   \ exactly one named field, no more
EV$ s" slot" REFLECT:SLOT 0 T=
EV$ s" slot" REFLECT:CELLS 1 T=
EV$ s" value" REFLECT:SLOT -1 T=        \ an undeclared name has no slot
EV$ 0 REFLECT:ARM-FLDS 0 T=             \ a record owns no per-case rows

\ ---- both arms construct through the production producers and match back --------
RD-RT-VALUE RD-T-VALUE T=               \ the value cell survives unchanged...
RD-RT-GBPS? TTRUE                       \ ...and so does the unit cell
RD-RT-NS? TFALSE                        \ which is NOT u-ns, what a zeroed cell would give
RD-NA-VALUE -1 T=                       \ the absent arm carries no value...
RD-NA-GBPS? TFALSE                      \ ...and no unit
RD-RT-REASON$ s" device-gated" T$=      \ it carries its named reason
RD-AT-REASON$ s" <present>" T$=         \ and the present arm carries none

public

\ rd-twin and ev-twin are the migrated families' SHAPES under different names: same
\ arity, same arms in the same order, same named payload cells. They exist only so
\ the negatives below can prove identity is NOMINAL - two identically shaped families
\ never unify, in either direction. They must be public: a private family publishes
\ no constructor package at all (REFLECT:FAMS would answer 2 for a colliding pair),
\ and the positive controls build through the twins' own constructors, so no negative
\ can pass by being unresolvable rather than ill-typed.
ENUM rd-twin 0
   VARIANT rd-twin-at FIELD value n FIELD unit unit ;VARIANT
   VARIANT rd-twin-na FIELD reason BENCH:absence ;VARIANT
;ENUM

STRUCTURE ev-twin 0
   FIELD slot n
;STRUCTURE

private

s" RD-TW ( n CEVID:unit -- rd-twin ) CEVID-RD--TWIN:RD-TWIN-AT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" RD-TW-X1 ( n CEVID:unit -- rd-twin ) CEVID-READING:RD-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" RD-TW-X2 ( n CEVID:unit -- CEVID:reading ) CEVID-RD--TWIN:RD-TWIN-AT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-TW ( n -- ev-twin ) CEVID-EV--TWIN:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-TW-X1 ( ev-twin -- CEVID:evidence ) "
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-TW-X2 ( CEVID:evidence -- ev-twin ) "
   CHECK-QUIET-CANDIDATE! 0 T=
\ the twin is a DIFFERENT family under the same tail-plus-package rule: REFLECT
\ reads each identity's own shape rather than whichever loaded first.
s" rd-twin" s" CEVID-RD--TWIN" REFLECT:FAMS 1 T=
s" rd-twin" s" CEVID-RD--TWIN" 0 REFLECT:ARM$ s" rd-twin-at" T$=
RD$ 0 REFLECT:ARM$ s" rd-at" T$=
' TRY-ROW          E-CEVID-ROW          TTHROWS  \ oversized row over the stable buffer

\ ==== STATIC checker rejects (CHECK-QUIET-CANDIDATE!): each 0 + resolving -1 ====
\ resolving positive control: every slot correct -> certifies.
s" P0 ( BENCH:workload CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! -1 T=
\ FP32/TF32 policy confusion: a precision class (EVID:prec-class) in the numeric-policy slot.
s" N-POL ( BENCH:workload CEVID:revision BENCH:shape EVID:prec-class CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-POL ( BENCH:workload CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! -1 T=
\ raw n where a nominal workload id is required.
s" N-RAW ( n CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! 0 T=
\ identity-slot swap: a baseline id where the workload id is required.
s" N-ID ( BENCH:baseline CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! 0 T=

;package
T-REPORT
