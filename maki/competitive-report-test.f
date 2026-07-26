\ maki/competitive-report-test.f - acceptance suite for the checked BENCH
\ competitive-comparison schema (dot habu-define-checked-bench-5341ffc8).
\
\ Pins the dot's acceptance: byte-stable canonical renders for the SAXPY FP32 and
\ Habu-MMM/Triton TF32 rows; every exact-key field (and cold/warm) alters the
\ rendered key; unavailable readings render their named absence reason; the
\ numeric-policy pairing verdict rejects the historical Habu-FP32-vs-Triton-TF32
\ confusion (verdict 0) with a resolving positive; the identity / raw-n / cache /
\ unit / precision confusions are checker rejects (CHECK-QUIET-CANDIDATE! 0) each
\ with a resolving positive; capacity overflows throw the named E-BENCH codes.
\ Every negative is paired with a resolving positive control (per LESSONS).
\
\ Fixture numbers are the real device measurements: SAXPY-V4 64.209 GB/s vs Triton
\ 63.0 GB/s (docs/eval-triton.md, tools/ptx/perf-rows.tsv), Habu MMM TF32 884.889
\ GFLOP/s vs Triton TF32 1890.5 GFLOP/s, and the historical Habu blocked FP32
\ 380.5 GFLOP/s vs Triton TF32 1890.5 GFLOP/s - milli-units (x1000).

require lib/test.f
require lib/string.f
require test/checker-assert.f       \ CHECK-QUIET-CANDIDATE! for the type-reject negatives
require maki/evidence/schema.f      \ EVID:prec-class for the FP32/TF32 policy-confusion negative
require maki/competitive-report.f

\ ---- registry-shape reflection -------------------------------------------------
\ The declarations publish their field names as type-registry rows: keyed
\ (family, no-variant) for a STRUCTURE, keyed (family, variant) for a payload arm.
\ These helpers read those rows through the public read-only registry axioms (the
\ same ones tools/public-signatures-core.f reads; they cannot mutate anything), so
\ the pins below can state each field NAME to payload SLOT mapping. That mapping is
\ the only observable an exchanged pair of SAME-TYPED fields moves: this schema has
\ two such pairs per record (spol/bpol, both NPOL:dom, and subj/base, both the same
\ reading family), and a positional MAKE/UNMAKE round-trip cannot see either
\ exchange even when every field carries a distinct value.
package BENCH-REC
private

: FAM-CTOR? ( n ptr u8 n -- bool ) {: fam:n pa:ptr pu:n :}
   fam TFAM-VAR-COUNT@ 0 <= if false exit then
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ pa pu STR= ;

: FAM-HIT? ( n ptr u8 n ptr u8 n -- bool ) {: fam:n ta:ptr tu:n pa:ptr pu:n :}
   fam TFAM-NAME$ ta tu STR= fam pa pu FAM-CTOR? and ;

\ A family is identified by its tail plus the constructor package its generated
\ operations carry - exactly the (package, tail) pair that owns family identity, so
\ a pin names the family it pins instead of guessing from shape.
: FAM-N ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}
   0
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if 1+ then
   loop ;

: FAM-ID ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}   \ family id, or -1
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if i unloop exit then
   loop -1 ;

\ FAM-ID answers -1 for a family that is not registered and the registry readers
\ take a live id, so every read refuses the sentinel first: a missing family must
\ report a wrong number, never read a record that is not there.
: LIVE-KIND ( n -- n ) {: fam:n :}
   fam 0 < if -1 exit then  fam TFAM-KIND@ ;
: LIVE-WIDTH ( n -- n ) {: fam:n :}
   fam 0 < if -1 exit then  fam TFAM-WIDTH@ ;
: LIVE-VARS ( n -- n ) {: fam:n :}
   fam 0 < if -1 exit then  fam TFAM-VAR-COUNT@ ;
: LIVE-VAR ( n n -- n ) {: fam:n k:n :}          \ the family's k-th variant id, or -1
   fam LIVE-VARS k <= if -1 exit then
   fam TFAM-VAR-START@ k + ;
: VAR-NAME$ ( n -- ptr u8 n ) {: var:n :}        \ variant name, or a name no declaration can spell
   var 0 < if s" <missing>" exit then  var SUMV-NAME$ ;

: OWNED-N ( n n -- n ) {: fam:n var:n :}         \ committed field rows owned by (fam,var)
   0
   TYPE-FIELD:COUNT 0 ?do
      i TYPE-FIELD:FAMILY@ fam = i TYPE-FIELD:VARIANT@ var = and if 1+ then
   loop ;

public

\ --- a record: fields keyed (family, no-variant) --------------------------------
: REC-FAMS ( ptr u8 n ptr u8 n -- n )    FAM-N ;
: REC-KIND ( ptr u8 n ptr u8 n -- n )    FAM-ID LIVE-KIND ;
: REC-WIDTH ( ptr u8 n ptr u8 n -- n )   FAM-ID LIVE-WIDTH ;
: REC-FLDS ( ptr u8 n ptr u8 n -- n )
   FAM-ID {: fam:n :}   fam TYPE-FIELD:NO-VARIANT OWNED-N ;
: REC-SLOT ( ptr u8 n ptr u8 n ptr u8 n -- n )   \ payload slot of a named field, -1 when absent
   {: ta:ptr tu:n pa:ptr pu:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam TYPE-FIELD:NO-VARIANT na nu TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:SLOT@ ;
: REC-CELLS ( ptr u8 n ptr u8 n ptr u8 n -- n )  \ cell width of a named field, -1 when absent
   {: ta:ptr tu:n pa:ptr pu:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam TYPE-FIELD:NO-VARIANT na nu TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:CELLS@ ;

\ --- a payload sum: fields keyed (family, arm) ----------------------------------
: SUM-FAMS ( ptr u8 n ptr u8 n -- n )    FAM-N ;
: SUM-KIND ( ptr u8 n ptr u8 n -- n )    FAM-ID LIVE-KIND ;
: SUM-WIDTH ( ptr u8 n ptr u8 n -- n )   FAM-ID LIVE-WIDTH ;
: SUM-VARS ( ptr u8 n ptr u8 n -- n )    FAM-ID LIVE-VARS ;
: SUM-ARM$ ( ptr u8 n ptr u8 n n -- ptr u8 n )   \ the k-th arm's name
   {: ta:ptr tu:n pa:ptr pu:n k:n :}
   ta tu pa pu FAM-ID k LIVE-VAR VAR-NAME$ ;
: SUM-ARM-FLDS ( ptr u8 n ptr u8 n n -- n )      \ named cells on the k-th arm
   {: ta:ptr tu:n pa:ptr pu:n k:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam  fam k LIVE-VAR  OWNED-N ;
: SUM-ARM-SLOT ( ptr u8 n ptr u8 n n ptr u8 n -- n )   \ slot of a named cell on the k-th arm
   {: ta:ptr tu:n pa:ptr pu:n k:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam  fam k LIVE-VAR  na nu TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:SLOT@ ;

\ the (tail, constructor package) identities this file pins
: CGB$ ( -- ptr u8 n ptr u8 n )   s" comparison-gbps" s" BENCH-COMPARISON--GBPS" ;
: CGF$ ( -- ptr u8 n ptr u8 n )   s" comparison-gflops" s" BENCH-COMPARISON--GFLOPS" ;
: GB$ ( -- ptr u8 n ptr u8 n )    s" gbps" s" BENCH-GBPS" ;
: GF$ ( -- ptr u8 n ptr u8 n )    s" gflops" s" BENCH-GFLOPS" ;

;package

package BENCH

\ ---- stable copy (the shared SB render is overwritten by the next render) -----
256 constant CT-CAP
create CT-BUF CT-CAP allot  variable CT-BU
: CT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a CT-BUF u BYTE-COPY  u CT-BU ! ;
: CT-BUF$ ( -- ptr u8 n )  CT-BUF CT-BU @ ;

\ ---- build-and-render helpers (product never crosses an interpret boundary) ---
\ ( 4 ids, cache, subj-policy, base-policy, subj-milli, base-milli -- rendered row )
: GBPS-ROW$ ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom n n -- ptr u8 n )
   {: sm:n bm:n :}  sm >GBPS bm >GBPS COMPARE-GBPS RENDER-GBPS ;
: GFLOPS-ROW$ ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom n n -- ptr u8 n )
   {: sm:n bm:n :}  sm >GFLOPS bm >GFLOPS COMPARE-GFLOPS RENDER-GFLOPS ;

\ base GB/s row = the SAXPY FP32 golden; each variant below changes ONE field.
: G-BASE$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-WL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-SH$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-PR$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-BL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:CUBLAS
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-CACHE$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:COLD NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-SPOL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-BPOL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:RELATIVE 64209 63000 GBPS-ROW$ ;
: G-SUBJ$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64210 63000 GBPS-ROW$ ;
: G-BASEV$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63001 GBPS-ROW$ ;
\ same identity/policy/values as the base but the GFLOP/s unit: only the unit token differs.
: G-UNIT$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GFLOPS-ROW$ ;

\ the Habu-MMM / Triton TF32 golden (both relative).
: MMM$ ( -- ptr u8 n )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:RELATIVE 884889 1890500 GFLOPS-ROW$ ;

\ an absent baseline reading renders its named reason.
: G-ABS$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:COLD NPOL-DOM:EXACT NPOL-DOM:EXACT
   64209 >GBPS BENCH-ABSENCE:DEVICE-GATED GBPS-ABSENT COMPARE-GBPS RENDER-GBPS ;

\ a normal row survives the stable copy (positive capacity control).
: OK-ROW$ ( -- ptr u8 n )  G-BASE$ ROW-STABLE ;

\ ---- comparison builders for the runtime pairing verdict ---------------------
: SAXPY-CMP ( -- bool )     \ both exact  -> comparable
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 >GBPS 63000 >GBPS COMPARE-GBPS GBPS-COMPARABLE? ;
: MMM-CMP ( -- bool )       \ both relative (Habu TF32 vs Triton TF32) -> comparable
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:RELATIVE 884889 >GFLOPS 1890500 >GFLOPS COMPARE-GFLOPS GFLOPS-COMPARABLE? ;
: BAD-CMP ( -- bool )       \ Habu FP32 (exact) vs Triton TF32 (relative) -> NOT comparable
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:RELATIVE 380501 >GFLOPS 1890500 >GFLOPS COMPARE-GFLOPS GFLOPS-COMPARABLE? ;

\ ---- capacity fail-closed triggers -------------------------------------------
: TRY-CAP-HI ( -- )  1000000001 >GBPS drop ;      \ over the milli-unit capacity
: TRY-CAP-NEG ( -- ) -1 >GFLOPS drop ;            \ negative throughput
create BIGROW 300 allot
: TRY-ROW ( -- )  BIGROW 300 ROW-STABLE 2drop ;   \ 300 > BENCH-ROW-CAP (256)

\ ---- named payload projections ------------------------------------------------
\ The readings are full-mode payload ENUMs: the present arm carries `milli`, the
\ absent arm carries `reason`. A reading is two cells wide, so it can be neither a
\ typed local nor an interpret-boundary value; every round-trip below stays inside a
\ colon word and reports a scalar.
: GBPS-MILLI ( gbps -- n )                        \ the reading in milli-units, -1 when absent
   MATCH gbps
      gbps-at OF ENDOF
      gbps-na OF drop -1 ENDOF
   ;MATCH ;
: GFLOPS-MILLI ( gflops -- n )
   MATCH gflops
      gflops-at OF ENDOF
      gflops-na OF drop -1 ENDOF
   ;MATCH ;
: GBPS-REASON ( gbps -- ptr u8 n )                \ the named absence reason, else a name no arm renders
   MATCH gbps
      gbps-at OF drop s" <present>" ENDOF
      gbps-na OF ABSENCE-NAME ENDOF
   ;MATCH ;
: GFLOPS-REASON ( gflops -- ptr u8 n )
   MATCH gflops
      gflops-at OF drop s" <present>" ENDOF
      gflops-na OF ABSENCE-NAME ENDOF
   ;MATCH ;

\ each arm binds its own payload and dispatches to its own branch.
: RD-AT-MILLI ( -- n )      64209 >GBPS GBPS-MILLI ;
: RD-NA-MILLI ( -- n )      BENCH-ABSENCE:WAIVED GBPS-ABSENT GBPS-MILLI ;
: RD-NA-REASON$ ( -- ptr u8 n )
   BENCH-ABSENCE:DEVICE-GATED GBPS-ABSENT GBPS-REASON ;
: RD-AT-REASON$ ( -- ptr u8 n )   64209 >GBPS GBPS-REASON ;
: RF-AT-MILLI ( -- n )      884889 >GFLOPS GFLOPS-MILLI ;
: RF-NA-MILLI ( -- n )      BENCH-ABSENCE:NOT-MEASURED GFLOPS-ABSENT GFLOPS-MILLI ;
: RF-NA-REASON$ ( -- ptr u8 n )
   BENCH-ABSENCE:NOT-MEASURED GFLOPS-ABSENT GFLOPS-REASON ;

\ ---- nine distinct field values, round-tripped through MAKE/UNMAKE -------------
\ Every field differs from every other field it could be confused with: the four
\ nominal ids are each the non-default member, cache is cold, the two policy
\ witnesses differ from each other, and the two readings carry different milli
\ values. So an exchange the checker cannot refuse (spol/bpol, subj/base) that the
\ VALUES could see would report the wrong number here.
: RB-DISTINCT ( -- comparison-gbps )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:CUBLAS
   BENCH-CACHE--STATE:COLD
   NPOL-DOM:EXACT NPOL-DOM:RELATIVE
   111 >GBPS 222 >GBPS
   COMPARE-GBPS ;
: RB-ORDER? ( -- bool )
   RB-DISTINCT BENCH-COMPARISON--GBPS:UNMAKE
   GBPS-MILLI {: bm:n :}                          \ base is on top: consume it first
   GBPS-MILLI {: sm:n :}
   {: wl:workload sh:shape pr:protocol bl:baseline cache:cache-state
      spol:NPOL:dom bpol:NPOL:dom :}
   wl BENCH-WORKLOAD:GEMM BENCH-WORKLOAD:EQ
   sh BENCH-SHAPE:SQ2048 BENCH-SHAPE:EQ and
   pr BENCH-PROTOCOL:WALLCLOCK BENCH-PROTOCOL:EQ and
   bl BENCH-BASELINE:CUBLAS BENCH-BASELINE:EQ and
   cache BENCH-CACHE--STATE:COLD BENCH-CACHE--STATE:EQ and
   spol NPOL-DOM:EXACT NPOL-DOM:EQ and
   bpol NPOL-DOM:RELATIVE NPOL-DOM:EQ and
   sm 111 = and
   bm 222 = and ;

: RF-DISTINCT ( -- comparison-gflops )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:CUBLAS
   BENCH-CACHE--STATE:COLD
   NPOL-DOM:EXACT NPOL-DOM:RELATIVE
   333 >GFLOPS 444 >GFLOPS
   COMPARE-GFLOPS ;
: RF-ORDER? ( -- bool )
   RF-DISTINCT BENCH-COMPARISON--GFLOPS:UNMAKE
   GFLOPS-MILLI {: bm:n :}
   GFLOPS-MILLI {: sm:n :}
   {: wl:workload sh:shape pr:protocol bl:baseline cache:cache-state
      spol:NPOL:dom bpol:NPOL:dom :}
   wl BENCH-WORKLOAD:GEMM BENCH-WORKLOAD:EQ
   sh BENCH-SHAPE:SQ2048 BENCH-SHAPE:EQ and
   pr BENCH-PROTOCOL:WALLCLOCK BENCH-PROTOCOL:EQ and
   bl BENCH-BASELINE:CUBLAS BENCH-BASELINE:EQ and
   cache BENCH-CACHE--STATE:COLD BENCH-CACHE--STATE:EQ and
   spol NPOL-DOM:EXACT NPOL-DOM:EQ and
   bpol NPOL-DOM:RELATIVE NPOL-DOM:EQ and
   sm 333 = and
   bm 444 = and ;

T-RESET

\ ==== byte goldens: SAXPY FP32 and Habu-MMM/Triton TF32 (commit these) =========
G-BASE$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000"
   T$=
MMM$
   s" bench/v1|wl=gemm|sh=sq2048|pr=cuda-events|bl=triton|cache=warm|spol=rel|bpol=rel|unit=gflops|subj=884889|base=1890500"
   T$=
\ the stable copy of a normal row is byte-identical (positive capacity control).
OK-ROW$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000"
   T$=

\ ==== every exact-key field (and cold/warm) alters the rendered key ============
G-BASE$ CT-COPY                  \ pin the base row; each variant must differ
G-WL$    CT-BUF$ T$<>            \ workload
G-SH$    CT-BUF$ T$<>            \ shape
G-PR$    CT-BUF$ T$<>            \ protocol
G-BL$    CT-BUF$ T$<>            \ baseline
G-CACHE$ CT-BUF$ T$<>            \ cache state (cold/warm)
G-SPOL$  CT-BUF$ T$<>            \ subject numeric policy
G-BPOL$  CT-BUF$ T$<>            \ baseline numeric policy
G-UNIT$  CT-BUF$ T$<>            \ metric unit token
G-SUBJ$  CT-BUF$ T$<>            \ subject throughput value
G-BASEV$ CT-BUF$ T$<>            \ baseline throughput value

\ ==== unavailable reading renders its named absence reason =====================
G-ABS$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=cold|spol=exact|bpol=exact|unit=gbps|subj=64209|base=na:device-gated"
   T$=

\ ==== runtime pairing verdict: numeric-policy witnesses must match =============
SAXPY-CMP TTRUE      \ both exact           -> comparable
MMM-CMP   TTRUE      \ both relative (TF32)  -> comparable (resolving positive)
BAD-CMP   TFALSE     \ exact FP32 vs relative TF32 (the historical confusion) -> verdict 0

\ ==== type-reject confusions (CHECK-QUIET-CANDIDATE!): each 0 + resolving -1 ====
\ resolving positive control: every slot correct -> certifies.
s" P0 ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ FP32/TF32 policy confusion: a precision class (EVID:prec-class) in the numeric-policy slot.
s" N-POL ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state EVID:prec-class NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-POL ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ identity-slot swap: a workload id where the baseline id is expected.
s" N-ID ( BENCH:workload BENCH:shape BENCH:protocol BENCH:workload BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-ID ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ raw n where a nominal id is required.
s" N-RAW ( n BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-RAW ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ cache-state confusion: a raw n where the closed cache-state sum is required.
s" N-CACHE ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline n NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-CACHE ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ throughput-unit confusion: a GFLOP/s reading where the GB/s baseline slot is expected.
s" N-UNIT ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gflops -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-UNIT ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ==== capacity overflows throw the named E-BENCH codes =========================
' TRY-CAP-HI  E-BENCH-CAP TTHROWS
' TRY-CAP-NEG E-BENCH-CAP TTHROWS
' TRY-ROW     E-BENCH-ROW TTHROWS

\ ==== the readings as full-mode payload ENUMs ===================================
\ Each arm's generated constructor, by exact spelling and exact effect. A -1 means
\ the checker resolved EXACTLY this name (it answers 1 for a name it cannot
\ resolve), so these pins also prove the constructor packages did not drift.
s" RB-AT ( n -- BENCH:gbps ) BENCH-GBPS:GBPS-AT"            CHECK-QUIET-CANDIDATE! -1 T=
s" RB-NA ( BENCH:absence -- BENCH:gbps ) BENCH-GBPS:GBPS-NA" CHECK-QUIET-CANDIDATE! -1 T=
s" RF-AT ( n -- BENCH:gflops ) BENCH-GFLOPS:GFLOPS-AT"      CHECK-QUIET-CANDIDATE! -1 T=
s" RF-NA ( BENCH:absence -- BENCH:gflops ) BENCH-GFLOPS:GFLOPS-NA" CHECK-QUIET-CANDIDATE! -1 T=
\ the two arms' payload types are not interchangeable, the payload is mandatory,
\ a reading is not a bare scalar, and the units never cross even though both
\ readings have identical shape and width.
s" RB-AT-ABS ( BENCH:absence -- BENCH:gbps ) BENCH-GBPS:GBPS-AT"   CHECK-QUIET-CANDIDATE! 0 T=
s" RB-NA-RAW ( n -- BENCH:gbps ) BENCH-GBPS:GBPS-NA"               CHECK-QUIET-CANDIDATE! 0 T=
s" RB-AT-NONE ( -- BENCH:gbps ) BENCH-GBPS:GBPS-AT"                CHECK-QUIET-CANDIDATE! 0 T=
s" RB-AT-BARE ( n -- n ) BENCH-GBPS:GBPS-AT"                       CHECK-QUIET-CANDIDATE! 0 T=
s" RB-XUNIT ( n -- BENCH:gflops ) BENCH-GBPS:GBPS-AT"              CHECK-QUIET-CANDIDATE! 0 T=
s" RF-XUNIT ( n -- BENCH:gbps ) BENCH-GFLOPS:GFLOPS-AT"            CHECK-QUIET-CANDIDATE! 0 T=

\ each arm binds its OWN payload: the present arm carries the milli reading through
\ unchanged, the absent arm carries its named reason, and neither answers for the
\ other (the sentinels below are values no arm can produce).
RD-AT-MILLI 64209 T=
RD-NA-MILLI -1 T=
RF-AT-MILLI 884889 T=
RF-NA-MILLI -1 T=
RD-NA-REASON$ s" device-gated" T$=
RD-AT-REASON$ s" <present>" T$=
RF-NA-REASON$ s" not-measured" T$=

\ the published shape of each reading family: exactly one family answers to each
\ (tail, constructor package), the arms keep their names and order, the width is
\ unchanged by the migration, and each arm carries exactly ONE named cell at
\ payload slot 0 - `milli` on the present arm, `reason` on the absent one.
BENCH-REC:GB$ BENCH-REC:SUM-FAMS 1 T=
BENCH-REC:GF$ BENCH-REC:SUM-FAMS 1 T=
BENCH-REC:GB$ BENCH-REC:SUM-VARS 2 T=
BENCH-REC:GF$ BENCH-REC:SUM-VARS 2 T=
BENCH-REC:GB$ BENCH-REC:SUM-WIDTH 2 T=    \ one payload cell plus one tag cell
BENCH-REC:GF$ BENCH-REC:SUM-WIDTH 2 T=
BENCH-REC:GB$ 0 BENCH-REC:SUM-ARM$ s" gbps-at" T$=
BENCH-REC:GB$ 1 BENCH-REC:SUM-ARM$ s" gbps-na" T$=
BENCH-REC:GF$ 0 BENCH-REC:SUM-ARM$ s" gflops-at" T$=
BENCH-REC:GF$ 1 BENCH-REC:SUM-ARM$ s" gflops-na" T$=
BENCH-REC:GB$ 0 BENCH-REC:SUM-ARM-FLDS 1 T=
BENCH-REC:GB$ 1 BENCH-REC:SUM-ARM-FLDS 1 T=
BENCH-REC:GF$ 0 BENCH-REC:SUM-ARM-FLDS 1 T=
BENCH-REC:GF$ 1 BENCH-REC:SUM-ARM-FLDS 1 T=
BENCH-REC:GB$ 0 s" milli" BENCH-REC:SUM-ARM-SLOT 0 T=
BENCH-REC:GB$ 1 s" reason" BENCH-REC:SUM-ARM-SLOT 0 T=
BENCH-REC:GF$ 0 s" milli" BENCH-REC:SUM-ARM-SLOT 0 T=
BENCH-REC:GF$ 1 s" reason" BENCH-REC:SUM-ARM-SLOT 0 T=
\ the two payload names are per-arm, so neither answers on the other's arm.
BENCH-REC:GB$ 0 s" reason" BENCH-REC:SUM-ARM-SLOT -1 T=
BENCH-REC:GB$ 1 s" milli" BENCH-REC:SUM-ARM-SLOT -1 T=

\ ==== the two comparisons as nine-field STRUCTUREs ==============================
\ The generated pair keeps its exact spelling and effect, so COMPARE-*, RENDER-*,
\ tools/eval-triton.f and maki/competitive-store.f are untouched.
s" CB-MK ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH-COMPARISON--GBPS:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" CB-UN ( BENCH:comparison-gbps -- BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps ) BENCH-COMPARISON--GBPS:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" CF-MK ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gflops BENCH:gflops -- BENCH:comparison-gflops ) BENCH-COMPARISON--GFLOPS:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" CF-UN ( BENCH:comparison-gflops -- BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gflops BENCH:gflops ) BENCH-COMPARISON--GFLOPS:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
\ the nine cells are mandatory and exact, and the record is not a bare scalar.
s" CB-SHORT ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps -- BENCH:comparison-gbps ) BENCH-COMPARISON--GBPS:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" CB-LONG ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH-COMPARISON--GBPS:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" CB-UN-BARE ( BENCH:comparison-gbps -- n ) BENCH-COMPARISON--GBPS:UNMAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" CB-MK-BARE ( n -- BENCH:comparison-gbps ) BENCH-COMPARISON--GBPS:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the two records never stand in for one another, in either direction, even though
\ they have identical field names, identical arity and identical width.
s" CB-XREC ( BENCH:comparison-gbps -- BENCH:comparison-gflops ) "
   CHECK-QUIET-CANDIDATE! 0 T=
s" CF-XREC ( BENCH:comparison-gflops -- BENCH:comparison-gbps ) "
   CHECK-QUIET-CANDIDATE! 0 T=

\ nine distinct field values survive MAKE/UNMAKE in their own slots.
RB-ORDER? T-ASSERT
RF-ORDER? T-ASSERT

\ the declared field names sit at the declared payload slots. The slots are NOT
\ 0..8: `subj` is a two-cell reading, so it spans slots 7-8 and `base` begins at 9.
\ These pins are what catch an exchange of the two SAME-TYPED pairs (spol/bpol and
\ subj/base) - the checker cannot refuse either, and RB-ORDER? above cannot see
\ either, because MAKE and UNMAKE are both positional.
BENCH-REC:CGB$ BENCH-REC:REC-FAMS 1 T=
BENCH-REC:CGF$ BENCH-REC:REC-FAMS 1 T=
BENCH-REC:CGB$ BENCH-REC:REC-KIND BENCH-REC:CGF$ BENCH-REC:REC-KIND T=   \ one record kind
BENCH-REC:CGB$ BENCH-REC:REC-WIDTH 11 T=   \ seven single cells plus two two-cell readings
BENCH-REC:CGF$ BENCH-REC:REC-WIDTH 11 T=
BENCH-REC:CGB$ BENCH-REC:REC-FLDS 9 T=     \ exactly nine named fields, no more
BENCH-REC:CGF$ BENCH-REC:REC-FLDS 9 T=
BENCH-REC:CGB$ s" wl" BENCH-REC:REC-SLOT 0 T=
BENCH-REC:CGB$ s" sh" BENCH-REC:REC-SLOT 1 T=
BENCH-REC:CGB$ s" pr" BENCH-REC:REC-SLOT 2 T=
BENCH-REC:CGB$ s" bl" BENCH-REC:REC-SLOT 3 T=
BENCH-REC:CGB$ s" cache" BENCH-REC:REC-SLOT 4 T=
BENCH-REC:CGB$ s" spol" BENCH-REC:REC-SLOT 5 T=
BENCH-REC:CGB$ s" bpol" BENCH-REC:REC-SLOT 6 T=
BENCH-REC:CGB$ s" subj" BENCH-REC:REC-SLOT 7 T=
BENCH-REC:CGB$ s" base" BENCH-REC:REC-SLOT 9 T=
BENCH-REC:CGF$ s" wl" BENCH-REC:REC-SLOT 0 T=
BENCH-REC:CGF$ s" sh" BENCH-REC:REC-SLOT 1 T=
BENCH-REC:CGF$ s" pr" BENCH-REC:REC-SLOT 2 T=
BENCH-REC:CGF$ s" bl" BENCH-REC:REC-SLOT 3 T=
BENCH-REC:CGF$ s" cache" BENCH-REC:REC-SLOT 4 T=
BENCH-REC:CGF$ s" spol" BENCH-REC:REC-SLOT 5 T=
BENCH-REC:CGF$ s" bpol" BENCH-REC:REC-SLOT 6 T=
BENCH-REC:CGF$ s" subj" BENCH-REC:REC-SLOT 7 T=
BENCH-REC:CGF$ s" base" BENCH-REC:REC-SLOT 9 T=
\ the two readings are the two-cell fields; the seven head fields are single cells.
BENCH-REC:CGB$ s" subj" BENCH-REC:REC-CELLS 2 T=
BENCH-REC:CGB$ s" base" BENCH-REC:REC-CELLS 2 T=
BENCH-REC:CGB$ s" spol" BENCH-REC:REC-CELLS 1 T=
BENCH-REC:CGB$ s" bpol" BENCH-REC:REC-CELLS 1 T=
\ an undeclared name resolves to no slot in either record.
BENCH-REC:CGB$ s" milli" BENCH-REC:REC-SLOT -1 T=
BENCH-REC:CGF$ s" unit" BENCH-REC:REC-SLOT -1 T=

public

\ cmp-twin and rd-twin are the migrated families' SHAPES under different names:
\ same arity, same field names in the same order, same payload arms. They exist only
\ so the negatives below can prove identity is NOMINAL - two identically shaped
\ families never unify, in either direction, and nine matching field names do not
\ make one record the other. They have to be public: a private family publishes no
\ constructors, and the positive controls build through the twins' own constructors,
\ so no negative can pass by being unresolvable rather than ill-typed. Their tails
\ are short on purpose: BENCH + cmp-twin renders BENCH-CMP--TWIN (15 bytes), inside
\ the 32-byte constructor-package limit, so these pins can be spelled at all.
ENUM rd-twin 0
   VARIANT rd-twin-at FIELD milli n ;VARIANT
   VARIANT rd-twin-na FIELD reason absence ;VARIANT
;ENUM

STRUCTURE cmp-twin 0
   FIELD wl workload
   FIELD sh shape
   FIELD pr protocol
   FIELD bl baseline
   FIELD cache cache-state
   FIELD spol NPOL:dom
   FIELD bpol NPOL:dom
   FIELD subj gbps
   FIELD base gbps
;STRUCTURE

private

s" TW-RD ( n -- BENCH:rd-twin ) BENCH-RD--TWIN:RD-TWIN-AT"          CHECK-QUIET-CANDIDATE! -1 T=
s" TW-RD-X1 ( n -- BENCH:rd-twin ) BENCH-GBPS:GBPS-AT"              CHECK-QUIET-CANDIDATE! 0 T=
s" TW-RD-X2 ( n -- BENCH:gbps ) BENCH-RD--TWIN:RD-TWIN-AT"          CHECK-QUIET-CANDIDATE! 0 T=
s" TW-CMP ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:cmp-twin ) BENCH-CMP--TWIN:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" TW-CMP-X1 ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:cmp-twin ) BENCH-COMPARISON--GBPS:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" TW-CMP-X2 ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH-CMP--TWIN:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=

;package
T-REPORT
