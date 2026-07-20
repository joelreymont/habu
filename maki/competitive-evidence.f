\ maki/competitive-evidence.f - the checked per-side competitive EVIDENCE row +
\ the closed metric UNITS vocabulary (dot habu-v2-competitive-evidence-5d07d471,
\ epic habu-epic-model-cad-9549fdd8; MODEL-CAD-V2-PLAN.md § 22.10 non-cherry-picked
\ evaluation matrix).
\
\ WHAT THIS IS. maki/competitive-report.f already types a THROUGHPUT-only
\ comparison (bench/v1: workload/shape/protocol/baseline/cache + spol/bpol + one
\ throughput reading per side). This file is the RICHER per-side evidence row the
\ § 22.10 matrix asks for: one measurement carrying its full identity
\ (workload / revision / shape / numeric-policy / target / compiler / cache-state /
\ protocol / baseline) AND the six metric readings the matrix reports
\ (latency / throughput / bytes / launches / memory / energy), each a typed
\ present-value-or-named-absence over the sealed metric UNIT.
\
\ THE METRIC UNITS VOCABULARY (folded here from maki/experiment/run-metric.f, whose
\ population/direction/aggregation axes landed but whose fourth axis - units - was
\ DEFERRED pending an owner; the run-identity dot habu-v2-experiment-run-7c1d1906
\ close-reason records the fold). It is a CLOSED, SEALED set derived from the REAL
\ rows (docs/eval-triton.md + tools/ptx/perf-rows.tsv), not an open registry:
\   ns / ms   - latency (GEMM gpu_elapsed_ns; Triton time_ms=272.6)
\   gflops    - GEMM throughput (perf-rows GFLOPS, e.g. 3026577 = 3026.577 GFLOP/s)
\   gbps      - SAXPY throughput (bandwidth GB/s; GB/s = bytes / elapsed_ns)
\   bytes     - SAXPY bytes moved (ITERS*3*N*4) and GEMM smem footprint (50176 B)
\   count     - launches / iters (200, 80, 30)
\   watts     - SoC power budget (orin-nx-25w device power mode)
\ joules is DELIBERATELY EXCLUDED: no measured joules exists in the corpus, so
\ minting it would be an unused unit. A future measured-energy row extends `unit`.
\ Reading values are integers: rate units (gflops/gbps) and ms are milli-scaled
\ (x1000, the perf-registry convention, so 3026.577 GFLOP/s -> 3026577); ns / bytes
\ / count / watts are natural integers.
\
\ WHAT IS TYPED / WHAT IS REJECTED.
\   - The identity nominal families (BENCH:workload/shape/protocol/baseline/
\     cache-state, plus this file's revision/compiler) are distinct closed enums,
\     so an id slot cannot take a raw n or a foreign id (a checker reject at ROW).
\   - target is the landed CAD-KIND:target-id (TARGET:SM87); numeric-policy is the
\     landed NPOL:dom witness.
\   - A comparison ROW is refused unless the two sides share a numeric-policy DOMAIN
\     (COMPARABLE? / RENDER-PAIR throw E-CEVID-INCOMPARABLE): the historical
\     Habu-FP32-exact vs Triton-TF32-relative confusion can never form a comparison.
\   - Each reading's unit must belong to its field's CATEGORY (a rate reading in the
\     latency slot is the named throw E-CEVID-UNIT); cache-state is a REQUIRED ROW
\     argument, never defaulted.
\
\ Rows are a bounded first-slice pool of single-cell handles (the run-metric ring
\ precedent); a durable evidence store is a later dot. CANONICAL RENDER ONLY here:
\ one versioned byte-stable render per row (cevid/v1) and per pair (cevid-cmp/v1);
\ every field participates. Each package owns its wire tokens (the maki/store.f
\ STORE-V$ precedent), so the render name words live here, not in package BENCH.
\
\ Fail closed: an over-capacity reading value, a wrong-category unit, a render past
\ the stable buffer, and an incomparable pairing are named throws. maki -> habu
\ only; competitive-evidence owns -5417..-5421.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/numpolicy.f            \ NPOL:dom witness + NPOL:NAME (the domain-match rejection)
require maki/target/target.f       \ CAD-KIND:target-id (TARGET:SM87) + TARGET:LABEL$
require maki/competitive-report.f  \ reuse BENCH:workload/shape/protocol/baseline/cache-state/absence

-5417 constant E-CEVID-CAP          \ a reading value beyond the representable capacity
-5418 constant E-CEVID-UNIT         \ a reading unit is outside its field's category
-5419 constant E-CEVID-ROW          \ a canonical row beyond the stable render buffer
-5420 constant E-CEVID-SLOT         \ evidence pool slot index out of range (unreachable; bounded)
-5421 constant E-CEVID-INCOMPARABLE \ a mismatched-domain pair formed into a comparison row

package CEVID
public

\ ---- the closed metric UNITS vocabulary (sealed; folded from run-metric) --------
ENUM unit DERIVE eq
   u-ns
   u-ms
   u-gflops
   u-gbps
   u-bytes
   u-count
   u-watts
;ENUM

\ ---- new identity vocabularies (workload/shape/protocol/baseline/cache reused from BENCH) ----
ENUM revision DERIVE eq
   saxpy-v4
   mm-blocked
   mmm-wide-b
   triton-jit
   triton-tuned
;ENUM
ENUM compiler DERIVE eq
   habu-ptxas
   triton-351
;ENUM

\ ---- a metric reading: a present (value,unit) or a named absence -----------------
SUMTYPE reading 0
   VARIANT rd-at n unit ;VARIANT
   VARIANT rd-na BENCH:absence ;VARIANT
;SUMTYPE

\ ---- the evidence-row handle: single-cell over a bounded pool slot ---------------
PRODUCT evidence 0
   FIELD slot n
;PRODUCT

\ ---- unit -> category (each metric field admits exactly one category) ------------
ENUM ucat DERIVE eq
   c-duration
   c-rate
   c-size
   c-tally
   c-power
;ENUM

private

: UCAT ( unit -- ucat )
   MATCH unit
      u-ns     OF CEVID-UCAT:C-DURATION ENDOF
      u-ms     OF CEVID-UCAT:C-DURATION ENDOF
      u-gflops OF CEVID-UCAT:C-RATE     ENDOF
      u-gbps   OF CEVID-UCAT:C-RATE     ENDOF
      u-bytes  OF CEVID-UCAT:C-SIZE     ENDOF
      u-count  OF CEVID-UCAT:C-TALLY    ENDOF
      u-watts  OF CEVID-UCAT:C-POWER    ENDOF
   ;MATCH ;

\ ---- value capacity (all readings non-negative; ceiling well above real rows) ----
1000000000000000 constant EV-VAL-MAX
: VAL-CK ( n -- n )  dup 0 < over EV-VAL-MAX > or if E-CEVID-CAP throw then ;

\ ---- bounded first-slice ring pool + parallel typed columns ----------------------
256 constant EV-CAP
variable ENEXT
: ESLOT ( -- n )  ENEXT @  dup 1+ EV-CAP mod ENEXT ! ;

EV-CAP TYPED-BUFFER EWL     BENCH:workload
EV-CAP TYPED-BUFFER EREV    revision
EV-CAP TYPED-BUFFER ESH     BENCH:shape
EV-CAP TYPED-BUFFER EPOL    NPOL:dom
EV-CAP TYPED-BUFFER ETGT    CAD-KIND:target-id
EV-CAP TYPED-BUFFER ECOMP   compiler
EV-CAP TYPED-BUFFER ECACHE  BENCH:cache-state
EV-CAP TYPED-BUFFER EPR     BENCH:protocol
EV-CAP TYPED-BUFFER EBL     BENCH:baseline
EV-CAP TYPED-BUFFER ELAT    reading
EV-CAP TYPED-BUFFER ETHR    reading
EV-CAP TYPED-BUFFER EBYT    reading
EV-CAP TYPED-BUFFER ELAUNCH reading
EV-CAP TYPED-BUFFER EMEM    reading
EV-CAP TYPED-BUFFER EENERGY reading

: EWL@ ( n -- BENCH:workload )     EWL @ ;
: EWL! ( BENCH:workload n -- )     EWL ! ;
: EREV@ ( n -- revision )          EREV @ ;
: EREV! ( revision n -- )          EREV ! ;
: ESH@ ( n -- BENCH:shape )        ESH @ ;
: ESH! ( BENCH:shape n -- )        ESH ! ;
: EPOL@ ( n -- NPOL:dom )          EPOL @ ;
: EPOL! ( NPOL:dom n -- )          EPOL ! ;
: ETGT@ ( n -- CAD-KIND:target-id ) ETGT @ ;
: ETGT! ( CAD-KIND:target-id n -- ) ETGT ! ;
: ECOMP@ ( n -- compiler )         ECOMP @ ;
: ECOMP! ( compiler n -- )         ECOMP ! ;
: ECACHE@ ( n -- BENCH:cache-state ) ECACHE @ ;
: ECACHE! ( BENCH:cache-state n -- ) ECACHE ! ;
: EPR@ ( n -- BENCH:protocol )     EPR @ ;
: EPR! ( BENCH:protocol n -- )     EPR ! ;
: EBL@ ( n -- BENCH:baseline )     EBL @ ;
: EBL! ( BENCH:baseline n -- )     EBL ! ;
: ELAT@ ( n -- reading )           ELAT @ ;
: ELAT! ( reading n -- )           ELAT ! ;
: ETHR@ ( n -- reading )           ETHR @ ;
: ETHR! ( reading n -- )           ETHR ! ;
: EBYT@ ( n -- reading )           EBYT @ ;
: EBYT! ( reading n -- )           EBYT ! ;
: ELAUNCH@ ( n -- reading )        ELAUNCH @ ;
: ELAUNCH! ( reading n -- )        ELAUNCH ! ;
: EMEM@ ( n -- reading )           EMEM @ ;
: EMEM! ( reading n -- )           EMEM ! ;
: EENERGY@ ( n -- reading )        EENERGY @ ;
: EENERGY! ( reading n -- )        EENERGY ! ;

\ ---- handle codec (single-cell PRODUCT over the slot) ----------------------------
: >EV ( n -- evidence )  CEVID-EVIDENCE:MAKE ;
: EV> ( evidence -- n )  CEVID-EVIDENCE:UNMAKE ;

\ ---- default every reading column of a fresh slot to not-measured -----------------
: NA-INIT ( n -- ) {: s:n :}
   BENCH-ABSENCE:NOT-MEASURED CEVID-READING:RD-NA s ELAT!
   BENCH-ABSENCE:NOT-MEASURED CEVID-READING:RD-NA s ETHR!
   BENCH-ABSENCE:NOT-MEASURED CEVID-READING:RD-NA s EBYT!
   BENCH-ABSENCE:NOT-MEASURED CEVID-READING:RD-NA s ELAUNCH!
   BENCH-ABSENCE:NOT-MEASURED CEVID-READING:RD-NA s EMEM!
   BENCH-ABSENCE:NOT-MEASURED CEVID-READING:RD-NA s EENERGY! ;

\ ---- per-package wire tokens (each package owns its render, STORE-V$ precedent) ---
: WL$ ( BENCH:workload -- ptr u8 n )
   MATCH BENCH:workload  saxpy OF s" saxpy" ENDOF  gemm OF s" gemm" ENDOF  ;MATCH ;
: SH$ ( BENCH:shape -- ptr u8 n )
   MATCH BENCH:shape  n1m OF s" n1m" ENDOF  sq2048 OF s" sq2048" ENDOF  ;MATCH ;
: PR$ ( BENCH:protocol -- ptr u8 n )
   MATCH BENCH:protocol  cuda-events OF s" cuda-events" ENDOF  wallclock OF s" wallclock" ENDOF  ;MATCH ;
: BL$ ( BENCH:baseline -- ptr u8 n )
   MATCH BENCH:baseline  triton OF s" triton" ENDOF  cublas OF s" cublas" ENDOF  ;MATCH ;
: CACHE$ ( BENCH:cache-state -- ptr u8 n )
   MATCH BENCH:cache-state  cold OF s" cold" ENDOF  warm OF s" warm" ENDOF  ;MATCH ;
: ABS$ ( BENCH:absence -- ptr u8 n )
   MATCH BENCH:absence
      not-measured OF s" not-measured" ENDOF
      device-gated OF s" device-gated" ENDOF
      waived       OF s" waived"       ENDOF
   ;MATCH ;
: REV$ ( revision -- ptr u8 n )
   MATCH revision
      saxpy-v4     OF s" saxpy-v4"           ENDOF
      mm-blocked   OF s" mm-cp-async-blocked" ENDOF
      mmm-wide-b   OF s" mmm-wide-b-m4-s1"   ENDOF
      triton-jit   OF s" triton-jit"         ENDOF
      triton-tuned OF s" triton-autotuned"   ENDOF
   ;MATCH ;
: COMP$ ( compiler -- ptr u8 n )
   MATCH compiler
      habu-ptxas OF s" ptxas-12.6"   ENDOF
      triton-351 OF s" triton-3.5.1" ENDOF
   ;MATCH ;
: U-NAME ( unit -- ptr u8 n )
   MATCH unit
      u-ns     OF s" ns"     ENDOF
      u-ms     OF s" ms"     ENDOF
      u-gflops OF s" gflops" ENDOF
      u-gbps   OF s" gbps"   ENDOF
      u-bytes  OF s" bytes"  ENDOF
      u-count  OF s" count"  ENDOF
      u-watts  OF s" watts"  ENDOF
   ;MATCH ;

\ ---- reading helpers -------------------------------------------------------------
: RD-APPEND ( reading -- )                     \ "<value>:<unit>" or "na:<absence>" onto the live SB
   MATCH reading
      rd-at OF  swap FMT:SB-INT s" :" SB-APPEND U-NAME SB-APPEND ENDOF
      rd-na OF  s" na:" SB-APPEND ABS$ SB-APPEND ENDOF
   ;MATCH ;

: RD-CAT-CK ( reading ucat -- )                \ a present reading's unit must be the field's category
   {: want:ucat :}
   MATCH reading
      rd-at OF  UCAT want CEVID-UCAT:EQ 0= if E-CEVID-UNIT throw then  drop ENDOF
      rd-na OF  drop ENDOF
   ;MATCH ;

: RD-UNIT? ( reading -- unit bool )            \ present -> (unit true); absent -> (u-ns false)
   MATCH reading
      rd-at OF  nip true ENDOF
      rd-na OF  drop CEVID-UNIT:U-NS false ENDOF
   ;MATCH ;

\ ---- stable render holds (SB is overwritten by the next render) ------------------
512 constant EV-ROW-CAP
create EV-ROW-BUF EV-ROW-CAP allot  variable EV-ROW-U
create HOLD-A EV-ROW-CAP allot      variable HOLD-A-U
create HOLD-B EV-ROW-CAP allot      variable HOLD-B-U
: HOLD-A! ( ptr u8 n -- ) {: a:ptr u:n :}
   u EV-ROW-CAP > if E-CEVID-ROW throw then  a HOLD-A u BYTE-COPY  u HOLD-A-U ! ;
: HOLD-A$ ( -- ptr u8 n )  HOLD-A HOLD-A-U @ ;
: HOLD-B! ( ptr u8 n -- ) {: a:ptr u:n :}
   u EV-ROW-CAP > if E-CEVID-ROW throw then  a HOLD-B u BYTE-COPY  u HOLD-B-U ! ;
: HOLD-B$ ( -- ptr u8 n )  HOLD-B HOLD-B-U @ ;

public

\ ---- category-pinned reading constructors ----------------------------------------
: >NS ( n -- reading )       VAL-CK CEVID-UNIT:U-NS     CEVID-READING:RD-AT ;
: >MS ( n -- reading )       VAL-CK CEVID-UNIT:U-MS     CEVID-READING:RD-AT ;
: >GFLOPS ( n -- reading )   VAL-CK CEVID-UNIT:U-GFLOPS CEVID-READING:RD-AT ;
: >GBPS ( n -- reading )     VAL-CK CEVID-UNIT:U-GBPS   CEVID-READING:RD-AT ;
: >BYTES ( n -- reading )    VAL-CK CEVID-UNIT:U-BYTES  CEVID-READING:RD-AT ;
: >LAUNCHES ( n -- reading ) VAL-CK CEVID-UNIT:U-COUNT  CEVID-READING:RD-AT ;
: >WATTS ( n -- reading )    VAL-CK CEVID-UNIT:U-WATTS  CEVID-READING:RD-AT ;
: ABSENT ( BENCH:absence -- reading )  CEVID-READING:RD-NA ;

\ ---- ROW: the total identity constructor; all six readings default not-measured --
: ROW ( BENCH:workload revision BENCH:shape NPOL:dom CAD-KIND:target-id compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- evidence )
   {: wl:BENCH:workload rev:revision sh:BENCH:shape pol:NPOL:dom tgt:CAD-KIND:target-id comp:compiler cache:BENCH:cache-state pr:BENCH:protocol bl:BENCH:baseline :}
   ESLOT {: s:n :}
   wl s EWL!  rev s EREV!  sh s ESH!  pol s EPOL!  tgt s ETGT!  comp s ECOMP!
   cache s ECACHE!  pr s EPR!  bl s EBL!
   s NA-INIT
   s >EV ;

\ ---- reading setters (threaded handle; each pins its field's unit category) -------
\ The reading rides the stack (a SUMTYPE is not a typed-local surface); only the
\ evidence handle binds. dup feeds RD-CAT-CK a copy, then the original is stored.
: LAT! ( evidence reading -- evidence )
   swap {: e:evidence :}  dup CEVID-UCAT:C-DURATION RD-CAT-CK  e EV> ELAT!  e ;
: THR! ( evidence reading -- evidence )
   swap {: e:evidence :}  dup CEVID-UCAT:C-RATE RD-CAT-CK  e EV> ETHR!  e ;
: BYT! ( evidence reading -- evidence )
   swap {: e:evidence :}  dup CEVID-UCAT:C-SIZE RD-CAT-CK  e EV> EBYT!  e ;
: LAUNCH! ( evidence reading -- evidence )
   swap {: e:evidence :}  dup CEVID-UCAT:C-TALLY RD-CAT-CK  e EV> ELAUNCH!  e ;
: MEM! ( evidence reading -- evidence )
   swap {: e:evidence :}  dup CEVID-UCAT:C-SIZE RD-CAT-CK  e EV> EMEM!  e ;
: ENERGY! ( evidence reading -- evidence )
   swap {: e:evidence :}  dup CEVID-UCAT:C-POWER RD-CAT-CK  e EV> EENERGY!  e ;

\ ---- reading accessors -----------------------------------------------------------
: LAT@ ( evidence -- reading )     EV> ELAT@ ;
: THR@ ( evidence -- reading )     EV> ETHR@ ;
: BYT@ ( evidence -- reading )     EV> EBYT@ ;
: LAUNCH@ ( evidence -- reading )  EV> ELAUNCH@ ;
: MEM@ ( evidence -- reading )     EV> EMEM@ ;
: ENERGY@ ( evidence -- reading )  EV> EENERGY@ ;
: POLICY@ ( evidence -- NPOL:dom ) EV> EPOL@ ;

\ ---- canonical versioned render (byte-stable; every field participates) ----------
: RENDER ( evidence -- ptr u8 n )
   EV> {: s:n :}
   SB-RESET
   s" cevid/v1|wl="  SB-APPEND  s EWL@ WL$ SB-APPEND
   s" |rev="    SB-APPEND s EREV@ REV$ SB-APPEND
   s" |sh="     SB-APPEND s ESH@ SH$ SB-APPEND
   s" |pol="    SB-APPEND s EPOL@ NPOL:NAME SB-APPEND
   s" |tgt="    SB-APPEND s ETGT@ TARGET:LABEL$ SB-APPEND
   s" |comp="   SB-APPEND s ECOMP@ COMP$ SB-APPEND
   s" |cache="  SB-APPEND s ECACHE@ CACHE$ SB-APPEND
   s" |pr="     SB-APPEND s EPR@ PR$ SB-APPEND
   s" |bl="     SB-APPEND s EBL@ BL$ SB-APPEND
   s" |lat="    SB-APPEND s ELAT@ RD-APPEND
   s" |thr="    SB-APPEND s ETHR@ RD-APPEND
   s" |byt="    SB-APPEND s EBYT@ RD-APPEND
   s" |launch=" SB-APPEND s ELAUNCH@ RD-APPEND
   s" |mem="    SB-APPEND s EMEM@ RD-APPEND
   s" |energy=" SB-APPEND s EENERGY@ RD-APPEND
   SB$ ;

: ROW-STABLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u EV-ROW-CAP > if E-CEVID-ROW throw then
   a EV-ROW-BUF u BYTE-COPY  u EV-ROW-U !
   EV-ROW-BUF EV-ROW-U @ ;

\ ---- comparability: two rows share a comparison ROW only on matched policy DOMAIN -
: THR-MATCH? ( unit bool unit bool -- bool ) {: ux:unit px:bool uy:unit py:bool :}
   px py and  ux uy CEVID-UNIT:EQ and ;
: COMPARABLE? ( evidence evidence -- bool ) {: x:evidence y:evidence :}
   x EV> {: sx:n :}
   y EV> {: sy:n :}
   sx EWL@    sy EWL@    BENCH-WORKLOAD:EQ
   sx ESH@    sy ESH@    BENCH-SHAPE:EQ and
   sx EPR@    sy EPR@    BENCH-PROTOCOL:EQ and
   sx ECACHE@ sy ECACHE@ BENCH-CACHE--STATE:EQ and
   sx EBL@    sy EBL@    BENCH-BASELINE:EQ and
   sx EPOL@   sy EPOL@   NPOL-DOM:EQ and
   sx ETHR@ RD-UNIT?  sy ETHR@ RD-UNIT?  THR-MATCH? and ;

\ ---- the comparison ROW: refused unless the two sides are comparable --------------
: RENDER-PAIR ( evidence evidence -- ptr u8 n )
   2dup COMPARABLE? 0= if 2drop E-CEVID-INCOMPARABLE throw then
   {: x:evidence y:evidence :}
   x RENDER HOLD-A!
   y RENDER HOLD-B!
   SB-RESET
   s" cevid-cmp/v1|comparable=yes|subj=" SB-APPEND HOLD-A$ SB-APPEND
   s" |base=" SB-APPEND HOLD-B$ SB-APPEND
   SB$ ;

private

: CEVID-INIT ( -- )  0 ENEXT ! ;
CEVID-INIT

;package
