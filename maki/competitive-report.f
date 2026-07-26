\ maki/competitive-report.f - the checked BENCH competitive-comparison schema
\ (dot habu-define-checked-bench-5341ffc8, epic habu-v2-competitive-evidence-5d07d471;
\ MODEL-CAD-V2-PLAN.md:1376-1395 non-cherry-picked exact-key comparisons).
\
\ A competitive row used to be an opaque string plus raw n, so a historical Habu
\ FP32 GEMM number and a Triton TF32 GEMM number could masquerade as ONE
\ comparable result (docs/eval-triton.md: full-f32 fma.rn, golden-gated rtol 1e-4
\ vs a silently-licensed TF32 dot). This file makes each row a typed value whose
\ identity slots, cache state, metric UNIT, and per-side numeric-policy witness
\ are all in the type, so the confusion is a type mismatch or a rejected verdict,
\ never a silent one.
\
\ WHAT IS TYPED
\   - workload / shape / protocol / baseline: four DISTINCT nominal id families
\     (closed enums), so a workload id can never sit in the baseline slot and a
\     raw n can never sit in any id slot (both are checker rejects at construction).
\   - cache-state (cold|warm) and absence (named reasons) are closed sums.
\   - the metric UNIT is in the READING type: `gbps` and `gflops` are distinct
\     throughput families (each a present-value-or-named-absence sum), so a GFLOP/s
\     reading can never fill a GB/s slot and the two competitive families
\     `comparison-gbps` / `comparison-gflops` never mix units - unit confusion is a
\     type mismatch at the VALUE level.
\   - each side carries an `NPOL:dom` numeric-policy witness (maki/numpolicy.f);
\     the pairing verdict COMPARABLE? holds iff the two witnesses match, so an
\     exact-policy FP32 side and a relative-policy TF32 side give verdict 0 (the
\     historical confusion, rejected) and two matching relative-policy TF32 sides
\     give verdict -1 (the resolving positive).
\
\ WHY NOT A GENERIC comparison<a>. The R7 addendum spells this "comparison<a>"
\ over the unit `a`. A product parametrised over the unit IS expressible here
\ (comparison<gbps> / comparison<gflops> both check and run), but a product type
\ parameter binds only CELL-tier types (sum/enum/product families cannot
\ instantiate a param - verified), so the two distinct unit TYPES would have to be
\ nominal cells minted through TRUSTED casts, and the throughput reading would fall
\ back to a unit-agnostic sum: only the ROW type, not the NUMBER, would carry the
\ unit. The concrete per-unit families are strictly stronger and need no trust:
\ the throughput NUMBER is unit-typed (a gflops value cannot enter a gbps slot),
\ with ZERO added TRUSTED surface. The unit-generic spelling is thin sugar over
\ these two families and can be layered on later without weakening the guarantees.
\
\ CANONICAL RENDER ONLY (this dot): one versioned byte-stable render per unit;
\ every field participates in the rendered key, so cold/warm and every id/policy
\ field alters it. The typed store codec and any docs migration are the later
\ sibling dots (habu-persist-typed-bench, habu-migrate-saxpy-and).
\
\ Fail closed: a throughput value beyond the representable capacity and a render
\ beyond the stable-row buffer are named throws. maki -> habu only; competitive
\ report owns -5257..-5258.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/numpolicy.f            \ NPOL:dom - the per-side numeric-policy witness

-5257 constant E-BENCH-CAP     \ throughput value beyond the representable milli-unit capacity
-5258 constant E-BENCH-ROW     \ canonical row beyond the stable render buffer

package BENCH
public

\ ---- nominal identity families (distinct, so slots cannot swap) -------------
ENUM workload DERIVE eq
   saxpy
   gemm
;ENUM
ENUM shape DERIVE eq
   n1m
   sq2048
;ENUM
ENUM protocol DERIVE eq
   cuda-events
   wallclock
;ENUM
ENUM baseline DERIVE eq
   triton
   cublas
;ENUM

\ ---- closed cache-state and absence sums ------------------------------------
ENUM cache-state DERIVE eq
   cold
   warm
;ENUM
ENUM absence DERIVE eq
   not-measured
   device-gated
   waived
;ENUM

\ ---- unit-typed throughput readings: present value or named absence ----------
\ Both are declared through the unified ENUM front end in full mode (the arity token
\ after the name selects it), so each arm's payload is a named FIELD rather than a
\ positional type token. The present arm carries `milli`, the reading in milli-units
\ (x1000) that RATE-CK bounds against BENCH-RATE-MAX and FMT:SB-INT renders; the
\ absent arm carries `reason`, the named absence reason the row renders as
\ na:<reason>. These families must stay declared BEFORE the two comparison records
\ below, which use them as field types. The generated BENCH-GBPS:GBPS-AT / :GBPS-NA
\ and BENCH-GFLOPS:GFLOPS-AT / :GFLOPS-NA constructors and every MATCH site are
\ unchanged: spelling and payload binding order come from the package, the family
\ tail and the declaration order, not from the mode.
ENUM gbps 0
   VARIANT gbps-at FIELD milli n ;VARIANT
   VARIANT gbps-na FIELD reason absence ;VARIANT
;ENUM
ENUM gflops 0
   VARIANT gflops-at FIELD milli n ;VARIANT
   VARIANT gflops-na FIELD reason absence ;VARIANT
;ENUM

\ ---- the two per-unit competitive comparisons -------------------------------
\ Declared through the unified STRUCTURE front end; the header clause and every
\ FIELD line are unchanged, so BENCH-COMPARISON--GBPS:MAKE / :UNMAKE and their
\ GFLOP/s counterparts keep their exact spelling, effect and field order, and
\ COMPARE-*, *-COMPARABLE? and RENDER-* below plus tools/eval-triton.f and
\ maki/competitive-store.f are untouched. The four leading id fields are four
\ DISTINCT nominal families, so the checker refuses an exchange among them, but
\ `spol` and `bpol` are both NPOL:dom and `subj` and `base` are both the same
\ reading family - two same-typed pairs an exchange could pass silently through, so
\ maki/competitive-report-test.f pins every field NAME to its payload SLOT through
\ the type registry. Note the slots are not 0..8: `subj` is a two-cell reading, so
\ it occupies slots 7 and 8 and `base` begins at slot 9.
STRUCTURE comparison-gbps 0
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
STRUCTURE comparison-gflops 0
   FIELD wl workload
   FIELD sh shape
   FIELD pr protocol
   FIELD bl baseline
   FIELD cache cache-state
   FIELD spol NPOL:dom
   FIELD bpol NPOL:dom
   FIELD subj gflops
   FIELD base gflops
;STRUCTURE

private

1000000000 constant BENCH-RATE-MAX
32         constant BENCH-HOLD-CAP
256        constant BENCH-ROW-CAP

: RATE-CK ( n -- n ) {: milli:n :}
   milli 0 <  milli BENCH-RATE-MAX >  or if E-BENCH-CAP throw then  milli ;

create SUBJ-HOLD BENCH-HOLD-CAP allot   variable SUBJ-HOLD-U
create BASE-HOLD BENCH-HOLD-CAP allot   variable BASE-HOLD-U
create ROW-BUF   BENCH-ROW-CAP allot    variable ROW-BUF-U

: SUBJ-HOLD! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BENCH-HOLD-CAP > if E-BENCH-ROW throw then
   a SUBJ-HOLD u BYTE-COPY  u SUBJ-HOLD-U ! ;
: BASE-HOLD! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BENCH-HOLD-CAP > if E-BENCH-ROW throw then
   a BASE-HOLD u BYTE-COPY  u BASE-HOLD-U ! ;
: SUBJ-HOLD$ ( -- ptr u8 n )  SUBJ-HOLD SUBJ-HOLD-U @ ;
: BASE-HOLD$ ( -- ptr u8 n )  BASE-HOLD BASE-HOLD-U @ ;

: WL-NAME ( workload -- ptr u8 n )
   MATCH workload  saxpy OF s" saxpy" ENDOF  gemm OF s" gemm" ENDOF  ;MATCH ;
: SH-NAME ( shape -- ptr u8 n )
   MATCH shape  n1m OF s" n1m" ENDOF  sq2048 OF s" sq2048" ENDOF  ;MATCH ;
: PR-NAME ( protocol -- ptr u8 n )
   MATCH protocol  cuda-events OF s" cuda-events" ENDOF  wallclock OF s" wallclock" ENDOF  ;MATCH ;
: BL-NAME ( baseline -- ptr u8 n )
   MATCH baseline  triton OF s" triton" ENDOF  cublas OF s" cublas" ENDOF  ;MATCH ;
: CACHE-NAME ( cache-state -- ptr u8 n )
   MATCH cache-state  cold OF s" cold" ENDOF  warm OF s" warm" ENDOF  ;MATCH ;
: ABSENCE-NAME ( absence -- ptr u8 n )
   MATCH absence
      not-measured OF s" not-measured" ENDOF
      device-gated OF s" device-gated" ENDOF
      waived OF s" waived" ENDOF
   ;MATCH ;

: GBPS$ ( gbps -- ptr u8 n )
   SB-RESET
   MATCH gbps
      gbps-at OF FMT:SB-INT ENDOF
      gbps-na OF s" na:" SB-APPEND ABSENCE-NAME SB-APPEND ENDOF
   ;MATCH  SB$ ;
: GFLOPS$ ( gflops -- ptr u8 n )
   SB-RESET
   MATCH gflops
      gflops-at OF FMT:SB-INT ENDOF
      gflops-na OF s" na:" SB-APPEND ABSENCE-NAME SB-APPEND ENDOF
   ;MATCH  SB$ ;

: GBPS-DROP ( gbps -- )
   MATCH gbps  gbps-at OF drop ENDOF  gbps-na OF drop ENDOF  ;MATCH ;
: GFLOPS-DROP ( gflops -- )
   MATCH gflops  gflops-at OF drop ENDOF  gflops-na OF drop ENDOF  ;MATCH ;

: PREFIX+ ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom -- )
   {: wl:workload sh:shape pr:protocol bl:baseline cache:cache-state spol:NPOL:dom bpol:NPOL:dom :}
   SB-RESET
   s" bench/v1|wl=" SB-APPEND wl WL-NAME SB-APPEND
   s" |sh="    SB-APPEND sh SH-NAME SB-APPEND
   s" |pr="    SB-APPEND pr PR-NAME SB-APPEND
   s" |bl="    SB-APPEND bl BL-NAME SB-APPEND
   s" |cache=" SB-APPEND cache CACHE-NAME SB-APPEND
   s" |spol="  SB-APPEND spol NPOL:NAME SB-APPEND
   s" |bpol="  SB-APPEND bpol NPOL:NAME SB-APPEND ;
: HEAD-EQ? ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom -- bool )
   {: wl:workload sh:shape pr:protocol bl:baseline cache:cache-state spol:NPOL:dom bpol:NPOL:dom :}
   spol bpol NPOL-DOM:EQ ;

public

: >GBPS ( n -- gbps )  RATE-CK BENCH-GBPS:GBPS-AT ;
: >GFLOPS ( n -- gflops )  RATE-CK BENCH-GFLOPS:GFLOPS-AT ;
: GBPS-ABSENT ( absence -- gbps )  BENCH-GBPS:GBPS-NA ;
: GFLOPS-ABSENT ( absence -- gflops )  BENCH-GFLOPS:GFLOPS-NA ;

: COMPARE-GBPS ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom gbps gbps -- comparison-gbps )
   BENCH-COMPARISON--GBPS:MAKE ;
: COMPARE-GFLOPS ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom gflops gflops -- comparison-gflops )
   BENCH-COMPARISON--GFLOPS:MAKE ;

: GBPS-COMPARABLE? ( comparison-gbps -- bool )
   BENCH-COMPARISON--GBPS:UNMAKE
   GBPS-DROP GBPS-DROP
   HEAD-EQ? ;
: GFLOPS-COMPARABLE? ( comparison-gflops -- bool )
   BENCH-COMPARISON--GFLOPS:UNMAKE
   GFLOPS-DROP GFLOPS-DROP
   HEAD-EQ? ;

: RENDER-GBPS ( comparison-gbps -- ptr u8 n )
   BENCH-COMPARISON--GBPS:UNMAKE
   GBPS$ BASE-HOLD!
   GBPS$ SUBJ-HOLD!
   PREFIX+
   s" |unit=gbps|subj=" SB-APPEND SUBJ-HOLD$ SB-APPEND
   s" |base=" SB-APPEND BASE-HOLD$ SB-APPEND
   SB$ ;
: RENDER-GFLOPS ( comparison-gflops -- ptr u8 n )
   BENCH-COMPARISON--GFLOPS:UNMAKE
   GFLOPS$ BASE-HOLD!
   GFLOPS$ SUBJ-HOLD!
   PREFIX+
   s" |unit=gflops|subj=" SB-APPEND SUBJ-HOLD$ SB-APPEND
   s" |base=" SB-APPEND BASE-HOLD$ SB-APPEND
   SB$ ;

: ROW-STABLE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u BENCH-ROW-CAP > if E-BENCH-ROW throw then
   a ROW-BUF u BYTE-COPY  u ROW-BUF-U !
   ROW-BUF ROW-BUF-U @ ;

;package
