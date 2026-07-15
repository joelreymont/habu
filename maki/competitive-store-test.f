\ maki/competitive-store-test.f - acceptance for the checked BENCH persistence codec
\ + sealed store records (dot habu-persist-typed-bench-2d15efa2).
\
\ Pins the dot's acceptance:
\ (a) the ONLY public writers derive a byte-stable canonical row from the typed
\     comparison alone (committed goldens for the SAXPY FP32 gbps row, the Habu-MMM /
\     Triton TF32 gflops row, and a device-gated absent baseline);
\ (b) rehydration of an untrusted persisted row yields the typed comparison, and every
\     forgery class REJECTS with its own named E-BENCH code (never a silent skip) - one
\     fixture per class: duplicate-field, malformed field count, bad label, bad token,
\     wrong-schema, cross-kind, cross-policy, digest-mismatch, noncanonical,
\     stale-promotion. Each negative resolves against the base golden, which is the same
\     row with the one broken axis fixed and decodes cleanly (positive control);
\ (c) round-trip: a valid comparison persists -> rehydrates -> re-persists BYTE-FOR-BYTE
\     (the committed golden), for both units and the absent-reading path;
\ (d) the store-seal bypass regression: the raw appender BENCH-ROW-APPEND is
\     package-BENCH-private, so a cross-package / qualified BENCH:BENCH-ROW-APPEND no
\     longer resolves (verdict 1), while the paired read control BENCH:BENCH-GET and the
\     typed public writers BENCH:BENCH-PUT-GBPS / BENCH:BENCH-ENCODE-GBPS still certify
\     (-1) through the same qualification - a real seal, not a broken qualifier.
\ CHECK-QUIET-CANDIDATE! returns -1 certified / 0 type-reject / 1 unresolvable.
\
\ Fixture numbers are the real device measurements (maki/competitive-report-test.f):
\ SAXPY-V4 64.209 GB/s vs Triton 63.0 GB/s; Habu MMM TF32 884.889 GFLOP/s vs Triton
\ TF32 1890.5 GFLOP/s - milli-units (x1000).

require lib/test.f
require lib/string.f
require test/checker-assert.f       \ CHECK-QUIET-CANDIDATE! for the store-seal bypass regression
require maki/competitive-store.f

T-RESET

\ ==== store-seal bypass regression (top-level / cross-package qualification) =======
\ The raw appender is sealed private (verdict 1); the read + typed-write surface resolves
\ (-1) through the same BENCH: qualification, so the seal is a real unresolvability.
s" SB-BYPASS ( ptr u8 n -- ) BENCH:BENCH-ROW-APPEND"
   CHECK-QUIET-CANDIDATE! 1 T=
s" SB-OK-GET ( ptr u8 n -- ptr u8 n bool ) BENCH:BENCH-GET"
   CHECK-QUIET-CANDIDATE! -1 T=
s" SB-OK-PUT ( BENCH:comparison-gbps -- ) BENCH:BENCH-PUT-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
s" SB-OK-ENC ( BENCH:comparison-gbps -- ptr u8 n ) BENCH:BENCH-ENCODE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=

package BENCH

\ ---- typed builders (the product never crosses an interpret boundary) ------------
: BASE ( -- comparison-gbps )        \ SAXPY FP32 gbps golden (both sides exact -> comparable)
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 >GBPS 63000 >GBPS COMPARE-GBPS ;
: MMM ( -- comparison-gflops )       \ Habu-MMM / Triton TF32 gflops golden (both relative)
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:RELATIVE 884889 >GFLOPS 1890500 >GFLOPS COMPARE-GFLOPS ;
: ABSROW ( -- comparison-gbps )      \ present subject, device-gated absent baseline
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:COLD NPOL-DOM:EXACT NPOL-DOM:EXACT
   64209 >GBPS BENCH-ABSENCE:DEVICE-GATED GBPS-ABSENT COMPARE-GBPS ;

\ ---- committed byte goldens (canonical render + derived metadata) ----------------
: GBPS-GOLD$ ( -- ptr u8 n )
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" ;
: GFLOPS-GOLD$ ( -- ptr u8 n )
   s" bench/v1|wl=gemm|sh=sq2048|pr=cuda-events|bl=triton|cache=warm|spol=rel|bpol=rel|unit=gflops|subj=884889|base=1890500|schema=bench/v1|mspol=rel|mbpol=rel|munit=gflops|digest=285B2855A495256A|promo=comparable" ;
: ABS-GOLD$ ( -- ptr u8 n )
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=cold|spol=exact|bpol=exact|unit=gbps|subj=64209|base=na:device-gated|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=8AE6BB189B5C62FD|promo=comparable" ;

\ ---- encode / decode / round-trip projections (return STRINGS or bools) ----------
: ENC-BASE$ ( -- ptr u8 n )  BASE   BENCH-ENCODE-GBPS ;
: ENC-MMM$  ( -- ptr u8 n )  MMM    BENCH-ENCODE-GFLOPS ;
: ENC-ABS$  ( -- ptr u8 n )  ABSROW BENCH-ENCODE-GBPS ;
: DEC-GOLD-ENC$ ( -- ptr u8 n )   \ decode the base golden and re-encode (in-memory round-trip)
   GBPS-GOLD$ BENCH-DECODE-GBPS BENCH-ENCODE-GBPS ;
: DEC-GOLD-COMPARABLE ( -- bool ) \ the rehydrated value carries the same COMPARABLE? verdict
   GBPS-GOLD$ BENCH-DECODE-GBPS GBPS-COMPARABLE? ;

\ persist -> get (the stored row) and persist -> get -> decode -> re-encode (rehydrate)
: RT-PERSIST-GBPS$ ( -- ptr u8 n )
   BENCH-STORE-RESET  BASE BENCH-PUT-GBPS
   BASE RENDER-GBPS BENCH-GET drop ;
: RT-REHYDRATE-GBPS$ ( -- ptr u8 n )
   BENCH-STORE-RESET  BASE BENCH-PUT-GBPS
   BASE RENDER-GBPS BENCH-GET drop  BENCH-DECODE-GBPS BENCH-ENCODE-GBPS ;
: RT-PERSIST-GFLOPS$ ( -- ptr u8 n )
   BENCH-STORE-RESET  MMM BENCH-PUT-GFLOPS
   MMM RENDER-GFLOPS BENCH-GET drop ;
: RT-REHYDRATE-GFLOPS$ ( -- ptr u8 n )
   BENCH-STORE-RESET  MMM BENCH-PUT-GFLOPS
   MMM RENDER-GFLOPS BENCH-GET drop  BENCH-DECODE-GFLOPS BENCH-ENCODE-GFLOPS ;
: RT-REHYDRATE-ABS$ ( -- ptr u8 n )   \ the na:<absence> reading survives the round-trip
   ENC-ABS$ BENCH-DECODE-GBPS BENCH-ENCODE-GBPS ;

\ ---- one fixture per rehydrate failure class (each throws its named code) ---------
\ Every fixture is the base golden with exactly ONE broken axis; the base golden itself
\ decodes cleanly (the resolving positive control below).
: F-DUP    ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|subj=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-FIELDS ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8" BENCH-DECODE-GBPS drop ;
: F-LABEL  ( -- )  s" bench/v1|xl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-TOKEN  ( -- )  s" bench/v1|wl=fma|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-SCHEMA ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v2|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-POLICY ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=rel|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-DIGEST ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=0000000000000000|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-CANON  ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=064209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable" BENCH-DECODE-GBPS drop ;
: F-PROMO  ( -- )  s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=incomparable" BENCH-DECODE-GBPS drop ;
\ cross-kind in both directions: a gflops row under the gbps decoder and vice versa.
: F-KIND-GB ( -- )  GFLOPS-GOLD$ BENCH-DECODE-GBPS drop ;
: F-KIND-GF ( -- )  GBPS-GOLD$   BENCH-DECODE-GFLOPS drop ;

T-RESET

\ ==== (a) byte goldens: the derived canonical row (commit these) ===================
ENC-BASE$ GBPS-GOLD$   T$=
ENC-MMM$  GFLOPS-GOLD$ T$=
ENC-ABS$  ABS-GOLD$    T$=

\ ==== positive control: the base golden rehydrates + re-encodes byte-identical ======
DEC-GOLD-ENC$ GBPS-GOLD$ T$=       \ rehydrate -> re-encode is the golden (resolving positive)
DEC-GOLD-COMPARABLE TTRUE          \ the rehydrated value's COMPARABLE? verdict survives

\ ==== (c) round-trip: persist -> rehydrate -> re-persist BYTE-FOR-BYTE =============
RT-PERSIST-GBPS$    GBPS-GOLD$   T$=   \ the persisted row == the golden
RT-REHYDRATE-GBPS$  GBPS-GOLD$   T$=   \ persist -> get -> decode -> re-persist == golden
RT-PERSIST-GFLOPS$  GFLOPS-GOLD$ T$=
RT-REHYDRATE-GFLOPS$ GFLOPS-GOLD$ T$=
RT-REHYDRATE-ABS$   ABS-GOLD$    T$=   \ the na:<absence> reading round-trips byte-for-byte

\ ==== (b) one reject per forgery class (each a distinct named throw) ===============
' F-DUP     E-BENCH-ROW-DUP    TTHROWS
' F-FIELDS  E-BENCH-ROW-FIELDS TTHROWS
' F-LABEL   E-BENCH-ROW-LABEL  TTHROWS
' F-TOKEN   E-BENCH-ROW-TOKEN  TTHROWS
' F-SCHEMA  E-BENCH-ROW-SCHEMA TTHROWS
' F-POLICY  E-BENCH-ROW-POLICY TTHROWS
' F-DIGEST  E-BENCH-ROW-DIGEST TTHROWS
' F-CANON   E-BENCH-ROW-CANON  TTHROWS
' F-PROMO   E-BENCH-ROW-PROMO  TTHROWS
' F-KIND-GB E-BENCH-ROW-KIND   TTHROWS
' F-KIND-GF E-BENCH-ROW-KIND   TTHROWS

BENCH-STORE-RESET
;package

T-REPORT
