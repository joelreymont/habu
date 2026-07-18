\ maki/competitive-evidence-store-test.f - acceptance for the durable typed cevid/v1
\ evidence-row store codec (maki/competitive-evidence-store.f; dot
\ habu-persist-typed-cevid-6f08452c).
\
\ Pins the dot's acceptance:
\ (a) ENCODE derives a byte-stable versioned row from the typed evidence alone: the
\     migrated flagship GEMM (MMM-WIDE-B-M4-S1 3026577 milli-GFLOP/s = 1.601x Triton) and
\     SAXPY (SAXPY-V4 64209 milli-GB/s) rows are committed goldens (the sibling
\     competitive-evidence-test.f render golden + the "|schema=cevid/v1" envelope);
\ (b) byte-stable round-trip: ENCODE -> DECODE -> re-ENCODE is byte-for-byte the golden,
\     for both flagship rows (the in-memory codec round-trip);
\ (c) durable round-trip: PUT -> (fresh handle) LOAD -> re-ENCODE is byte-for-byte the
\     golden - the flagship rows survive publication keyed by SHA-256(render) and rehydrate
\     with a byte-equal render (the acceptance tying the codec to reality);
\ (d) typed LOAD verdicts: absent (no file), malformed (a planted foreign valid row at the
\     content key -> content-path mismatch; and planted garbage -> structural reject), ok;
\ (e) the content key composes the WHOLE canonical render including cache-state - cold vs
\     warm is a DISTINCT durable file (the key-bearing cold/warm field);
\ (f) DECODE treats untrusted bytes fail-closed: one forgery per axis is the `malformed`
\     verdict (schema tag, meta schema, bad token, field count, bad label, wrong-category
\     unit, over-capacity value, non-canonical spelling), each resolving against the clean
\     base row that decodes ok.
\
\ Fixture numbers are the real device measurements (maki/competitive-evidence-test.f):
\ MMM-WIDE-B-M4-S1 3026.577 GFLOP/s, SAXPY-V4 64.209 GB/s; milli-scaled rate units. The
\ store writes to a private TMPDIR tree (never the shared default root).

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/competitive-evidence-store.f

package CEVIDSTORE-TEST

\ One private store dir for the whole suite; tests RESET their files as needed.
s" hb-cevidstore-test" TMPDIR-MKDIR CEVID:ROOT!

create CT-BUF 1024 allot  variable CT-BU        \ stable copy (a path / render survives the next build)
: CT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a CT-BUF u BYTE-COPY  u CT-BU ! ;

: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- the migrated flagship evidence handles (identical to competitive-evidence-test) ----
: ST-GEMM ( -- CEVID:evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MMM-WIDE-B BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON CEVID:ROW
   3026577 CEVID:>GFLOPS CEVID:THR!
   30 CEVID:>LAUNCHES CEVID:LAUNCH!
   50176 CEVID:>BYTES CEVID:MEM!
   25 CEVID:>WATTS CEVID:ENERGY! ;
: ST-SAXPY ( -- CEVID:evidence )
   BENCH-WORKLOAD:SAXPY CEVID-REVISION:SAXPY-V4 BENCH-SHAPE:N1M NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON CEVID:ROW
   64209 CEVID:>GBPS CEVID:THR!
   2516582400 CEVID:>BYTES CEVID:BYT!
   200 CEVID:>LAUNCHES CEVID:LAUNCH!
   25 CEVID:>WATTS CEVID:ENERGY! ;
\ the SAME GEMM handle but cache=cold: one key-bearing field changed.
: ST-GEMM-COLD ( -- CEVID:evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MMM-WIDE-B BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:COLD
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON CEVID:ROW
   3026577 CEVID:>GFLOPS CEVID:THR!
   30 CEVID:>LAUNCHES CEVID:LAUNCH!
   50176 CEVID:>BYTES CEVID:MEM!
   25 CEVID:>WATTS CEVID:ENERGY! ;

\ ---- committed byte goldens (the render golden + the "|schema=cevid/v1" envelope) --------
: GEMM-GOLD$ ( -- ptr u8 n )
   s" cevid/v1|wl=gemm|rev=mmm-wide-b-m4-s1|sh=sq2048|pol=rel|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=na:not-measured|thr=3026577:gflops|byt=na:not-measured|launch=30:count|mem=50176:bytes|energy=25:watts|schema=cevid/v1" ;
: SAXPY-GOLD$ ( -- ptr u8 n )
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;

\ ---- encode / round-trip projections (return STRINGS or verdict ordinals) ----------------
: ENC-GEMM$ ( -- ptr u8 n )   ST-GEMM  CEVID:ENCODE ;
: ENC-SAXPY$ ( -- ptr u8 n )  ST-SAXPY CEVID:ENCODE ;

: LR>N ( CEVID:load-result -- n )              \ 0 ok / 1 absent / 2 malformed
   MATCH CEVID:load-result
      ok        OF drop 0 ENDOF
      absent    OF 1 ENDOF
      malformed OF 2 ENDOF
   ;MATCH ;

: DEC>ENC$ ( ptr u8 n -- ptr u8 n )            \ DECODE ok -> re-ENCODE ; absent/malformed -> throw
   CEVID:DECODE MATCH CEVID:load-result
      ok        OF CEVID:ENCODE ENDOF
      absent    OF -777 throw ENDOF
      malformed OF -777 throw ENDOF
   ;MATCH ;
: DEC>N ( ptr u8 n -- n )   CEVID:DECODE LR>N ;

: RT-GEMM$ ( -- ptr u8 n )   ENC-GEMM$  DEC>ENC$ ;   \ encode -> decode -> re-encode
: RT-SAXPY$ ( -- ptr u8 n )  ENC-SAXPY$ DEC>ENC$ ;

\ ---- durable round-trip: PUT then LOAD a FRESH handle, re-ENCODE the rehydrated value ----
: LOAD>ENC$ ( CEVID:evidence -- ptr u8 n )
   CEVID:LOAD MATCH CEVID:load-result
      ok        OF CEVID:ENCODE ENDOF
      absent    OF -777 throw ENDOF
      malformed OF -777 throw ENDOF
   ;MATCH ;
: RT-PUT-GEMM$ ( -- ptr u8 n )   CEVID:RESET  ST-GEMM  CEVID:PUT  ST-GEMM  LOAD>ENC$ ;
: RT-PUT-SAXPY$ ( -- ptr u8 n )  CEVID:RESET  ST-SAXPY CEVID:PUT  ST-SAXPY LOAD>ENC$ ;

\ ---- durable verdict / keying probes -----------------------------------------------------
: T-HAS-YES ( -- bool )   CEVID:RESET  ST-GEMM CEVID:PUT  ST-GEMM CEVID:HAS? ;
: T-HAS-NO ( -- bool )    CEVID:RESET  ST-GEMM CEVID:HAS? 0= ;
: T-ABSENT ( -- n )       CEVID:RESET  ST-SAXPY CEVID:LOAD LR>N ;
: T-ROOT-PLUS ( -- bool ) CEVID:ROOT+ EXISTS? ;

: T-PATH-STABLE ( -- bool )                    \ same handle -> same content path across calls
   CEVID:RESET
   ST-GEMM CEVID:PATH$ {: pa:ptr pu:n :}
   pa CT-BUF pu BYTE-COPY
   ST-GEMM CEVID:PATH$ {: pb:ptr pu2:n :}
   pu pu2 = if CT-BUF pb pu MEM= else false then ;
: T-PATH-DISTINCT ( -- bool )                  \ cold vs warm (key-bearing) -> different content path
   CEVID:RESET
   ST-GEMM CEVID:PATH$ {: pa:ptr pu:n :}
   pa CT-BUF pu BYTE-COPY
   ST-GEMM-COLD CEVID:PATH$ {: pb:ptr pu2:n :}
   pu pu2 = if CT-BUF pb pu MEM= 0= else true then ;

: T-MALFORMED-CONTENT ( -- n )                 \ a foreign VALID row planted at the key -> content-path mismatch
   CEVID:RESET
   ST-GEMM CEVID:PUT
   ST-GEMM CEVID:PATH$ SAXPY-GOLD$ WRITE-ALL   \ plant SAXPY's row at GEMM's content key
   ST-GEMM CEVID:LOAD LR>N ;
: T-MALFORMED-GARBAGE ( -- n )                 \ planted structural garbage -> malformed
   CEVID:RESET
   ST-GEMM CEVID:PUT
   ST-GEMM CEVID:PATH$ s" not-a-cevid-row" WRITE-ALL
   ST-GEMM CEVID:LOAD LR>N ;

\ ---- one forgery per DECODE axis (each the malformed verdict; base row decodes ok) --------
\ Every fixture is the SAXPY base row with exactly ONE broken axis.
: F-SCHEMATAG$ ( -- ptr u8 n )
   s" cevid/v2|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;
: F-METASCHEMA$ ( -- ptr u8 n )
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v2" ;
: F-TOKEN$ ( -- ptr u8 n )
   s" cevid/v1|wl=fma|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;
: F-FIELDS$ ( -- ptr u8 n )   s" cevid/v1|wl=saxpy" ;
: F-LABEL$ ( -- ptr u8 n )
   s" cevid/v1|xl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;
: F-UNIT$ ( -- ptr u8 n )     \ a rate unit in the latency (duration) slot -> E-CEVID-UNIT
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=100:gflops|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;
: F-CAP$ ( -- ptr u8 n )      \ a reading value beyond EV-VAL-MAX -> E-CEVID-CAP
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=1000000000000001:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;
: F-CANON$ ( -- ptr u8 n )    \ a non-canonical value spelling (leading zero) -> E-CEVIDST-CANON
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=064209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts|schema=cevid/v1" ;

T-RESET

\ ==== (a) byte goldens: the derived canonical row (commit these) ===========================
ENC-GEMM$  GEMM-GOLD$  T$=
ENC-SAXPY$ SAXPY-GOLD$ T$=

\ ==== (b) in-memory round-trip: encode -> decode -> re-encode is byte-for-byte the golden ==
RT-GEMM$  GEMM-GOLD$  T$=
RT-SAXPY$ SAXPY-GOLD$ T$=

\ ==== (c) durable round-trip: PUT -> fresh-handle LOAD -> re-encode == golden ==============
RT-PUT-GEMM$  GEMM-GOLD$  T$=
RT-PUT-SAXPY$ SAXPY-GOLD$ T$=

\ ==== (d) typed LOAD verdicts + (e) content-key composition ================================
T-HAS-YES TTRUE
T-HAS-NO  TTRUE
T-ABSENT  1 T=                 \ no durable file -> absent
T-ROOT-PLUS TTRUE
T-PATH-STABLE TTRUE
T-PATH-DISTINCT TTRUE          \ cache-state is key-bearing: cold vs warm is a distinct file
T-MALFORMED-CONTENT 2 T=       \ a foreign valid row at the content key -> malformed
T-MALFORMED-GARBAGE 2 T=       \ structural garbage -> malformed

\ ==== (f) DECODE forgery classes: base decodes ok (0), each forgery is malformed (2) =======
SAXPY-GOLD$  DEC>N 0 T=        \ positive control: the clean base row decodes ok
F-SCHEMATAG$ DEC>N 2 T=
F-METASCHEMA$ DEC>N 2 T=
F-TOKEN$     DEC>N 2 T=
F-FIELDS$    DEC>N 2 T=
F-LABEL$     DEC>N 2 T=
F-UNIT$      DEC>N 2 T=
F-CAP$       DEC>N 2 T=
F-CANON$     DEC>N 2 T=

CEVID:RESET
;package

T-REPORT
