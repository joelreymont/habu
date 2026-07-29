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
require test/checker-assert.f      \ CHECK-QUIET-CANDIDATE! plus REFLECT, which reads the registry
require maki/competitive-evidence-store.f
\ The OTHER production `load-result`. Loaded here so both families sharing the tail are
\ registered in ONE process: that is the only condition under which the pair-keyed
\ identity pins and the cross-package non-unification negatives below actually prove
\ anything. With just one of them loaded the tail would be trivially unambiguous.
require maki/db/diff-case-store.f

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

\ ---- load-result: construct each arm directly and dispatch it back ------------------------
\ LOAD and DECODE reach these arms only through a real file probe or a real byte decode.
\ These construct each arm DIRECTLY through the generated constructors the LR-* wrappers
\ compile against, and match it straight back, so the named `evidence` FIELD is proven to
\ bind in declaration order. Construction and matching are separate words because the
\ checker requires MATCH's scrutinee to be a concretely instantiated family value.
: MK-OK ( CEVID:evidence -- CEVID:load-result )   CEVID-LOAD--RESULT:OK ;
: MK-AB ( -- CEVID:load-result )                  CEVID-LOAD--RESULT:ABSENT ;
: MK-MF ( -- CEVID:load-result )                  CEVID-LOAD--RESULT:MALFORMED ;

: ARM>N ( CEVID:load-result -- n )     \ 1 ok / 2 absent / 3 malformed
   MATCH CEVID:load-result
      ok        OF drop 1 ENDOF
      absent    OF 2 ENDOF
      malformed OF 3 ENDOF
   ;MATCH ;

\ The carried handle is re-ENCODEd rather than compared as a cell: the render is derived
\ from the whole evidence, so a payload the constructor dropped, zeroed or swapped cannot
\ reproduce the golden row. That is the non-zero payload discipline for a handle payload -
\ a zeroed handle has no valid render at all, and an exchanged one renders differently.
: OK>ENC$ ( CEVID:load-result -- ptr u8 n )       \ the ok arm's payload, re-encoded
   MATCH CEVID:load-result
      ok        OF CEVID:ENCODE ENDOF
      absent    OF -777 throw ENDOF
      malformed OF -777 throw ENDOF
   ;MATCH ;

: RT-ARM-OK ( -- n )    ST-SAXPY MK-OK ARM>N ;
: RT-ARM-AB ( -- n )    MK-AB ARM>N ;
: RT-ARM-MF ( -- n )    MK-MF ARM>N ;
: RT-OK-CARRIES$ ( -- ptr u8 n )   ST-SAXPY MK-OK OK>ENC$ ;
: RT-OK-DISTINCT ( -- bool )       \ the field carries THIS handle, not just any handle
   ST-GEMM MK-OK OK>ENC$ CT-COPY
   ST-SAXPY MK-OK OK>ENC$ {: sa:ptr su:n :}
   su CT-BU @ = if CT-BUF sa su MEM= 0= else true then ;

\ Every row below runs AFTER T-RESET: that call zeroes both the case and the FAILURE
\ counter, so an assertion placed above it is reported by T-REPORT as if it had never run -
\ a red row there would leave the suite green.
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;
: UNRESOLVED ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! 1 T= ;

\ The two production families that share the tail `load-result`. Identity is the tail PLUS
\ the constructor package its variants carry; the bare tail names neither of them.
: LR$ ( -- ptr u8 n ptr u8 n )     s" load-result" s" CEVID-LOAD--RESULT" ;
: CS-LR$ ( -- ptr u8 n ptr u8 n )  s" load-result" s" CASESTORE-LOAD--RESULT" ;

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

\ ==== (g) load-result construct + dispatch, per arm ========================================
RT-ARM-OK 1 T=                 \ a constructed ok dispatches to its own arm
RT-ARM-AB 2 T=                 \ absent dispatches to its own arm
RT-ARM-MF 3 T=                 \ malformed dispatches to its own arm
RT-OK-CARRIES$ SAXPY-GOLD$ T$= \ and the named field carries the handle through unchanged
RT-OK-DISTINCT TTRUE           \ a DIFFERENT handle comes back different, so the pin is live

\ ==== (h) the generated constructors: exact spelling + exact effect ========================
\ The SPELLING is load-bearing: the checker answers 1 (uncheckable) for a name it cannot
\ resolve and -1 only for one it resolved and accepted, so a -1 means EXACTLY this
\ constructor name typechecked; a 0 means the name resolved and the TYPES were refused. The
\ two 1-verdict rows are the calibration that proves that split.
s" CC-OK ( CEVID:evidence -- CEVID:load-result ) CEVID-LOAD--RESULT:OK" YES
s" CC-AB ( -- CEVID:load-result ) CEVID-LOAD--RESULT:ABSENT" YES
s" CC-MF ( -- CEVID:load-result ) CEVID-LOAD--RESULT:MALFORMED" YES
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is not a
\ bare scalar, and the payload is mandatory.
s" CC-RAW ( n -- CEVID:load-result ) CEVID-LOAD--RESULT:OK" NO
s" CC-BARE ( CEVID:evidence -- n ) CEVID-LOAD--RESULT:OK" NO
s" CC-NONE ( -- CEVID:load-result ) CEVID-LOAD--RESULT:OK" NO
s" CC-K1 ( -- CEVID:load-result ) CEVID-LOAD--RESULT:NOPE" UNRESOLVED
s" CC-K2 ( -- CEVID:load-result ) CEVID-LOAD--RESULTX:ABSENT" UNRESOLVED

\ ==== (i) the shared tail: two production families, neither unifying with the other =======
\ This is the cross-PACKAGE negative, and it needs no synthetic twin: maki/db/diff-case-store.f
\ declares a REAL second `load-result`, so each row below asks one production store's
\ constructor to build the other store's result. Both directions, on the payload arm and on
\ the payloadless arms. The YES rows above are the positive controls that keep these from
\ passing by being unresolvable rather than ill-typed.
s" DX-1 ( CEVID:evidence -- CASESTORE:load-result<CEVID:evidence> ) CEVID-LOAD--RESULT:OK" NO
s" DX-2 ( n -- CEVID:load-result ) CASESTORE-LOAD--RESULT:OK" NO
s" DX-3 ( -- CASESTORE:load-result<n> ) CEVID-LOAD--RESULT:ABSENT" NO
s" DX-4 ( -- CEVID:load-result ) CASESTORE-LOAD--RESULT:ABSENT" NO
s" DX-5 ( -- CASESTORE:load-result<n> ) CEVID-LOAD--RESULT:MALFORMED" NO
s" DX-6 ( -- CEVID:load-result ) CASESTORE-LOAD--RESULT:MALFORMED" NO
\ CASESTORE's fourth case does not exist in CEVID's family at all.
s" DX-7 ( -- CEVID:load-result ) CEVID-LOAD--RESULT:MISMATCH" UNRESOLVED

\ ==== (j) the recorded declaration shape, pair-keyed ======================================
\ Both families are registered in this process and share the tail, so these two blocks are
\ the R7 proof: each identity resolves EXACTLY one family, and the two resolve DIFFERENT
\ families - ARITY 0 vs 1 and VARS 3 vs 4 could not both hold if the pair-keying collapsed
\ to the tail. These pins are the case-order and payload-slot detector either family has.
LR$ REFLECT:FAMS 1 T=
LR$ REFLECT:KIND TK-SUM T=              \ a payload family stays a general sum ...
LR$ REFLECT:KIND TK-ENUM = 0 T=         \ ... and is NOT recorded as a compact enum
LR$ REFLECT:ARITY 0 T=                  \ CEVID's carries a concrete handle, no parameter
LR$ REFLECT:WIDTH 2 T=                  \ one payload cell plus one tag cell
LR$ REFLECT:VIS 1 T=                    \ public, so the constructors are generated
LR$ REFLECT:VARS 3 T=
LR$ 0 REFLECT:ARM$ s" ok" T$=           \ case order fixes the tags
LR$ 1 REFLECT:ARM$ s" absent" T$=
LR$ 2 REFLECT:ARM$ s" malformed" T$=
LR$ 0 REFLECT:ARM-CTOR$ s" CEVID-LOAD--RESULT" T$=      \ constructor spelling
LR$ 2 REFLECT:ARM-CTOR$ s" CEVID-LOAD--RESULT" T$=
LR$ 0 REFLECT:ARM-FLDS 1 T=             \ exactly one named cell on ok, none elsewhere
LR$ 1 REFLECT:ARM-FLDS 0 T=
LR$ 2 REFLECT:ARM-FLDS 0 T=
LR$ 0 s" evidence" REFLECT:ARM-SLOT 0 T=   \ the payload is named `evidence` at slot 0
LR$ 0 s" evidence" REFLECT:ARM-CELLS 1 T=
LR$ 1 s" evidence" REFLECT:ARM-SLOT -1 T=  \ the name is per-arm: absent carries none
LR$ 0 s" ev" REFLECT:ARM-SLOT -1 T=        \ and it is `evidence`, not the EVID-style `ev`
\ the same tail under the OTHER constructor package is the OTHER family, not this one.
CS-LR$ REFLECT:FAMS 1 T=
CS-LR$ REFLECT:ARITY 1 T=               \ CASESTORE's is parametric where CEVID's is not
CS-LR$ REFLECT:VARS 4 T=                \ and has the fourth `mismatch` case
CS-LR$ 3 REFLECT:ARM$ s" mismatch" T$=
CS-LR$ 0 s" slot" REFLECT:ARM-SLOT 0 T=    \ its payload is named `slot`, not `evidence`
CS-LR$ 0 s" evidence" REFLECT:ARM-SLOT -1 T=
LR$ 0 s" slot" REFLECT:ARM-SLOT -1 T=      \ and CEVID's is not named `slot`
\ a tail with a constructor package no family carries resolves nothing at all.
s" load-result" s" NOPE-LOAD--RESULT" REFLECT:FAMS 0 T=

CEVID:RESET
;package

T-REPORT
