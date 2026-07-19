\ maki/prec-attr-test.f - per-op COMPUTE-PRECISION attribute + reduced-precision GOLDEN
\ tolerance contract (maki/prec-attr.f, dot habu-per-op-precision, docs/model-unified.md
\ "Per-op compute precision").
\
\ Host-only. Proves the attribute plumbing and its policy:
\   - tags + names + range fail-closed;
\   - attrs-cell composition: precision packs into the HIGH field over the op's LOW
\     payload (equation slot / 0), tf32 is byte-identical, an fp16/bf16 tag leaves the
\     payload intact (CPREC-PAYLOAD@);
\   - the GEMM-class guard: only matmul/linear/equation are taggable, everything else is
\     the named E-CPREC-OP reject;
\   - capture -> IR -> executor round-trip: a fp16-tagged matmul carries CPREC-FP16 through
\     MODEL: capture into its IR node, yet the f32/f64-exact host executor produces the
\     SAME result as the tf32 capture (the tag is a device-lowering knob only);
\   - the reduced-precision golden tolerance rows, derived from the mantissa width.
\
\ GOLDEN TOLERANCE DERIVATION (the test-header contract; host executor is f32/f64 EXACT).
\ A reduced-precision GEMM golden is compared under a tolerance DERIVED from the compute
\ dtype's mantissa - never zero-tolerance integer-fill on real data, never a bare epsilon.
\ Error model: each input is rounded f32 -> the reduced type before the tensor-core product
\ (unit roundoff u = 2^-(p+1), p = stored mantissa bits); the accumulator stays f32, so the
\ dot-product relative error is bounded by the input rounding (a small constant times u).
\   tf32/fp16: p=10 -> u = 2^-11 ~= 4.9e-4. fp16 shares tf32's 10-bit significand, so its
\     bound EQUALS the measured tf32 GEMM row (maki/precision.f rtol 2e-3 ~= 4u, ~2.5x over
\     the measured ~8e-4).
\   bf16: p=7 -> u = 2^-8 ~= 3.9e-3, 8x tf32's unit roundoff -> rtol 2e-2.
\   atol: the f32 accumulator floor 1e-6 for every dtype.
\ These analytical bounds REPLACE NOTHING: the gate-licensed rows (maki/precision.f) stay
\ f32/tf32-only; an fp16/bf16 licensed row lands with a device measurement (the MMA lane).

require lib/test.f
require lib/float.f
require maki/prec-attr.f
require maki/cad.f
require maki/executor.f

package MAKI

\ float equality within a hair (the rows are exact powers of ten times a mantissa)
: PA-TR= ( r r -- )  f- fabs 0.000000000001 f< TTRUE ;
: PA-I ( ptr a n -- n )  T-GET 0.5 f+ f>s ;   \ read cell as nearest int

\ ---- fail-closed probes (top level cannot push quotations) -------------------
: TRY-PACK-BAD      ( -- )  0 CPREC-N CPREC-PACK drop ;
: TRY-NAME-BAD      ( -- )  CPREC-N CPREC-NAME 2drop ;
: TRY-DEFAULT-BAD   ( -- )  CPREC-N CPREC-DEFAULT! ;
: TRY-GOLD-RTOL-BAD ( -- )  CPREC-N CPREC-GOLD-RTOL fdrop ;
: TRY-GOLD-ATOL-BAD ( -- )  CPREC-N CPREC-GOLD-ATOL fdrop ;
: TRY-TAG-NONGEMM   ( -- )  0 CPREC-TF32 MAKI-OPKIND:GELU CPREC-TAG drop ;
: TRY-GEMM-CK-NON   ( -- )  MAKI-OPKIND:SOFTMAX-ROW CPREC-GEMM-CK ;

\ ---- matmul round-trip fixture: X=[[1,2],[3,4]] W=[[5,6],[7,8]] -> [[19,22],[43,50]] --
create MB-X 4 cells allot   create MB-W 4 cells allot
: MB-FILL ( -- )
   1.0 MB-X 0 T-SET 2.0 MB-X 1 T-SET 3.0 MB-X 2 T-SET 4.0 MB-X 3 T-SET
   5.0 MB-W 0 T-SET 6.0 MB-W 1 T-SET 7.0 MB-W 2 T-SET 8.0 MB-W 3 T-SET ;
MB-FILL

T-RESET

\ ---- tags + names ------------------------------------------------------------
CPREC-TF32 CPREC-NAME s" tf32" T$=
CPREC-FP16 CPREC-NAME s" fp16" T$=
CPREC-BF16 CPREC-NAME s" bf16" T$=

\ ---- attrs-cell composition: precision HIGH, payload LOW ----------------------
\ tf32 default is byte-identical; a payload (equation-slot-like) survives an fp16/bf16 tag.
0 CPREC-TF32 CPREC-PACK 0 T=                                    \ tf32 + payload 0 = 0 (unchanged)
7 CPREC-TF32 CPREC-PACK 7 T=                                    \ tf32 + slot 7 = 7 (unchanged)
0 CPREC-FP16 CPREC-PACK CPREC@ CPREC-FP16 T=
7 CPREC-BF16 CPREC-PACK dup CPREC@ CPREC-BF16 T=  CPREC-PAYLOAD@ 7 T=   \ slot survives the tag
0 CPREC-FP16 CPREC-PACK CPREC-PAYLOAD@ 0 T=

\ ---- GEMM-class predicate ----------------------------------------------------
MAKI-OPKIND:MATMUL   CPREC-GEMM? TTRUE
MAKI-OPKIND:LINEAR   CPREC-GEMM? TTRUE
MAKI-OPKIND:EQUATION CPREC-GEMM? TTRUE
MAKI-OPKIND:GELU     CPREC-GEMM? TFALSE
MAKI-OPKIND:ADD      CPREC-GEMM? TFALSE
MAKI-OPKIND:SEG-ATTN CPREC-GEMM? TFALSE

\ ---- CPREC-TAG happy path (the single guarded tagging entry) ------------------
0 CPREC-FP16 MAKI-OPKIND:MATMUL   CPREC-TAG CPREC@ CPREC-FP16 T=
7 CPREC-BF16 MAKI-OPKIND:EQUATION CPREC-TAG dup CPREC@ CPREC-BF16 T=  CPREC-PAYLOAD@ 7 T=

\ ---- workload default (set / get / reset) ------------------------------------
CPREC-DEFAULT-RESET  CPREC-DEFAULT@ CPREC-TF32 T=
CPREC-BF16 CPREC-DEFAULT!  CPREC-DEFAULT@ CPREC-BF16 T=
CPREC-DEFAULT-RESET  CPREC-DEFAULT@ CPREC-TF32 T=

\ ---- reduced-precision golden tolerance rows (derived; see file head) ---------
CPREC-TF32 CPREC-GOLD-RTOL 0.002    PA-TR=       \ tf32/fp16: 10-bit significand, 4u
CPREC-FP16 CPREC-GOLD-RTOL 0.002    PA-TR=       \ fp16 shares tf32's significand
CPREC-BF16 CPREC-GOLD-RTOL 0.02     PA-TR=       \ bf16: 8x unit roundoff
CPREC-TF32 CPREC-GOLD-ATOL 0.000001 PA-TR=       \ f32 accumulator floor, all dtypes
CPREC-FP16 CPREC-GOLD-ATOL 0.000001 PA-TR=
CPREC-BF16 CPREC-GOLD-ATOL 0.000001 PA-TR=

\ ---- fail-closed throws ------------------------------------------------------
' TRY-PACK-BAD      E-CPREC-TAG TTHROWS      \ pack an out-of-range tag
' TRY-NAME-BAD      E-CPREC-TAG TTHROWS      \ render an out-of-range tag
' TRY-DEFAULT-BAD   E-CPREC-TAG TTHROWS      \ set the workload default out of range
' TRY-GOLD-RTOL-BAD E-CPREC-TAG TTHROWS      \ tolerance query on an out-of-range tag
' TRY-GOLD-ATOL-BAD E-CPREC-TAG TTHROWS
' TRY-TAG-NONGEMM   E-CPREC-OP  TTHROWS      \ tag a non-GEMM op (gelu)
' TRY-GEMM-CK-NON   E-CPREC-OP  TTHROWS      \ guard a non-GEMM op (softmax-row)

\ ---- capture -> IR -> executor round-trip: tf32 default (byte-identical attr) -----
CPREC-DEFAULT-RESET
MODEL: PCMM-TF32 ( x:2x2 w:2x2 -- y ) MATMUL ;
MODEL-K 1 T=
0 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:MATMUL MAKI-OPKIND:EQ -1 T=
0 MIR-NODE-ID MIR-ATTR@ 0 T=                              \ tf32 matmul: attrs byte-identical (0)
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-TF32 T=
EX-RESET  MB-X 0 MIR-SLOT-ID EX-BIND  MB-W 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 PA-I 19 T=  0 MIR-NODE-ID EX-OUT@ 1 PA-I 22 T=
0 MIR-NODE-ID EX-OUT@ 2 PA-I 43 T=  0 MIR-NODE-ID EX-OUT@ 3 PA-I 50 T=

\ ---- capture -> IR -> executor round-trip: fp16 opt-in (tag rides, executor unchanged) --
CPREC-FP16 CPREC-DEFAULT!
MODEL: PCMM-FP16 ( x:2x2 w:2x2 -- y ) MATMUL ;
CPREC-DEFAULT-RESET                                       \ restore immediately (no cross-suite leak)
MODEL-K 1 T=
0 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:MATMUL MAKI-OPKIND:EQ -1 T=
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-FP16 T=             \ the tag rode capture -> IR
0 MIR-NODE-ID MIR-ATTR@ CPREC-PAYLOAD@ 0 T=              \ matmul payload still 0
EX-RESET  MB-X 0 MIR-SLOT-ID EX-BIND  MB-W 1 MIR-SLOT-ID EX-BIND  EX-RUN
0 MIR-NODE-ID EX-OUT@ 0 PA-I 19 T=  0 MIR-NODE-ID EX-OUT@ 1 PA-I 22 T=   \ f32-exact, tag ignored
0 MIR-NODE-ID EX-OUT@ 2 PA-I 43 T=  0 MIR-NODE-ID EX-OUT@ 3 PA-I 50 T=

T-REPORT

;package
