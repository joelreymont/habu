\ maki/prec-grammar-test.f - per-op precision GRAMMAR tokens (dot habu-per-op-precision-49716b19,
\ follow-up to habu-per-op-precision-4b64eee3). Proves the explicit per-op source that the
\ attribute plumbing (maki/prec-attr.f) was missing: a `<OP>:<PREC>` body token at the MODEL:
\ layer (MATMUL:FP16, LINEAR:BF16, <EQ>:TF32) parsed in maki/cad.f, resolving to the base op
\ + CPREC-TAG on the captured node. Attribute plumbing + policy only - NO device codegen.
\
\ The grammar overrides the workload default (CPREC-DEFAULT@) for the TAGGED node ALONE: the
\ parser emits a CPREC-NEXT! before each tagged composer and every GEMM composer resolves its
\ precision through CPREC-NEXT@ (override consumed -> one node; else the default), so two
\ differently tagged ops each carry their own precision and an untagged sibling keeps the
\ default. GEMM-class only (matmul/linear/equation), enforced by CPREC-GEMM? - a precision
\ suffix on a non-GEMM op is E-CPREC-OP; an unknown precision suffix on a GEMM op is
\ E-CPREC-TAG (matmul/linear/equation carry no ":params" movement form). Round-trip: MODEL:
\ capture -> IR, asserting MIR-ATTR bits[33:32] (CPREC@) per node and the equation registry
\ slot intact in the low-32 payload (CPREC-PAYLOAD@).

require lib/test.f
require lib/float.f
require maki/prec-attr.f
require maki/cad.f

package MAKI

\ ---- equation fixture (own extents/tensors; unique names load cleanly beside every suite) --
4 EXTENT: #PGA  4 EXTENT: #PGB  3 EXTENT: #PGK
TENSOR: PG-Q  ( #PGA #PGK )    \ Q : L x d
TENSOR: PG-KT ( #PGB #PGK )    \ K : L x d (the transposed operand, shares the trailing d index)
TENSOR: PG-S  ( #PGA #PGB )    \ S : L x L (attention scores)
SPEC: PG-QK  PG-S[pga pgb] = Σpgk PG-Q[pga pgk] · PG-KT[pgb pgk] ;   \ S = Q.Kᵀ

\ the registered slot of PG-QK (image-order dependent, so read it live, not hardcoded)
: PG-EQ-SLOT ( -- n )
   s" PG-QK" EQ-FIND MATCH option  none OF -1 ENDOF  some OF EQ-SLOT>N ENDOF ;MATCH ;

\ ---- reject fixtures: drive the token dispatcher directly (as maki/cad-test.f does), since
\ the MODEL: driver's own throws cross an `evaluate` boundary and are not catchable in-process.
: PG-TRY-NONGEMM ( -- )  CAP-BEGIN  s" RELU:FP16"   EMIT-OP-TOKEN ;   \ precision on a non-GEMM op
: PG-TRY-MOVE    ( -- )  CAP-BEGIN  s" RESHAPE:BF16" EMIT-OP-TOKEN ;   \ precision on a movement op
: PG-TRY-BADPREC ( -- )  CAP-BEGIN  s" MATMUL:FP8"   EMIT-OP-TOKEN ;   \ unknown precision suffix on a GEMM op

T-RESET

\ ---- tagged matmul + untagged sibling: the tag overrides the default for THAT node only -----
CPREC-DEFAULT-RESET
MODEL: PGM2 ( x:2x2 w:2x2 v:2x2 -- y ) MATMUL:FP16 MATMUL ;
MODEL-K 2 T=
0 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:MATMUL MAKI-OPKIND:EQ -1 T=
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-FP16 T=             \ tagged node: fp16
0 MIR-NODE-ID MIR-ATTR@ CPREC-PAYLOAD@ 0 T=              \ matmul payload still 0
1 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-TF32 T=             \ untagged sibling keeps the tf32 default

\ ---- two distinct tags in one body: each op carries its own precision (positional thread) ---
MODEL: PGMIX ( x:2x2 w:2x2 v:2x2 -- y ) MATMUL:FP16 MATMUL:BF16 ;
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-FP16 T=
1 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-BF16 T=

\ ---- LINEAR:BF16 tags the linear node ---------------------------------------------
MODEL: PGLIN ( x:2x3 w:3x2 b:1x2 -- y ) LINEAR:BF16 ;
0 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:LINEAR MAKI-OPKIND:EQ -1 T=
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-BF16 T=

\ ---- equation form <EQ>:<PREC>: the tag rides, the registry slot is intact in the low-32 ----
MODEL: PGEQ ( q:4x3 k:4x3 -- s ) PG-QK:FP16 ;
MODEL-K 1 T=
0 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:EQUATION MAKI-OPKIND:EQ -1 T=
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-FP16 T=
0 MIR-NODE-ID MIR-ATTR@ CPREC-PAYLOAD@ PG-EQ-SLOT T=     \ the slot survives the fp16 tag

\ ---- explicit TF32 tag on a tf32-default workload: byte-identical attrs (CPREC-TF32 = 0) -----
CPREC-DEFAULT-RESET
MODEL: PGTF ( x:2x2 w:2x2 -- y ) MATMUL:TF32 ;
0 MIR-NODE-ID MIR-ATTR@ 0 T=                             \ tf32 tag + tf32 default = 0 (unchanged)
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-TF32 T=

\ ---- fp16-default workload + explicit TF32 tag: the override wins BOTH directions -----------
CPREC-FP16 CPREC-DEFAULT!
MODEL: PGOVR ( x:2x2 w:2x2 v:2x2 -- y ) MATMUL:TF32 MATMUL ;
CPREC-DEFAULT-RESET                                      \ restore immediately (persistent state)
0 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-TF32 T=            \ explicit tf32 overrides the fp16 default
0 MIR-NODE-ID MIR-ATTR@ 0 T=                             \ tf32 packing is byte-identical (0)
1 MIR-NODE-ID MIR-ATTR@ CPREC@ CPREC-FP16 T=            \ untagged sibling keeps the fp16 default

\ ---- fail closed: non-GEMM tag / movement tag / unknown precision suffix --------------------
' PG-TRY-NONGEMM E-CPREC-OP  TTHROWS                     \ precision on relu -> E-CPREC-OP (CPREC-GEMM?)
' PG-TRY-MOVE    E-CPREC-OP  TTHROWS                     \ precision on reshape -> E-CPREC-OP
' PG-TRY-BADPREC E-CPREC-TAG TTHROWS                     \ MATMUL:FP8 unknown suffix -> E-CPREC-TAG

CPREC-DEFAULT-RESET                                      \ leave the workload default at tf32

T-REPORT

;package
