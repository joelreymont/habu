\ maki/spec-attention-test.f - single-head attention forward authored as SPEC: lines,
\ proven numerically equal to the maki/attention.f golden (ATTN-FWD).
\
\ This is the acceptance vehicle for "multi-operand dataflow (Q@K^T) via SPEC:"
\ (dot habu-fix-model-dsl-d066701e). Attention forward is
\   S = Q.K^T / sqrt(d) ;  A = softmax_rows(S) ;  O = A.V
\ Its two contractions are BOTH expressible in the shipped SPEC: grammar today:
\
\   S[am an] = Q[am ak] K[an ak] * +SUM ak    \ transposed-operand GEMM: Q.K^T
\   O[am ac] = A[am an] V[an ac] * +SUM an    \ standard GEMM: A.V
\
\ The transposed operand is not a new form - it is which position the shared
\ contraction index takes in each factor (K's key index `an` is its ROW, the
\ contraction `ak` its COLUMN, exactly K^T). The row ops between the two
\ contractions - the 1/sqrt(d) scale and the row softmax - STAY plain named ops
\ (maki/attention.f ATTN-SCALE! / ATTN-SOFTMAX-ROWS), per the SPEC: escape-hatch
\ design (docs/golden-syntax.md:153, docs/nanogpt-inventory.md:115): SPEC: expresses
\ the contraction dataflow, not the softmax numerics. Composing the two SPEC: goldens
\ with those named ops reproduces ATTN-FWD to exact numeric equality.
\
\ Assertions on the parsed dataflow / shape record run at top level (interpret mode),
\ exactly as maki/spec-test.f does, so `STR=`/`0=` bools read directly against `-1`.
\ All globals carry a SPAT- prefix and own extents #AM/#AN/#AK/#AC, so this loads
\ cleanly into the single shared maki/test.f image alongside every other suite.

require lib/test.f
require test/checker-assert.f
require maki/spec.f
require maki/attention.f

T-RESET

package MAKI

\ L = 4 (query/key positions), d = 3 (head dim).
4 EXTENT: #AM   4 EXTENT: #AN   3 EXTENT: #AK   3 EXTENT: #AC
TENSOR: SPAT-Q ( #AM #AK )   \ Q : L x d
TENSOR: SPAT-K ( #AN #AK )   \ K : L x d  (indexed [key col]; the transposed operand)
TENSOR: SPAT-S ( #AM #AN )   \ S : L x L  (scores)
TENSOR: SPAT-A ( #AM #AN )   \ A : L x L  (attention weights)
TENSOR: SPAT-V ( #AN #AC )   \ V : L x d
TENSOR: SPAT-O ( #AM #AC )   \ O : L x d  (output)

SPEC: SPAT-QK  SPAT-S[am an] = Σak SPAT-Q[am ak] · SPAT-K[an ak] ;   \ S = Q.K^T

\ --- derivation (2): the dataflow record proves the transposed-operand parse -------
\ Q.K^T: two free output indices (am,an), one contraction (ak) shared by both factors
\ in the SAME (column) slot - that shared trailing index IS the transpose.
SPEC-NAME$ s" SPAT-QK" STR= -1 T=
SPEC-OUT$  s" SPAT-S" STR= -1 T=
SPEC-FREE-N 2 T=   0 SPEC-FREE@ s" am" STR= -1 T=   1 SPEC-FREE@ s" an" STR= -1 T=
SPEC-CT-N   1 T=   0 SPEC-CT@   s" ak" STR= -1 T=
SPEC-FAC-N  2 T=
0 SPEC-FAC-NAME@ s" SPAT-Q" STR= -1 T=   1 SPEC-FAC-NAME@ s" SPAT-K" STR= -1 T=
0 0 SPEC-FAC-IDX@ s" am" STR= -1 T=   1 0 SPEC-FAC-IDX@ s" an" STR= -1 T=
0 1 SPEC-FAC-IDX@ s" ak" STR= -1 T=   1 1 SPEC-FAC-IDX@ s" ak" STR= -1 T=   \ shared trailing index
0 0 SPEC-FAC-GATHER@ nip 0= -1 T=                                          \ no gather in plain Q.K^T

\ --- derivation (3): the PROMOTE shape obligations (extent magnitudes) -------------
0 SPEC-FREE-EXTENT@ 4 T=   1 SPEC-FREE-EXTENT@ 4 T=   \ score shape 4 x 4
0 SPEC-CT-EXTENT@ 3 T=                                \ contraction span d = 3

SPEC: SPAT-AV  SPAT-O[am ac] = Σan SPAT-A[am an] · SPAT-V[an ac] ;   \ O = A.V
SPEC-OUT$  s" SPAT-O" STR= -1 T=
SPEC-CT-N  1 T=   0 SPEC-CT@ s" an" STR= -1 T=        \ A.V contracts the key index an

\ --- derivation (1): attention forward via SPEC: matches the golden exactly --------
create SPAT-BQ #AM #AK * cells allot
create SPAT-BK #AN #AK * cells allot
create SPAT-BV #AN #AC * cells allot
create SPAT-BS #AM #AN * cells allot
create SPAT-BA #AM #AN * cells allot
create SPAT-BO #AM #AC * cells allot
create SPAT-GS #AM #AN * cells allot        \ golden score scratch
create SPAT-GA #AM #AN * cells allot        \ golden attention scratch
create SPAT-GO #AM #AC * cells allot        \ golden output

: SPAT-FILL ( -- )
   #AM 0 ?do #AK 0 ?do  i j 3 * + 1 + s>f 7.0 f/  SPAT-BQ j #AK * i + T-SET  loop loop
   #AN 0 ?do #AK 0 ?do  i 2 * j + 1 + s>f 11.0 f/  SPAT-BK j #AK * i + T-SET  loop loop
   #AN 0 ?do #AC 0 ?do  i j + 1 + s>f 5.0 f/  SPAT-BV j #AC * i + T-SET  loop loop ;

\ the SPEC:-authored forward: two generated contraction goldens bridged by the two
\ named row ops (scale by 1/sqrt(d), then row softmax) - exactly ATTN-FWD's dataflow.
: SPAT-FWD ( -- )
   SPAT-QK                                                     \ S = Q.K^T
   SPAT-S-BASE @  #AM #AN *  1.0 #AK s>f fsqrt f/  ATTN-SCALE!  \ S *= 1/sqrt(d)
   SPAT-S-BASE @ SPAT-A-BASE @ #AM  ATTN-SOFTMAX-ROWS           \ A = softmax_rows(S)
   SPAT-AV ;                                                    \ O = A.V

: SPAT-PARITY ( -- )
   SPAT-FILL
   SPAT-BQ SPAT-Q-BIND  SPAT-BK SPAT-K-BIND  SPAT-BV SPAT-V-BIND
   SPAT-BS SPAT-S-BIND  SPAT-BA SPAT-A-BIND  SPAT-BO SPAT-O-BIND
   SPAT-FWD
   SPAT-BQ SPAT-BK SPAT-BV SPAT-GS SPAT-GA SPAT-GO #AM #AK ATTN-FWD   \ maki/attention.f golden
   SPAT-BO SPAT-GO #AM #AC * T-DIST2  1000000.0 f* 0.5 f+ f>s  0 T= ; \ exact numeric equality
SPAT-PARITY

\ --- red-first negatives: every malformed attention spec is a NAMED throw ----------
\ (no code generated) - malformed contraction, rank mismatch, unknown extent/tensor.
: SPAT-NOSTAR   ( -- ) s" SPAT-S[am an] = SPAT-Q[am ak] SPAT-K[an ak] +SUM ak" SPEC-CHECK$ ;  \ two factors, no *
: SPAT-RANK     ( -- ) s" SPAT-S[am an] = SPAT-Q[am] SPAT-K[an ak] * +SUM ak" SPEC-CHECK$ ;    \ SPAT-Q rank 2, one index
: SPAT-UNKEXT   ( -- ) s" SPAT-S[am zz] = SPAT-Q[am ak] SPAT-K[zz ak] * +SUM ak" SPEC-CHECK$ ; \ zz -> #ZZ undeclared
: SPAT-UNKTENS  ( -- ) s" SPAT-S[am an] = SPAT-Q[am ak] ZZ[an ak] * +SUM ak" SPEC-CHECK$ ;     \ tensor ZZ undeclared
: SPAT-OKSPEC   ( -- ) s" SPAT-S[am an] = SPAT-Q[am ak] SPAT-K[an ak] * +SUM ak" SPEC-CHECK$ ;  \ valid: no throw
' SPAT-NOSTAR   E-SPEC-SYNTAX  TTHROWS
' SPAT-RANK     E-SPEC-ARITY   TTHROWS
' SPAT-UNKEXT   E-SPEC-EXTENT  TTHROWS
' SPAT-UNKTENS  E-SPEC-TENSOR  TTHROWS
' SPAT-OKSPEC   0              TTHROWS

\ --- a valid-but-transposed spec is a CHECKER reject of the generated accessor loop:
\ writing K's key index into its d-column slot flips ix<extan> where ix<extak> is
\ demanded, so the candidate scores 0 (reject); the correct order scores -1 (accept).
SPEC-CAND: SPAT-COK   SPAT-S[am an] = SPAT-Q[am ak] SPAT-K[an ak] * +SUM ak ;
SPEC-CAND$ CHECK-QUIET-CANDIDATE! -1 T=
SPEC-CAND: SPAT-CFLIP SPAT-S[am an] = SPAT-Q[am ak] SPAT-K[ak an] * +SUM ak ;   \ SPAT-K[ak an]: extents swapped
SPEC-CAND$ CHECK-QUIET-CANDIDATE!  0 T=

;package

T-REPORT
