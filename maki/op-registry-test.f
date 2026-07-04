\ maki/op-registry-test.f - checked tests for the model op registry.
\ Row facts, membership gating (complete vs incomplete), and fail-closed lookups.

require lib/test.f
require lib/string.f
require maki/op-registry.f

package MAKI

\ ---- fail-closed probes (top level cannot push quotations) ------------------
: TRY-OPR-KIND       ( -- )  OP-N OPR-CLASS drop ;
: TRY-OPR-INCOMPLETE ( -- )  OP-MATMUL OPR-REF drop ;
: TRY-OPR-NAME-BAD   ( -- )  -1 OPR-NAME 2drop ;

T-RESET

\ ---- class / cost / numeric facts ------------------------------------------
OP-GELU        OPR-CLASS   CLASS-EW         T=
OP-GELU        OPR-FLOPS   8                T=
OP-GELU        OPR-NUMERIC NUM-RELTOL       T=
OP-GELU        OPR-ACCUM   ACC-SAME         T=
OP-GELU        OPR-ARITY   1                T=
OP-RELU        OPR-NUMERIC NUM-EXACT        T=
OP-SOFTMAX-ROW OPR-CLASS   CLASS-ROW-REDUCE T=
OP-SOFTMAX-ROW OPR-ACCUM   ACC-F32          T=
OP-MATMUL      OPR-CLASS   CLASS-MATMUL     T=
OP-LINEAR      OPR-ARITY   3                T=
OP-ROPE        OPR-ARITY   3                T=
OP-ROPE        OPR-CLASS   CLASS-EW         T=

\ ---- movement ops: no compute, exact rewrites, class MOVEMENT --------------
OP-RESHAPE     OPR-CLASS   CLASS-MOVEMENT   T=
OP-RESHAPE     OPR-FLOPS   0                T=
OP-RESHAPE     OPR-NUMERIC NUM-EXACT        T=
OP-RESHAPE     OPR-ACCUM   ACC-SAME         T=
OP-RESHAPE     OPR-ARITY   1                T=
OP-TRANSPOSE   OPR-ARITY   1                T=
OP-SLICE       OPR-ARITY   1                T=
OP-CONCAT      OPR-ARITY   2                T=
OP-GATHER      OPR-ARITY   2                T=
OP-CONCAT      OPR-CLASS   CLASS-MOVEMENT   T=

\ ---- bytes model derived from class ----------------------------------------
OP-GELU        OPR-BYTES-MODEL BYM-INOUT    T=
OP-SOFTMAX-ROW OPR-BYTES-MODEL BYM-ROW      T=
OP-MATMUL      OPR-BYTES-MODEL BYM-TILES    T=
OP-RESHAPE     OPR-BYTES-MODEL BYM-MOVE     T=
OP-GATHER      OPR-BYTES-MODEL BYM-MOVE     T=

\ ---- membership gating: silu/rmsnorm/rope complete; matmul/linear/cast not --
OP-SILU        OPR-COMPLETE?  TTRUE
OP-RMSNORM     OPR-COMPLETE?  TTRUE
OP-ROPE        OPR-COMPLETE?  TTRUE
OP-GELU        OPR-COMPLETE?  TTRUE
OP-MATMUL      OPR-COMPLETE?  TFALSE
OP-LINEAR      OPR-COMPLETE?  TFALSE
OP-CAST        OPR-COMPLETE?  TFALSE
\ movement ops are complete: their buffer references (maki/move.f) are bound
OP-RESHAPE     OPR-COMPLETE?  TTRUE
OP-CONCAT      OPR-COMPLETE?  TTRUE
OP-GATHER      OPR-COMPLETE?  TTRUE

\ a complete op yields a non-zero reference xt
OP-GELU        OPR-REF 0 T<>
OP-SILU        OPR-REF 0 T<>

\ ---- elementwise predicate (used by region extraction) ---------------------
OP-GELU        OPR-ELEMENTWISE? TTRUE
OP-CAST        OPR-ELEMENTWISE? TTRUE
OP-SOFTMAX-ROW OPR-ELEMENTWISE? TFALSE
OP-MATMUL      OPR-ELEMENTWISE? TFALSE
OP-RESHAPE     OPR-ELEMENTWISE? TFALSE       \ movement breaks elementwise chains
OP-GATHER      OPR-ELEMENTWISE? TFALSE

\ ---- names ------------------------------------------------------------------
OP-SILU  OPR-NAME s" silu" T$=
OP-ROPE  OPR-NAME s" rope" T$=
OP-RESHAPE   OPR-NAME s" reshape"   T$=
OP-TRANSPOSE OPR-NAME s" transpose" T$=
OP-GATHER    OPR-NAME s" gather"    T$=
CLASS-ROW-REDUCE OPR-CLASS-NAME s" row-reduce" T$=
NUM-RELTOL       OPR-NUMERIC-NAME s" rel-tol"   T$=

\ ---- fail closed ------------------------------------------------------------
' TRY-OPR-KIND       E-OPR-KIND       TTHROWS
' TRY-OPR-INCOMPLETE E-OPR-INCOMPLETE TTHROWS
' TRY-OPR-NAME-BAD   E-OPR-KIND       TTHROWS

T-REPORT

end-package
