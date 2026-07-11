\ maki/op-registry-test.f - checked tests for the model op registry.
\ Row facts, membership gating (complete vs incomplete), and fail-closed lookups.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/op-registry.f

package MAKI

\ ---- fail-closed probes (top level cannot push quotations) ------------------
\ the op-kind out-of-range throws (E-OPR-KIND, incl. the OPR-NAME render default)
\ are unrepresentable: the accessors take an `opkind`, so a raw code is a CHECKER
\ reject (pinned below), never a runtime throw.
: TRY-OPR-INCOMPLETE ( -- )  MAKI-OPKIND:CAST OPR-REF drop ;   \ cast is the only incomplete op

T-RESET

\ ---- class / cost / numeric facts ------------------------------------------
MAKI-OPKIND:GELU        OPR-CLASS   CLASS-EW         T=
MAKI-OPKIND:GELU        OPR-FLOPS   8                T=
MAKI-OPKIND:GELU        OPR-NUMERIC NUM-RELTOL       T=
MAKI-OPKIND:GELU        OPR-ACCUM   ACC-SAME         T=
MAKI-OPKIND:GELU        OPR-ARITY   1                T=
MAKI-OPKIND:RELU        OPR-NUMERIC NUM-EXACT        T=
MAKI-OPKIND:SOFTMAX-ROW OPR-CLASS   CLASS-ROW-REDUCE T=
MAKI-OPKIND:SOFTMAX-ROW OPR-ACCUM   ACC-F32          T=
MAKI-OPKIND:MATMUL      OPR-CLASS   CLASS-MATMUL     T=
MAKI-OPKIND:LINEAR      OPR-ARITY   3                T=
MAKI-OPKIND:ROPE        OPR-ARITY   3                T=
MAKI-OPKIND:ROPE        OPR-CLASS   CLASS-EW         T=

\ ---- movement ops: no compute, exact rewrites, class MOVEMENT --------------
MAKI-OPKIND:RESHAPE     OPR-CLASS   CLASS-MOVEMENT   T=
MAKI-OPKIND:RESHAPE     OPR-FLOPS   0                T=
MAKI-OPKIND:RESHAPE     OPR-NUMERIC NUM-EXACT        T=
MAKI-OPKIND:RESHAPE     OPR-ACCUM   ACC-SAME         T=
MAKI-OPKIND:RESHAPE     OPR-ARITY   1                T=
MAKI-OPKIND:TRANSPOSE   OPR-ARITY   1                T=
MAKI-OPKIND:SLICE       OPR-ARITY   1                T=
MAKI-OPKIND:CONCAT      OPR-ARITY   2                T=
MAKI-OPKIND:GATHER      OPR-ARITY   2                T=
MAKI-OPKIND:CONCAT      OPR-CLASS   CLASS-MOVEMENT   T=

\ ---- bytes model derived from class ----------------------------------------
MAKI-OPKIND:GELU        OPR-BYTES-MODEL BYM-INOUT    T=
MAKI-OPKIND:SOFTMAX-ROW OPR-BYTES-MODEL BYM-ROW      T=
MAKI-OPKIND:MATMUL      OPR-BYTES-MODEL BYM-TILES    T=
MAKI-OPKIND:RESHAPE     OPR-BYTES-MODEL BYM-MOVE     T=
MAKI-OPKIND:GATHER      OPR-BYTES-MODEL BYM-MOVE     T=

\ ---- membership gating: silu/rmsnorm/rope/matmul/linear complete; only cast not --
MAKI-OPKIND:SILU        OPR-COMPLETE?  TTRUE
MAKI-OPKIND:RMSNORM     OPR-COMPLETE?  TTRUE
MAKI-OPKIND:ROPE        OPR-COMPLETE?  TTRUE
MAKI-OPKIND:GELU        OPR-COMPLETE?  TTRUE
\ cad-7a: matmul/linear now complete (buffer references bound: MATMUL, LINEAR)
MAKI-OPKIND:MATMUL      OPR-COMPLETE?  TTRUE
MAKI-OPKIND:LINEAR      OPR-COMPLETE?  TTRUE
MAKI-OPKIND:CAST        OPR-COMPLETE?  TFALSE
\ movement ops are complete: their buffer references (maki/move.f) are bound
MAKI-OPKIND:RESHAPE     OPR-COMPLETE?  TTRUE
MAKI-OPKIND:CONCAT      OPR-COMPLETE?  TTRUE
MAKI-OPKIND:GATHER      OPR-COMPLETE?  TTRUE

\ a complete op yields a non-zero reference xt
MAKI-OPKIND:GELU        OPR-REF 0 T<>
MAKI-OPKIND:SILU        OPR-REF 0 T<>
MAKI-OPKIND:MATMUL      OPR-REF 0 T<>
MAKI-OPKIND:LINEAR      OPR-REF 0 T<>

\ ---- elementwise predicate (used by region extraction) ---------------------
MAKI-OPKIND:GELU        OPR-ELEMENTWISE? TTRUE
MAKI-OPKIND:CAST        OPR-ELEMENTWISE? TTRUE
MAKI-OPKIND:SOFTMAX-ROW OPR-ELEMENTWISE? TFALSE
MAKI-OPKIND:MATMUL      OPR-ELEMENTWISE? TFALSE
MAKI-OPKIND:RESHAPE     OPR-ELEMENTWISE? TFALSE       \ movement breaks elementwise chains
MAKI-OPKIND:GATHER      OPR-ELEMENTWISE? TFALSE

\ ---- names ------------------------------------------------------------------
MAKI-OPKIND:SILU  OPR-NAME s" silu" T$=
MAKI-OPKIND:ROPE  OPR-NAME s" rope" T$=
MAKI-OPKIND:RESHAPE   OPR-NAME s" reshape"   T$=
MAKI-OPKIND:TRANSPOSE OPR-NAME s" transpose" T$=
MAKI-OPKIND:GATHER    OPR-NAME s" gather"    T$=
CLASS-ROW-REDUCE OPR-CLASS-NAME s" row-reduce" T$=
NUM-RELTOL       OPR-NUMERIC-NAME s" rel-tol"   T$=

\ ---- fail closed ------------------------------------------------------------
' TRY-OPR-INCOMPLETE E-OPR-INCOMPLETE TTHROWS

\ op-kind out of range is a CHECKER reject now (a raw code cannot index the
\ registry, an opkind cannot leak as n, and a dtype cannot cross into an op
\ accessor); positive control pins the well-typed accessor.
s" OPX-OK    ( opkind -- n ) OPR-CLASS"      CHECK-QUIET-CANDIDATE! -1 T=
s" OPX-N     ( n -- n ) OPR-CLASS"           CHECK-QUIET-CANDIDATE!  0 T=
s" OPX-NAME-N ( n -- ptr u8 n ) OPR-NAME"    CHECK-QUIET-CANDIDATE!  0 T=
s" OPX-DT    ( dtype -- n ) OPR-CLASS"       CHECK-QUIET-CANDIDATE!  0 T=

T-REPORT

end-package
