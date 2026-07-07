\ maki/plan-vocab.f - the NAMED, package-scoped model planning vocabulary (CAD-PLAN section 3).
\
\ Section 3 wants a model body written as the SAME source text (LINEAR GELU LINEAR)
\ that the checker verifies as an ordinary word over tensor DESCRIPTORS. maki/plan-ops.f
\ already supplies that behaviour, but only as PARAMETRIC appenders (PLAN-UNARY /
\ PLAN-BIN-EW / PLAN-TERN-EW / PLAN-MATMUL / PLAN-LINEAR + the movement appenders) that
\ take the op-kind as a stack argument - so a hand-authored block still has to thread
\ OP-* constants (see maki/plan-compose-test.f). This file names each model op: a one-line
\ checked word with a proper ( tensor ... -- tensor ) effect that fixes the op-kind and
\ RE-EXPORTS the existing appender. No op semantics are duplicated - shape/dtype
\ inference and the plan store stay in plan-ops.f / tensor-value.f; these words only bind
\ the name to the op-kind so a composition reads as section-3 source and is checker-verified.
\
\ Its OWN package (PLAN), because the eager LINEAR / MATMUL / ADD already occupy MAKI: a
\ distinct planning wordlist is exactly what section 3 means by "MODEL: capture opens the
\ planning package". A hand-authored block opens `package PLAN` and its bare LINEAR / GELU
\ resolve to these descriptor-typed words TODAY (proven in maki/plan-vocab-test.f). Wiring
\ this package as the lexical scope of a MODEL: body waits on the checker-reentrancy blocker
\ (dot habu-checker-reentrancy-certify-86771a6f); the vocabulary itself does not.
\
\ Broadcast-shape legality (cad.f SHP-CHECK, E-CAD-PARAM-SHAPE) layers ABOVE this vocabulary
\ during MODEL: capture exactly as it does over the raw appenders today; the named words are
\ the arity/kind surface, not the broadcast checker. maki -> habu only (no new error codes).

require maki/op-kind.f
require maki/plan-ops.f

package PLAN
public

\ ---- elementwise / row-reduce unary ops ( tensor -- tensor ) ----------------
\ Each fixes its op-kind and re-exports MAKI:PLAN-UNARY (same-shape output descriptor).
: RELU        ( tensor -- tensor )  MAKI:OP-RELU        MAKI:PLAN-UNARY ;
: GELU        ( tensor -- tensor )  MAKI:OP-GELU        MAKI:PLAN-UNARY ;
: SILU        ( tensor -- tensor )  MAKI:OP-SILU        MAKI:PLAN-UNARY ;
: LAYERNORM   ( tensor -- tensor )  MAKI:OP-LAYERNORM   MAKI:PLAN-UNARY ;
: RMSNORM     ( tensor -- tensor )  MAKI:OP-RMSNORM     MAKI:PLAN-UNARY ;
: SOFTMAX-ROW ( tensor -- tensor )  MAKI:OP-SOFTMAX-ROW MAKI:PLAN-UNARY ;
: CAST        ( tensor -- tensor )  MAKI:OP-CAST        MAKI:PLAN-UNARY ;

\ ---- binary elementwise ops ( tensor tensor -- tensor ) : data then param ----
\ Re-export MAKI:PLAN-BIN-EW; broadcast legality of the param is cad.f's SHP-CHECK.
: ADD          ( tensor tensor -- tensor )  MAKI:OP-ADD          MAKI:PLAN-BIN-EW ;
: MUL          ( tensor tensor -- tensor )  MAKI:OP-MUL          MAKI:PLAN-BIN-EW ;
: SCALE        ( tensor tensor -- tensor )  MAKI:OP-SCALE        MAKI:PLAN-BIN-EW ;
: BIAS         ( tensor tensor -- tensor )  MAKI:OP-BIAS         MAKI:PLAN-BIN-EW ;
: RESIDUAL-ADD ( tensor tensor -- tensor )  MAKI:OP-RESIDUAL-ADD MAKI:PLAN-BIN-EW ;

\ ---- ternary elementwise ( tensor tensor tensor -- tensor ) : data then two params --
: ROPE ( tensor tensor tensor -- tensor )  MAKI:OP-ROPE MAKI:PLAN-TERN-EW ;

\ ---- contraction ops (rows from data, cols from weight; inner dim fails closed) ----
: MATMUL ( tensor tensor -- tensor )         MAKI:OP-MATMUL MAKI:PLAN-MATMUL ;
: LINEAR ( tensor tensor tensor -- tensor )  MAKI:OP-LINEAR MAKI:PLAN-LINEAR ;

\ ---- movement ops (layout rewrites; scalar params travel on the stack) -------
\ Pure re-exports: the appenders already carry the op-kind, verdict, and packed attrs.
: RESHAPE   ( tensor n n -- tensor )      MAKI:PLAN-RESHAPE ;
: TRANSPOSE ( tensor -- tensor )          MAKI:PLAN-TRANSPOSE ;
: SLICE     ( tensor n n -- tensor )      MAKI:PLAN-SLICE ;
: CONCAT    ( tensor tensor -- tensor )   MAKI:PLAN-CONCAT ;
: GATHER    ( tensor tensor -- tensor )   MAKI:PLAN-GATHER ;

end-package
