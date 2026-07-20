\ maki/plan-vocab.f - the NAMED, package-scoped model planning vocabulary (docs/archive/cad-plan.md section 3).
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
require maki/model-ir.f              \ MAKI:LN-FORM>ATTR + MAKI-LNFORM:AFFINE (explicit affine payload)
require maki/dropout.f               \ MAKI:DO-CFG>ATTR: the workload dropout mode/p resolved into the attr

package PLAN
public

\ ---- elementwise / row-reduce unary ops ( tensor -- tensor ) ----------------
\ Each fixes its op-kind and re-exports MAKI:PLAN-UNARY (same-shape output descriptor).
: RELU        ( tensor -- tensor )  MAKI-OPKIND:RELU        MAKI:PLAN-UNARY ;
: GELU        ( tensor -- tensor )  MAKI-OPKIND:GELU        MAKI:PLAN-UNARY ;
: SILU        ( tensor -- tensor )  MAKI-OPKIND:SILU        MAKI:PLAN-UNARY ;
: LAYERNORM   ( tensor -- tensor )  MAKI-OPKIND:LAYERNORM   MAKI:PLAN-UNARY ;
\ affine LayerNorm (GPT-2): y = gamma*xhat + beta. Shared OP-LAYERNORM opcode, arity 3
\ (x, gamma, beta), but the affine variant is stamped EXPLICITLY into the plan attr as
\ MAKI-LNFORM:AFFINE - not left to be rediscovered from the input count downstream
\ (dot habu-make-affine-layernorm). Mirrors PLAN-TERN-EW plus the form-attr stamp.
: LAYERNORM-AFFINE ( tensor tensor tensor -- tensor )
   MAKI-OPKIND:LAYERNORM TENSOR:PLAN-OP-BEGIN {: x:tensor g:tensor b:tensor :}
   x MAKI:PLAN-LIKE {: y:tensor :}
   x TENSOR:PLAN-IN+  g TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+
   MAKI-LNFORM:AFFINE MAKI:LN-FORM>ATTR TENSOR:PLAN-ATTR!
   y TENSOR:PLAN-OP+  y ;
: RMSNORM     ( tensor -- tensor )  MAKI-OPKIND:RMSNORM     MAKI:PLAN-UNARY ;
: SOFTMAX-ROW ( tensor -- tensor )  MAKI-OPKIND:SOFTMAX-ROW MAKI:PLAN-UNARY ;
: CAST        ( tensor -- tensor )  MAKI-OPKIND:CAST        MAKI:PLAN-UNARY ;
\ dropout (dot habu-dropout-op-train): arity 1 (x); mode (train/eval) and p are ATTRS, not
\ operands. The workload config (MAKI:DROPOUT-MODE!/DROPOUT-P!) resolves into each node's attr
\ at capture (MAKI:DO-CFG>ATTR, which guards p) - the CPREC-DEFAULT precedent, so a train/eval
\ switch is a re-capture, never an arity change. Mirrors PLAN-UNARY plus the config-attr stamp.
: DROPOUT ( tensor -- tensor )
   MAKI-OPKIND:DROPOUT TENSOR:PLAN-OP-BEGIN {: x:tensor :}
   x MAKI:PLAN-LIKE {: y:tensor :}
   x TENSOR:PLAN-IN+
   MAKI:DO-CFG>ATTR TENSOR:PLAN-ATTR!
   y TENSOR:PLAN-OP+  y ;

\ ---- binary elementwise ops ( tensor tensor -- tensor ) : data then param ----
\ Re-export MAKI:PLAN-BIN-EW; broadcast legality of the param is cad.f's SHP-CHECK.
: ADD          ( tensor tensor -- tensor )  MAKI-OPKIND:ADD          MAKI:PLAN-BIN-EW ;
: MUL          ( tensor tensor -- tensor )  MAKI-OPKIND:MUL          MAKI:PLAN-BIN-EW ;
: SCALE        ( tensor tensor -- tensor )  MAKI-OPKIND:SCALE        MAKI:PLAN-BIN-EW ;
: BIAS         ( tensor tensor -- tensor )  MAKI-OPKIND:BIAS         MAKI:PLAN-BIN-EW ;
: BCAST-MUL    ( tensor tensor -- tensor )  MAKI-OPKIND:BCAST-MUL    MAKI:PLAN-BIN-EW ;
: RESIDUAL-ADD ( tensor tensor -- tensor )  MAKI-OPKIND:RESIDUAL-ADD MAKI:PLAN-BIN-EW ;

\ ---- ternary elementwise ( tensor tensor tensor -- tensor ) : data then two params --
: ROPE ( tensor tensor tensor -- tensor )  MAKI-OPKIND:ROPE MAKI:PLAN-TERN-EW ;

\ ---- contraction ops (rows from data, cols from weight; inner dim fails closed) ----
: MATMUL ( tensor tensor -- tensor )         MAKI-OPKIND:MATMUL MAKI:PLAN-MATMUL ;
: LINEAR ( tensor tensor tensor -- tensor )  MAKI-OPKIND:LINEAR MAKI:PLAN-LINEAR ;

\ ---- movement ops (layout rewrites; scalar params travel on the stack) -------
\ Pure re-exports: the appenders already carry the op-kind, verdict, and packed attrs.
: RESHAPE   ( tensor CAD-KIND:rows CAD-KIND:cols -- tensor ) MAKI:PLAN-RESHAPE ;
: TRANSPOSE ( tensor -- tensor )          MAKI:PLAN-TRANSPOSE ;
: SLICE     ( tensor CAD-KIND:rows CAD-KIND:rows -- tensor ) MAKI:PLAN-SLICE ;
: CONCAT    ( tensor tensor -- tensor )   MAKI:PLAN-CONCAT ;
: GATHER    ( tensor tensor -- tensor )   MAKI:PLAN-GATHER ;

\ ---- equation op (docs/model-unified.md stage 1) ----------------------------
\ A SPEC:-declared einsum referenced in a composition. MODEL: emits the registry slot
\ as a literal (maki/cad.f), so this crosses the raw slot to its EQ-SLOT type and
\ re-exports the composer (extent check + node append) in maki/plan-ops.f.
: EQUATION ( tensor tensor n -- tensor )  MAKI:>EQ-SLOT MAKI:PLAN-EQUATION ;
\ three-factor equation (a folded projection contraction, dot habu-differentiable-attention).
: EQUATION3 ( tensor tensor tensor n -- tensor )  MAKI:>EQ-SLOT MAKI:PLAN-EQUATION3 ;

;package
