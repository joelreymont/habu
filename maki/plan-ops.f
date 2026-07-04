\ maki/plan-ops.f - the descriptor-mode planning vocabulary for the full op set.
\
\ CAD-PLAN section 3: a model word is ordinary checked Forth executed against a
\ planning vocabulary - the ops append IR records over tensor DESCRIPTORS instead
\ of computing. maki/tensor-value.f already carries the plan store (PLAN-OP-BEGIN /
\ PLAN-IN+ / PLAN-OP+) and PLINEAR/PGELU; this file adds the rest of the op set as
\ PARAMETRIC appenders (op-kind is an argument), so one word covers every op of a
\ given arity/class and MODEL: capture (maki/cad.f) dispatches to them.
\
\ Output descriptor facts are inferred and recorded (CAD-PLAN 4.1): elementwise
\ ops keep the data operand's shape/dtype/layout; matmul/linear take rows from the
\ data operand and cols from the weight and fail closed on an inner-dim mismatch
\ (E-TV-SHAPE). Each appender returns the output descriptor so the words compose.
\ maki -> habu only; reuses tensor-value's error range (no new codes).

require maki/tensor-value.f
require maki/op-kind.f

package MAKI
public

\ ---- same-shape output descriptor (elementwise: like the data operand) ------
: PLAN-LIKE ( tensor -- tensor ) {: x:tensor :}
   x TV-ROWS@  x TV-COLS@  x TV-DTYPE@  x TV-LAYOUT@  TV-DESC ;

\ ---- elementwise appenders (parametric on op-kind) --------------------------
: PLAN-UNARY ( tensor n -- tensor ) {: x:tensor op:n :}
   x PLAN-LIKE {: y:tensor :}
   op PLAN-OP-BEGIN  x PLAN-IN+  y PLAN-OP+  y ;

: PLAN-BIN-EW ( tensor tensor n -- tensor ) {: x:tensor p:tensor op:n :}
   x PLAN-LIKE {: y:tensor :}
   op PLAN-OP-BEGIN  x PLAN-IN+  p PLAN-IN+  y PLAN-OP+  y ;

: PLAN-TERN-EW ( tensor tensor tensor n -- tensor ) {: x:tensor a:tensor b:tensor op:n :}
   x PLAN-LIKE {: y:tensor :}
   op PLAN-OP-BEGIN  x PLAN-IN+  a PLAN-IN+  b PLAN-IN+  y PLAN-OP+  y ;

private

\ ---- contraction output descriptor (rows from data, cols from weight) -------
: PLAN-MM-DESC ( tensor tensor -- tensor ) {: x:tensor w:tensor :}
   x TV-COLS@ w TV-ROWS@ <> if E-TV-SHAPE throw then
   x TV-ROWS@  w TV-COLS@  x TV-DTYPE@  LAY-ROW  TV-DESC ;

public

: PLAN-MATMUL ( tensor tensor n -- tensor ) {: x:tensor w:tensor op:n :}
   x w PLAN-MM-DESC {: y:tensor :}
   op PLAN-OP-BEGIN  x PLAN-IN+  w PLAN-IN+  y PLAN-OP+  y ;

: PLAN-LINEAR ( tensor tensor tensor n -- tensor ) {: x:tensor w:tensor b:tensor op:n :}
   x w PLAN-MM-DESC {: y:tensor :}
   op PLAN-OP-BEGIN  x PLAN-IN+  w PLAN-IN+  b PLAN-IN+  y PLAN-OP+  y ;

end-package
