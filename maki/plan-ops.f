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
require maki/move-facts.f

package MAKI
public

\ ---- same-shape output descriptor (elementwise: like the data operand) ------
: PLAN-LIKE ( tensor -- tensor ) {: x:tensor :}
   x TENSOR:TV-ROWS@  x TENSOR:TV-COLS@  x TENSOR:TV-DTYPE@  x TENSOR:TV-LAYOUT@  TENSOR:TV-DESC ;

\ ---- elementwise appenders (parametric on op-kind) --------------------------
: PLAN-UNARY ( tensor n -- tensor ) {: x:tensor op:n :}
   x PLAN-LIKE {: y:tensor :}
   op TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

: PLAN-BIN-EW ( tensor tensor n -- tensor ) {: x:tensor p:tensor op:n :}
   x PLAN-LIKE {: y:tensor :}
   op TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  p TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

: PLAN-TERN-EW ( tensor tensor tensor n -- tensor ) {: x:tensor a:tensor b:tensor op:n :}
   x PLAN-LIKE {: y:tensor :}
   op TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  a TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

private

\ ---- contraction output descriptor (rows from data, cols from weight) -------
: PLAN-MM-DESC ( tensor tensor -- tensor ) {: x:tensor w:tensor :}
   x TENSOR:TV-COLS@ w TENSOR:TV-ROWS@ <> if E-TV-SHAPE throw then
   x TENSOR:TV-ROWS@  w TENSOR:TV-COLS@  x TENSOR:TV-DTYPE@  MAKI-LAYOUT:ROW  TENSOR:TV-DESC ;

public

: PLAN-MATMUL ( tensor tensor n -- tensor ) {: x:tensor w:tensor op:n :}
   x w PLAN-MM-DESC {: y:tensor :}
   op TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  w TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

: PLAN-LINEAR ( tensor tensor tensor n -- tensor ) {: x:tensor w:tensor b:tensor op:n :}
   x w PLAN-MM-DESC {: y:tensor :}
   op TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  w TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

\ ---- movement appenders (append IR facts + packed attrs; verdict per 6.3) ----
\ Output extents are inferred layout rewrites, not compute; each stages an attrs
\ cell (transform tag + verdict + params) so the bridge (maki/cad.f) carries it
\ into the model-IR node and drives its materialization flag.

\ reshape: same elements, target RxC (params); free on contiguous else materialize.
\ (the layout family cannot bind into a local, so each use refetches from x)
: PLAN-RESHAPE ( tensor n n -- tensor ) {: x:tensor tr:n tc:n :}
   x TENSOR:TV-ELEMS  tr tc *  <> if E-TV-SHAPE throw then
   tr tc x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ TENSOR:TV-DESC {: y:tensor :}
   MV-RESHAPE  x TENSOR:TV-LAYOUT@ MV-RESHAPE-VERDICT  tr tc MV-PACK {: attr:n :}
   OP-RESHAPE TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ transpose: RxC -> CxR (no params); dissolves inside a staged region.
: PLAN-TRANSPOSE ( tensor -- tensor ) {: x:tensor :}
   x TENSOR:TV-COLS@ x TENSOR:TV-ROWS@ x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ TENSOR:TV-DESC {: y:tensor :}
   MV-TRANSPOSE  MV-TRANSPOSE-VERDICT  0 0 MV-PACK {: attr:n :}
   OP-TRANSPOSE TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ slice: rows [r0,r1) (params); free when the offset is lane-aligned else materialize.
: PLAN-SLICE ( tensor n n -- tensor ) {: x:tensor r0:n r1:n :}
   r0 0 < r1 x TENSOR:TV-ROWS@ > or  r0 r1 > or if E-TV-SHAPE throw then
   x TENSOR:TV-COLS@ {: cols:n :}
   r1 r0 -  cols  x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ TENSOR:TV-DESC {: y:tensor :}
   MV-SLICE  x TENSOR:TV-LAYOUT@ r0 cols MV-SLICE-VERDICT  r0 r1 MV-PACK {: attr:n :}
   OP-SLICE TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ concat: row-append b to x (cols must agree); v1 always materializes.
: PLAN-CONCAT ( tensor tensor -- tensor ) {: x:tensor b:tensor :}
   x TENSOR:TV-COLS@ b TENSOR:TV-COLS@ <> if E-TV-SHAPE throw then
   x TENSOR:TV-ROWS@ b TENSOR:TV-ROWS@ +  x TENSOR:TV-COLS@  x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ TENSOR:TV-DESC {: y:tensor :}
   MV-CONCAT  MV-CONCAT-VERDICT  0 0 MV-PACK {: attr:n :}
   OP-CONCAT TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ gather: select idx rows of x (output rows = index element count); reported gathered.
: PLAN-GATHER ( tensor tensor -- tensor ) {: x:tensor idx:tensor :}
   idx TENSOR:TV-ELEMS  x TENSOR:TV-COLS@  x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ TENSOR:TV-DESC {: y:tensor :}
   MV-GATHER  MV-GATHER-VERDICT  0 0 MV-PACK {: attr:n :}
   OP-GATHER TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  idx TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

end-package
