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

\ ---- movement appenders (append IR facts + packed attrs; verdict per 6.3) ----
\ Output extents are inferred layout rewrites, not compute; each stages an attrs
\ cell (transform tag + verdict + params) so the bridge (maki/cad.f) carries it
\ into the model-IR node and drives its materialization flag.

\ reshape: same elements, target RxC (params); free on contiguous else materialize.
: PLAN-RESHAPE ( tensor n n -- tensor ) {: x:tensor tr:n tc:n :}
   x TV-ELEMS  tr tc *  <> if E-TV-SHAPE throw then
   x TV-LAYOUT@ {: lay:n :}
   tr tc x TV-DTYPE@ lay TV-DESC {: y:tensor :}
   MV-RESHAPE  lay MV-RESHAPE-VERDICT  tr tc MV-PACK {: attr:n :}
   OP-RESHAPE PLAN-OP-BEGIN  x PLAN-IN+  attr PLAN-ATTR!  y PLAN-OP+  y ;

\ transpose: RxC -> CxR (no params); dissolves inside a staged region.
: PLAN-TRANSPOSE ( tensor -- tensor ) {: x:tensor :}
   x TV-COLS@ x TV-ROWS@ x TV-DTYPE@ x TV-LAYOUT@ TV-DESC {: y:tensor :}
   MV-TRANSPOSE  MV-TRANSPOSE-VERDICT  0 0 MV-PACK {: attr:n :}
   OP-TRANSPOSE PLAN-OP-BEGIN  x PLAN-IN+  attr PLAN-ATTR!  y PLAN-OP+  y ;

\ slice: rows [r0,r1) (params); free when the offset is lane-aligned else materialize.
: PLAN-SLICE ( tensor n n -- tensor ) {: x:tensor r0:n r1:n :}
   r0 0 < r1 x TV-ROWS@ > or  r0 r1 > or if E-TV-SHAPE throw then
   x TV-COLS@ {: cols:n :}
   r1 r0 -  cols  x TV-DTYPE@ x TV-LAYOUT@ TV-DESC {: y:tensor :}
   MV-SLICE  x TV-LAYOUT@ r0 cols MV-SLICE-VERDICT  r0 r1 MV-PACK {: attr:n :}
   OP-SLICE PLAN-OP-BEGIN  x PLAN-IN+  attr PLAN-ATTR!  y PLAN-OP+  y ;

\ concat: row-append b to x (cols must agree); v1 always materializes.
: PLAN-CONCAT ( tensor tensor -- tensor ) {: x:tensor b:tensor :}
   x TV-COLS@ b TV-COLS@ <> if E-TV-SHAPE throw then
   x TV-ROWS@ b TV-ROWS@ +  x TV-COLS@  x TV-DTYPE@ x TV-LAYOUT@ TV-DESC {: y:tensor :}
   MV-CONCAT  MV-CONCAT-VERDICT  0 0 MV-PACK {: attr:n :}
   OP-CONCAT PLAN-OP-BEGIN  x PLAN-IN+  b PLAN-IN+  attr PLAN-ATTR!  y PLAN-OP+  y ;

\ gather: select idx rows of x (output rows = index element count); reported gathered.
: PLAN-GATHER ( tensor tensor -- tensor ) {: x:tensor idx:tensor :}
   idx TV-ELEMS  x TV-COLS@  x TV-DTYPE@ x TV-LAYOUT@ TV-DESC {: y:tensor :}
   MV-GATHER  MV-GATHER-VERDICT  0 0 MV-PACK {: attr:n :}
   OP-GATHER PLAN-OP-BEGIN  x PLAN-IN+  idx PLAN-IN+  attr PLAN-ATTR!  y PLAN-OP+  y ;

end-package
