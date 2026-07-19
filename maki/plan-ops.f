\ maki/plan-ops.f - the descriptor-mode planning vocabulary for the full op set.
\
\ docs/archive/cad-plan.md section 3: a model word is ordinary checked Forth executed against a
\ planning vocabulary - the ops append IR records over tensor DESCRIPTORS instead
\ of computing. maki/tensor-value.f already carries the plan store (PLAN-OP-BEGIN /
\ PLAN-IN+ / PLAN-OP+) and PLINEAR/PGELU; this file adds the rest of the op set as
\ PARAMETRIC appenders (op-kind is an argument), so one word covers every op of a
\ given arity/class and MODEL: capture (maki/cad.f) dispatches to them.
\
\ Output descriptor facts are inferred and recorded (docs/archive/cad-plan.md §4.1): elementwise
\ ops keep the data operand's shape/dtype/layout; matmul/linear take rows from the
\ data operand and cols from the weight and fail closed on an inner-dim mismatch
\ (E-TV-SHAPE). Each appender returns the output descriptor so the words compose.
\ maki -> habu only; reuses tensor-value's error range (no new codes).

require maki/tensor-value.f
require maki/op-kind.f
require maki/move-facts.f
require maki/spec.f                   \ the equation registry (EQ-*) the composer reads

package MAKI
public

\ ---- same-shape output descriptor (elementwise: like the data operand) ------
: PLAN-LIKE ( tensor -- tensor ) {: x:tensor :}
   x TENSOR:TV-ROWS@ x TENSOR:TV-COLS@ x TENSOR:TV-DTYPE@
   x TENSOR:TV-LAYOUT@ x TENSOR:TV-SPACE@ TENSOR:TV-DESC ;

\ ---- elementwise appenders (parametric on op-kind) --------------------------
\ op is the family arg on top; it cannot bind into a local, so PLAN-OP-BEGIN
\ consumes it first (elementwise output inference never throws), then the tensors bind.
: PLAN-UNARY ( tensor opkind -- tensor )
   TENSOR:PLAN-OP-BEGIN {: x:tensor :}
   x PLAN-LIKE {: y:tensor :}
   x TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

: PLAN-BIN-EW ( tensor tensor opkind -- tensor )
   TENSOR:PLAN-OP-BEGIN {: x:tensor p:tensor :}
   x PLAN-LIKE {: y:tensor :}
   x TENSOR:PLAN-IN+  p TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

: PLAN-TERN-EW ( tensor tensor tensor opkind -- tensor )
   TENSOR:PLAN-OP-BEGIN {: x:tensor a:tensor b:tensor :}
   x PLAN-LIKE {: y:tensor :}
   x TENSOR:PLAN-IN+  a TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

private

\ ---- contraction output descriptor (rows from data, cols from weight) -------
: PLAN-MM-DESC ( tensor tensor -- tensor ) {: x:tensor w:tensor :}
   x TENSOR:TV-COLS@ w TENSOR:TV-ROWS@ INNER-EQUAL? 0= if E-TV-SHAPE throw then
   x TENSOR:TV-ROWS@ w TENSOR:TV-COLS@ x TENSOR:TV-DTYPE@
   MAKI-LAYOUT:ROW x TENSOR:TV-SPACE@ TENSOR:TV-DESC ;

public

\ the contraction descriptor (PLAN-MM-DESC) can throw on an inner-dim mismatch, so
\ it runs BEFORE PLAN-OP-BEGIN opens a record; the op family rides the stack under
\ the operands (-rot / swap move it clear of the tensors that bind).
: PLAN-MATMUL ( tensor tensor opkind -- tensor )
   -rot {: x:tensor w:tensor :}                 \ ( op )
   x w PLAN-MM-DESC                              \ ( op y ) -- throws before any record opens
   swap TENSOR:PLAN-OP-BEGIN {: y:tensor :}
   x TENSOR:PLAN-IN+  w TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

: PLAN-LINEAR ( tensor tensor tensor opkind -- tensor )
   swap {: b:tensor :}                           \ ( x w op )
   -rot {: x:tensor w:tensor :}                  \ ( op )
   x w PLAN-MM-DESC                              \ ( op y )
   swap TENSOR:PLAN-OP-BEGIN {: y:tensor :}
   x TENSOR:PLAN-IN+  w TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+  y TENSOR:PLAN-OP+  y ;

\ ---- movement appenders (append IR facts + packed attrs; verdict per 6.3) ----
\ Output extents are inferred layout rewrites, not compute; each stages an attrs
\ cell (transform tag + verdict + params) so the bridge (maki/cad.f) carries it
\ into the model-IR node and drives its materialization flag.

\ reshape: same elements, target RxC (params); free on contiguous else materialize.
\ (the layout family cannot bind into a local, so each use refetches from x)
: PLAN-RESHAPE ( tensor CAD-KIND:rows CAD-KIND:cols -- tensor )
   {: x:tensor tr:CAD-KIND:rows tc:CAD-KIND:cols :}
   x TENSOR:TV-ELEMS tr tc SHAPE-ELEMS DIM-EQUAL? 0= if E-TV-SHAPE throw then
   tr tc x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ x TENSOR:TV-SPACE@ TENSOR:TV-DESC {: y:tensor :}
   MV-RESHAPE  x TENSOR:TV-LAYOUT@ MV-RESHAPE-VERDICT
   tr tc MV-PACK-SHAPE {: attr:n :}
   MAKI-OPKIND:RESHAPE TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ transpose: RxC -> CxR (no params); dissolves inside a staged region.
: PLAN-TRANSPOSE ( tensor -- tensor ) {: x:tensor :}
   x TENSOR:TV-ROWS@ x TENSOR:TV-COLS@ TRANSPOSE-SHAPE
   x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ x TENSOR:TV-SPACE@ TENSOR:TV-DESC {: y:tensor :}
   MV-TRANSPOSE  MV-TRANSPOSE-VERDICT  0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:TRANSPOSE TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ slice: rows [r0,r1) (params); free when the offset is lane-aligned else materialize.
: PLAN-SLICE ( tensor CAD-KIND:rows CAD-KIND:rows -- tensor )
   {: x:tensor r0:CAD-KIND:rows r1:CAD-KIND:rows :}
   r1 ROWS-RAW x TENSOR:TV-ROWS@ ROWS-RAW >
   r0 ROWS-RAW r1 ROWS-RAW > or if E-TV-SHAPE throw then
   x TENSOR:TV-COLS@ {: cols:CAD-KIND:cols :}
   r1 r0 ROWS- cols x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ x TENSOR:TV-SPACE@ TENSOR:TV-DESC {: y:tensor :}
   MV-SLICE  x TENSOR:TV-LAYOUT@ r0 cols MV-SLICE-VD
   r0 r1 MV-PACK-ROWS {: attr:n :}
   MAKI-OPKIND:SLICE TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ concat: row-append b to x (cols must agree); v1 always materializes.
: PLAN-CONCAT ( tensor tensor -- tensor ) {: x:tensor b:tensor :}
   x TENSOR:TV-COLS@ b TENSOR:TV-COLS@ COLS-EQUAL? 0= if E-TV-SHAPE throw then
   x TENSOR:TV-ROWS@ b TENSOR:TV-ROWS@ ROWS+  x TENSOR:TV-COLS@
   x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ x TENSOR:TV-SPACE@ TENSOR:TV-DESC {: y:tensor :}
   MV-CONCAT  MV-CONCAT-VERDICT  0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:CONCAT TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  b TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ gather: select idx rows of x (output rows = index element count); reported gathered.
: PLAN-GATHER ( tensor tensor -- tensor ) {: x:tensor idx:tensor :}
   idx TENSOR:TV-ELEMS DIM>ROWS  x TENSOR:TV-COLS@
   x TENSOR:TV-DTYPE@ x TENSOR:TV-LAYOUT@ x TENSOR:TV-SPACE@ TENSOR:TV-DESC {: y:tensor :}
   MV-GATHER  MV-GATHER-VERDICT  0 0 MV-PACK {: attr:n :}
   MAKI-OPKIND:GATHER TENSOR:PLAN-OP-BEGIN  x TENSOR:PLAN-IN+  idx TENSOR:PLAN-IN+  attr TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

\ ---- equation composer (docs/model-unified.md stage 1) ----------------------
\ Compose a SPEC:-declared einsum (the equation registry, maki/spec.f) as a node. The
\ composition-time extent check requires each operand's (rows,cols) to equal the
\ equation's declared per-factor (rows,cols) per the 2D BTC convention (docs/batch-
\ sequence-design.md section 4); a mismatch is E-TV-SHAPE, like the matmul inner-dim
\ check. Output extents are the equation's (rows,cols); the node carries the registry
\ slot in its attrs so the executor (maki/executor.f) dispatches to the generated RUN
\ word. Operand order is factor order. Stage 1 composes two-factor equations (Q Kᵀ);
\ a one- or three-factor equation underflows / overflows the fixed arity and is a
\ fail-closed checker reject of the composition.
: EQ-OPERAND-CK ( tensor eq-slot n -- ) {: x:tensor s:eq-slot k:n :}
   x TENSOR:TV-ROWS@ ROWS-RAW  s k EQ-FROW@ <> if E-TV-SHAPE throw then
   x TENSOR:TV-COLS@ COLS-RAW  s k EQ-FCOL@ <> if E-TV-SHAPE throw then ;
: PLAN-EQUATION ( tensor tensor eq-slot -- tensor ) {: q:tensor k:tensor s:eq-slot :}
   q s 0 EQ-OPERAND-CK   k s 1 EQ-OPERAND-CK
   s EQ-ROWS@ s EQ-COLS@ SHAPE  q TENSOR:TV-DTYPE@ MAKI-LAYOUT:ROW q TENSOR:TV-SPACE@ TENSOR:TV-DESC {: y:tensor :}
   MAKI-OPKIND:EQUATION TENSOR:PLAN-OP-BEGIN
   q TENSOR:PLAN-IN+  k TENSOR:PLAN-IN+  s EQ-SLOT>N TENSOR:PLAN-ATTR!  y TENSOR:PLAN-OP+  y ;

;package
