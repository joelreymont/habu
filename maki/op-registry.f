\ maki/op-registry.f - the model op registry (CAD-PLAN section 4.2).
\
\ One row per op-kind (maki/op-kind.f), the single extension point planners read
\ instead of learning op names: class, flops-per-element, bytes model (derived
\ from class), accum-dtype rule, numeric class, input arity, VJP id, and the
\ scalar CPU reference execution token (the golden oracle at op granularity).
\
\ Membership is gated (CAD-PLAN 4.2): a row is COMPLETE only once its reference xt
\ is bound. OPR-REF fails closed (E-OPR-INCOMPLETE) on an op without one, so GOLDEN
\ downstream cannot silently skip. silu/rmsnorm/rope references (maki/silu.f,
\ rmsnorm.f, rope.f) are bound here so those ops are complete; matmul/linear are
\ bound to their buffer references (maki/matmul.f MATMUL, maki/linear.f LINEAR;
\ cad-7a host executor) so only cast now stays incomplete (non-executable decode).
\
\ Storing an xt in a cell and reading it back is the checked pattern used by
\ src/core/render.f (`' WORD ADDR !`); cad-1 only stores/queries it, execution is
\ the cad-7 golden executor. maki -> habu only; op-registry owns -5050..-5054.

require maki/op-kind.f
require maki/autograd.f
require maki/gelu.f
require maki/silu.f
require maki/layernorm.f
require maki/rmsnorm.f
require maki/softmax.f
require maki/rope.f
require maki/move.f
require maki/reduce-bwd.f
require maki/scatter.f
require maki/matmul.f
require maki/linear.f

-5050 constant E-OPR-KIND        \ op-kind out of range
-5051 constant E-OPR-INCOMPLETE  \ reference requested from an op with no scalar oracle
-5052 constant E-OPR-CLASS       \ op class tag out of range
-5053 constant E-OPR-ACCUM       \ accum-dtype rule tag out of range
-5054 constant E-OPR-NUMERIC     \ numeric class tag out of range

package MAKI
public

\ ---- op classes (the planner reasons over classes, never names) ----
0 constant CLASS-EW              \ elementwise
1 constant CLASS-ROW-REDUCE      \ per-row reduction
2 constant CLASS-MATMUL          \ tiled contraction
3 constant CLASS-MOVEMENT        \ layout rewrite, no compute
4 constant CLASS-DECODE          \ decode / dequant chain
5 constant CLASS-N               \ range bound

\ ---- accum-dtype rule ----
0 constant ACC-SAME              \ accumulate in the input dtype
1 constant ACC-F32               \ accumulate f32 regardless (reductions / matmul)
2 constant ACC-N

\ ---- numeric class (drives GOLDEN tolerance, CAD-PLAN 11) ----
0 constant NUM-EXACT             \ bit-exact reference
1 constant NUM-ULP               \ small ulp arithmetic
2 constant NUM-RELTOL            \ relative-tolerance (transcendental / accumulated)
3 constant NUM-N

\ ---- bytes model (derived from class, CAD-PLAN 4.2) ----
0 constant BYM-INOUT             \ elementwise: read inputs + write output
1 constant BYM-ROW               \ row-reduce: read row + write row(s)
2 constant BYM-TILES             \ matmul: tiles model
3 constant BYM-MOVE              \ movement: rewrite
4 constant BYM-CHAIN             \ decode chain

private

\ one create-array per field, indexed by op-kind 0..OP-N-1
create R-CLASS OP-N cells allot
create R-FLOPS OP-N cells allot
create R-ACCUM OP-N cells allot
create R-NUM   OP-N cells allot
create R-ARITY OP-N cells allot        \ total tensor-input count (data + params)
create R-VJP   OP-N cells allot         \ model-op adjoint id (0 = none)
create R-REF   OP-N cells allot         \ scalar reference xt (0 = incomplete)

: OPR-CK ( n -- n )                   \ validate op-kind, fail closed
   dup 0 < over OP-N >= or if E-OPR-KIND throw then ;

\ load-time row writer: class flops accum num arity op --
: OPR! ( n n n n n n -- ) {: cls:n fl:n ac:n nm:n ar:n op:n :}
   op OPR-CK drop
   cls 0 < cls CLASS-N >= or if E-OPR-CLASS throw then
   ac  0 < ac  ACC-N   >= or if E-OPR-ACCUM throw then
   nm  0 < nm  NUM-N   >= or if E-OPR-NUMERIC throw then
   cls op cells R-CLASS + !
   fl  op cells R-FLOPS + !
   ac  op cells R-ACCUM + !
   nm  op cells R-NUM   + !
   ar  op cells R-ARITY + !
   0   op cells R-VJP   + !
   0   op cells R-REF   + ! ;           \ incomplete until a reference is bound

public

\ ---- accessors (each validates the op-kind, fail closed) ----
: OPR-CLASS   ( n -- n )  OPR-CK cells R-CLASS + @ ;
: OPR-FLOPS   ( n -- n )  OPR-CK cells R-FLOPS + @ ;
: OPR-ACCUM   ( n -- n )  OPR-CK cells R-ACCUM + @ ;
: OPR-NUMERIC ( n -- n )  OPR-CK cells R-NUM   + @ ;
: OPR-ARITY   ( n -- n )  OPR-CK cells R-ARITY + @ ;
: OPR-VJP     ( n -- n )  OPR-CK cells R-VJP   + @ ;

\ load-time setter: the adjoint registry (maki/adjoint.f) writes each op's model-op
\ adjoint id here (the cad-9 wiring; 0 stays "no adjoint"). Kept out of OPR! so the
\ adjoint-id constants live in adjoint.f without a load-order cycle back into this row.
: OPR-VJP! ( n n -- ) {: op:n vjp:n :}  op OPR-CK drop  vjp op cells R-VJP + ! ;

: OPR-COMPLETE? ( n -- bool )  OPR-CK cells R-REF + @ 0= 0= ;

\ the scalar reference xt; fail closed when the row has no oracle yet
: OPR-REF ( n -- n ) {: op:n :}
   op OPR-COMPLETE? 0= if E-OPR-INCOMPLETE throw then
   op cells R-REF + @ ;

\ bytes model is a pure function of class (CAD-PLAN 4.2)
: OPR-BYTES-MODEL ( n -- n )
   OPR-CLASS case
      CLASS-EW         of BYM-INOUT endof
      CLASS-ROW-REDUCE of BYM-ROW   endof
      CLASS-MATMUL     of BYM-TILES endof
      CLASS-MOVEMENT   of BYM-MOVE  endof
      CLASS-DECODE     of BYM-CHAIN endof
      E-OPR-CLASS throw
   endcase ;

: OPR-ELEMENTWISE? ( n -- bool )  OPR-CLASS CLASS-EW = ;

\ ---- enum -> text (fail closed) ----
: OPR-NAME ( n -- ptr u8 n )
   OPR-CK case
      OP-ADD          of s" add"          endof
      OP-MUL          of s" mul"          endof
      OP-SCALE        of s" scale"        endof
      OP-BIAS         of s" bias"         endof
      OP-RELU         of s" relu"         endof
      OP-GELU         of s" gelu"         endof
      OP-LAYERNORM    of s" layernorm"    endof
      OP-RMSNORM      of s" rmsnorm"      endof
      OP-SOFTMAX-ROW  of s" softmax-row"  endof
      OP-MATMUL       of s" matmul"       endof
      OP-LINEAR       of s" linear"       endof
      OP-RESIDUAL-ADD of s" residual-add" endof
      OP-CAST         of s" cast"         endof
      OP-SILU         of s" silu"         endof
      OP-ROPE         of s" rope"         endof
      OP-RESHAPE      of s" reshape"      endof
      OP-TRANSPOSE    of s" transpose"    endof
      OP-SLICE        of s" slice"        endof
      OP-CONCAT       of s" concat"       endof
      OP-GATHER       of s" gather"       endof
      OP-RELU-BWD        of s" relu-bwd"        endof
      OP-GELU-BWD        of s" gelu-bwd"        endof
      OP-SILU-BWD        of s" silu-bwd"        endof
      OP-LAYERNORM-BWD   of s" layernorm-bwd"   endof
      OP-RMSNORM-BWD     of s" rmsnorm-bwd"     endof
      OP-SOFTMAX-ROW-BWD of s" softmax-row-bwd" endof
      OP-ROPE-BWD        of s" rope-bwd"        endof
      OP-ROWSUM-BWD      of s" rowsum-bwd"      endof
      OP-FULLSUM-DOT-BWD of s" fullsum-dot-bwd" endof
      OP-PAD-SCATTER     of s" pad-scatter"     endof
      OP-SCATTER-ADD     of s" scatter-add"     endof
      E-OPR-KIND throw
   endcase ;

: OPR-CLASS-NAME ( n -- ptr u8 n )
   case
      CLASS-EW         of s" elementwise" endof
      CLASS-ROW-REDUCE of s" row-reduce"  endof
      CLASS-MATMUL     of s" matmul"      endof
      CLASS-MOVEMENT   of s" movement"    endof
      CLASS-DECODE     of s" decode"      endof
      E-OPR-CLASS throw
   endcase ;

: OPR-NUMERIC-NAME ( n -- ptr u8 n )
   case
      NUM-EXACT  of s" exact"   endof
      NUM-ULP    of s" ulp"     endof
      NUM-RELTOL of s" rel-tol" endof
      E-OPR-NUMERIC throw
   endcase ;

: OPR-ACCUM-NAME ( n -- ptr u8 n )
   case
      ACC-SAME of s" same" endof
      ACC-F32  of s" f32"  endof
      E-OPR-ACCUM throw
   endcase ;

private

\ ---- the registry (one row per op; class flops accum num arity op OPR!) ----
: OPR-BUILD ( -- )
   CLASS-EW         1 ACC-SAME NUM-ULP    2 OP-ADD          OPR!
   CLASS-EW         1 ACC-SAME NUM-ULP    2 OP-MUL          OPR!
   CLASS-EW         1 ACC-SAME NUM-ULP    2 OP-SCALE        OPR!
   CLASS-EW         1 ACC-SAME NUM-ULP    2 OP-BIAS         OPR!
   CLASS-EW         1 ACC-SAME NUM-EXACT  1 OP-RELU         OPR!
   CLASS-EW         8 ACC-SAME NUM-RELTOL 1 OP-GELU         OPR!
   CLASS-ROW-REDUCE 5 ACC-F32  NUM-RELTOL 1 OP-LAYERNORM    OPR!
   CLASS-ROW-REDUCE 4 ACC-F32  NUM-RELTOL 1 OP-RMSNORM      OPR!
   CLASS-ROW-REDUCE 5 ACC-F32  NUM-RELTOL 1 OP-SOFTMAX-ROW  OPR!
   CLASS-MATMUL     2 ACC-F32  NUM-RELTOL 2 OP-MATMUL       OPR!
   CLASS-MATMUL     2 ACC-F32  NUM-RELTOL 3 OP-LINEAR       OPR!
   CLASS-EW         1 ACC-SAME NUM-ULP    2 OP-RESIDUAL-ADD OPR!
   CLASS-EW         0 ACC-SAME NUM-EXACT  1 OP-CAST         OPR!
   CLASS-EW         5 ACC-SAME NUM-RELTOL 1 OP-SILU         OPR!
   CLASS-EW         6 ACC-SAME NUM-RELTOL 3 OP-ROPE         OPR!
   \ movement ops: no compute (flops 0), exact rewrites (NUM-EXACT).
   CLASS-MOVEMENT   0 ACC-SAME NUM-EXACT  1 OP-RESHAPE      OPR!
   CLASS-MOVEMENT   0 ACC-SAME NUM-EXACT  1 OP-TRANSPOSE    OPR!
   CLASS-MOVEMENT   0 ACC-SAME NUM-EXACT  1 OP-SLICE        OPR!
   CLASS-MOVEMENT   0 ACC-SAME NUM-EXACT  2 OP-CONCAT       OPR!
   CLASS-MOVEMENT   0 ACC-SAME NUM-EXACT  2 OP-GATHER       OPR!
   \ backward op-kinds (cad-9): arity = cotangent + the saved tensors the VJP reads.
   \ elementwise *-bwd read (dz, saved-input); reductions read (dz, saved-row); the
   \ softmax adjoint reads the saved OUTPUT row; rope-bwd reads (dz, cos, sin).
   CLASS-EW         1 ACC-SAME NUM-EXACT  2 OP-RELU-BWD        OPR!
   CLASS-EW        10 ACC-SAME NUM-RELTOL 2 OP-GELU-BWD        OPR!
   CLASS-EW         7 ACC-SAME NUM-RELTOL 2 OP-SILU-BWD        OPR!
   CLASS-ROW-REDUCE 8 ACC-F32  NUM-RELTOL 2 OP-LAYERNORM-BWD   OPR!
   CLASS-ROW-REDUCE 6 ACC-F32  NUM-RELTOL 2 OP-RMSNORM-BWD     OPR!
   CLASS-ROW-REDUCE 4 ACC-F32  NUM-RELTOL 2 OP-SOFTMAX-ROW-BWD OPR!
   CLASS-EW         6 ACC-SAME NUM-RELTOL 3 OP-ROPE-BWD        OPR!
   \ reduce/scatter backward op-kinds (cad-9e). No CLASS-FULL-REDUCE in v1: the
   \ full-reduce dot is classified CLASS-ROW-REDUCE (the nearest reduction class;
   \ its bytes/accum model - read a row, f32-accumulate - is the row-reduce model).
   \ The scatters are exact placements into a zero buffer -> CLASS-MOVEMENT (the
   \ gather/slice forward ops they invert are movement too); scatter-add accumulates
   \ duplicates so it f32-accumulates.
   CLASS-ROW-REDUCE 1 ACC-F32  NUM-RELTOL 1 OP-ROWSUM-BWD      OPR!
   CLASS-ROW-REDUCE 2 ACC-F32  NUM-RELTOL 2 OP-FULLSUM-DOT-BWD OPR!
   CLASS-MOVEMENT   0 ACC-SAME NUM-EXACT  1 OP-PAD-SCATTER     OPR!
   CLASS-MOVEMENT   0 ACC-F32  NUM-RELTOL 2 OP-SCATTER-ADD     OPR! ;

OPR-BUILD

\ ---- reference bindings (membership gate: op is COMPLETE once bound) --------
\ scale/bias/residual-add reuse add/mul at scalar-element granularity.
' ADD-F     OP-ADD          cells R-REF + !
' MUL-F     OP-MUL          cells R-REF + !
' MUL-F     OP-SCALE        cells R-REF + !
' ADD-F     OP-BIAS         cells R-REF + !
' RELU-F    OP-RELU         cells R-REF + !
' GELU-F    OP-GELU         cells R-REF + !
' LN-FWD    OP-LAYERNORM    cells R-REF + !
' RMS-FWD   OP-RMSNORM      cells R-REF + !
' SM-FWD    OP-SOFTMAX-ROW  cells R-REF + !
' ADD-F     OP-RESIDUAL-ADD cells R-REF + !
' SILU-F    OP-SILU         cells R-REF + !
' ROPE-PAIR OP-ROPE         cells R-REF + !
\ movement references are BUFFER-granularity (maki/move.f), the exact rewrite the
\ planner must reproduce on materialization; they complete their op rows too.
' MOVE-RESHAPE   OP-RESHAPE   cells R-REF + !
' MOVE-TRANSPOSE OP-TRANSPOSE cells R-REF + !
' MOVE-SLICE     OP-SLICE     cells R-REF + !
' MOVE-CONCAT    OP-CONCAT    cells R-REF + !
' MOVE-GATHER    OP-GATHER    cells R-REF + !
\ matmul / linear buffer references (cad-7a): the row-major f32-cell contraction +
\ the affine linear layer (maki/matmul.f, maki/linear.f). Binding completes their
\ op rows so OPR-COMPLETE? is true and the host executor (maki/executor.f) runs them.
' MATMUL        OP-MATMUL     cells R-REF + !
' LINEAR        OP-LINEAR     cells R-REF + !
\ cast stays incomplete: a decode/cast has no host-executable numeric oracle.
\ backward op-kinds (cad-9): bind the existing scalar/buffer VJP references so the
\ synthesized backward nodes are COMPLETE ops (GOLDEN never fails closed on them).
' RELU-BWD  OP-RELU-BWD        cells R-REF + !
' GELU-BWD  OP-GELU-BWD        cells R-REF + !
' SILU-BWD  OP-SILU-BWD        cells R-REF + !
' LN-BWD    OP-LAYERNORM-BWD   cells R-REF + !
' RMS-BWD   OP-RMSNORM-BWD     cells R-REF + !
' SM-BWD    OP-SOFTMAX-ROW-BWD cells R-REF + !
' ROPE-BWD  OP-ROPE-BWD        cells R-REF + !
\ reduce/scatter backward references are BUFFER-granularity (maki/reduce-bwd.f,
\ maki/scatter.f) - like the movement references, the exact rewrite/reduction the
\ planner must reproduce on materialization; binding completes their op rows.
' ROWSUM-BWD      OP-ROWSUM-BWD       cells R-REF + !
' FULLSUM-DOT-BWD OP-FULLSUM-DOT-BWD  cells R-REF + !
' PAD-SCATTER     OP-PAD-SCATTER      cells R-REF + !
' SCATTER-ADD     OP-SCATTER-ADD      cells R-REF + !

;package
