\ maki/adjoint.f - the model-op adjoint registry (CAD-PLAN section 12, dot cad-9a).
\
\ Per op-kind, the FACTS the model-IR reverse transform (maki/backward.f) needs to
\ emit a backward region: the adjoint id (also written into the op-registry vjp
\ field, OPR-VJP!), which inputs receive a gradient (a bitmask), and what the adjoint
\ must read from the forward pass - the forward INPUT, the forward OUTPUT, or neither
\ (relu/gelu need the input sign/value; softmax-row needs the OUTPUT; matmul dX needs
\ W and dW needs X; movement/rope need only static shape/params). A few adjoints ARE
\ an existing op (add copies the cotangent; matmul adjoints are transposed matmuls;
\ reshape->reshape); the rest name a dedicated backward op-kind (OP-*-BWD) whose
\ scalar reference is the existing *-BWD word.
\
\ cad-9e unlocked the reduce/scatter adjoints: bias/linear bias-grad is OP-ROWSUM-BWD,
\ scale-grad is OP-FULLSUM-DOT-BWD, the slice input-grad is OP-PAD-SCATTER, the gather
\ input-grad is OP-SCATTER-ADD (maki/reduce-bwd.f, maki/scatter.f). So scale/bias/linear/
\ slice/gather now carry ADJ-SUP? = 1 and backward.f emits them.
\
\ v1 boundary that remains (fail-closed, named reason - NEVER a silent partial gradient):
\   cast   no adjoint (non-differentiable) -> reject on GRADCHECK / backward
\   scale  the INPUT gradient dx = ct*s is a broadcast-multiply; v1 emits it as an
\          elementwise OP-MUL when the scale operand matches the cotangent shape and
\          as OP-SCALE (broadcast-by-1x1) when the operand is a 1x1 scalar, but fails
\          closed (E-BW-BROADCAST, backward.f) on a partial broadcast (1xC / Rx1) that
\          needs a broadcast-reduce not in the op set yet. The scale FACTOR gradient
\          (OP-FULLSUM-DOT-BWD) is always emittable. maki -> habu only; owns -5100..-5104.

require maki/op-kind.f
require maki/op-registry.f
require maki/gelu.f
require maki/silu.f
require maki/layernorm.f
require maki/rmsnorm.f
require maki/softmax.f
require maki/rope.f
require maki/autograd.f

-5100 constant E-ADJ-KIND     \ op-kind out of range
-5101 constant E-ADJ-ID       \ adjoint id out of range
-5102 constant E-ADJ-SAVE     \ save-need tag out of range
-5103 constant E-ADJ-NONE     \ adjoint fact requested from a non-differentiable op
-5104 constant E-ADJ-UNSUP    \ adjoint requested for a v1-unsupported op

package MAKI
public

\ ---- adjoint ids (also the op-registry vjp field value; 0 = no adjoint) ------
0  constant ADJ-NONE          \ cast / decode: non-differentiable
1  constant ADJ-COPY          \ linear op: cotangent copies to each input (add/residual/bias)
2  constant ADJ-MUL           \ product rule: dx = dz * other-operand (mul/scale)
3  constant ADJ-RELU          \ OP-RELU-BWD
4  constant ADJ-GELU          \ OP-GELU-BWD
5  constant ADJ-SILU          \ OP-SILU-BWD
6  constant ADJ-LAYERNORM     \ OP-LAYERNORM-BWD (row-coupled)
7  constant ADJ-RMSNORM       \ OP-RMSNORM-BWD (row-coupled)
8  constant ADJ-SOFTMAX       \ OP-SOFTMAX-ROW-BWD (reads the OUTPUT row)
9  constant ADJ-MATMUL        \ dX = dZ Wt, dW = Xt dZ (transposed matmuls)
10 constant ADJ-LINEAR        \ matmul adjoints + bias row-reduce
11 constant ADJ-ROPE          \ OP-ROPE-BWD (rotate by -angle; reads cos/sin)
12 constant ADJ-RESHAPE       \ reshape the cotangent back to the input shape
13 constant ADJ-TRANSPOSE     \ transpose the cotangent
14 constant ADJ-SLICE         \ OP-PAD-SCATTER the cotangent into a zero buffer at r0
15 constant ADJ-CONCAT        \ split the cotangent into a slice per input
16 constant ADJ-GATHER        \ OP-SCATTER-ADD the cotangent rows at the gathered indices
17 constant ADJ-BIAS          \ dx = cotangent copy ; d-bias = OP-ROWSUM-BWD (row reduce)
18 constant ADJ-SCALE         \ dx = ct*s (mul/scale) ; d-scale = OP-FULLSUM-DOT-BWD
19 constant ADJ-GELU-BWD      \ second order: d-dz = OP-GELU-BWD, d-x = OP-GELU-BWD2
20 constant ADJ-N             \ range bound

\ ---- what the adjoint must read from the forward pass ------------------------
0 constant SAVE-NONE          \ needs only static shape / operand refs (add/reshape/rope)
1 constant SAVE-INPUT         \ needs the forward INPUT saved (relu/gelu/norms/matmul)
2 constant SAVE-OUTPUT        \ needs the forward OUTPUT saved (softmax-row)
3 constant SAVE-N

private

\ one create-array per fact, indexed by op-kind 0..OP-N-1
create A-ID   OP-N cells allot     \ adjoint id
create A-SAVE OP-N cells allot      \ save-need
create A-MASK OP-N cells allot      \ per-input gradient bitmask (bit i = input i gets a grad)
create A-SUP  OP-N cells allot      \ 1 = the reverse transform can emit it in v1
create A-BOP  OP-N cells allot      \ dedicated backward op-kind, or -1 (adjoint reuses ops)

: ADJ-CK ( n -- n )  dup 0 < over OP-N >= or if E-ADJ-KIND throw then ;

\ load-time row writer: id save mask sup bop op --
: ADJ! ( n n n n n n -- ) {: id:n sv:n mask:n sup:n bop:n op:n :}
   op ADJ-CK drop
   id 0 < id ADJ-N >= or if E-ADJ-ID throw then
   sv 0 < sv SAVE-N >= or if E-ADJ-SAVE throw then
   id   op cells A-ID   + !
   sv   op cells A-SAVE + !
   mask op cells A-MASK + !
   sup  op cells A-SUP  + !
   bop  op cells A-BOP  + ! ;

public

\ ---- accessors (take the op-kind family; convert ONCE at the private table
\ index via OPKIND>N, so the row arrays stay indexed by the wire code) --------
: ADJ-ID   ( opkind -- n )  OPKIND>N cells A-ID   + @ ;
: ADJ-SAVE ( opkind -- n )  OPKIND>N cells A-SAVE + @ ;
: ADJ-MASK ( opkind -- n )  OPKIND>N cells A-MASK + @ ;
: ADJ-BOP  ( opkind -- n )  OPKIND>N cells A-BOP  + @ ;   \ dedicated backward op CODE, or -1

: ADJ-HAS? ( opkind -- bool )  ADJ-ID ADJ-NONE <> ;         \ op is differentiable
: ADJ-SUP? ( opkind -- bool )  OPKIND>N cells A-SUP + @ 0= 0= ; \ transform can emit it (v1)

\ does input k of this op receive a gradient? (op below k on the stack)
: ADJ-GRAD-IN? ( opkind n -- bool ) {: k:n :}  ADJ-MASK  1 k lshift  and 0= 0= ;

\ does the adjoint use a dedicated backward op-kind (vs reusing existing ops)?
: ADJ-BWD-OP? ( opkind -- bool )  ADJ-BOP 0 >= ;

\ the dedicated backward op-kind as a FAMILY (backward.f emits it as a node op).
\ Lifts the stored code back to an opkind at this named boundary; fail closed on
\ an op with no dedicated backward op (reached for relu/gelu/silu/norms and the
\ second-order gelu-bwd row).
: BOP-LIFT ( n -- opkind )
   dup OP-RELU-BWD        = if drop MAKI-OPKIND:RELU-BWD        exit then
   dup OP-GELU-BWD        = if drop MAKI-OPKIND:GELU-BWD        exit then
   dup OP-SILU-BWD        = if drop MAKI-OPKIND:SILU-BWD        exit then
   dup OP-LAYERNORM-BWD   = if drop MAKI-OPKIND:LAYERNORM-BWD   exit then
   dup OP-RMSNORM-BWD     = if drop MAKI-OPKIND:RMSNORM-BWD     exit then
   dup OP-SOFTMAX-ROW-BWD = if drop MAKI-OPKIND:SOFTMAX-ROW-BWD exit then
   dup OP-ROPE-BWD        = if drop MAKI-OPKIND:ROPE-BWD        exit then
   dup OP-GELU-BWD2       = if drop MAKI-OPKIND:GELU-BWD2       exit then
   drop E-ADJ-UNSUP throw ;
: ADJ-BWD-KIND ( opkind -- opkind )  ADJ-BOP BOP-LIFT ;

\ ---- save-need text ---------------------------------------------------------
: ADJ-SAVE-NAME ( n -- ptr u8 n )
   case
      SAVE-NONE   of s" none"   endof
      SAVE-INPUT  of s" input"  endof
      SAVE-OUTPUT of s" output" endof
      E-ADJ-SAVE throw
   endcase ;

\ ---- v1-boundary reason (why an op with an adjoint is not yet emittable) -----
\ Fail closed on a supported op (there is no reason) or a differentiable-op query.
\ Only cast remains v1-unsupported (non-differentiable); scale/bias/linear/slice/gather
\ became emittable in cad-9e. A supported op has no reason and fails closed here.
: ADJ-UNSUP$ ( opkind -- ptr u8 n )
   MATCH opkind
      cast            OF s" cast has no adjoint (non-differentiable)" ENDOF
      add             OF E-ADJ-UNSUP throw ENDOF
      mul             OF E-ADJ-UNSUP throw ENDOF
      scale           OF E-ADJ-UNSUP throw ENDOF
      bias            OF E-ADJ-UNSUP throw ENDOF
      relu            OF E-ADJ-UNSUP throw ENDOF
      gelu            OF E-ADJ-UNSUP throw ENDOF
      layernorm       OF E-ADJ-UNSUP throw ENDOF
      rmsnorm         OF E-ADJ-UNSUP throw ENDOF
      softmax-row     OF E-ADJ-UNSUP throw ENDOF
      matmul          OF E-ADJ-UNSUP throw ENDOF
      linear          OF E-ADJ-UNSUP throw ENDOF
      residual-add    OF E-ADJ-UNSUP throw ENDOF
      silu            OF E-ADJ-UNSUP throw ENDOF
      rope            OF E-ADJ-UNSUP throw ENDOF
      reshape         OF E-ADJ-UNSUP throw ENDOF
      transpose       OF E-ADJ-UNSUP throw ENDOF
      slice           OF E-ADJ-UNSUP throw ENDOF
      concat          OF E-ADJ-UNSUP throw ENDOF
      gather          OF E-ADJ-UNSUP throw ENDOF
      relu-bwd        OF E-ADJ-UNSUP throw ENDOF
      gelu-bwd        OF E-ADJ-UNSUP throw ENDOF
      silu-bwd        OF E-ADJ-UNSUP throw ENDOF
      layernorm-bwd   OF E-ADJ-UNSUP throw ENDOF
      rmsnorm-bwd     OF E-ADJ-UNSUP throw ENDOF
      softmax-row-bwd OF E-ADJ-UNSUP throw ENDOF
      rope-bwd        OF E-ADJ-UNSUP throw ENDOF
      rowsum-bwd      OF E-ADJ-UNSUP throw ENDOF
      fullsum-dot-bwd OF E-ADJ-UNSUP throw ENDOF
      pad-scatter     OF E-ADJ-UNSUP throw ENDOF
      scatter-add     OF E-ADJ-UNSUP throw ENDOF
      gelu-bwd2       OF E-ADJ-UNSUP throw ENDOF
   ;MATCH ;

private

\ ---- the adjoint registry ( id save mask sup bop op ADJ! ) -------------------
\ default: every op-kind is non-differentiable until a row overrides it, so a
\ synthesized backward op-kind stays ADJ-NONE (fail-closed E-BW-NOADJ on a deeper
\ BW-BUILD) until its own adjoint row lands. gelu-bwd carries one (second-order
\ pilot below); the remaining *-BWD kinds are tracked by the higher-order dot.
: ADJ-DEFAULTS ( -- )
   OP-N 0 ?do  ADJ-NONE SAVE-NONE 0 0 -1 i ADJ!  loop ;

: ADJ-BUILD ( -- )
   ADJ-DEFAULTS
   \ ---- linear / product-rule elementwise ----
   ADJ-COPY SAVE-NONE  3  1 -1 OP-ADD          ADJ!
   ADJ-COPY SAVE-NONE  3  1 -1 OP-RESIDUAL-ADD ADJ!
   ADJ-MUL  SAVE-INPUT 3  1 -1 OP-MUL          ADJ!
   \ ---- dedicated elementwise backward ops ----
   ADJ-RELU SAVE-INPUT 1   1 OP-RELU-BWD OP-RELU ADJ!
   ADJ-GELU SAVE-INPUT 1   1 OP-GELU-BWD OP-GELU ADJ!
   ADJ-SILU SAVE-INPUT 1   1 OP-SILU-BWD OP-SILU ADJ!
   ADJ-ROPE SAVE-NONE  1   1 OP-ROPE-BWD OP-ROPE ADJ!
   \ ---- reduction backward ops ----
   ADJ-LAYERNORM SAVE-INPUT  1 1 OP-LAYERNORM-BWD   OP-LAYERNORM   ADJ!
   ADJ-RMSNORM   SAVE-INPUT  1 1 OP-RMSNORM-BWD     OP-RMSNORM     ADJ!
   ADJ-SOFTMAX   SAVE-OUTPUT 1 1 OP-SOFTMAX-ROW-BWD OP-SOFTMAX-ROW ADJ!
   \ ---- contraction (transposed matmuls; both operands saved by policy) ----
   ADJ-MATMUL SAVE-INPUT 3 1 -1 OP-MATMUL ADJ!
   \ ---- movement (structural cotangent rewrites) ----
   ADJ-RESHAPE   SAVE-NONE 1  1 -1 OP-RESHAPE   ADJ!
   ADJ-TRANSPOSE SAVE-NONE 1  1 -1 OP-TRANSPOSE ADJ!
   ADJ-CONCAT    SAVE-NONE 3 1 -1 OP-CONCAT    ADJ!
   \ ---- reduce / scatter adjoints (cad-9e: now emittable, ADJ-SUP? = 1) ------
   \ scale: dx = ct*s, d-scale = OP-FULLSUM-DOT-BWD (both operands saved).
   ADJ-SCALE   SAVE-INPUT 3  1 -1 OP-SCALE  ADJ!
   \ bias: dx = ct copy, d-bias = OP-ROWSUM-BWD(ct) (nothing saved from forward).
   ADJ-BIAS    SAVE-NONE  3  1 -1 OP-BIAS   ADJ!
   \ linear: matmul adjoints (x, w saved) + d-bias = OP-ROWSUM-BWD(ct).
   ADJ-LINEAR  SAVE-INPUT 7  1 -1 OP-LINEAR ADJ!
   \ slice/gather: input-grad is a scatter into a zero buffer (static params/index).
   ADJ-SLICE   SAVE-NONE  1  1 -1 OP-SLICE  ADJ!
   ADJ-GATHER  SAVE-NONE  1  1 -1 OP-GATHER ADJ!
   \ ---- second order (higher-order grad pilot): the gelu-bwd node z = dz*gelu'(x)
   \ is itself differentiable in BOTH operands: d-dz = ct*gelu'(x) (OP-GELU-BWD
   \ again), d-x = ct*dz*gelu''(x) (OP-GELU-BWD2). Its operands ARE the tensors the
   \ adjoint reads -> SAVE-INPUT; gelu-bwd2 itself stays ADJ-NONE (third order is
   \ fail-closed E-BW-NOADJ).
   ADJ-GELU-BWD SAVE-INPUT 3 1 OP-GELU-BWD2 OP-GELU-BWD ADJ! ;

\ ---- wire the op-registry vjp field to the adjoint id (cad-9 requirement) ----
\ pure private-table wiring over the code index space: copy A-ID[i] -> registry
\ vjp[i] for every op code (no opkind conversion; both tables index by code).
: ADJ-WIRE-VJP ( -- )  OP-N 0 ?do  i  i cells A-ID + @  OPR-VJP!  loop ;

ADJ-BUILD
ADJ-WIRE-VJP

;package
