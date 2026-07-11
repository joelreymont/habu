\ maki/op-kind.f - the canonical model op-kind registry (CAD-PLAN section 4.2).
\
\ The single op set shared by the Phase-0 MODEL: parser (maki/cad.f) and the
\ descriptor-mode planning vocabulary (maki/tensor-value.f). One enum, defined
\ once, so capture and planners agree on op identity - the registry is the single
\ extension point (planners never learn op names). Adding an op is one entry
\ here plus its scalar reference + VJP in the op registry (cad-1). maki -> habu
\ only; needs no library beyond core (named constants, no runtime logic).
\
\ Backward op-kinds (OP-*-BWD, cad-9) extend the SAME enum: the model-IR reverse
\ transform (maki/backward.f) emits them as ordinary nodes so they enter the same
\ fusion/traffic/memory planners. They are SYNTHESIZED only - never parseable in a
\ MODEL: body (the maki/cad.f OP-KIND token map does not name them); matmul/movement
\ adjoints reuse the existing ops (transpose+matmul, reshape/transpose/slice), so
\ only the elementwise/reduction adjoints that are genuinely new ops appear here.

package MAKI
public

0  constant OP-ADD
1  constant OP-MUL
2  constant OP-SCALE
3  constant OP-BIAS
4  constant OP-RELU
5  constant OP-GELU
6  constant OP-LAYERNORM
7  constant OP-RMSNORM
8  constant OP-SOFTMAX-ROW
9  constant OP-MATMUL
10 constant OP-LINEAR
11 constant OP-RESIDUAL-ADD
12 constant OP-CAST
13 constant OP-SILU            \ x * sigmoid(x) (reference: maki/silu.f)
14 constant OP-ROPE            \ rotary pair rotation (reference: maki/rope.f)
\ ---- movement ops (CLASS-MOVEMENT: exact layout rewrites, no compute; 6.3) ---
15 constant OP-RESHAPE         \ same elements, new RxC   (reference: maki/move.f)
16 constant OP-TRANSPOSE       \ RxC -> CxR
17 constant OP-SLICE           \ row-range copy
18 constant OP-CONCAT          \ row-wise append
19 constant OP-GATHER          \ row indexed select
\ ---- backward op-kinds (cad-9: synthesized by maki/backward.f, not MODEL:-typed) --
20 constant OP-RELU-BWD          \ dz gated by sign(x)              (ref: RELU-BWD)
21 constant OP-GELU-BWD          \ dz * gelu'(x)                    (ref: GELU-BWD)
22 constant OP-SILU-BWD          \ dz * silu'(x)                    (ref: SILU-BWD)
23 constant OP-LAYERNORM-BWD     \ row-coupled layernorm VJP        (ref: LN-BWD)
24 constant OP-RMSNORM-BWD       \ row-coupled rmsnorm VJP          (ref: RMS-BWD)
25 constant OP-SOFTMAX-ROW-BWD   \ softmax VJP over the OUTPUT row  (ref: SM-BWD)
26 constant OP-ROPE-BWD          \ rotate cotangent by -angle       (ref: ROPE-BWD)
\ ---- reduce / scatter backward op-kinds (cad-9e: reduce/scatter adjoints) ------
\ These synthesize the param/movement adjoints the elementwise/reduction *-BWD ops
\ cannot: bias/linear bias-grad is a reduce over rows; scale-grad a full-reduce dot;
\ the slice/gather input-grads are scatters into a zero buffer. Buffer-granularity
\ references (maki/reduce-bwd.f, maki/scatter.f), never MODEL:-parseable.
27 constant OP-ROWSUM-BWD        \ sum cotangent rows -> 1xC     (bias grad; ref: ROWSUM-BWD)
28 constant OP-FULLSUM-DOT-BWD   \ sum(ct (.) x) -> 1x1          (scale grad; ref: FULLSUM-DOT-BWD)
29 constant OP-PAD-SCATTER       \ place (r1-r0)xC ct into zero RxC at r0 (slice adj; ref: PAD-SCATTER)
30 constant OP-SCATTER-ADD       \ add ct rows into zero RxC at gathered idx (gather adj; ref: SCATTER-ADD)
31 constant OP-N               \ op-kind range bound (private op-registry / adjoint table dimension + wire code range)

\ ---- the op-kind family (dot habu-cad-adt-swap, corrected plan) -------------
\ `opkind` is the semantic type carried from construction (MIR-OP-BEGIN /
\ PLAN-OP-BEGIN / onnx import) through Model IR storage and every consumer. The
\ OP-* codes above remain ONLY as the wire/hash/private-table-index vocabulary
\ crossed at the named boundaries below (OPKIND>N, the op-registry / adjoint
\ private table indexes, sched-key's hash fold, test assertions). No internal API
\ takes or returns a raw op code. Declaration order == OP-ADD(0)..OP-SCATTER-ADD(30),
\ so OPKIND>N below is the single source that ties a variant to its code.
\ NB variant tails are the wire spellings with hyphens; inline `\` comments inside
\ the ENUM block are a parse error, so per-variant notes stay in this header.
ENUM opkind
  add mul scale bias relu gelu layernorm rmsnorm softmax-row matmul linear
  residual-add cast silu rope reshape transpose slice concat gather
  relu-bwd gelu-bwd silu-bwd layernorm-bwd rmsnorm-bwd softmax-row-bwd rope-bwd
  rowsum-bwd fullsum-dot-bwd pad-scatter scatter-add
;ENUM

\ named render boundary: opkind -> wire/table code (exhaustive MATCH; a bad tag
\ is unrepresentable, so no throw arm exists). This is THE only place a family
\ becomes an op code; the op-registry / adjoint private tables index through it.
: OPKIND>N ( opkind -- n )
   MATCH opkind
      add             OF OP-ADD             ENDOF
      mul             OF OP-MUL             ENDOF
      scale           OF OP-SCALE           ENDOF
      bias            OF OP-BIAS            ENDOF
      relu            OF OP-RELU            ENDOF
      gelu            OF OP-GELU            ENDOF
      layernorm       OF OP-LAYERNORM       ENDOF
      rmsnorm         OF OP-RMSNORM         ENDOF
      softmax-row     OF OP-SOFTMAX-ROW     ENDOF
      matmul          OF OP-MATMUL          ENDOF
      linear          OF OP-LINEAR          ENDOF
      residual-add    OF OP-RESIDUAL-ADD    ENDOF
      cast            OF OP-CAST            ENDOF
      silu            OF OP-SILU            ENDOF
      rope            OF OP-ROPE            ENDOF
      reshape         OF OP-RESHAPE         ENDOF
      transpose       OF OP-TRANSPOSE       ENDOF
      slice           OF OP-SLICE           ENDOF
      concat          OF OP-CONCAT          ENDOF
      gather          OF OP-GATHER          ENDOF
      relu-bwd        OF OP-RELU-BWD        ENDOF
      gelu-bwd        OF OP-GELU-BWD        ENDOF
      silu-bwd        OF OP-SILU-BWD        ENDOF
      layernorm-bwd   OF OP-LAYERNORM-BWD   ENDOF
      rmsnorm-bwd     OF OP-RMSNORM-BWD     ENDOF
      softmax-row-bwd OF OP-SOFTMAX-ROW-BWD ENDOF
      rope-bwd        OF OP-ROPE-BWD        ENDOF
      rowsum-bwd      OF OP-ROWSUM-BWD      ENDOF
      fullsum-dot-bwd OF OP-FULLSUM-DOT-BWD ENDOF
      pad-scatter     OF OP-PAD-SCATTER     ENDOF
      scatter-add     OF OP-SCATTER-ADD     ENDOF
   ;MATCH ;

\ family identity test: the checker gives enums no derived eq, so op identity is
\ compared through the canonical codes. Type-safe (both args must be opkind, so a
\ dtype/layout can never sneak into an op compare) and injective (distinct ops ->
\ distinct codes), so this IS op-identity equality, not a raw-n compatibility path.
: OPK= ( opkind opkind -- bool )  OPKIND>N swap OPKIND>N = ;

end-package
