\ attention-cg.f - emit the fused attention kernel ATTN (see lib/ptx/cg-attention.f
\ for the design notes: fused QK^T -> shared softmax -> PV, scores never global;
\ device-correct vs CPU, ~8 GFLOP/s baseline, flash optimization path dotted).
\ Load after src/arch/ptx/emit.f + lib/ptx/cg.f + lib/ptx/cg-attention.f.
EMIT-ATTN
