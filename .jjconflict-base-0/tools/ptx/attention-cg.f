\ attention-cg.f - emit the fused attention kernel ATTN (see lib/ptx/cg-attention.f
\ for the design notes: fused QK^T -> shared softmax -> PV, scores never global;
\ device-correct vs CPU, ~8 GFLOP/s baseline, flash optimization path dotted).
\ Standalone checked emitter; writes PTX to stdout.

require lib/ptx/cg-attention.f

ATTN:EMIT
