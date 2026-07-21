---
title: "Infer: SwiGLU op with device kernel"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-21T15:57:38.320348+02:00\\\"\""
closed-at: "2026-07-21T16:47:23.477295+02:00"
close-reason: "Landed 8d527e37: SwiGLU across all three legs. Host: fused arity-2 elementwise op through the complete op discipline (dropout template - registry, adjoint with saved-input, executor via the two-input elementwise engine, grammar token, shape rejects), with the VJP decomposed into existing complete ops (silu-backward on ct*up + mul on silu(gate)) rather than a dedicated backward kind - the broadcast-mul precedent - gradchecked and proven load-bearing (corrupting the VJP reds three assertions). Device: SWIGLU_ROWS per-row kernel via a phantom-preserving SILU tile op on the register-emitter (RELU/ROPE precedent, zero shared-file churn); measured on the GB10: forward within 5e-7 of the f64 host, FD-vs-analytic within 2.4e-4, tolerance documented at 8x margin. Registered everywhere the ratchets demand (perf-watch + WAIVER row proven load-bearing, FILEMAP, four suite registrations). Full tests green at the merged tip. Deferred per precedent: device timing to the timing lane, Orin goldens to the standing Orin-owed dot, in-graph device lowering to the decode-kernel phase"
---

The LLaMA-family MLP activation, forward + VJP + device kernel: silu(x) = x*sigmoid(x); swiglu(gate,up) = silu(gate) elementwise-times up. Host op follows the landed op discipline (op-kind/registry/adjoint/executor rows - the dropout landing 29602d57 is the freshest template); VJP closed-form gradchecked; device kernel is elementwise (lib/ptx cg-activation.f GELU precedent) with a GB10 device test (golden vs host + FD gradcheck, arch probe idiom). New producer file registers in the kernel-perf watch table (tools/ptx/perf-watch.f - the completeness ratchet WILL demand it) with a correctness WAIVER perf row. Red-first throughout; run-twice locked; fail-closed device-lowering reject for unsupported dtypes.

Claim: agent=swiglu workspace=.jj-ws/fable-swiglu machine=spark (owns the SwiGLU op host+VJP+device)
