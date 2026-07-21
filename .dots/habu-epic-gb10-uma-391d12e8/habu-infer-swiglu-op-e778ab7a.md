---
title: "Infer: SwiGLU op with device kernel"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.320348+02:00\""
---

The LLaMA-family MLP activation, forward + VJP + device kernel: silu(x) = x*sigmoid(x); swiglu(gate,up) = silu(gate) elementwise-times up. Host op follows the landed op discipline (op-kind/registry/adjoint/executor rows - the dropout landing 29602d57 is the freshest template); VJP closed-form gradchecked; device kernel is elementwise (lib/ptx cg-activation.f GELU precedent) with a GB10 device test (golden vs host + FD gradcheck, arch probe idiom). New producer file registers in the kernel-perf watch table (tools/ptx/perf-watch.f - the completeness ratchet WILL demand it) with a correctness WAIVER perf row. Red-first throughout; run-twice locked; fail-closed device-lowering reject for unsupported dtypes.

Claim: agent=swiglu workspace=.jj-ws/fable-swiglu machine=spark (owns the SwiGLU op host+VJP+device)
