---
title: Complete Qwen runtime
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.685821+02:00"
blocks:
  - habu-open-qwen-device-0db2dea3
  - habu-assemble-qwen-runtime-454f769a
---

Why: individually proven Qwen operations become a usable session only after one all-or-nothing inventory check. Interface: DEVRT:COMPLETE-QWEN takes qbuild after the exact WEIGHTS, RMSNORM, ROPE, LINEAR, SWIGLU, PAGED, QKV, ATTN, BLOCK, and LOGITS transitions each filled their sole closed slot; verifies every slot, buffer, descriptor, and generation; then returns completed(session) after total retyping or refused(qbuild,complete-error) without mutation. The Qwen session exposes only the final DEVRT:QWEN-LOGITS operation and footprint/close operations; primitive and builder calls remain private. DROP-QWEN remains the sole cleanup for a refused builder. Owner: Qwen session publication and exact inventory-completeness check only. Production red: a partial Qwen builder can otherwise escape. Acceptance: the named closed inventory publishes once; missing, duplicate, wrong function, incomplete weights, wrong generation, and every finalization failure publish no session and return a byte-identical qbuild; two Qwen sessions coexist. Forbidden: second session type, module implementation, generic registry, plugin, fallback, ABI version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/qwen-runtime-complete-test.f on DGX Spark.
