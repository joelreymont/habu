---
title: Load GPT-2 runtime modules
status: closed
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:48.332822+02:00"
closed-at: "2026-08-04T19:31:43.576583+02:00"
close-reason: "Superseded by GPT2:model at 1056a0ad14d5: OPEN builds the exact tensor and attention modules once, resolves the seven functions, retains them across LOGITS calls, and CLOSE unloads them; a DEVRT module inventory would duplicate production code."
blocks:
  - habu-prove-gb10-inference-d43eecce
---

Why: GPT-2 needs one exact persistent function inventory compiled once. Interface: package-private DEVRT:LOAD-GPT2-MODULES takes core, compiles and loads only embedding, LayerNorm, QKV/linear, paged attention, GELU, residual, and vocabulary functions from the proven GB10 target, and returns core plus private modules; refusal unloads partial modules. Owner: GPT-2 module and function inventory only. Production red: modules are assembled per test region. Acceptance: exact names and addresses remain stable across launches; missing, duplicate, compile, load, and function failures release partial state; two inventories coexist. Forbidden: allocation plan, Qwen module, generic registry, callback, lazy compile, ABI version, compatibility target, or caller-selected toolchain identity. Smallest owning check: bin/hb --load maki/infer/gpt2-runtime-modules-test.f on DGX Spark.
