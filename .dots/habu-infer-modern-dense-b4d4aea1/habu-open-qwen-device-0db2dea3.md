---
title: Open Qwen device session
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:02:08.924735+02:00"
blocks:
  - habu-own-qwen-runtime-aee01377
  - habu-add-qwen-model-bf23d2ff
  - habu-parse-qwen-config-cd65019c
---

Why: Qwen device operations need one production DEVRT owner before their exact modules can be installed. Interface: DEVRT declares public linear owner product qbuild with no public constructor; foreign packages may only carry it intact through checked DEVRT signatures. BEGIN-QWEN ( DEVRT:core MDLCFG:mcfg -- DEVRT:qwen-begin-result ) calls private PLAN-QWEN, allocates the checked weight, activation, descriptor, logit, and workspace regions once, initializes empty weight rows plus the closed slots WEIGHTS, RMSNORM, ROPE, LINEAR, SWIGLU, PAGED, QKV, ATTN, BLOCK, and LOGITS, and returns exactly begun(qbuild) or refused(core,qwen-begin-error). qwen-begin-error is plan, allocation, or publication. DROP-QWEN ( DEVRT:qbuild -- DEVRT:qwen-drop-result ) releases an incomplete builder in reverse order and returns dropped(core) or refused(qbuild,drop-error). Only the named DEVRT weight and ADD-QWEN transitions owned by downstream leaves may consume and return qbuild; there is no generic install, function handle, registry, or foreign field access. qbuild carries the original core, generation, stable buffers, empty closed inventory, and immutable FOOTPRINT. Owner: sole qbuild declaration, closed inventory layout, BEGIN-QWEN, DROP-QWEN, provisional allocation, and lifetime. Production red: Qwen operation leaves have no valid package-owned builder mutation seam. Acceptance: GPT-2 sessions and two Qwen builders coexist; foreign inspection and construction reject; exact and one-short capacity, every allocation, publication, and teardown failure follows the stated result arms; addresses stay stable and every inventory slot is empty. Forbidden: second qbuild owner, public constructor, exposed field authority, generic installer, kernel implementation, complete inventory, second session type, plugin, callback, per-token allocation, fixed cap, CUDA graph, compatibility arm, or fallback. Smallest owning check: bin/hb --load maki/infer/qwen-runtime-foundation-test.f on DGX Spark. Claim: unassigned.
