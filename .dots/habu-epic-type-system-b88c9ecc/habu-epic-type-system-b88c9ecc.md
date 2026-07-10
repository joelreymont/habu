---
title: "EPIC: type system, habu switchover, dot burndown"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:50:28.082247+02:00"
---

The campaign goal in three ordered phases. PHASE 1 - implement the type-family/ADT system: every PLAN.md item 1-16 (TFAM dots) lands green through its per-item 17a-p gate - registered parametric families, sums/enums/products, generated constructors without trust, checker-owned exhaustive MATCH, runtime tag death, linear layouts, layout-aware stack ops with width-aware native+Gforth lowering, sealed registries with boot-latch friend capability, ADT diagnostics/repair packets; no new TRUST/TRUSTED:/set-check/TRUSTED.md rows; master moves only by verified-green fast-forward. PHASE 2 - switch habu itself over: once MATCH+constructors execute (TFAM 9/10), migrate habu sources to the new types wherever the checker can express them - option/result returns replace sentinel/flag conventions in lib/ and tools/ public APIs, block ENUM families retire ENUM+ call sites, PRODUCT unifies VALUE-RECORD (TFAM 15) and PTX IR, typed ADT protocols replace raw-cell conventions so TRUSTED boundaries shrink (feeds [[habu-epic-type-habu-a34713f0]] retire-TRUSTED); every migration checked, gated, size-ratcheted. PHASE 3 - tackle remaining dots on the typed foundation: maki adoption epic (habu-epic-adopt-adts-64833911), layout-polymorphic params (habu-checker-capability-layout-9b8540bd), deriving (habu-checker-capability-derive-23788e95), then the open PTX/AD/maki backlog (fusion, attention, ONNX, training loop) written in typed ADT style. DONE WHEN: all TFAM dots closed; no sentinel-encoded results remain in checked public APIs; maki suite green on ADT-typed APIs; dot list empty or every survivor explicitly deferred with rationale.
