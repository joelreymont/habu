---
title: "V2 R3: type model IR identities"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:09.712242+02:00"
blocks:
  - habu-v2-r3-declare-3fcdeebb
---

Problem: maki/model-ir.f exposes node, input-slot, operand-ref, and table positions as interchangeable n cells. Fix: migrate public model-IR signatures and internal locals to CAD-KIND:node-id, CAD-KIND:obj-id, and a package-owned ref/index kind; keep raw cells only inside private checked table-index conversion words after bounds validation. Acceptance: node-vs-slot, node-vs-ref, and node-vs-object swaps reject statically; builder/accessor happy paths and rollback still pass; no public MIR handle remains n. Files: maki/model-ir.f, maki/model-ir-test.f, direct consumers named by rg. Verify: model-ir focused test, maki/test.f, typed-local diff lint. Depends: CAD kind declarations.
