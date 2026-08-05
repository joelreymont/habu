---
title: Package PTX VJP registry
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:19:41.751534+02:00"
---

src/arch/ptx/vjp.f:18-133 exposes 33 VJP-* registry/table/lookup words globally; lib/ptx/ad.f and ad-gen.f require it, but consumers need only declaration and lookup/dispatch APIs. Put the registry in package VJP, export the smallest checked registration and query surface, keep capacity cells, tables, indexes, validation, and builder helpers private, and update AD consumers directly with no VJP-* compatibility globals. Preserve registration order, op identity, lookup/dispatch behavior, duplicate/capacity diagnostics, generated adjoints, and PTX bytes. Add negative fixtures proving old globals and qualified table/storage helpers reject, plus public declaration/lookup positives and duplicate/overflow behavior. Measure persisted name bytes, loaded JIT/DATA, CODELEN, registration/lookup latency, and generated PTX before/after; require no unexplained growth. Verify VJP and every AD/AD-gen/finite-difference suite, ptx-stdlib, Maki autograd, typed-local diff, package/host/dot lints, fixpoint, and full native gate. Parent: habu-pkg-ptx-compiler-db8cfa46; package boundary only, with registry representation/type changes left to their exact owners.
