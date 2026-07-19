---
title: Package PTX IR and optimizer
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:19:29.156291+02:00"
---

lib/ptx/ir.f:9-262 exposes 61 PTXIR-* globals; opt-ir.f:23-356 and opt.f:41-302 expose about 176 OPTX-* and optimizer globals, including raw arenas, line tables, symbol maps, pass cursors, and mutation helpers. IR is active; opt.f is explicitly dormant pending habu-adjudicate-dormant-ptx-482310bc. If the optimizer is retired, delete its globals and package only the retained IR. If retained, package IR and optimizer as distinct owners, reopening the optimizer package across its representation and passes; export only construction/query/pass entry points, keep storage and rewrite internals private, and remove all forwarding globals. This dot owns namespacing only: habu-libs-migrate-ptx-1071a2e6 owns IR STRUCTURE representation and habu-type-ptx-ir-23af6a9b owns typed op/edge/value semantics. Preserve exact IR identity, emitted PTX, pass order, idempotence, opaque-input behavior, diagnostics, and zero-allocation bounds. Add old-global/private-access rejection fixtures and public qualified positives. Measure dictionary-name bytes, JIT/DATA/CODELEN, IR build/optimization latency, and emitted PTX before/after. Verify IR/AD-IR/optimizer suites, ptx-stdlib, Maki consumers, typed-local diff, package/host/filemap/dot lints, fixpoint, and full native gate. Prerequisites: optimizer adjudication before opt migration; coordinate representation/type owners to avoid churn. Parent: habu-pkg-ptx-compiler-db8cfa46.
