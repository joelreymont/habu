---
title: Define finite CAD effect rows
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.390552+02:00"
blocks:
  - habu-fix-wide-product-5c81dada
---

Full context: src/core/checker.f and maki/op-registry.f have no finite semantic effect vocabulary, so pure, parameter read, state write, random, host IO, device launch, atomic, collective, allocation, and publication operations are indistinguishable for cache, fusion, and recompute legality. Fix: add a checked package-scoped static canonical row whose finite atoms bind sorted semantic operand/attribute/capability slots. A binding key includes atom, stable site path, slot kind, and slot index; local declarations start with an empty path, and checked REMAP prefixes a caller/call-site segment without changing the original resolvable slot. PURE is the unique empty row; non-pure rows support multiple bindings for one atom, allow different atoms to bind the same site/slot, reject exact duplicate insertion, and expose canonical UNION where repeated identical entries are idempotent. Rows contain no runtime digest, address, generation, sequence, or authority instance. Define conservative duplication, commute, cacheability, and barrier truth tables. Acceptance: incomplete, malformed, noncanonical, over-capacity, or direct duplicate bindings reject; capacity is a named composition bound sufficient for stored-word and quotation unions rather than the current largest op, and overflow is a typed diagnostic; weight and bias can both bind parameter-read; state-write plus atomic may share a site/slot; REMAP preserves slot kind/index, prefixes paths capture-free, and is deterministic; UNION is associative, commutative, idempotent and deterministic after remap; every atom/table row has focused tests. Files: new src/cad/effect.f, focused test, docs/effects.md. Verify: standalone effect tests, remap/property fixtures, host/filemap/dot lints. Ownership: static vocabulary and row algebra only; no checker persistence, Maki registry migration, runtime binding resolution, or cache-key integration.

Claim: agent=cad-effects workspace=.jj-ws/habu-define-finite-cad-0bdf52ad.

Review blocker: the first one-binding-per-atom implementation is not mergeable.
The multi-binding redesign proved checker width accounting overcounts W34
UNMAKE and repeated W12 row inputs past ER.MINI=255. Resume only after
habu-fix-wide-product-5c81dada lands; capacity nine is not a valid workaround.
