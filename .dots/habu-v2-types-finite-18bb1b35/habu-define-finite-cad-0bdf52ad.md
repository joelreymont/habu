---
title: Define finite CAD effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.390552+02:00"
blocks:
---

Full context: src/core/checker.f and maki/op-registry.f have no finite semantic effect vocabulary, so pure, parameter read, state write, random, host IO, device launch, atomic, collective, allocation, and publication operations are indistinguishable for cache, fusion, and recompute legality. Fix: add a checked package-scoped static canonical row whose finite atoms bind sorted semantic operand/attribute/capability/capture slots through opaque immutable arena handles. A binding key includes atom, stable site path, slot kind, and slot index; local declarations start with an empty path, and checked REMAP prefixes a caller/call-site segment without changing the original resolvable slot. PURE is the unique canonical empty row; non-pure rows support multiple bindings for one atom, allow different atoms to bind the same site/slot, reject direct duplicate insertion, and expose canonical UNION where repeated identical entries are idempotent. Rows contain no runtime digest, address, generation, sequence, authority instance, handle number, or allocation-order identity. Define conservative duplication, commute, cacheability, and barrier truth tables. Acceptance: forged/stale handle, incomplete builder, malformed/noncanonical wire row, protocol-count overflow, allocator/resource failure, or direct duplicate insertion rejects transactionally with a typed diagnostic; no small by-value semantic capacity is imposed; weight and bias can both bind parameter-read; state-write plus atomic may share a site/slot; REMAP preserves slot kind/index, prefixes paths capture-free, and is deterministic; UNION is associative, commutative, idempotent and deterministic after remap; canonical equality/serialization ignore handle and allocation order; every atom/table row has focused tests including 4096-binding composition. Files: src/cad/effect-types.f, src/cad/effect.f, focused and property tests, docs/effects.md. Verify: standalone effect tests, remap/property/scale fixtures, canonical fresh-process replay, host/filemap/dot lints. Ownership: static vocabulary and row algebra only; immutable storage belongs to habu-add-immutable-nominal-9290a81f; no checker persistence, Maki registry migration, runtime binding resolution, or cache-key integration.

Review blocker: commit 725dc1bb is permanently rejected. Its one-binding-per-
atom, digest-bearing, by-value row is not mergeable, and a maximal 127-cell
replacement would still cap ordinary transitive composition at 25 bindings.
Resume only after habu-add-immutable-nominal-9290a81f lands; raw handles,
fixed-width packed paths, hashes as authority, and capacity shrinkage are not
valid workarounds.
