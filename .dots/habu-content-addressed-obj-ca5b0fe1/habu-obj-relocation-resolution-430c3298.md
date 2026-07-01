---
title: Object relocation resolution
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:30:03.546932+02:00\\\"\""
closed-at: "2026-07-01T21:34:44.149178+02:00"
close-reason: "completed: OBJLINK now records reloc kind/symbol, merged patch addresses, fail-closed unresolved targets, resolved target addresses from DEF rows, table overflow, and export-only import validation coverage. Proof: object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18951ms <= 40000ms."
---

Problem: OBJLINK validates relocation offsets but still discards relocation rows, so the future object linker cannot patch merged text without rescanning objects and re-resolving symbols. Fix: extend lib/object-link.f with a bounded relocation table that copies kind/symbol strings, stores merged patch addresses from object text base + reloc offset, resolves relocation targets against DEF addresses during OBJLINK:CHECK, and exposes count/accessors for kind, symbol, patch, target. Keep import validation export-only; defs provide addresses, not visibility. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: ADD records reloc rows with merged patch address; CHECK rejects reloc target without DEF; CHECK records target address for valid reloc; bad reloc offset still rejects; relocation table overflow rejects; tests prove local/private DEF can satisfy a reloc while IMPORT still requires EXPORT. Verify object-link test, typed-local-diff-lint, manifest/lints, full native suite before master.
