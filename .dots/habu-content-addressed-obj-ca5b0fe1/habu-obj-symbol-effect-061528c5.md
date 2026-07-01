---
title: Object symbol effect matching
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:36:15.841428+02:00\\\"\""
closed-at: "2026-07-01T21:39:13.219522+02:00"
close-reason: "completed: OBJLINK now stores export/import/def effects, checks import/export effect equality, exposes effect accessors, and makes direct EXPORT+/IMPORT+ effect-aware. Proof: object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18554ms <= 40000ms."
---

Problem: OBJLINK currently matches imports/exports by name only and discards effect strings from export/import/def rows, so the object cache could link a symbol with the wrong checked stack effect. Fix: store name+effect for export, import, and def tables; make OBJLINK:CHECK require each import's effect to equal the matching export effect; expose effect accessors; update direct EXPORT+/IMPORT+ signatures to accept effects. Relocation target lookup remains by DEF name/address. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: matching import/export effects pass; mismatched effect fails E-OBJ-SCHEMA; public effect accessors return stored effects; direct EXPORT+/IMPORT+ APIs are effect-aware; existing def/reloc/layout tests still pass. Verify object-link test, typed-local-diff-lint, manifest/lints, full native suite before master.
