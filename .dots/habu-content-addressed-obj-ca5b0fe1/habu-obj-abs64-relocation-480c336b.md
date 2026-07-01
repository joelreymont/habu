---
title: Object abs64 relocation apply
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T21:44:04.980510+02:00\\\"\""
closed-at: "2026-07-01T21:46:35.428441+02:00"
close-reason: "completed: OBJLINK:APPLY now runs CHECK and applies abs64 relocations little-endian into merged text, with fail-closed unknown kind and patch-width bounds tests. Proof: object-link-test, stdlib-manifest-test, typed-local-diff-lint, filemap-lint, host-lint, trust-lint, stale-status-lint, lint-artifacts-fast, full native suite 18538ms <= 40000ms."
---

Problem: OBJLINK now resolves relocation target addresses but does not patch merged text bytes, so the future linker still lacks an output-ready section image. Fix: add OBJLINK:APPLY that calls CHECK, supports abs64 relocations by writing resolved target addresses little-endian into the merged text buffer, rejects unknown kinds, and rejects patches whose width would run past text. Files: lib/object-link.f, lib/object-link-test.f, docs/stdlib.md, lib/std.manifest. Acceptance: abs64 patches write 8 bytes at merged patch address; APPLY is fail-closed for unresolved targets via CHECK; unknown relocation kind fails; patch+8 past text fails; existing layout/symbol/effect/section tests still pass. Verify object-link test, typed-local-diff-lint, manifest/lints, full native suite before master.
