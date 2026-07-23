---
title: Own type registry rollback phases
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:11.550005+02:00"
blocks:
  - habu-consume-registry-events-efe7fe5e
---

Problem: type-family.f installs raw combined rollback callbacks, schema owns a separate fallible save, and restore can partly mutate before a field-depth mismatch rejects. Owner: new sealed package TYPE-REGISTRY-ROLLBACK over type-family and type-schema marks only. Expose RESERVE, SAVE, RESTORE-READY, RESTORE, FINALIZE-READY, and FINALIZE. RESERVE grows both type-family and schema frame arenas before either depth changes; SAVE is then infallible. RESTORE-READY and FINALIZE-READY validate both frame depths, field-transaction depth, and saved/live ranges without mutation. RESTORE scrubs retired field rows and restores schema/type marks only after readiness succeeds; FINALIZE releases both frames only after readiness succeeds. Preserve serial tokens and current restoration order: schema before type-family. Remove raw hook installation from type-family.f; do not expose registry cells, add test setters, or add TRUSTED. Acceptance: injected growth failure in either arena leaves both owners byte-identical; readiness failure leaves both frames live and unchanged; nested save/restore/finalize stays lockstep; reversed order mutation fails. Files: src/core/type-family.f, src/core/type-schema.f, focused rollback suites, TRUSTED.md only if rows retire. Smallest check: real checker scope around a product/schema declaration with allocator and field-depth mutations; typed-local and package gates.
