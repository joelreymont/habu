---
title: Make native migration publication fail closed by default
status: active
priority: 2
issue-type: task
created-at: "2026-08-24T17:25:42.420370+02:00"
---

Problem: Public NMIGRATE:DEFINE, NMIGRATE:NEXT, and NMIGRATE:DEFINE-DATA publish the old emitter before the checked native chain succeeds, contrary to docs/compiler-ir-design.md; DEFINE-HELD is a test-only duplicate while non-held fallback branches remain live. Acceptance: all three public authoring entries use the existing hold transaction by default; a rejected definition leaves no word, signature, code/log publication, held state, or poisoned next run; a successful default definition executes and reports NPUB old start and length as zero; AOT chain acceptance proves its existing default DEFINE path retains no old-emitter fallback; DEFINE-HELD and dead non-held/fallback state are deleted after a real consumer census; MEASURE-HELD remains the no-publication measurement entry. Files: src/compiler/native/migrate.f, focused migrate/stream tests, AOT chain capture acceptance, and only real callers forced by deleting DEFINE-HELD. Verify: forced fixpoint refresh; focused native migrate, stream, publication, AOT capture/judge/fuzz suites; maki, native, error-code, and dot gates. Depends: exact robustness base commit 56068f6b6c1099d2b18851ec4fc33e52189709f9; no open-dot dependency. Ownership: native migration authoring/publication entries and their direct tests only; no overlap with unrelated active leaves.
