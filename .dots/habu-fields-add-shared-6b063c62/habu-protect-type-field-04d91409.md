---
title: Protect TYPE-FIELD private state
status: active
priority: 1
issue-type: task
created-at: "2026-07-17T12:11:01.362381+02:00"
---

Review finding: src/core/type-family.f:712-1130 leaves PF constants, variables, arenas, and raw record helpers in the global data namespace; src/core/internal-mark.f exempts data records, so bare source can obtain and mutate registry cells beside the documented TYPE-FIELD package. Fix: place all PF storage/constants/helpers in TYPE-FIELD private scope, provide the smallest colon-only internal builder seam needed by sumtype/type-family/tests, retain only COUNT/NO-VARIANT/FIND/EACH/reflection as checker-visible public API, protect both WIDs, migrate every consumer atomically, and add bare-load/internal-word negatives for raw state and builder names. No compatibility aliases. Acceptance: raw PF data/mutator names reject on --load and stdin; documented TYPE-FIELD API remains checked; type-family, declaration, rollback, internal-word, seal, fixpoint, trust, host, filemap gates green. Files: src/core/type-family.f:712-1154, src/core/sumtype.f, src/core/checker.f, test/type-family-suite.f, test/type-family-rollback-suite.f, test/internal-word-gate.f, docs/type-families.md. Claim: agent=sol workspace=.jj-ws/fields-layout-fix.
