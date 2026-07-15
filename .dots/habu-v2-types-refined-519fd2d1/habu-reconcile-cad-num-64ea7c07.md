---
title: Reconcile CAD-NUM V2 plan state
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T06:41:26.288476+02:00"
---

Full context: MODEL-CAD-V2-PLAN.md:1258, :1277, and :1296 still say TVK-RAW is open and its laundering probe is accepted even though habu-nominal-storage-raw-a3430ef2 closed at 085cf242; :1454 says the numeric-role design still needs an amendment already present; :1251 names removed SUMTYPE machinery; TRUSTED.md:730-747 ties CAD-NUM boundary retirement to TVK-RAW although runtime-predicate mints still require refinement evidence and projections retire only when consuming primitives accept nominal roles. Cause: implementation and plan/trust ownership advanced independently. Fix: reconcile only current proven state, replace removed-syntax wording with unified ENUM/STRUCTURE intent, and assign each TRUSTED boundary its real capability/removal owner without claiming unlanded enforcement. Acceptance: every TVK-RAW/status statement matches closed-dot evidence; no stale accepted probe remains; every CAD-NUM TRUSTED row has an accurate concrete removal condition; no runtime or consumer code changes. Files: MODEL-CAD-V2-PLAN.md, TRUSTED.md. Verify: exact source/dot cross-check, trust lint, host/filemap/dot/status lints.
