---
title: Ignore inactive using slots in FILE scope
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T05:10:02.638661+03:00"
---

Matching native 6ef44489 cannot build even an empty stripped application: AOT-MAKER reaches STORE-RANGE, then SCOPE-SAVE serializes every USE-MAX slot. USE-DEPTH is 0 but stale inactive USE-WIDS[0] is 530; REF-WID cannot find a live namespace owner and throws E-NUNIT-PROFILE (-8597). Encode references only for active using slots and canonicalize inactive wire slots to zero. Preserve active scopes, nested loads and genuine missing-owner refusals. Existing stripped-path, stripped GETENV and maker report E2Es reproduce the failure before any code change; run those real builders, executable artifacts and scope replay checks after repair. Do not weaken REF-WID or add a fallback for active unknown WIDs. Source boundary tools/native-file-scope.f; Carl owns integration and independent review.
