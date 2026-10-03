---
title: Link native fragments through relocation providers
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.859388+03:00"
---

Problem: publish.f places one emission at a time and link-x64.f patches rel32/MOVABS by hand (headers); there is no relocation expression model, relaxation fixpoint or prepare/install/commit/retire protocol beyond pending->commit (publish.f:161-180) (PA-r2 §13-§14, P5). Acceptance: NativeFragment and Relocation records with provider-owned bit rules, monotone relaxation, a native publisher with prepare/install_private/commit/abort/retire; L01-L06, L10, L11 pass; existing image and capture gates unchanged. Files: src/link/ (new), src/object/ (new), src/compiler/native/{emission,publish}.f, src/habu/link-x64.f. Verify: full gate; L03 thunk insertion revalidates all ranges. Depends: habu-give-each-backend-b6f7ea4f, habu-make-cross-target-934166cc. Ownership: src/link/, src/object/, publish.f. Lane: dave. Claim: unassigned.
