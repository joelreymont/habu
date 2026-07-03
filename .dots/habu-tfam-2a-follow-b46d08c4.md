---
title: "TFAM 2a follow-ups: ambiguous resolve, hyphen canon, image fixture"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T01:13:31.893346+02:00\""
---

From the 2a destruction review (F3/F5/F7), deferred to avoid type-family.f conflicts while TFAM 3 is in flight. (1) TFAM-RESOLVE/TFAM-FIND-PUBLIC (src/core/type-family.f) silently picks lowest-id public family when two packages export the same public tail - replace first-public-wins with a named E-TFAM-AMBIG throw when two other-package publics tie (own-package family winning is not ambiguity); fixture proving the throw + qualified access still works. (2) TF-CANON?/TF-TAILBYTE? comment says internal hyphen but accepts leading/trailing/double hyphens - enforce internal-only single hyphens (item 8 escaping assumes it; leading/trailing hyphens can break constructor-package injectivity); negative fixtures for -a, a-, a--b. (3) Once TFAM 4 populates families at load, add a real image build+restore fixture covering the grown-buffer persist path in production (F5). Depends: TFAM 3 merge (file ownership).
