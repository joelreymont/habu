---
title: Check test/name-length-test.f through check.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T19:07:10.814232+02:00"
---

Problem (lane 355 r4-resnames, a777a5c2): with its reserved FIELD probe renamed, tools/check.f on test/name-length-test.f stops at the pre-verifier, rc 74 'TRUST missing signature string', at :349 'CEIL$ SIG$ TRUST' (a top-level TRUST with computed arguments, on purpose). Acceptance: the test passes check.f (state the computed TRUST another way the pre-verifier reads, or give check.f a named, located refusal for a computed TRUST signature rather than a die 74), seen failing first. Files: test/name-length-test.f, src/habu/verify-source.f or tools/check-core.f.
