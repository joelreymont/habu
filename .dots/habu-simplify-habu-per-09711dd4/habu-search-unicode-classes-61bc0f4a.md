---
title: Search Unicode classes with one range search
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.999495+03:00"
closed-at: "2026-10-15T18:07:57.531589+03:00"
close-reason: One RANGE-SEARCH over typed tables filled by lo hi R, rows; class-data.f 14,327 -> 5,479 tokens; stripped build passes.
---

Problem: src/lib/unicode/class.f:15-71 contains seven cloned binary searches. class-data.f is 3,498 top-level store statements, about 14k tokens per load. FALSE-VALUE (:13) restates false. Acceptance: one RANGE-SEARCH serves every class; tools/unicode/class-generate.f (:111-129) emits each class as one buffer plus count (a compact table encoding, not compression); class-data.f regenerated; FALSE-VALUE replaced by false; unicode tests green, with load time for class-data.f stated before and after. Files: src/lib/unicode/class.f, src/lib/unicode/class-data.f, tools/unicode/class-generate.f. Verify: unicode test rows; regenerate and diff. Depends: none. Ownership: listed files. Claim: unassigned.
