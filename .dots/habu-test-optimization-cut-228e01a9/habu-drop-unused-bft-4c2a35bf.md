---
title: Drop unused BFT-DOC-OUT$
status: closed
priority: 4
issue-type: task
created-at: "\"2026-10-01T17:24:37.400249+02:00\""
closed-at: "2026-10-01T17:44:01.298165+02:00"
close-reason: Fixed by plmxzykt aa6dc643 (review 186 ACCEPT)
---

Problem: tools/build-fixpoint-snapshot-test.f:75 defines BFT-DOC-OUT$ and nothing calls it; snapcell (dot 9f60f293, c52385cf) deleted its only reader with the probe block (review 176). Dead code. Acceptance: the word and anything only it used are gone; `rg -n BFT-DOC-OUT` finds nothing; the build-fixpoint-snapshot row still passes through its real load path. Base: c52385cf or later. Files: tools/build-fixpoint-snapshot-test.f.
